# mortality_sample.R
library(sf)
library(dplyr)
library(arrow)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)

# ---- CONFIG -------------------------------------------------------------------
hurr_path   <- "data/hurricanes.gpkg"
hurr_layer  <- "hurricanes"
mort_path   <- "data/all_deaths_hurricane.csv"
lon_col     <- "final_lon"
lat_col     <- "final_lat"
date_col    <- "DATE_OF_DEATH"
buffer_km   <- 10
sample_n    <- 1000       # per-storm
date_window <- 1         # ± days (set to 0 to disable)

landfall_tbl <- tibble::tibble(
  storm_canon = c("andrew", "charley", "ian"),
  landfall    = as.Date(c("1992-08-24","2004-08-13","2022-09-28"))
)

canon <- function(x) {
  x %>% str_to_lower() %>% str_remove("^hurricane\\s+") %>% str_squish()
}

# ---- READ HURRICANES ----------------------------------------------------------
hurr <- st_read(hurr_path, layer = hurr_layer, quiet = TRUE)

# If geometry column is 'geom', make it active
if ("geom" %in% names(hurr)) st_geometry(hurr) <- "geom"

hurr <- hurr %>%
  st_make_valid() %>%
  mutate(storm_canon = canon(.data$NAME)) %>%
  left_join(landfall_tbl, by = "storm_canon")

if (any(is.na(hurr$landfall))) {
  warning("No landfall date for: ",
          paste(unique(hurr$NAME[is.na(hurr$landfall)]), collapse = ", "),
          ". Date filtering will be skipped for those.")
}

# Buffer per storm in meters, then back to WGS84
FL_ALBERS <- 3086L
hurr_buf <- hurr %>%
  st_transform(FL_ALBERS) %>%
  group_by(NAME, storm_canon, landfall) %>%
  summarise(.groups = "drop") %>%
  st_buffer(buffer_km * 1000) %>%
  st_transform(4326)

# Per-storm bbox (use active geometry accessor)
bbox_tbl <- hurr_buf %>%
  mutate(bbox = purrr::map(sf::st_geometry(.), sf::st_bbox)) %>%
  unnest_wider(bbox)
# columns: NAME, storm_canon, landfall, xmin, ymin, xmax, ymax

# ---- HELPERS ------------------------------------------------------------------
parse_date_relaxed <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    if (max(x, na.rm = TRUE) > 10^7) return(as.Date(as.POSIXct(x, origin = "1970-01-01", tz = "UTC")))
    return(as.Date(x, origin = "1970-01-01"))
  }
  # Character: try multiple formats
  d <- suppressWarnings(lubridate::ymd(x))
  miss <- is.na(d)
  if (any(miss)) d[miss] <- suppressWarnings(lubridate::mdy(x[miss]))
  miss <- is.na(d)
  if (any(miss)) d[miss] <- suppressWarnings(lubridate::ymd_hms(x[miss], quiet = TRUE))
  miss <- is.na(d)
  if (any(miss)) d[miss] <- suppressWarnings(lubridate::mdy_hms(x[miss], quiet = TRUE))
  as.Date(d)
}

# Classify effect relative to landfall (only -1/0/+1 labeled)
effect_from_offset <- function(offset_days) {
  dplyr::case_when(
    offset_days == -1L ~ "threat",
    offset_days ==  0L ~ "impact",
    offset_days ==  1L ~ "cleanup",
    TRUE               ~ NA_character_
  )
}

read_bbox_then_filter <- function(storm_row) {
  stnm <- storm_row$NAME
  lf   <- storm_row$landfall[[1]]
  dr   <- if (!is.na(lf) && date_window > 0) c(lf - date_window, lf + date_window) else NULL
  bb   <- unlist(storm_row[c("xmin","ymin","xmax","ymax")], use.names = TRUE)
  
  # Arrow dataset (CSV or Parquet)
  ds <- if (grepl("\\.parquet$", mort_path, ignore.case = TRUE)) {
    arrow::open_dataset(mort_path, format = "parquet")
  } else {
    arrow::open_dataset(mort_path, format = "csv")
  }
  
  lon <- as.name(lon_col); lat <- as.name(lat_col); dt <- as.name(date_col)
  
  # Coarse bbox filter in Arrow
  df <- ds %>%
    dplyr::select(!!lon, !!lat, !!dt) %>%
    dplyr::filter(!!lon >= bb["xmin"], !!lon <= bb["xmax"],
                  !!lat >= bb["ymin"], !!lat <= bb["ymax"]) %>%
    dplyr::collect()
  
  if (!nrow(df)) return(st_sf(df, geometry = st_sfc(), crs = 4326))
  
  df <- df %>%
    dplyr::rename(lon = !!lon, lat = !!lat, date_raw = !!dt) %>%
    dplyr::filter(!is.na(lon), !is.na(lat))
  
  # Parse date, then precise spatial filter
  df$date <- parse_date_relaxed(df$date_raw)
  
  pts <- st_as_sf(df, coords = c("lon","lat"), crs = 4326)
  geom_storm <- hurr_buf[hurr_buf$NAME == stnm, ]
  idx_spatial <- lengths(st_intersects(pts, geom_storm)) > 0
  pts_in <- pts[idx_spatial, ]
  
  # Date window (inclusive)
  if (!is.null(dr)) {
    pts_in <- pts_in %>% filter(!is.na(date), date >= dr[1], date <= dr[2])
  }
  
  # Effect classification (relative to THIS storm's landfall)
  if (!is.na(lf) && nrow(pts_in)) {
    off <- as.integer(pts_in$date - lf)
    pts_in$storm_effect <- effect_from_offset(off)
  } else {
    pts_in$storm_effect <- NA_character_
  }
  
  # Sample
  if (nrow(pts_in) > sample_n) {
    set.seed(42)
    pts_in <- pts_in[sample(seq_len(nrow(pts_in)), sample_n), ]
  }
  
  pts_in$storm <- as.character(stnm)
  pts_in
}

# ---- RUN ----------------------------------------------------------------------
samples <- pmap(
  bbox_tbl[, c("NAME","storm_canon","landfall","xmin","ymin","xmax","ymax")],
  ~read_bbox_then_filter(tibble::tibble(NAME=..1, storm_canon=..2, landfall=..3,
                                        xmin=..4, ymin=..5, xmax=..6, ymax=..7))
)

mortality_sample <- do.call(rbind, samples)

dir.create("data", showWarnings = FALSE)
saveRDS(mortality_sample, "data/mortality_sample.rds")

message("Wrote ", nrow(mortality_sample), " rows to data/mortality_sample.rds")
print(table(mortality_sample$storm, mortality_sample$storm_effect, useNA = "ifany"))
