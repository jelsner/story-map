# merged_zcta.R
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

# Optional: GEOS tends to be faster than s2 for these ops
old_s2 <- sf_use_s2()
sf_use_s2(FALSE)

# --- 0) Read deaths (points) ---------------------------------------------------
# If you're using here::here, make sure library(here) is loaded, or use a plain path.
deaths_sf <- sf::st_read(dsn = "data", layer = "All_Deaths_Storm_Effects", quiet = TRUE)
colnames(deaths_sf)[1:5] <- c("Death_ID", "Death_Date", "Storm_Name",
                              "Storm_Category", "Storm_Effect")
deaths_sf$Storm_Effect <- tolower(deaths_sf$Storm_Effect)

# Keep only the columns we actually need before the heavy join
deaths_sf <- deaths_sf |>
  select(Death_ID, Death_Date, Storm_Name, Storm_Category, Storm_Effect, geometry)

# --- 1) Florida boundary (WGS84) ----------------------------------------------
florida <- states(cb = TRUE, year = 2020) |>
  filter(STUSPS == "FL") |>
  st_transform(4326)

# --- 2) Get ZCTAs but avoid the full national heavy set -----------------------
# Strategy A (fastest): request only likely Florida prefixes (32–34), then bind
zcta_chunks <- lapply(c("32","33","34"), function(prefix) {
  zctas(cb = TRUE, starts_with = prefix, year = 2020)
})
zctas_partial <- do.call(rbind, zcta_chunks)

# If you prefer not to rely on prefixes, you can fall back to this (slower):
# zctas_partial <- zctas(cb = TRUE, year = 2020)

# Make sure CRS matches Florida
zctas_partial <- st_transform(zctas_partial, 4326)

# Now *subset only those ZCTAs that touch Florida* (no geometry cutting!)
# st_filter returns the original polygons; it does not compute intersections.
zctas_fl <- st_filter(zctas_partial, florida, .predicate = st_intersects)

# Optional but helpful: ensure valid geometries
zctas_fl <- st_make_valid(zctas_fl)

# --- 3) Pre-crop death points to Florida bbox before the join -----------------
# This trims the candidate points dramatically.
fl_bb <- st_as_sfc(st_bbox(florida), crs = 4326)

# Align CRS and crop
deaths_sf <- st_transform(deaths_sf, 4326)
deaths_sf <- st_crop(deaths_sf, fl_bb)

# --- 4) Spatial join: points -> ZCTA id (subset geometry, no cutting) ---------
# Only bring GEOID20 across; use st_within predicate (points within polygons).
deaths_with_zcta <- st_join(
  deaths_sf,
  zctas_fl[, c("GEOID20")],
  join = st_within,
  left = TRUE
)

# --- 5) Count deaths by ZCTA × effect -----------------------------------------
deaths_with_zcta <- deaths_with_zcta |>
  mutate(
    Storm_Effect = case_when(
      Storm_Effect %in% c("threat","impact","cleanup") ~ Storm_Effect,
      TRUE ~ NA_character_
    )
  )

zcta_effect_counts <- deaths_with_zcta |>
  st_drop_geometry() |>
  filter(!is.na(GEOID20), !is.na(Storm_Effect)) |>
  count(GEOID20, Storm_Effect, name = "n") |>
  tidyr::pivot_wider(
    names_from = Storm_Effect,
    values_from = n,
    values_fill = 0
  ) |>
  mutate(total = threat + impact + cleanup)

# --- 6) Make polygons Leaflet-ready (lightweight & clean) ---------------------
FL_ALBERS <- 3086L

zctas_fl_poly <- zctas_fl |>
  select(GEOID20, geometry) |>
  group_by(GEOID20) |>
  summarise(.groups = "drop") |>
  st_transform(FL_ALBERS) |>
  st_simplify(dTolerance = 75, preserveTopology = TRUE) |>
  st_make_valid() |>
  st_collection_extract("POLYGON") |>
  st_cast("MULTIPOLYGON") |>
  st_transform(4326)

zctas_fl_counts <- zctas_fl_poly |>
  left_join(zcta_effect_counts, by = "GEOID20") |>
  mutate(across(c(threat, impact, cleanup, total), ~coalesce(.x, 0L))) |>
  filter(!st_is_empty(geometry))

# --- 7) Save -------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(zctas_fl_counts, "data/zcta_mortality_counts.rds")

# restore s2 setting
sf_use_s2(old_s2)
