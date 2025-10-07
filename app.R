# app.R — minimal, robust story map (no sf), clickable chapters + progress bar
options(shiny.fullstacktrace = TRUE)

library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(bslib)
library(magrittr)
library(RColorBrewer)

# ---- Helper: make polygon layers Leaflet-safe ---------------------------------
poly_only_4326 <- function(x, geom_col = NULL) {
  if (!is.null(geom_col) && geom_col %in% names(x)) sf::st_geometry(x) <- geom_col
  x <- sf::st_make_valid(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- suppressWarnings(sf::st_cast(x, "MULTIPOLYGON"))
  x <- x[!sf::st_is_empty(x), ]
  crs <- sf::st_crs(x)
  if (is.na(crs$epsg) || crs$epsg != 4326) x <- sf::st_transform(x, 4326)
  x
}

# ---- Chapters -----------------------------------------------------------------
chapters <- data.frame(
  id    = c("intro","storms","mortality","merge","model","hotspots"),
  title = c(
    "Introduction",
    "Chapter 1: Florida tropical cyclones",
    "Chapter 2: Mortality events",
    "Chapter 3: Merging datasets",
    "Chapter 4: Modeling relative all-cause mortality",
    "Chapter 5: Hotspot analysis"
  ),
  text  = c(
    "We assess the effects of tropical cyclones (TCs) on mortality in Florida.",
    "Tropical cyclone model applied to three strong tropical cyclones affecting the state.",
    "Mortality events associated with these three tropical cyclones.",
    "Storm effects on mortality by zip code tabulation areas (1985-2022).",
    "Model specification and rate ratio surfaces.",
    "Clusters of high impacts."
  ),
  lng   = c(-83.5, -83.5, -82.3, -82.3, -81.6, -81.4),
  lat   = c(28.0,   28.0,   27.0,   27.8,   27.2, 27.1),
  zoom  = c(6,      6,      7,      7,      7,   8),
  layer = c(NA, "storms", "mort", "merge", "model", "hotspot"),
  stringsAsFactors = FALSE
)

# ---- Data ------------------------------------------
# Hurricanes
hurricanes_raw <- sf::st_read("data/hurricanes.gpkg", layer = "hurricanes", quiet = TRUE)
hurricanes <- poly_only_4326(hurricanes_raw, geom_col = if ("geom" %in% names(hurricanes_raw)) "geom" else NULL)
hurricanes$NAME <- as.character(hurricanes$NAME)

# Mortality sample → plain df with lng/lat
mort_sample <- readRDS("data/mortality_sample.rds")
coords <- sf::st_coordinates(mort_sample)
mort_df <- mort_sample %>%
  sf::st_drop_geometry() %>%
  mutate(
    lon = coords[,1], lat = coords[,2],
    storm = as.character(storm),
    storm_effect = as.character(storm_effect),
    date = as.Date(date)
  )

# ZCTA counts
zcta_counts <- readRDS("data/zcta_mortality_counts.rds") %>% poly_only_4326()
zcta_counts <- zcta_counts %>%
  mutate(
    GEOID20 = as.character(GEOID20),
    threat  = as.numeric(threat),
    impact  = as.numeric(impact),
    cleanup = as.numeric(cleanup),
    total   = as.numeric(total)
  )

# RR by ZCTA: read raw, flatten, then join to zcta_counts geometry
pluck1_num <- function(x) {
  if (!is.list(x)) return(suppressWarnings(as.numeric(x)))
  vapply(x, function(el) {
    if (is.null(el) || length(el) == 0) return(NA_real_)
    if (is.list(el)) el <- el[[1]]
    suppressWarnings(as.numeric(el))
  }, numeric(1))
}

rr_raw <- readRDS("data/zcta_results.rds")
if (!inherits(rr_raw, "sf")) rr_raw <- sf::st_as_sf(rr_raw)

rr_attr <- rr_raw |>
  sf::st_drop_geometry() |>
  mutate(
    GEOID20    = as.character(GEOID20),
    RR_Threat  = pluck1_num(RR_Threat),
    RR_Impact  = pluck1_num(RR_Impact),
    RR_Cleanup = pluck1_num(RR_Cleanup)
  )

rr_zcta <- zcta_counts |>
  select(GEOID20, geometry) |>
  left_join(rr_attr, by = "GEOID20") |>
  poly_only_4326()

# ---- Palettes -----------------------------------------------------------------
max_count <- max(zcta_counts$threat, zcta_counts$impact, zcta_counts$cleanup, na.rm = TRUE)
bins <- pretty(c(0, max_count), n = 7)

storm_pal <- colorFactor(
  palette = c("Andrew"="#1f77b4", "Charley"="#ff7f0e", "Ian"="#2ca02c"),
  domain  = hurricanes$NAME
)

pal_effect <- colorBin(
  palette = "YlOrRd",
  domain  = c(0, max_count),
  bins    = bins,
  na.color = "#cccccc"
)

# RR palette from data, symmetric-ish around 1
rr_all <- unlist(rr_zcta[, c("RR_Threat","RR_Impact","RR_Cleanup")], use.names = FALSE)
rr_all <- rr_all[is.finite(rr_all)]
if (length(rr_all)) {
  max_dev <- max(abs(rr_all - 1), na.rm = TRUE)
  upper <- 1 + max_dev * 1.02
  lower <- max(1 - max_dev * 1.02, min(rr_all, na.rm = TRUE))
  base_breaks <- c(0.7, 0.8, 0.9, 0.95, 1.0, 1.05, 1.1, 1.2, 1.3)
  rr_breaks <- sort(unique(c(lower, base_breaks[base_breaks >= lower & base_breaks <= upper], upper)))
} else {
  rr_breaks <- c(0.9, 1.0, 1.1)
}
pal_rr <- leaflet::colorBin(
  palette = rev(RColorBrewer::brewer.pal(11, "RdBu")),
  domain  = rr_all,
  bins    = rr_breaks,
  na.color = "#cccccc"
)

# Florida bbox for context
FL_XMIN <- -87.7; FL_XMAX <- -79.8; FL_YMIN <- 24.3; FL_YMAX <- 31.1

# ---- UI -----------------------------------------------------------------------
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      body { overflow-y: hidden; }
      .grid { display: grid; grid-template-columns: 48% 52%; height: 100vh; }
      #map { height: 100vh; }
      .rhs { position: relative; height: 100vh; display:flex; flex-direction:column; }
      .progress-wrap { height: 6px; background: #e9ecef; position: sticky; top: 0; z-index: 5; }
      .progress-bar { height: 100%; width: 0%; background: #1f77b4; transition: width 200ms ease; }
      .chapters { overflow-y: auto; padding: 1rem 1.25rem; flex: 1; background: #fff; }
      .chapter { margin: 1.25rem 0; padding: 1rem; border-left: 4px solid #e5e7eb; cursor: pointer;
                 border-radius: 0.25rem; background: #fff; }
      .chapter:hover { background: #f8fbff; }
      .chapter.active { border-left-color: #1f77b4; background: #f4f9ff; }
      .nav-controls { display:flex; justify-content: space-between; gap: .5rem; padding: .75rem 1rem;
                      border-top: 1px solid #edf2f7; background:#fff; position: sticky; bottom: 0; z-index: 4; }
      .nav-controls .btn { border-radius: 9999px; }
      h2 { margin: 0 0 .25rem 0; font-size: 1.1rem; }
      p { margin: 0; }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        const list   = document.querySelector('.chapters');
        const items  = Array.from(document.querySelectorAll('.chapter'));
        const bar    = document.querySelector('.progress-bar');

        function setActiveById(id){
          items.forEach(el => el.classList.toggle('active', el.dataset.id === id));
          const idx = items.findIndex(el => el.dataset.id === id);
          const el  = items[idx];
          if (el) el.scrollIntoView({block:'center', behavior:'smooth'});
          if (window.Shiny) Shiny.setInputValue('current_chapter', id, {priority:'event'});
          if (idx >= 0) bar.style.width = Math.round(((idx+1) / items.length) * 100) + '%';
        }

        items.forEach(el => el.addEventListener('click', () => setActiveById(el.dataset.id)));

        const obs = new IntersectionObserver((entries)=>{
          entries.forEach(e=>{ if(e.isIntersecting) setActiveById(e.target.dataset.id); });
        }, {root: list, threshold: 0.6});
        items.forEach(el => obs.observe(el));

        document.addEventListener('keydown', (ev)=>{
          const prevKeys = ['ArrowUp','ArrowLeft','PageUp'];
          const nextKeys = ['ArrowDown','ArrowRight','PageDown',' '];
          if (![...prevKeys,...nextKeys].includes(ev.key)) return;
          ev.preventDefault();
          const active = items.findIndex(x => x.classList.contains('active'));
          if (prevKeys.includes(ev.key) && active > 0) setActiveById(items[active-1].dataset.id);
          if (nextKeys.includes(ev.key) && active < items.length-1) setActiveById(items[active+1].dataset.id);
        });

        setTimeout(()=>setActiveById(items[0].dataset.id), 50);
      });
    "))
  ),
  div(class="grid",
      leafletOutput("map", height = "100%"),
      div(class="rhs",
          div(class="progress-wrap", div(class="progress-bar")),
          div(class="chapters",
              lapply(seq_len(nrow(chapters)), function(i){
                ch <- chapters[i,]
                div(class="chapter", `data-id`=ch["id"],
                    h2(ch["title"]), p(ch["text"]))
              })
          ),
          div(class="nav-controls",
              actionButton("prev", "◀ Prev", class="btn btn-outline-primary"),
              actionButton("next_btn", "Next ▶", class="btn btn-primary")
          )
      )
  )
)

# ---- Server -------------------------------------------------------------------
server <- function(input, output, session){
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRectangles(lng1 = FL_XMIN, lat1 = FL_YMIN, lng2 = FL_XMAX, lat2 = FL_YMAX,
                    fillOpacity = 0.04, weight = 1, color = "#1f77b4", group = "outline") %>%
      
      # Hurricanes
      addPolygons(
        data = hurricanes,
        fillColor = ~storm_pal(NAME),
        color     = ~storm_pal(NAME),
        weight    = 1, opacity = 1, fillOpacity = 0.35,
        label     = ~if ("year" %in% names(hurricanes)) paste0(NAME, " (", year, ")") else NAME,
        group     = "storms",
        highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
      ) %>%
      
      # ZCTA choropleths
      addPolygons(
        data = zcta_counts,
        fillColor = ~pal_effect(threat),
        color = "#555", weight = 0.3, opacity = 1, fillOpacity = 0.7,
        label = ~sprintf("ZCTA %s — Threat: %d", GEOID20, threat),
        popup = ~sprintf("<b>ZCTA %s</b><br/>Threat: %d<br/>Impact: %d<br/>Cleanup: %d<br/>Total: %d",
                         GEOID20, threat, impact, cleanup, total),
        group = "ZCTA: Threat"
      ) %>%
      addPolygons(
        data = zcta_counts,
        fillColor = ~pal_effect(impact),
        color = "#555", weight = 0.3, opacity = 1, fillOpacity = 0.7,
        label = ~sprintf("ZCTA %s — Impact: %d", GEOID20, impact),
        popup = ~sprintf("<b>ZCTA %s</b><br/>Threat: %d<br/>Impact: %d<br/>Cleanup: %d<br/>Total: %d",
                         GEOID20, threat, impact, cleanup, total),
        group = "ZCTA: Impact"
      ) %>%
      addPolygons(
        data = zcta_counts,
        fillColor = ~pal_effect(cleanup),
        color = "#555", weight = 0.3, opacity = 1, fillOpacity = 0.7,
        label = ~sprintf("ZCTA %s — Cleanup: %d", GEOID20, cleanup),
        popup = ~sprintf("<b>ZCTA %s</b><br/>Threat: %d<br/>Impact: %d<br/>Cleanup: %d<br/>Total: %d",
                         GEOID20, threat, impact, cleanup, total),
        group = "ZCTA: Cleanup"
      ) %>%
      
      # RR choropleths
      addPolygons(
        data = rr_zcta,
        fillColor = ~pal_rr(RR_Threat),
        color = "#555", weight = 0.3, opacity = 1, fillOpacity = 0.7,
        label = ~sprintf("ZCTA %s — RR (Threat): %.2f", GEOID20, RR_Threat),
        popup = ~sprintf("<b>ZCTA %s</b><br/>RR (Threat): %.2f", GEOID20, RR_Threat),
        group = "RR: Threat"
      ) %>%
      addPolygons(
        data = rr_zcta,
        fillColor = ~pal_rr(RR_Impact),
        color = "#555", weight = 0.3, opacity = 1, fillOpacity = 0.7,
        label = ~sprintf("ZCTA %s — RR (Impact): %.2f", GEOID20, RR_Impact),
        popup = ~sprintf("<b>ZCTA %s</b><br/>RR (Impact): %.2f", GEOID20, RR_Impact),
        group = "RR: Impact"
      ) %>%
      addPolygons(
        data = rr_zcta,
        fillColor = ~pal_rr(RR_Cleanup),
        color = "#555", weight = 0.3, opacity = 1, fillOpacity = 0.7,
        label = ~sprintf("ZCTA %s — RR (Cleanup): %.2f", GEOID20, RR_Cleanup),
        popup = ~sprintf("<b>ZCTA %s</b><br/>RR (Cleanup): %.2f", GEOID20, RR_Cleanup),
        group = "RR: Cleanup"
      ) %>%
      
      # Mortality points
      addCircleMarkers(
        data = mort_df,
        lng = ~lon, lat = ~lat,
        radius = 3, opacity = 0.7, fillOpacity = 0.35,
        group  = "mort",
        popup  = ~sprintf("<b>%s</b><br/>Date: %s<br/>Effect: %s",
                          storm, as.character(date), storm_effect),
        clusterOptions = if (nrow(mort_df) > 2000) markerClusterOptions() else NULL
      ) %>%
      
      # Hide everything at load (Introduction = clean map)
      hideGroup("storms") %>% hideGroup("mort") %>%
      hideGroup("ZCTA: Threat") %>% hideGroup("ZCTA: Impact") %>% hideGroup("ZCTA: Cleanup") %>%
      hideGroup("RR: Threat") %>% hideGroup("RR: Impact") %>% hideGroup("RR: Cleanup") %>%
      setView(lng = chapters$lng[1], lat = chapters$lat[1], zoom = chapters$zoom[1])
  })
  
  observeEvent(input$current_chapter, ignoreInit = TRUE, {
    ch <- chapters[chapters$id == input$current_chapter, , drop = FALSE]
    if (!nrow(ch)) return(NULL)
    
    # Hide all overlays & remove any legends/controls
    proxy <- leafletProxy("map") %>%
      flyTo(lng = ch$lng[[1]], lat = ch$lat[[1]], zoom = ch$zoom[[1]]) %>%
      hideGroup("storms") %>% hideGroup("mort") %>%
      hideGroup("ZCTA: Threat") %>% hideGroup("ZCTA: Impact") %>% hideGroup("ZCTA: Cleanup") %>%
      hideGroup("RR: Threat") %>% hideGroup("RR: Impact") %>% hideGroup("RR: Cleanup") %>%
      clearControls()
    
    id <- ch$id[[1]]
    if (id == "storms") {
      proxy %>% showGroup("storms") %>%
        addLegend("bottomright", pal = storm_pal, values = hurricanes$NAME,
                  title = "Hurricanes", opacity = 1)
      
    } else if (id == "mortality") {
      proxy %>% showGroup("mort")
      
    } else if (id == "merge") {
      proxy %>%
        showGroup("ZCTA: Threat") %>%
        addLegend("bottomright", pal = pal_effect, values = c(0, max_count),
                  title = "Deaths per ZCTA", opacity = 1) %>%
        addLayersControl(
          overlayGroups = c("ZCTA: Threat", "ZCTA: Impact", "ZCTA: Cleanup"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } else if (id == "model") {
      proxy %>%
        showGroup("RR: Threat") %>%
        addLegend("bottomright", pal = pal_rr, values = rr_breaks,
                  title = "Relative Rate (RR)", opacity = 1) %>%
        addLayersControl(
          overlayGroups = c("RR: Threat", "RR: Impact", "RR: Cleanup"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } else if (id %in% c("hotspots", "intro")) {
      # cleared map: nothing to add
    }
  })
  
  observeEvent(input$prev,     session$sendCustomMessage("goChapter", "prev"))
  observeEvent(input$next_btn, session$sendCustomMessage("goChapter", "next"))
}

shinyApp(ui, server)
