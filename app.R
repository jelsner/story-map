# app.R — minimal, robust story map (no sf), clickable chapters + progress bar
options(shiny.fullstacktrace = TRUE)

library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(bslib)
library(magrittr)   # for %>%

# ---- Chapters -----------------------------------------------------------------
chapters <- data.frame(
  id    = c("intro","storms","mortality","merge","model","hotspots"),
  title = c(
    "Introduction",
    "Chapter 1: Florida's recent hurricanes",
    "Chapter 2: Mortality data",
    "Chapter 3: Merging the events",
    "Chapter 4: Modeling relative all-cause mortality",
    "Chapter 5: Hotspot analysis"
  ),
  text  = c(
    "We built a comprehensive spatial-temporal model to assess the effects of tropical cyclones (TCs) on mortality in Florida.",
    "Focus on recent hurricane tracks/landfalls.",
    "Show all-cause mortality data sources and spatial coverage.",
    "Demonstrate the event–exposure join (counties/ZCTAs × TC footprints).",
    "Preview model specification and show an RR surface.",
    "Analyze areas susceptible to effects."
  ),
  lng   = c(-83.5, -83.5, -82.3, -82.3, -81.6, -81.4),
  lat   = c(28.0,   28.0,   27.0,   27.8,   27.2, 27.1),
  zoom  = c(6,      6,      7,      7,      7,   8),
  layer = c(NA, "storms", "mort", "merge", "model", "hotspot"),
  stringsAsFactors = FALSE
)

# ---- Data ------------------------------------------
hurricanes <- sf::st_read("data/hurricanes.gpkg", layer = "hurricanes", quiet = TRUE) |>
  sf::st_make_valid()

# Simple palette by storm name
storm_pal <- leaflet::colorFactor(
  palette = c("Andrew"="#1f77b4", "Charley"="#ff7f0e", "Ian"="#2ca02c"),
  domain  = hurricanes$storm
)

mort_sample <- readRDS("data/mortality_sample.rds")

merge_df <- data.frame(
  name=c("Joined cell 1","Joined cell 2","Joined cell 3"),
  lng=c(-82.9,-82.1,-81.3), lat=c(27.6,27.9,27.1)
)
model_df <- data.frame(
  label=paste0("RR=", c(0.92,1.08,1.23,0.76,1.41)),
  lng=c(-82.8,-82.2,-81.8,-81.2,-80.9), lat=c(27.5,27.7,27.3,27.9,27.4)
)

# Florida bounding box for context (no sf)
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
      .chapter {
        margin: 1.25rem 0; padding: 1rem; border-left: 4px solid #e5e7eb; cursor: pointer;
        border-radius: 0.25rem; background: #fff;
      }
      .chapter:hover { background: #f8fbff; }
      .chapter.active { border-left-color: #1f77b4; background: #f4f9ff; }
      .nav-controls {
        display:flex; justify-content: space-between; gap: .5rem; padding: .75rem 1rem; border-top: 1px solid #edf2f7; background:#fff;
        position: sticky; bottom: 0; z-index: 4;
      }
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

        // Click to activate
        items.forEach(el => el.addEventListener('click', () => setActiveById(el.dataset.id)));

        // Scroll-activate (when 60% visible)
        const obs = new IntersectionObserver((entries)=>{
          entries.forEach(e=>{ if(e.isIntersecting) setActiveById(e.target.dataset.id); });
        }, {root: list, threshold: 0.6});
        items.forEach(el => obs.observe(el));

        // Keyboard navigation
        document.addEventListener('keydown', (ev)=>{
          const prevKeys = ['ArrowUp','ArrowLeft','PageUp'];
          const nextKeys = ['ArrowDown','ArrowRight','PageDown',' '];
          if (![...prevKeys,...nextKeys].includes(ev.key)) return;
          ev.preventDefault();
          const active = items.findIndex(x => x.classList.contains('active'));
          if (prevKeys.includes(ev.key) && active > 0) setActiveById(items[active-1].dataset.id);
          if (nextKeys.includes(ev.key) && active < items.length-1) setActiveById(items[active+1].dataset.id);
        });

        // Buttons from Shiny
        if (window.Shiny) {
          Shiny.addCustomMessageHandler('goChapter', function(dir){
            const active = items.findIndex(x => x.classList.contains('active'));
            if (dir === 'prev' && active > 0) setActiveById(items[active-1].dataset.id);
            if (dir === 'next' && active < items.length-1) setActiveById(items[active+1].dataset.id);
          });
        }

        // Initialize first active
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
      # Florida outline rectangle (context)
      addRectangles(lng1 = FL_XMIN, lat1 = FL_YMIN, lng2 = FL_XMAX, lat2 = FL_YMAX,
                    fillOpacity = 0.04, weight = 1, color = "#1f77b4", group = "outline") %>%
      # Pre-add groups; hide initially
      # --- Hurricanes polygons as one overlay group: "storms" ---
      addPolygons(
        data = hurricanes,
        fillColor   = ~storm_pal(NAME),
        color       = ~storm_pal(NAME),
        weight      = 1,
        opacity     = 1,
        fillOpacity = 0.35,
        label       = ~paste0(NAME, " (", year, ")"),
        group       = "storms",
        highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend(
        "bottomright",
        pal     = storm_pal,
        values  = hurricanes$NAME,
        title   = "Hurricanes",
        group   = "storms",
        opacity = 1
      ) %>%

      addCircleMarkers(
        data = mort_sample,
        radius = 3, opacity = 0.7, fillOpacity = 0.35,
        group  = "mort",
        popup  = ~sprintf("<b>%s</b><br/>Date: %s<br/>Effect: %s",
                          storm, as.character(date), storm_effect),
        clusterOptions = if (nrow(mort_sample) > 2000) markerClusterOptions() else NULL
      ) %>%
      addCircleMarkers(data = merge_df,  lng = ~lng, lat = ~lat, label = ~name, radius = 6, group = "merge") %>%
      addCircleMarkers(data = model_df,  lng = ~lng, lat = ~lat, label = ~label, radius = 6, group = "model") %>%
      hideGroup("storms") %>%
      hideGroup("mort") %>%
      hideGroup("merge") %>%
      hideGroup("model") %>%
      setView(lng = chapters$lng[1], lat = chapters$lat[1], zoom = chapters$zoom[1])
  })
  
  observeEvent(input$current_chapter, ignoreInit = TRUE, {
    ch <- chapters[chapters$id == input$current_chapter, , drop = FALSE]
    if (nrow(ch) != 1) return(NULL)
    
    proxy <- leafletProxy("map") %>%
      flyTo(lng = ch$lng[[1]], lat = ch$lat[[1]], zoom = ch$zoom[[1]]) %>%
      hideGroup("storms") %>%
      hideGroup("mort") %>%
      hideGroup("merge") %>%
      hideGroup("model")
    
    lyr <- ch$layer[[1]]
    if (!is.na(lyr)) proxy %>% showGroup(lyr)
  })
  
  observeEvent(input$prev, session$sendCustomMessage("goChapter", "prev"))
  observeEvent(input$next_btn, session$sendCustomMessage("goChapter", "next"))
}

shinyApp(ui, server)
