# ============================================================
# Minnesota Geocache Finder - Shiny App (v9)
# ============================================================

library(shiny)
library(leaflet)
library(tidyverse)
library(httr)
library(jsonlite)

# ── 1. DATA IMPORT ───────────────────────────────────────────

fetch_mn_caches <- function() {
  consumer_key <- "RSSguaeJw6kvwgqdgA6G"

  lats <- c(43.5, 45.47, 47.43, 49.4)
  lons <- c(-97.2, -94.63, -92.07, -89.5)

  all_ids <- character(0)

  for (i in seq_len(length(lats) - 1)) {
    for (j in seq_len(length(lons) - 1)) {
      bbox <- paste0(lats[i], "|", lons[j], "|", lats[i+1], "|", lons[j+1])
      search_url <- paste0(
        "https://www.opencaching.us/okapi/services/caches/search/bbox?",
        "bbox=", URLencode(bbox, reserved = TRUE),
        "&limit=500",
        "&status=", URLencode("Available|Temporarily unavailable", reserved = TRUE),
        "&consumer_key=", consumer_key
      )
      resp <- tryCatch(GET(search_url, timeout(15)), error = function(e) NULL)
      if (is.null(resp)) next
      ids <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"))$results,
                      error = function(e) NULL)
      if (!is.null(ids)) all_ids <- c(all_ids, ids)
      Sys.sleep(0.5)
    }
  }

  all_ids <- unique(all_ids)
  message("Total unique cache IDs found: ", length(all_ids))

  if (length(all_ids) == 0) {
    return(tibble(name=character(), lat=numeric(), lon=numeric(),
                  type=character(), difficulty=numeric(), terrain=numeric(),
                  status=character()))
  }

  batches <- split(all_ids, ceiling(seq_along(all_ids) / 50))

  map_dfr(batches, function(batch_ids) {
    ids_string <- paste(batch_ids, collapse = "|")
    detail_url <- paste0(
      "https://www.opencaching.us/okapi/services/caches/geocaches?",
      "cache_codes=", URLencode(ids_string, reserved = TRUE),
      "&fields=name|location|type|status|difficulty|terrain",
      "&consumer_key=", consumer_key
    )
    resp <- tryCatch(GET(detail_url, timeout(15)), error = function(e) NULL)
    if (is.null(resp)) return(tibble())
    clean_data <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                           error = function(e) NULL)
    if (is.null(clean_data) || length(clean_data) == 0) return(tibble())
    Sys.sleep(0.3)
    bind_rows(clean_data) %>%
      mutate(
        lat        = as.numeric(sub("\\|.*", "", location)),
        lon        = as.numeric(sub(".*\\|", "", location)),
        difficulty = as.numeric(difficulty),
        terrain    = as.numeric(terrain)
      )
  })
}

message("Fetching MN geocache data...")
mn_caches <- tryCatch(
  fetch_mn_caches(),
  error = function(e) {
    message("Fetch failed: ", e$message)
    tibble(name=character(), lat=numeric(), lon=numeric(),
           type=character(), difficulty=numeric(), terrain=numeric(),
           status=character())
  }
)
message("Loaded ", nrow(mn_caches), " caches.")

# ── 2. HELPERS ───────────────────────────────────────────────

# Returns distance in MILES
haversine_mi <- function(lat1, lon1, lat2, lon2) {
  R    <- 3958.8   # Earth radius in miles
  phi1 <- lat1 * pi / 180; phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi/2)^2 + cos(phi1)*cos(phi2)*sin(dlam/2)^2
  2 * R * asin(sqrt(a))
}

type_colors <- function(types) {
  pal <- c("Traditional"="#4ade80","Multi"="#60a5fa","Unknown"="#c084fc",
           "Virtual"="#fb923c","Webcam"="#f87171","Event"="#facc15","Other"="#94a3b8")
  cols <- pal[types]; cols[is.na(cols)] <- "#94a3b8"; unname(cols)
}

# ── 3. UI ─────────────────────────────────────────────────────

ui <- fluidPage(

  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=Syne:wght@400;600;700;800&family=DM+Sans:ital,wght@0,300;0,400;0,500;1,300&display=swap"),
    tags$link(rel="stylesheet",
      href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),

    tags$style(HTML("

      :root {
        --bg-page:        #0f1923;
        --sidebar-bg:     rgba(10, 18, 28, 0.84);
        --card-bg:        rgba(255,255,255,0.04);
        --card-border:    rgba(255,255,255,0.08);
        --card-hover:     rgba(255,255,255,0.13);
        --text-primary:   #ffffff;
        --text-secondary: rgba(255,255,255,0.55);
        --text-muted:     rgba(255,255,255,0.28);
        --accent:         #4ade80;
        --accent-dark:    #16a34a;
        --accent-glow:    rgba(74,222,128,0.35);
        --toggle-bg:      rgba(10, 18, 28, 0.85);
        --toggle-border:  rgba(255,255,255,0.1);
        --pill-bg:        rgba(255,255,255,0.05);
        --pill-border:    rgba(255,255,255,0.08);
        --divider:        rgba(255,255,255,0.06);
        --slider-track:   rgba(255,255,255,0.1);
        --irs-text:       rgba(255,255,255,0.3);
        --scrollbar:      rgba(255,255,255,0.15);
        --popup-bg:       #1a2535;
        --popup-text:     #e2e8f0;
        --popup-sub:      #94a3b8;
      }

      body.light-mode {
        --bg-page:        #eef2f7;
        --sidebar-bg:     rgba(255,255,255,0.88);
        --card-bg:        rgba(0,0,0,0.03);
        --card-border:    rgba(0,0,0,0.08);
        --card-hover:     rgba(0,0,0,0.06);
        --text-primary:   #0f1923;
        --text-secondary: rgba(0,0,0,0.6);
        --text-muted:     rgba(0,0,0,0.35);
        --accent:         #16a34a;
        --accent-dark:    #14532d;
        --accent-glow:    rgba(22,163,74,0.25);
        --toggle-bg:      rgba(255,255,255,0.9);
        --toggle-border:  rgba(0,0,0,0.12);
        --pill-bg:        rgba(0,0,0,0.04);
        --pill-border:    rgba(0,0,0,0.09);
        --divider:        rgba(0,0,0,0.07);
        --slider-track:   rgba(0,0,0,0.12);
        --irs-text:       rgba(0,0,0,0.35);
        --scrollbar:      rgba(0,0,0,0.18);
        --popup-bg:       #ffffff;
        --popup-text:     #1a202c;
        --popup-sub:      #64748b;
      }

      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
      html, body, .container-fluid {
        height: 100%; width: 100%; overflow: hidden;
        font-family: 'DM Sans', sans-serif; background: var(--bg-page);
        transition: background 0.35s ease;
      }

      #map-wrap { position: fixed; inset: 0; z-index: 0; }
      #cacheMap { width: 100% !important; height: 100% !important; }

      #mode-btn {
        position: fixed; top: 16px; right: 16px; z-index: 500;
        display: flex; align-items: center; gap: 8px; padding: 9px 16px;
        background: var(--toggle-bg); border: 1px solid var(--toggle-border);
        border-radius: 40px; backdrop-filter: blur(14px); -webkit-backdrop-filter: blur(14px);
        cursor: pointer; font-family: 'Syne', sans-serif; font-size: 12px; font-weight: 600;
        color: var(--text-secondary); letter-spacing: 0.06em; text-transform: uppercase;
        transition: background 0.3s, color 0.3s, box-shadow 0.2s;
        box-shadow: 0 2px 12px rgba(0,0,0,0.25);
      }
      #mode-btn:hover { color: var(--text-primary); box-shadow: 0 4px 20px rgba(0,0,0,0.35); }
      #mode-btn i { font-size: 14px; color: var(--accent); transition: transform 0.4s ease; }
      #mode-btn:hover i { transform: rotate(20deg); }

      #sidebar {
        position: fixed; top: 0; left: 0; height: 100%;
        width: 340px; z-index: 100; display: flex; flex-direction: column;
        transition: transform 0.4s cubic-bezier(0.77,0,0.175,1); pointer-events: none;
      }
      #sidebar.collapsed { transform: translateX(-340px); }

      #sidebar-inner {
        flex: 1; overflow-y: auto; overflow-x: hidden; padding: 24px 20px;
        background: var(--sidebar-bg);
        backdrop-filter: blur(22px) saturate(180%); -webkit-backdrop-filter: blur(22px) saturate(180%);
        border-right: 1px solid var(--card-border); pointer-events: all;
        transition: background 0.35s ease, border-color 0.35s ease;
        scrollbar-width: thin; scrollbar-color: var(--scrollbar) transparent;
      }
      #sidebar-inner::-webkit-scrollbar { width: 4px; }
      #sidebar-inner::-webkit-scrollbar-thumb { background: var(--scrollbar); border-radius: 4px; }

      #toggle-btn {
        position: fixed; top: 50%; left: 340px; transform: translateY(-50%);
        z-index: 200; width: 28px; height: 64px;
        background: var(--toggle-bg); border: 1px solid var(--toggle-border);
        border-left: none; border-radius: 0 10px 10px 0; cursor: pointer;
        display: flex; align-items: center; justify-content: center;
        color: var(--text-muted); font-size: 11px;
        transition: left 0.4s cubic-bezier(0.77,0,0.175,1), background 0.3s, color 0.2s;
        pointer-events: all; backdrop-filter: blur(12px);
      }
      #toggle-btn:hover { color: var(--text-primary); }
      #toggle-btn.collapsed {
        left: 0; border-left: 1px solid var(--toggle-border); border-radius: 0 10px 10px 0;
      }

      .app-header { margin-bottom: 22px; }
      .app-logo { display: flex; align-items: center; gap: 12px; margin-bottom: 4px; }
      .app-logo-icon {
        width: 38px; height: 38px;
        background: linear-gradient(135deg, #16a34a, #4ade80);
        border-radius: 10px; display: flex; align-items: center; justify-content: center;
        font-size: 20px; flex-shrink: 0; box-shadow: 0 4px 14px var(--accent-glow);
      }
      .app-title {
        font-family: 'Syne', sans-serif; font-weight: 800; font-size: 20px;
        color: var(--text-primary); letter-spacing: -0.3px; line-height: 1.1; transition: color 0.35s;
      }
      .app-subtitle {
        font-size: 11px; color: var(--text-muted); font-weight: 300;
        letter-spacing: 0.06em; text-transform: uppercase; margin-top: 3px; transition: color 0.35s;
      }

      .stat-row { display: flex; gap: 8px; margin-bottom: 20px; }
      .stat-pill {
        flex: 1; background: var(--pill-bg); border: 1px solid var(--pill-border);
        border-radius: 12px; padding: 12px 10px; text-align: center;
        transition: background 0.35s, border-color 0.35s;
      }
      .stat-pill .val {
        font-family: 'Syne', sans-serif; font-size: 22px; font-weight: 700;
        color: var(--accent); line-height: 1; transition: color 0.35s;
      }
      .stat-pill .lbl {
        font-size: 10px; color: var(--text-muted); text-transform: uppercase;
        letter-spacing: 0.08em; margin-top: 4px; transition: color 0.35s;
      }

      .panel-card {
        background: var(--card-bg); border: 1px solid var(--card-border);
        border-radius: 16px; padding: 18px; margin-bottom: 14px;
        transition: border-color 0.2s, background 0.35s;
      }
      .panel-card:hover { border-color: var(--card-hover); }

      .card-label {
        font-family: 'Syne', sans-serif; font-size: 11px; font-weight: 600;
        letter-spacing: 0.1em; text-transform: uppercase; color: var(--text-muted);
        margin-bottom: 14px; display: flex; align-items: center; gap: 8px; transition: color 0.35s;
      }
      .card-label i { font-size: 10px; color: var(--accent); }

      .irs--shiny .irs-bar {
        background: linear-gradient(90deg, #16a34a, #4ade80) !important;
        border-top: none !important; border-bottom: none !important; height: 4px !important;
      }
      .irs--shiny .irs-line {
        background: var(--slider-track) !important; border: none !important;
        height: 4px !important; border-radius: 4px;
      }
      .irs--shiny .irs-handle {
        background: #fff !important; border: 2px solid var(--accent) !important;
        box-shadow: 0 2px 8px var(--accent-glow) !important;
        width: 18px !important; height: 18px !important; top: 22px !important; border-radius: 50% !important;
      }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: var(--accent-dark) !important; color: #fff !important;
        font-family: 'DM Sans', sans-serif !important; font-size: 11px !important; border-radius: 6px !important;
      }
      .irs--shiny .irs-min, .irs--shiny .irs-max {
        color: var(--irs-text) !important; background: transparent !important; font-size: 10px !important;
      }
      .control-label {
        color: var(--text-secondary) !important; font-size: 12px !important;
        font-weight: 400 !important; font-family: 'DM Sans', sans-serif !important; margin-bottom: 4px !important;
      }

      @keyframes fadeSlideIn {
        from { opacity:0; transform:translateY(6px); }
        to   { opacity:1; transform:translateY(0); }
      }
      .nearest-row {
        display: flex; align-items: center; gap: 10px; padding: 10px 0;
        border-bottom: 1px solid var(--divider); animation: fadeSlideIn 0.25s ease both;
      }
      .nearest-row:last-child { border-bottom: none; }
      .nearest-rank {
        width: 24px; height: 24px; background: rgba(74,222,128,0.1);
        border: 1px solid rgba(74,222,128,0.22); border-radius: 8px;
        display: flex; align-items: center; justify-content: center;
        font-family: 'Syne', sans-serif; font-size: 11px; font-weight: 700;
        color: var(--accent); flex-shrink: 0;
      }
      .nearest-info { flex:1; min-width:0; }
      .nearest-name {
        font-size: 13px; font-weight: 500; color: var(--text-primary);
        white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
        line-height: 1.2; transition: color 0.35s;
      }
      .nearest-meta { font-size: 11px; color: var(--text-muted); margin-top: 2px; transition: color 0.35s; }
      .nearest-dist {
        font-family: 'Syne', sans-serif; font-size: 13px; font-weight: 700;
        color: var(--accent); flex-shrink: 0; white-space: nowrap;
      }

      .click-hint {
        display: flex; align-items: center; gap: 10px; padding: 12px;
        background: var(--pill-bg); border-radius: 10px; color: var(--text-muted);
        font-size: 12px; font-style: italic; transition: background 0.35s, color 0.35s;
      }
      .click-hint i { color: var(--text-muted); font-size: 16px; }

      .legend-row { display: flex; flex-wrap: wrap; gap: 8px; }
      .legend-item {
        display: flex; align-items: center; gap: 5px;
        font-size: 11px; color: var(--text-secondary); transition: color 0.35s;
      }
      .legend-dot { width: 8px; height: 8px; border-radius: 50%; flex-shrink: 0; }

      .coords-badge {
        display: inline-flex; align-items: center; gap: 6px;
        background: rgba(74,222,128,0.08); border: 1px solid rgba(74,222,128,0.18);
        border-radius: 20px; padding: 4px 10px; font-size: 11px;
        color: var(--text-secondary); margin-bottom: 12px; font-family: monospace;
        transition: color 0.35s;
      }
      .coords-badge i { color: var(--accent); font-size: 10px; }

      .leaflet-popup-content-wrapper {
        background: var(--popup-bg) !important; border-radius: 12px !important;
        box-shadow: 0 8px 30px rgba(0,0,0,0.3) !important; border: 1px solid var(--card-border) !important;
      }
      .leaflet-popup-tip { background: var(--popup-bg) !important; }
      .popup-name  { font-weight:600; font-size:14px; color:var(--popup-text); margin-bottom:4px; }
      .popup-type  { font-size:12px; color:var(--popup-sub); margin-bottom:8px; }
      .popup-stats { display:flex; gap:12px; font-size:12px; color:var(--popup-text); }

      .shiny-text-output {
        background: transparent !important; border: none !important;
        color: var(--text-secondary) !important; font-size: 12px !important;
        padding: 0 !important; font-family: 'DM Sans', sans-serif !important;
      }

    "))
  ),

  div(id = "map-wrap", leafletOutput("cacheMap")),

  tags$button(id = "mode-btn", onclick = "toggleMode()",
    tags$i(class = "fa-solid fa-sun", id = "mode-icon"),
    tags$span(id = "mode-label", "Light Mode")
  ),

  div(id = "sidebar",
    div(id = "sidebar-inner",

      div(class = "app-header",
        div(class = "app-logo",
          div(class = "app-logo-icon", HTML("&#x1F9ED;")),
          div(
            div(class = "app-title",    "GeoCache MN"),
            div(class = "app-subtitle", "Minnesota Cache Finder")
          )
        )
      ),

      div(class = "stat-row",
        div(class = "stat-pill",
          div(class = "val", textOutput("stat_shown", inline = TRUE)),
          div(class = "lbl", "Shown")
        ),
        div(class = "stat-pill",
          div(class = "val", textOutput("stat_total", inline = TRUE)),
          div(class = "lbl", "Loaded")
        )
      ),

      div(class = "panel-card",
        div(class = "card-label", tags$i(class = "fa-solid fa-sliders"), "Filter Caches"),
        sliderInput("diff_range", label = "Difficulty",
                    min=1, max=5, value=c(1,5), step=0.5, width="100%"),
        sliderInput("terr_range", label = "Terrain",
                    min=1, max=5, value=c(1,5), step=0.5, width="100%")
      ),

      div(class = "panel-card",
        div(class = "card-label", tags$i(class = "fa-solid fa-circle-dot"), "Cache Types"),
        div(class = "legend-row",
          div(class="legend-item", div(class="legend-dot",style="background:#4ade80"), "Traditional"),
          div(class="legend-item", div(class="legend-dot",style="background:#60a5fa"), "Multi"),
          div(class="legend-item", div(class="legend-dot",style="background:#c084fc"), "Unknown"),
          div(class="legend-item", div(class="legend-dot",style="background:#fb923c"), "Virtual"),
          div(class="legend-item", div(class="legend-dot",style="background:#facc15"), "Event"),
          div(class="legend-item", div(class="legend-dot",style="background:#94a3b8"), "Other")
        )
      ),

      div(class = "panel-card",
        div(class = "card-label", tags$i(class = "fa-solid fa-location-crosshairs"), "Nearest 5 Caches"),
        uiOutput("nearest_ui")
      )

    )
  ),

  tags$button(id = "toggle-btn", onclick = "toggleSidebar()",
    tags$i(class = "fa-solid fa-chevron-left", id = "toggle-icon")
  ),

  tags$script(HTML("
    var sidebarOpen = true;
    function toggleSidebar() {
      var sb = document.getElementById('sidebar');
      var btn = document.getElementById('toggle-btn');
      var icon = document.getElementById('toggle-icon');
      sidebarOpen = !sidebarOpen;
      if (sidebarOpen) {
        sb.classList.remove('collapsed'); btn.classList.remove('collapsed');
        btn.style.left = '340px'; icon.className = 'fa-solid fa-chevron-left';
      } else {
        sb.classList.add('collapsed'); btn.classList.add('collapsed');
        btn.style.left = '0px'; icon.className = 'fa-solid fa-chevron-right';
      }
      setTimeout(function() {
        var m = document.getElementById('cacheMap');
        if (m && m._leaflet_map) m._leaflet_map.invalidateSize();
      }, 420);
    }

    var isDark = true;
    function toggleMode() {
      isDark = !isDark;
      var body = document.body;
      var icon = document.getElementById('mode-icon');
      var label = document.getElementById('mode-label');
      if (isDark) {
        body.classList.remove('light-mode');
        icon.className = 'fa-solid fa-sun'; label.textContent = 'Light Mode';
        Shiny.setInputValue('tile_mode', 'dark', {priority: 'event'});
      } else {
        body.classList.add('light-mode');
        icon.className = 'fa-solid fa-moon'; label.textContent = 'Dark Mode';
        Shiny.setInputValue('tile_mode', 'light', {priority: 'event'});
      }
    }
  "))
)

# ── 4. SERVER ─────────────────────────────────────────────────

server <- function(input, output, session) {

  filtered_caches <- reactive({
    mn_caches %>%
      filter(
        difficulty >= input$diff_range[1], difficulty <= input$diff_range[2],
        terrain    >= input$terr_range[1], terrain    <= input$terr_range[2]
      )
  })

  output$stat_shown <- renderText({ nrow(filtered_caches()) })
  output$stat_total <- renderText({ nrow(mn_caches) })

  output$cacheMap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "base_tiles") %>%
      setView(lng = -94.0, lat = 46.5, zoom = 7) %>%
      addEasyButton(easyButton(
        icon = "fa-location-arrow", title = "Reset view",
        onClick = JS("function(btn,map){ map.setView([46.5,-94.0],7); }")
      ))
  })

  observeEvent(input$tile_mode, {
    tile <- if (input$tile_mode == "light") "CartoDB.Positron" else "CartoDB.DarkMatter"
    leafletProxy("cacheMap") %>%
      removeTiles("base_tiles") %>%
      addProviderTiles(tile, layerId = "base_tiles")
  })

  observe({
    df <- filtered_caches()
    leafletProxy("cacheMap", data = df) %>%
      clearMarkers() %>% clearShapes() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = 6,
        color = ~type_colors(type), fillColor = ~type_colors(type),
        fillOpacity = 0.85, weight = 1.5, opacity = 1,
        popup = ~paste0(
          "<div style='font-family:DM Sans,sans-serif;min-width:190px;'>",
          "<div class='popup-name'>", name, "</div>",
          "<div class='popup-type'>", type, "</div>",
          "<div class='popup-stats'>",
          "<span>&#9881; <b>", difficulty, "</b> diff</span>",
          "<span>&#127956; <b>", terrain, "</b> terrain</span>",
          "</div></div>"
        ),
        layerId = ~name
      )
  })

  click_loc <- reactiveVal(NULL)
  observeEvent(input$cacheMap_click, { click_loc(input$cacheMap_click) })

  nearest_df <- reactive({
    req(click_loc())
    clat <- click_loc()$lat; clng <- click_loc()$lng
    df <- filtered_caches(); req(nrow(df) >= 1)
    df %>%
      mutate(dist_mi = haversine_mi(clat, clng, lat, lon)) %>%
      arrange(dist_mi) %>% slice_head(n = 5) %>%
      select(name, type, difficulty, terrain, dist_mi) %>%
      mutate(dist_mi = round(dist_mi, 2))
  })

  observe({
    req(click_loc())
    clat <- click_loc()$lat; clng <- click_loc()$lng
    near5 <- nearest_df()
    # Convert radius back to meters for leaflet (1 mi = 1609.34 m)
    radius_mi <- if (nrow(near5) == 5) max(near5$dist_mi) * 1.05 else 5
    leafletProxy("cacheMap") %>%
      removeMarker("click_marker") %>% removeShape("click_circle") %>%
      addCircleMarkers(
        lng = clng, lat = clat, layerId = "click_marker",
        radius = 8, color = "#fff", fillColor = "#4ade80",
        fillOpacity = 1, weight = 2, popup = "&#x1F4CD; You clicked here"
      ) %>%
      addCircles(
        lng = clng, lat = clat, layerId = "click_circle",
        radius = radius_mi * 1609.34,
        color = "#4ade80", fillColor = "#4ade80",
        fillOpacity = 0.05, weight = 1.5, dashArray = "6 4"
      )
  })

  output$nearest_ui <- renderUI({
    if (is.null(click_loc())) {
      return(div(class = "click-hint",
        tags$i(class = "fa-regular fa-hand-pointer"),
        "Click anywhere on the map to find the 5 nearest caches"
      ))
    }
    df <- nearest_df()
    if (nrow(df) == 0) {
      return(div(style = "color:#f87171;font-size:13px;", "No caches match current filters."))
    }
    coords_badge <- div(class = "coords-badge",
      tags$i(class = "fa-solid fa-circle-dot"),
      paste0(round(click_loc()$lat, 4), "N  ", round(abs(click_loc()$lng), 4), "W")
    )
    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      div(class = "nearest-row", style = paste0("animation-delay:", (i-1)*0.05, "s"),
        div(class = "nearest-rank", i),
        div(class = "nearest-info",
          div(class = "nearest-name", r$name),
          div(class = "nearest-meta", paste0(r$type, " - D:", r$difficulty, " T:", r$terrain))
        ),
        div(class = "nearest-dist", paste0(r$dist_mi, " mi"))
      )
    })
    tagList(coords_badge, rows)
  })
}

shinyApp(ui, server)