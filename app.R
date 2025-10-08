library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)
library(vroom)
library(shinyWidgets)
library(lubridate)
library(magrittr)
library(scales)
library(htmltools)

data_dir <- "~/Documents/"
data_dir <- "/Users/yongjoonpark/Downloads/turkey_airquality"


dt_syria <- fread(sprintf("%s/acled_dt_syr_war.csv", data_dir))
dt_turkey <- fread(sprintf("%s/turkey_airquality_and_syrian_war.csv", data_dir))
syria_admin2_raw <- st_read(sprintf("%s/syr_adm_unocha/syr_admbnda_adm2_uncs_unocha.json", data_dir))
dt_turkey_weekly <- fread(sprintf("%s/dt_turkey_weekly.csv", data_dir))

tmp_weeklydata <- data.table(dt_turkey_weekly)
tmp_weeklydata[is.nan(avg_pm10), avg_pm10 := NA]
tmp_weeklydata[is.nan(dw_events), dw_events := NA]
tmp_weeklydata[is.nan(uw_events), uw_events := NA]


syria_admin2 <- syria_admin2_raw %>%
  rename(admin2 = ADM2_EN) %>%
  mutate(admin2 = str_replace_all(admin2, "-", " ")) %>%
  mutate(admin2 = str_replace(admin2, "'", ""))

countries <- ne_countries(
  country = c("Turkey", "Syria"),
  returnclass = "sf",
  scale = "medium"
)

dt_turkey <- dt_turkey %>%
  mutate(
    yy = year(date_v1),
    week = isoweek(date_v1)
  )

sum_syria2 <- dt_syria
sum_syria2[, yy := year(date_v1)]
sum_syria2[, .(.N, sum(fatalities)), keyby = c("yy", "admin1", "admin2")]
sum_syria2 <- sum_syria2[, .(latitude = mean(latitude), longitude = mean(longitude), num_events = .N, total_fatalities = sum(fatalities)), by = .(yy, admin2)]

sum_syria3 <- dt_syria
sum_syria3[, yy := year(date_v1)]
sum_syria3[, .(.N, sum(fatalities)), keyby = c("yy", "admin1", "admin2")]

sum_syria3 <- sum_syria3[, .(latitude = mean(latitude), longitude = mean(longitude), num_events = .N, total_fatalities = sum(fatalities)), by = .(yy, admin2, event_type)]


# all events
custom_breaks <- as.integer(sum_syria2[, .(quantile(num_events, probs = seq(0, 1, 0.1)))][, V1])
sum_syria3[, .(quantile(num_events, probs = seq(0, 1, 0.1))), by = event_type]

# Create color palette
custom_pal <- colorBin("YlOrRd", domain = NULL, bins = custom_breaks, na.color = "transparent")


green_red_manual <- c("#4CAF50", "#F44336")
pos_neg_corr <- c("Positive Correlation", "Negative Correlation")
corr_labels <- factor(pos_neg_corr, levels = pos_neg_corr)
green_red_pal <- colorFactor(palette = green_red_manual, domain = corr_labels)





stations <- dt_turkey %>% 
  distinct(monitor_id, monitor, mon_lat, mon_lon)

ui <- fluidPage(
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%;margin:0;padding:0;}
    #map {position:absolute; top:0; bottom:0; right:0; left:0;}
  "),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    id = "title", class = "panel", 
    top = 10, left = "50%",
    style = "
    transform: translateX(-50%);   /* centers the panel horizontally */
    display: inline-block;          /* make width fit content */
    background-color: transparent; 
    backdrop-filter: blur(4px);
    border-radius: 10px;
    padding: 10px 10px;
    text-align: center;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
  ",
    h2("Syrian Armed Conflicts and Air Quality in Turkey (2017–2020)")
  ),
  
  absolutePanel(
    id = "panel_desc_toggle", class = "panel",
    top = 105, right = 10, width = 285,
    style = "padding: 10px; background-color: #f7f7f7; border-radius: 8px;",
    selectInput(
      inputId = "show_description",
      label = "Show Map Description:",
      choices = c("No", "Yes"),
      selected = "No"
    )
  ),
  
  conditionalPanel(
    condition = "input.show_description == 'Yes'",
    absolutePanel(
      id = "description", class = "panel", 
      top = 210, right = 10, width = 285,
      style = "padding: 5px; background-color: #f7f7f7; border-radius: 10px; font-size: 90%;",
      p("In this interactive map, we visualize the intensity of armed conflicts in Syria and their wind-direction-adjusted correlation with air quality in Turkey. Specifically, for each monitoring station and Syrian conflict event pair, we determine whether the wind direction at the station is from the conflict location (downwind conflict) or toward it (upwind conflict). We then count the weekly number of downwind and upwind conflicts for each station in the given year. Using this data, we calculate the correlation between PM₁₀ levels in Turkey and the number of downwind/upwind conflicts in Syria. The size of each circle represents the strength of the correlation, its color indicates whether the correlation is positive (green) or negative (red), and its opacity reflects the statistical significance (more opaque means more significant).")
    )
  ),
  
  absolutePanel(
    id = "panel_year", class = "panel", 
    top = 150, left = 10, width = 325,
    style = "padding: 10px; background-color: #f7f7f7; border-radius: 8px;",
    pickerInput("year", "Select Year", choices = unique(dt_syria$yy), multiple = FALSE)
  ),
  
  absolutePanel(
    id = "panel_corr", class = "panel", 
    top = 275, left = 10, width = 325, 
    style = "padding: 10px; background-color: #f7f7f7; border-radius: 8px;",
    h4(""),
    selectInput("correlation_type", "Downwind vs Upwind conflicts relative to the monitoring station in Turkey:",
                choices = c("Upwind Conflicts" = "uw", "Downwind Conflicts" = "dw"),
                selected = "dw"),    
  ),
  
  absolutePanel(
    id = "panel_admin", class = "panel", 
    top = 430, left = 10, width = 325, 
    style = "padding: 10px; background-color: #f7f7f7; border-radius: 8px;",
    h4(""),
    selectInput("syria_event", "Types of Armed Conflicts in Syria:",
                choices = c("All", "Explosions/Remote violence", "Battles", "Violence against civilians"),
                multiple = FALSE),
    p("Choosing an event type in this panel (along with the selected year above) will display a choropleth map of Syria showing the number of conflicts of that type.")
  ),
  
  absolutePanel(
    id = "acknowledgment", class = "panel", 
    bottom = 0, left = 10, width = 1000,
    style = "padding: 5px; background-color: #f7f7f7; border-radius: 8px; font-size: 80%;",
    p("This Shiny application was developed by Ben Heep (benheep@gmail.com) and Yongjoon Park (yongjoonpark@umass.edu). Please reach out to them for any questions or feedback. Data sources: Armed Conflict Location & Event Data Project (ACLED) and Turkish air quality monitoring stations."),
  ),
  
)


server <- function(input, output, session) {
  
  filtered_data_turkey <- reactive({
    data <- dt_turkey
    
    if(!is.null(input$year) && length(input$year) > 0) {
      data <- data[data$yy %in% input$year]
    }
    
    return(data)
  })
  
  
  testing_corr <- reactive({
    req(input$year)
    req(input$correlation_type)
    
    this_yy <- input$year
    
    if(input$correlation_type == "uw"){ corvar <- "uw_events" }
    else if(input$correlation_type == "dw"){ corvar <- "dw_events" }
    else{}
    
    tmp_weeklydata[yy == this_yy & !is.na(avg_pm10) & !is.na(get(corvar)), 
                   {
                     x <- get(corvar)
                     y <- avg_pm10
                     x <- x[complete.cases(x, y)]
                     y <- y[complete.cases(x, y)]
                     
                     if (length(x) > 2 && sd(x) > 0 && sd(y) > 0) {
                       ct <- suppressWarnings(cor.test(x, y))
                       list(
                         cor_var = cor(x, y),
                         se      = sd(x, na.rm = TRUE) / sqrt(length(x)), 
                         p_val   = unname(ct$p.value)
                       )
                     } else {
                       list(
                         cor_var = NA_real_,
                         se      = NA_real_,
                         p_val   = NA_real_
                       )
                     }
                   },
                   by = .(monitor, mon_lat, mon_lon, monitor_id)
    ]
    
    
  })
  
  filtered_data_weekly <- reactive({
    req(input$year)
    
    dt_turkey_weekly %>%
      filter(yy == input$year)
  })
  
  
  syria_map_data <- reactive({
    req(input$year)
    
    filtered <- dt_syria %>%
      filter(yy == input$year)
    
    if (!is.null(input$syria_event) && input$syria_event != "All") {
      filtered <- filtered %>% filter(event_type == input$syria_event)
    }    
  
    sum_events <- filtered %>%
      group_by(yy, admin2) %>%
      summarise(
        latitude = mean(latitude, na.rm = TRUE),
        longitude = mean(longitude, na.rm = TRUE),
        num_events = n(),
        total_fatalities = sum(fatalities, na.rm = TRUE),
        .groups = "drop"
      )
    
    syria_admin2 %>%
      left_join(sum_events, by = "admin2") %>%
      mutate(admin_name = admin2)
    
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addPolygons(
        data = countries,
        fill = FALSE, color = "black", weight = 1.2
      )
  })
  
  observe({
    data <- filtered_data_weekly()
    stations_filtered <- distinct(data, monitor_id, monitor, mon_lat, mon_lon)
    corr_dt <- testing_corr()

    get_opacity <- function(p) {
      case_when(
        p < 0.01 ~ 1.0,
        p < 0.05 ~ 0.8,
        p < 0.10 ~ 0.6,
        TRUE ~ 0.4
      )
    }
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() 
    
    leafletProxy("map") %>%
      addCircleMarkers(
        data = corr_dt,
        lng = ~mon_lon, lat = ~mon_lat,
        layerId = ~monitor,
        label = ~lapply(paste0(
          "<b>", monitor, "</b><br>",
          "Station ID: ", monitor_id, "<br>",
          "Correlation (Events ~ PM10): ", round(cor_var, 3), "<br>",
          "P Value: ", signif(p_val, 3)
        ), HTML),
        radius = ~rescale(abs(cor_var), to = c(5, 15)),
        stroke = FALSE,
        fillOpacity = ~get_opacity(p_val),
        color = ~ifelse(cor_var >= 0, "#4CAF50", "#F44336")
      ) %>%
      addLegend(
        position = "topright",
        pal = green_red_pal,
        values = corr_labels,
        title = "Correlation between PM10 in Turkey and<br>number of armed conflicts in Syria"
        )
    
    syria_data <- syria_map_data()
    
    pal <- colorBin("Reds", domain = syria_data$num_events, bins = 5, na.color = "transparent")
    
    leafletProxy("map") %>%
      addPolygons(
        data = syria_data,
        fillColor = ~custom_pal(num_events),
        fillOpacity = 0.6,
        color = "black", weight = 1,
        # popup = ~paste0("<b>", admin_name, "</b><br>",
        #                 "Events: ", num_events, "<br>",
        #                 "Fatalities: ", total_fatalities),
        label = ~lapply(paste0(
          "<b>", admin_name, "</b><br>",
          "Events: ", num_events, "<br>",
          "Fatalities: ", total_fatalities
        ), HTML),
        labelOptions = labelOptions(
          offset = c(-10, 0)
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "white",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = custom_pal,
        values = custom_breaks[-length(custom_breaks)],        
        title = "Number of Conflicts in Syria"
      )
      
  })
  
  
  clicked_station <- reactiveVal(NULL)
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    clicked_station(click$id)
  })
  
  # output$scatter_uw <- renderPlot({
  #   req(clicked_station())
  #   df <- filtered_data_weekly() %>%
  #     filter(monitor == clicked_station()) %>%
  #     filter(uw_events != 0)
  #   
  #   ggplot(df, aes(x = uw_events, y = avg_pm10)) +
  #     geom_point(color = "darkblue", alpha = 0.6) +
  #     geom_smooth(method = "lm", color = "red") +
  #     labs(title = paste("Upwind Events vs PM10 -", clicked_station(), "-", input$year),
  #          x = "Upwind Events", y = "PM10") +
  #     theme_minimal()
  # })
  # 
  # output$scatter_dw <- renderPlot({
  #   req(clicked_station())
  #   df <- filtered_data_weekly() %>%
  #     filter(monitor == clicked_station()) %>%
  #     filter(dw_events != 0)
  #   
  #   ggplot(df, aes(x = dw_events, y = avg_pm10)) +
  #     geom_point(color = "darkgreen", alpha = 0.6) +
  #     geom_smooth(method = "lm", color = "red") +
  #     labs(title = paste("Downwind Events vs PM10 -", clicked_station(), "-", input$year),
  #          x = "Downwind Events", y = "PM10") +
  #     theme_minimal()
  # })
}

shinyApp(ui, server)
