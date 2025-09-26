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

data_dir <- "~/Documents/"
data_dir <- "/Users/yongjoonpark/Downloads/turkey_airquality"


dt_syria <- fread(sprintf("%s/acled_dt_syr_war.csv", data_dir))
dt_turkey <- fread(sprintf("%s/turkey_airquality_and_syrian_war.csv", data_dir))
syria_admin1 <- st_read(sprintf("%s/syr_adm_unocha/syr_admbnda_adm1_uncs_unocha.json", data_dir))
syria_admin2 <- st_read(sprintf("%s/syr_adm_unocha/syr_admbnda_adm2_uncs_unocha.json", data_dir))
dt_turkey_weekly <- fread(sprintf("%s/dt_turkey_weekly.csv", data_dir))

tmp_weeklydata <- data.table(dt_turkey_weekly)
tmp_weeklydata[is.nan(avg_pm10), avg_pm10 := NA]
tmp_weeklydata[is.nan(dw_events), dw_events := NA]
tmp_weeklydata[is.nan(uw_events), uw_events := NA]


syria_admin1 <- syria_admin1 %>%
  rename(admin1 = ADM1_EN) %>%
  mutate(admin1 = str_replace_all(admin1, "-", " ")) %>%
  mutate(admin1 = str_replace(admin1, "'", ""))

syria_admin2 <- syria_admin2 %>%
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

# dt_turkey_weekly <- dt_turkey %>%
#   group_by(yy, week, monitor_id) %>%
#   summarise(
#     monitor         = first(monitor),
#     mon_lat         = first(mon_lat),
#     mon_lon         = first(mon_lon),
#     avg_windspeed   = mean(windspeed, na.rm = TRUE),
#     avg_temperature = mean(temperature, na.rm = TRUE),
#     avg_precip      = mean(precip, na.rm = TRUE),
#     avg_humidity    = mean(humidity, na.rm = TRUE),
#     avg_pm10        = mean(pm10, na.rm = TRUE),
#     avg_no2         = mean(no2, na.rm = TRUE),
#     tot_events      = sum(tot_events, na.rm = TRUE),
#     dw_events       = sum(dw_events, na.rm = TRUE),
#     dw_fatals       = sum(dw_fatals, na.rm = TRUE),
#     uw_events       = sum(uw_events, na.rm = TRUE),
#     uw_fatals       = sum(uw_fatals, na.rm = TRUE),
#     tot_fatals      = sum(tot_fatals, na.rm = TRUE),
#     nw_events       = sum(nw_events, na.rm = TRUE),
#     nw_fatals       = sum(nw_fatals, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(monitor_id, yy, week)

sum_syria1 <- dt_syria
sum_syria1[, yy := year(date_v1)]
sum_syria1[, .(.N, sum(fatalities)), keyby = c("yy", "admin1", "admin2")]
sum_syria1 <- sum_syria1[, .(latitude = mean(latitude), longitude = mean(longitude), num_events = .N, total_fatalities = sum(fatalities)), by = .(yy, admin1)]

sum_syria2 <- dt_syria
sum_syria2[, yy := year(date_v1)]
sum_syria2[, .(.N, sum(fatalities)), keyby = c("yy", "admin1", "admin2")]
sum_syria2 <- sum_syria2[, .(latitude = mean(latitude), longitude = mean(longitude), num_events = .N, total_fatalities = sum(fatalities)), by = .(yy, admin1)]


stations <- dt_turkey %>% 
  distinct(monitor_id, monitor, mon_lat, mon_lon)

ui <- fluidPage(
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%;margin:0;padding:0;}
    #map {position:absolute; top:0; bottom:0; right:0; left:0;}
  "),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    id = "controls", class = "panel", top = 80, left = 10, width = 325,
    draggable = FALSE, fixed = TRUE, 
    style = "padding-left: 10px; background-color: #f7f7f7;",
    h4("Filters"),
    pickerInput("year", "Select Year", choices = unique(dt_syria$yy), multiple = FALSE),
    selectInput("correlation_type", "Correlation Type:", choices = c("Upwind Events" = "uw", "Downwind Events" = "dw"), selected = "dw"),
    selectInput("select_admin", "Admin", choices = c("Admin 1" = "admin1", "Admin 2" = "admin2"), multiple = FALSE),
    selectInput("syria_event", "Event Type:", choices = unique(dt_syria$event_type), multiple = TRUE),
  ),
  
  absolutePanel(
    id = "plots", class = "panel", bottom = 10, left = 10, width = 600, height = 300,
    draggable = FALSE, fixed = TRUE,
    fluidRow(
      column(6, plotOutput("scatter_uw", height = "280px")),
      column(6, plotOutput("scatter_dw", height = "280px"))
    )
  )
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
    
    if(input$select_admin == "admin1"){syria_admin1 %>%
        left_join(
          sum_syria1 %>% filter(yy == input$year),
          by = "admin1"
        )}
    else if(input$select_admin == "admin2"){syria_admin2 %>%
        left_join(
          sum_syria2 %>% filter(yy == input$year),
          by = "admin2"
        )}
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = countries,
        fill = FALSE, color = "black", weight = 1.2
      )
  })
  
  observe({
    data <- filtered_data_weekly()
    stations_filtered <- distinct(data, monitor_id, monitor, mon_lat, mon_lon)
    corr_dt <- testing_corr()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() 
    
    leafletProxy("map") %>%
      addCircleMarkers(
        data = corr_dt,
        lng = ~mon_lon, lat = ~mon_lat,
        layerId = ~monitor,
        popup = ~sprintf("%s \n %.2f (%.2f)", monitor, cor_var, se),
        radius = ~rescale(abs(cor_var), to = c(5, 15)),
        fillOpacity = ~rescale(-se, to = c(0.3, 1)), 
        color = ~ifelse(cor_var >= 0, "#4CAF50", "#F44336")
      ) 
      # %>%
      # fitBounds(
      #   lng1 = min(stations$mon_lon), lat1 = min(stations$mon_lat),
      #   lng2 = max(stations$mon_lon), lat2 = max(stations$mon_lat)
      # )
    
    syria_data <- syria_map_data()
    
    pal <- colorBin("Reds", domain = syria_data$num_events, bins = 5, na.color = "transparent")
    
    leafletProxy("map") %>%
      addPolygons(
        data = syria_data,
        fillColor = ~pal(num_events),
        fillOpacity = 0.6,
        color = "black", weight = 1,
        popup = ~paste0("<b>", ifelse("admin1" %in% names(syria_data), admin1, admin2), "</b><br>",
                        "Events: ", num_events, "<br>",
                        "Fatalities: ", total_fatalities)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = syria_data$num_events,
        title = "Number of Conflicts"
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
    
  #   ggplot(df, aes(x = uw_events, y = avg_pm10)) +
  #     geom_point(color = "darkblue", alpha = 0.6) +
  #     geom_smooth(method = "lm", color = "red") +
  #     labs(title = paste("Upwind Events vs PM10 -", clicked_station(), "-", input$year),
  #          x = "Upwind Events", y = "PM10") +
  #     theme_minimal()
  # })
  
  # output$scatter_dw <- renderPlot({
  #   req(clicked_station())
  #   df <- filtered_data_weekly() %>%
  #     filter(monitor == clicked_station()) %>%
  #     filter(dw_events != 0)
    
  #   ggplot(df, aes(x = dw_events, y = avg_pm10)) +
  #     geom_point(color = "darkgreen", alpha = 0.6) +
  #     geom_smooth(method = "lm", color = "red") +
  #     labs(title = paste("Downwind Events vs PM10 -", clicked_station(), "-", input$year),
  #          x = "Downwind Events", y = "PM10") +
  #     theme_minimal()
  # })

}

shinyApp(ui, server)
