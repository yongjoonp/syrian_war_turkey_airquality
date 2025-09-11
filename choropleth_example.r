  pkgs_to_load  <- c("data.table", "magrittr", "ggplot2", "stringr", "sf")
  load_the_pkgs <- lapply(pkgs_to_load, require, character.only = TRUE)

  # data path 
  data_dir <- "/Users/yongjoonparkadmin/Downloads/turkey_airquality"

  selected_year <- 2017

  # load syria conflict data and get number of conflicts by admin1 for year
  dt_syria <- fread(sprintf("%s/acled_dt_syr_war.csv", data_dir))
  dt_syria[, yy := year(date_v1)]
  dt_summ <- dt_syria[yy == selected_year, .(num_conflict = .N), admin1]

  # load the shape file for Syria  
  shape_file <- sprintf("%s/syr_adm_unocha/syr_admbnda_adm1_uncs_unocha.json", data_dir)
  syria_shp <- st_read(shape_file, quiet = TRUE)
  sy_dt <- data.table(syria_shp)

  # modify the admin1 names in sy_dt to match the names in both datasets
  sy_dt[, admin1 := ADM1_EN]
  sy_dt[, admin1 := str_replace_all(admin1, "-", " ")]
  sy_dt[admin1 == "Dar'a", admin1 := "Dara"]
  
  dt_summ_shape <- dt_summ[sy_dt, on = c("admin1")] 
  # convert the data.table to shape object
  dt_summ_shape <- sf::st_as_sf(dt_summ_shape)

  # plot the map
  ggplot(dt_summ_shape) + 
  geom_sf(aes(fill = num_conflict), size = .1, color = "grey65") + 
  theme_void()


data_dir <- "/Users/yongjoonpark/Downloads/turkey_airquality"


dt_syria <- fread(sprintf("%s/acled_dt_syr_war.csv", data_dir))
dt_turkey <- fread(sprintf("%s/turkey_airquality_and_syrian_war.csv", data_dir))
syria_admin1 <- st_read(sprintf("%s/syr_adm_unocha/syr_admbnda_adm1_uncs_unocha.json", data_dir))

dt_turkey <- dt_turkey %>%
  mutate(
    yy = year(date_v1),
    week = isoweek(date_v1)
  )

dt_turkey_weekly <- dt_turkey %>%
  group_by(yy, week, monitor_id) %>%
  summarise(
    monitor         = first(monitor),
    mon_lat         = first(mon_lat),
    mon_lon         = first(mon_lon),
    avg_windspeed   = mean(windspeed, na.rm = TRUE),
    avg_temperature = mean(temperature, na.rm = TRUE),
    avg_precip      = mean(precip, na.rm = TRUE),
    avg_humidity    = mean(humidity, na.rm = TRUE),
    avg_pm10        = mean(pm10, na.rm = TRUE),
    avg_no2         = mean(no2, na.rm = TRUE),
    tot_events      = sum(tot_events, na.rm = TRUE),
    dw_events       = sum(dw_events, na.rm = TRUE),
    dw_fatals       = sum(dw_fatals, na.rm = TRUE),
    uw_events       = sum(uw_events, na.rm = TRUE),
    uw_fatals       = sum(uw_fatals, na.rm = TRUE),
    tot_fatals      = sum(tot_fatals, na.rm = TRUE),
    nw_events       = sum(nw_events, na.rm = TRUE),
    nw_fatals       = sum(nw_fatals, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(monitor_id, yy, week)


dt <- data.table(dt_turkey_weekly)
dt[yy == 2017 & !is.nan(avg_pm10) & !is.nan(dw_events), .(.N, corr_dw_pm10 = cor(dw_events, avg_pm10)), by = .(monitor_id)]


dt_turkey_weekly <- fread(sprintf("%s/dt_turkey_weekly.csv", data_dir))
setnames(dt_turkey_weekly, "latitude", "mon_lat")
setnames(dt_turkey_weekly, "longitude", "mon_lon")
fwrite(dt_turkey_weekly, "/Users/yongjoonpark/Downloads/turkey_airquality/dt_turkey_weekly.csv")