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
