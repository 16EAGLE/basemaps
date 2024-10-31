# map test settings
test <- list()
test$maps = as.logical(Sys.getenv("basemaps_test_maps"))
if(is.na(test$maps)) test$maps <- FALSE

mapbox_token = Sys.getenv("basemaps_mapbox_token")
run_mapbox <- mapbox_token != ""

osmtf_token = Sys.getenv("basemaps_osmtf_token")
run_osmtf <- osmtf_token != ""

osmstamen_token = Sys.getenv("basemaps_osmstamen_token")
run_stamen <- osmstamen_token != ""

osmstadia_token = Sys.getenv("basemaps_osmstadia_token")
run_stadia <- osmstadia_token != ""

maptiler_token = Sys.getenv("basemaps_maptiler_token")
run_maptiler <- maptiler_token != ""

run_esri = as.logical(Sys.getenv("basemaps_run_esri"))
if(is.na(run_esri)) run_esri <- FALSE

# test dir
map_dir <- Sys.getenv("basemaps_test_dir")
if(map_dir == ""){
  map_dir <- paste0(tempdir(), "/basemaps/")
}
if(!dir.exists(map_dir)) dir.create(map_dir, recursive = T)

# example extent
data("ext_eur", package = "basemaps", envir = environment())
ext <- sf::st_transform(ext_eur, 3857)

# special case: mutliple extents and custom crs
ext_sc <- list(
  west = sf::st_bbox(c(xmin = -180, ymin = 47.72572, xmax = -179.94753, ymax = 47.77003), crs = sf::st_crs(4326)),
  east = sf::st_bbox(c(xmin = 179.96495, ymin = 47.72572, xmax = 180.00000, ymax = 47.77003), crs = sf::st_crs(4326))
)
custom_crs <- sf::st_crs(4326)$wkt

# suggests installed?
test$ggplot <- any(grepl("ggplot2", installed.packages()[,1]))
test$stars <- any(grepl("stars", installed.packages()[,1]))
test$raster <- any(grepl("raster", installed.packages()[,1]))
test$mapview <- any(grepl("mapview", installed.packages()[,1]))
test$mapedit <- any(grepl("mapedit", installed.packages()[,1]))
