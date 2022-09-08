# map test settings
test_maps = as.logical(Sys.getenv("basemaps_test_maps"))
if(is.na(test_maps)) test_maps <- FALSE

mapbox_token = Sys.getenv("basemaps_mapbox_token")
if(mapbox_token != "") run_mapbox <- TRUE else run_mapbox <- FALSE

osmtf_token = Sys.getenv("basemaps_osmtf_token")
if(osmtf_token != "") run_osmtf <- TRUE else run_osmtf <- FALSE

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

# suggests installed?
check_ggplot <- any(grepl("ggplot2", installed.packages()[,1]))
check_mapview <- any(grepl("mapview", installed.packages()[,1]))
