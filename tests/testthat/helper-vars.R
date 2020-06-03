# map test settings
map_token = Sys.getenv("basemaps_map_token")
if(map_token != "") run_mapbox <- TRUE else run_mapbox <- FALSE
run_esri <- FALSE
test_maps = as.logical(Sys.getenv("basemaps_test_maps"))

# test dir
map_dir = Sys.getenv("basemaps_test_dir")
if(map_dir == ""){
  map_dir <- paste0(tempdir(), "/basemaps/")
}
if(!dir.exists(map_dir)) dir.create(map_dir, recursive = T)

# example extent
data("ext", package = "basemaps", envir = environment())

# suggests installed?
check_ggplot <- any(grepl("ggplot2", installed.packages()[,1]))
check_stars <- any(grepl("stars", installed.packages()[,1]))
check_mapview <- any(grepl("mapview", installed.packages()[,1]))
