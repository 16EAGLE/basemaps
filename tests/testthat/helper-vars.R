# map test settings
map_token = Sys.getenv("moveVis_map_token")
if(map_token != "") run_mapbox <- TRUE else run_mapbox <- FALSE
run_esri <- FALSE
test_maps = as.logical(Sys.getenv("moveVis_test_maps"))

# example extent
data("ext", package = "basemaps", envir = environment())

# suggests installed?
check_ggplot <- any(grepl("ggplot2", installed.packages()[,1]))
check_stars <- any(grepl("stars", installed.packages()[,1]))
check_mapview <- any(grepl("mapview", installed.packages()[,1]))
