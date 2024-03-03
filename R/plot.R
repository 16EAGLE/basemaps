#' Plot raster objects using \code{ggplot}
#' 
#' This function plots objects of class \code{SpatRaster}, \code{RasterLayer}, \code{RasterBrick} or \code{RasterStack} as \code{ggplot2}. It is used internally by \code{basemap*} functions that return \code{ggplot} plots.
#' 
#' @param r raster of class \code{SpatRaster}, \code{RasterLayer}, \code{RasterBrick} or \code{RasterStack}.
#' @param r_type character, either \code{"gradient"} or \code{"discrete"}.
#' @param gglayer logical, if \code{FALSE} (default), a \code{ggplot2} plot is returned, if \code{TRUE}, a \code{ggplot2} layer is returned.
#' @param ... additional arguments, including
#' \itemize{
#'    \item \code{maxpixels}, numeric, maximum number of pixels to be plotted (default: 500000)
#'    \item \code{alpha}, numeric between 0 and 1, alpha value of the plotted data (transparency).
#'    \item \code{maxColorValue}, numeric, the value  to use as colour maximum.
#'    \item \code{interpolate}, logical, whether to smooth the plot (default is \code{TRUE}).
#' }
#' 
#' @return A \code{ggplot2} object
#' 
#' @examples 
#' library(basemaps)
#' 
#' # example extent
#' data(ext)
#' 
#' \dontrun{
#' # raster object: Brick
#' map <- basemap_raster(ext)
#' 
#' # plotting RasterBrick
#' gg_raster(map, r_type = "RGB")
#' }
#' 
#' @importFrom terra rast ncell aggregate aggregate
#' @name plot
#' @export
gg_raster <- function(r, r_type = "RGB", gglayer = F, ...){
  
  if(inherits(r, "Raster")){
    r <- rast(r)
  }
  
  extras <- list(...)
  if(!is.null(extras$maxpixels)) maxpixels <- extras$maxpixels else maxpixels <- 500000
  if(!is.null(extras$alpha)) alpha <- extras$alpha else alpha <- 1
  if(!is.null(extras$maxColorValue)) maxColorValue <- extras$maxColorValue else maxColorValue <- NA
  if(!is.null(extras$interpolate)) interpolate <- extras$interpolate else interpolate <- TRUE
  if(!is.null(extras$add_coord)) add_coord <- extras$add_coord else add_coord <- TRUE
  
  # aggregate raster if too large
  if(maxpixels < ncell(r)) r <- aggregate(r, fact = ceiling(ncell(r)/maxpixels))
  
  # transform into data.frame
  df <- data.frame(as.data.frame(r, xy = T))
  colnames(df) <- c("x", "y", paste0("val", 1:(ncol(df)-2)))
  
  # factor if discrete to show categrocial legend
  df$fill <- df$val1
  if(r_type == "discrete") df$fill <- as.factor(df$fill)
  
  # transform to RGB colours
  if(r_type == "RGB"){
    if(is.na(maxColorValue)) maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = T)
    
    if(maxColorValue < max(c(df$val1, df$val2, df$val3), na.rm = T)){
      out("maxColorValue < maximum raster value. maxColorValue is set to maximum raster value.", type = 2)
      maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = T)
    }
    
    # remove NAs
    na.sel <- is.na(df$val1) | is.na(df$val2) | is.na(df$val3)
    if(any(na.sel)) df <- df[!na.sel,]
    
    df$fill <- grDevices::rgb(red = df$val1, green = df$val2, blue = df$val3, maxColorValue = maxColorValue)
  } else{
    
    # remove NAs
    na.sel <- is.na(df$val1)
    if(any(na.sel)) df <- df[!na.sel,]
  }
  # if NA gaps are there, use geom_tile, otherwise make it fast using geom_raster
  if(any(na.sel)){
    # remark: is this ever called?
    gg <- ggplot2::geom_tile(ggplot2::aes_string(x = "x", y = "y", fill = "fill"), data = df, alpha = alpha)
  } else{
    gg <- ggplot2::geom_raster(ggplot2::aes_string(x = "x", y = "y", fill = "fill"), data = df, alpha = alpha, interpolate = interpolate)
  }
  
  if(isFALSE(gglayer)){
    gg <- ggplot2::ggplot() + gg
    if(isTRUE(add_coord)) gg <-  gg + ggplot2::coord_sf()
    if(r_type == "RGB") gg <- gg + ggplot2::scale_fill_identity() 
  }
  return(gg)
}
