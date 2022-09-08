#' Flush \code{basemaps} cache
#'
#' This function flushes the \code{basemaps} cache and thereby removes all previously queried and/or composited products from the map directories (temporary or user-defined using the argument \code{map_dir}) used during the current session.
#' 
#' @return None.
#' 
#' @examples
#' library(basemaps)
#' flush_cache()
#' 
#' @name cache
#' @export
flush_cache <- function(){
  cached <- getOption("basemaps.cached")
  if(length(cached) > 0){
    out(paste0("Removing ", length(cached), " elements from map directories used during the current session..."))
    sapply(cached, function(x) unlink(x$file_comp))
  }
  options(basemaps.cached = list())
}