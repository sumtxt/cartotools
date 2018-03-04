#' Slice raster image in square tiles 
#' 
#' \code{tiling} uses \code{gdal_translate} to slice a raster
#' image into square tiles preserving the georeferencing of the input raster. 
#' 
#' The default settings preserve the resolution but 
#' use JPEG compression to reduce the tiles' file size. 
#' 
#' 
#' @param file raster image file name
#' @param size size of square tiles in pixels
#' @param outsize reduction in resolution (default=none)
#' @param path path to write to 
#' @param co compression algorithm
#' @param of output format
#' @param ... further parameters passed to \code{gdal_translate}
#'  
#' @return none 
#' 
#' @seealso \code{\link[gdalUtils]{gdal_translate}}
#' 
#' 
#' @examples 
#' \dontrun{
#'
#' library(raster)
#' 
#' file <- system.file("extdata", "example.tif", package = "cartotools")
#' plot(raster(file))
#' 
#' tiling(file, size=10)
#'  
#' }
#' 
#' @export
tiling <- function(file,size,outsize=100, path=NULL, co="COMPRESS=JPEG", of="GTiff", ...) {
          
    info <- raster(file)      
    outsize <- paste0(outsize,"%")

    y <- nrow(info)
    x <- ncol(info)

    xrang <- seq(0,x, by=size)
    yrang <- seq(0,y, by=size)

    xrang <- xrang[xrang!=x]
    yrang <- yrang[yrang!=y]

    if(!is.null(path)) path <- normalizePath(path, mustWork=TRUE)
    else path <- getwd()

    for (i in xrang) {
        for (j in yrang) {
            fout <- paste0(basename(file), "_", i, "_", j, ".tif")
            fout <- file.path(path,fout)
            srcwin <- c(i, j, size, size)
            gdal_translate(file, fout, 
            	srcwin=srcwin, of=of, 
            	outsize=c(outsize,outsize), 
            	co=co, ...)
        }
    }
}
