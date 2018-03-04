#' Calculate the bounding box for a set of points
#' 
#' \code{mbr} uses a fast convex hull algorithm to calculate the 
#' minimum bounding rectangle.
#' 
#' The code was adapted from the code posted here
#' \url{https://gis.stackexchange.com/a/174577}
#' 
#' @param points a two-dimensional matrix of points
#'  
#' @return \code{data.frame} of vertices of the minimum bounding rectangle
#' 
#' 
#' @seealso \code{\link[spatstat]{boundingbox}}
#' 
#' @examples 
#' \dontrun{
#'
#' points <- matrix(rnorm(1000), ncol=2) 
#' box <- mbr(points)
#' 
#' plot(points,pch=20)
#' polygon(box,border='red')
#' 
#' 
#' }
#' 
#' @export
mbr <- function(points) {

	points <- as.matrix(points)
	if (ncol(points) != 2) stop("points should be a matrix with two columns")

    tryCatch({

        a2 <- convhulln(points, options = 'FA')
        e <- points[a2$hull[,2],] - points[a2$hull[,1],]
        norms <- apply(e, 1, function(x) sqrt(x %*% x)) 
        v <- diag(1/norms) %*% as.matrix(e)
        w <- cbind(-v[,2], v[,1])

        vertices <- as.matrix((points) [a2$hull, 1:2])
        minmax <- function(x) c(min(x), max(x))
        x <- apply(vertices %*% t(v), 2, minmax)
        y <- apply(vertices %*% t(w), 2, minmax)
        areas <- (y[1,]-y[2,])*(x[1,]-x[2,])
        k <- which.min(areas)
        res <- as.data.frame(cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,]))
        return(unique(res))

    }, error = function(e) {
        assign('points', points, .GlobalEnv)
        stop(e)  
    })
}

