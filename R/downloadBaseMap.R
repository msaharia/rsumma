#' Title
#'
#' @return sp.raster
#' @export
#'
#' @examples
#'downloadBaseMap('sfmer')
downloadBaseMap <- function(spdf){
  # Reprojects data
  spdf <- sp::spTransform(spdf, CRS("+init=epsg:3857"))

  # Dealing with inconsistencies in units for API.
  box <- spdf@bbox

  midpoint <- c(mean(box[1, ]), mean(box[2, ]))
  left.bottom <- c(box[1, 1], box[2, 1])
  top.right <- c(box[1, 2], box[2, 2])

  boundaries <- SpatialPoints(rbind(left.bottom, top.right))
  proj4string(boundaries) <- CRS("+init=epsg:3857")
  boundaries.latlong <- c(t(spTransform(boundaries, CRS("+init=epsg:4326"))@coords))

  # SET MAP TYPE HERE, LEAVE OTHER PARAMETERS AS THEY ARE
  gmap <- get_map(boundaries.latlong, maptype = "terrain", source = "stamen", crop = TRUE)

  # COPY-PASTE SEGMENT 2 Create object that sp.layout likes.
  long.center <- midpoint[1]
  lat.center <- midpoint[2]
  height <- box[2, 2] - box[2, 1]
  width <- box[1, 2] - box[1, 1]

  sp.raster <- list("grid.raster", gmap, x = long.center, y = lat.center, width = width,
                    height = height, default.units = "native", first = TRUE)
  return(sp.raster)
}


