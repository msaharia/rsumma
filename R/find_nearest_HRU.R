#' Geographically closest HRU to given coordinates
#'
#' This function finds the HRU Index that is closest to a given latitude and longitude
#' @param ncname Filename (with filepath) of the SUMMA Local Attributes netCDF file
#' @param uLat Latitude
#' @param uLong Longitude
#'
#' @return HRU Index and its coordinates that is nearest to uLat and uLong
#' @export
#'
#' @examples
#' nearestHRU('data_raw/summa_zLocalAttributes_columbia_gru.nc', 47.277603, -121.786773)
nearestHRU <- function(ncname, uLat, uLong){
  local_attrs <- ncdf4::nc_open(ncname) #Opens the NetCDF file
  lat <- ncdf4::ncvar_get(local_attrs, 'latitude') #Extracts all latitudes
  long <- ncdf4::ncvar_get(local_attrs, 'longitude') #Extracts all longitudes

  df <- data.frame(lat, long) #Creates dataframe with all latitudes and longitudes

  #Finds the HRU Index and Lat/Long
  hru <- df[which.min(unlist(lapply(1:NROW(df), function(i)
    dist(rbind(c(df[i, c("lat", "long")]), c(uLat, uLong)))))),]

  return(hru)
}
