#' Plot SUMMA input variable for a given HRU
#'
#' Variables available for plotting \cr \cr
#' pptrate = Precipitation Rate \cr
#' SWRadAtm = Shortwave Radiation \cr
#' LWRadAtm = Longwave Radiation \cr
#' windspd = Wind Speed \cr
#' spechum = Specific Humidity \cr
#' airpres = Air Pressure \cr
#'
#' @param ncname Filename (with filepath) of the SUMMA forcing netCDF file produced by MetSim
#' @param varname Variable of interest in SUMMA forcing
#' @param hruindex HRU Index of interest in SUMMA forcing
#'
#' @return
#' @export p Plot of chosen variable v/s time
#'
#' @examples
#' forcingvar('data_raw/forcing/forecastforcing_modified.nc','pptrate', 1)
forcingvar <- function(ncname, ncvar, hruindex){
  summa_forc <- ncdf4::nc_open(ncname) #Load NetCDF file produced by MetSim
  summa_atts <- ncdf4::ncatt_get(summa_forc, 0) #Extract NetCDF file attributes

  # Load variables
  summa_forc_var <- ncdf4::ncvar_get(summa_forc, ncvar)
  summa_forc_hru <- ncdf4::ncvar_get(summa_forc, "hru")
  summa_forc_time <- ncdf4::ncvar_get(summa_forc, "time") # Hours from starting time

  start_time <- lubridate::ymd_hms(summa_atts$start) # Start time of forcing file
  stime <- start_time + lubridate::dhours(summa_forc_time)

  # Plot a particular HRU
  summa_forc_var <- summa_forc_var[hruindex,]
  plotdat <- data.frame(summa_forc_var,stime)

  # Nicer plot
  p <- ggplot2::ggplot(plotdat, ggplot2::aes(y = summa_forc_var, x = as.POSIXct(stime)))+
    ggplot2::geom_line()+
    ggplot2::xlab('Time')+
    ggplot2::ylab('Variable')+
    ggplot2::theme_bw()

  return(p)
}


