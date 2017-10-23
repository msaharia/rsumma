#' Extracts variable data from a SUMMA output file for a single HRU
#'
#' @param ncfile #SUMMA output file in netCDF format
#' @param hruIndex #Index of the HRU set for data is sought
#'
#' @return Dataframe of variables contained in the SUMMA output netCDF file
#' @export
#'
#' @examples
#' sumdata_all <- temporaloutputdata('vault/benchmarking/PNW_3L_1H.nc', 1)
temporaloutputdata <- function(ncfile, hruIndex){
  ########################################################################
  # Reading data and extracing variables
  ########################################################################
  summaout <- ncdf4::nc_open(ncfile) #Open the SUMMA output NetCDF file

  #Get all the variables in the netCDF file and remove those without necessary time dimension
  ncvars <- names(summaout[['var']]) %>%
    .[!grepl('^HRUarea$|^fieldCapacity|^averageInstantRunoff$|^averageRoutedRunoff$|basin__',.)]

  ########################################################################
  # Extract time indices #seconds since 1990
  ########################################################################
  stime <- ncdf4::ncvar_get(summaout, 'time') %>%
    as.POSIXct(., origin="1990-01-01 00:00", tz = "GMT")

  #Create a dataframe with time and all variables
  sumdata_all <- sapply(ncvars, function(x) ncdf4::ncvar_get(summaout, x)[hruIndex,], USE.NAMES = TRUE)
  sumdata_all <- data.frame(stime, sumdata_all)

  return(sumdata_all)
  ncdf4::nc_close(summaout)
}
