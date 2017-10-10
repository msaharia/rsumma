#' Returns a dataframe of SUMMA output variables for plotting purposes
#'
#' @return
#' @export
#'
#' @examples
temporalplot('data_raw/20150101_20161231_GRU9561_3L/PNW3L_2015-2016_H9561_1.nc', "figures/summaoutput_wateryear_H9561_3L.pdf")
temporalplot <- function(ncname, outfile){

  ########################################################################
  # Opens the NetCDF file
  ########################################################################
  # ncname <- file.path(ncname,fsep = .Platform$file.sep) #Get full file path of the NetCDF file
  summaout <- nc_open(ncname) #Open the SUMMA output NetCDF file
  ncvars <- names(summaout[['var']]) #Get all the variables in the netCDF file

  ########################################################################
  # Functions to extract a particular variable and the time indices
  ########################################################################
  stime <- ncvar_get(summaout, 'time') #seconds since 1990
  stime <- as.POSIXct(stime, origin="1990-01-01 00:00", tz = "GMT")

  #Forcings
  pptrate <- ncvar_get(summaout, 'pptrate')*3600 #Convert from kgm-2s-1 to mm/hour
  airtemp <- ncvar_get(summaout, 'airtemp')-272.15 #Convert to Centigrade
  windspd <- ncvar_get(summaout, 'windspd') #m/s
  scalarRainfall <- ncvar_get(summaout, 'scalarRainfall')*3600 # "computed rainfall rate (instant)" # Convert from kgm-2s-1 to mm/hour
  SWRadAtm <- ncvar_get(summaout, 'SWRadAtm')
  LWRadAtm <- ncvar_get(summaout, 'LWRadAtm')

  #Others
  scalarSurfaceTemp <- ncvar_get(summaout, 'scalarSurfaceTemp') -272.15 #Convert to Centigrade

  #Canopy
  scalarCanopyIce <- ncvar_get(summaout, 'scalarCanopyIce') #mass of ice on the vegetation canopy (instant), kg m-2
  scalarCanopyLiq <- ncvar_get(summaout, 'scalarCanopyLiq')
  scalarSnowDepth <- ncvar_get(summaout, 'scalarSnowDepth')*1000 #From m to mm
  scalarBelowCanopySolar <- ncvar_get(summaout, 'scalarBelowCanopySolar')
  scalarWindspdCanopyBottom <- ncvar_get(summaout, 'scalarWindspdCanopyBottom')*3600000 #m/s to mm/h
  scalarSenHeatGround <- ncvar_get(summaout, 'scalarSenHeatGround')
  scalarSenHeatTotal <- ncvar_get(summaout, 'scalarSenHeatTotal')
  scalarLatHeatGround <- ncvar_get(summaout, 'scalarLatHeatGround')
  scalarLatHeatTotal <- ncvar_get(summaout, 'scalarLatHeatTotal')

  #Soil
  scalarSnowSublimation <- ncvar_get(summaout, 'scalarSnowSublimation')*3600 #Convert from kgm-2s-1 to mm/hour
  scalarThroughfallSnow <- ncvar_get(summaout, 'scalarThroughfallSnow')*3600 #Convert from kgm-2s-1 to mm/hour
  scalarThroughfallRain <- ncvar_get(summaout, 'scalarThroughfallRain')*3600 #Convert from kgm-2s-1 to mm/hour
  scalarRainPlusMelt <- ncvar_get(summaout, 'scalarRainPlusMelt')*3600000 #m/s to mm/h
  scalarInfiltration <- ncvar_get(summaout, 'scalarInfiltration')*3600000 #m/s to mm/h
  scalarExfiltration <- ncvar_get(summaout, 'scalarExfiltration')*3600000 #m/s to mm/h
  scalarSWE <- ncvar_get(summaout, 'scalarSWE')*3600000 #m/s to mm/h
  scalarSurfaceRunoff <- ncvar_get(summaout, 'scalarSurfaceRunoff')*3600000 #m/s to mm/h
  scalarSoilDrainage <- ncvar_get(summaout, 'scalarSoilDrainage')*3600000 #m/s to mm/h

  # Layer information
  nLayers <- ncvar_get(summaout, 'nLayers') #Total of layers at every time step
  nSnow <- ncvar_get(summaout, 'nSnow') #Number of snow layers at every time step
  nSoil <- ncvar_get(summaout, 'nSoil') #Number of soil layers at every time step
  nLayers <- ncvar_get(summaout, 'nLayers') #Number of snow layers at every time step

  ########################################################################
  # Soil Moisture
  ########################################################################
  #Dimensions
  midTotoStartIndex <- ncvar_get(summaout, 'midTotoStartIndex') #start index of the midToto (snow+soil) vector for a given timestep

  #Make a list of soil layer indices ignoring the snow layers
  soillayerindexlist <- lapply(seq(1,max(nSoil)), function(x) midTotoStartIndex+nSnow+x-1)

  #Extracting for soil moisture for different layers where total no. of layers is max(nLayers)
  mLayerVolFracLiq <- ncvar_get(summaout, 'mLayerVolFracLiq') #Soil Moisture

  #Separate the soil moisture for different soil layers for easier plotting
  mLayerVolFracLiq_soil <- data.frame(sapply(soillayerindexlist, function(x) mLayerVolFracLiq[x])) #Make a list of data
  mLayerVolFracLiq_soil <- setNames(mLayerVolFracLiq_soil,  #Set the names of different soil layers automatically
           lapply(seq(1,max(nSoil)), function(x) paste0('mLayerVolFracLiq_SoilLayer',x)))

  ########################################################################
  # mLayerVolFracWat - Volumetric fraction of total water in each layer
  ########################################################################
  mLayerVolFracWat <- ncvar_get(summaout, 'mLayerVolFracWat') #Soil Moisture
  mLayerVolFracWat_soil <- data.frame(sapply(soillayerindexlist, function(x) mLayerVolFracWat[x])) #Make a list of data
  mLayerVolFracWat_soil <- setNames(mLayerVolFracWat_soil,  #Set the names of different soil layers automatically
                                    lapply(seq(1,max(nSoil)), function(x) paste0('mLayerVolFracWat_SoilLayer',x)))

  ########################################################################
  # mLayerVolFracIce - Volumetric fraction of Ice in each layer
  ########################################################################
  mLayerVolFracIce <- ncvar_get(summaout, 'mLayerVolFracIce') #Soil Moisture
  mLayerVolFracIce_soil <- data.frame(sapply(soillayerindexlist, function(x) mLayerVolFracIce[x])) #Make a list of data
  mLayerVolFracIce_soil <- setNames(mLayerVolFracIce_soil,  #Set the names of different soil layers automatically
                                    lapply(seq(1,max(nSoil)), function(x) paste0('mLayerVolFracIce_SoilLayer',x)))

  ########################################################################
  # mLayerMatricHead - Matric Head in each layer
  ########################################################################
  mLayerMatricHead <- ncvar_get(summaout, 'mLayerMatricHead') #Soil Moisture
  mLayerMatricHead_soil <- data.frame(sapply(soillayerindexlist, function(x) mLayerMatricHead[x])) #Make a list of data
  mLayerMatricHead_soil <- setNames(mLayerMatricHead_soil,  #Set the names of different soil layers automatically
                                    lapply(seq(1,max(nSoil)), function(x) paste0('mLayerMatricHead_SoilLayer',x)))

  ########################################################################
  # mLayerTemp - #Temperature in each layer
  ########################################################################
  mLayerTemp <- ncvar_get(summaout, 'mLayerTemp')-272.15 #Soil Moisture
  mLayerTemp_soil <- data.frame(sapply(soillayerindexlist, function(x) mLayerTemp[x])) #Make a list of data
  mLayerTemp_soil <- setNames(mLayerTemp_soil,  #Set the names of different soil layers automatically
                                    lapply(seq(1,max(nSoil)), function(x) paste0('mLayerTemp_SoilLayer',x)))

  ########################################################################
  # mLayerHeight - #Height in each layer
  ########################################################################
  # mLayerHeight <- ncvar_get(summaout, 'mLayerHeight') #Soil Moisture
  # mLayerHeight_soil <- data.frame(sapply(soillayerindexlist, function(x) mLayerHeight[x])) #Make a list of data
  # mLayerHeight_soil <- setNames(mLayerHeight_soil,  #Set the names of different soil layers automatically
  #                             lapply(seq(1,max(nSoil)), function(x) paste0('mLayerHeight_SoilLayer',x)))


  ########################################################################
  # Create dataframes for plotting
  ########################################################################
  #sumdata_long <- melt(sumdata, id='stimevec')
  sumdata_all <- data.frame(stime, pptrate, airtemp, windspd, scalarRainfall, SWRadAtm, LWRadAtm, #forcings
                            scalarSurfaceTemp, #others
                            scalarCanopyIce, scalarCanopyLiq, scalarSnowDepth, #Canopy
                            scalarBelowCanopySolar, scalarSenHeatGround, scalarSenHeatTotal,
                            scalarLatHeatGround, scalarLatHeatTotal, scalarSnowSublimation, scalarThroughfallSnow, scalarThroughfallRain,
                            scalarRainPlusMelt , scalarInfiltration, scalarExfiltration,  #Soil
                            scalarSWE, scalarSurfaceRunoff, scalarSoilDrainage,
                            mLayerVolFracLiq_soil, #Soil layer variables
                            mLayerVolFracWat_soil,
                            mLayerVolFracIce_soil,
                            mLayerTemp_soil,
                            check.names=FALSE)
  return(sumdata_all)

  sumdata_units_lookup <- data.frame(var_names=c("stime", "pptrate", "airtemp", "windspd", "scalarRainfall", "SWRadAtm", "LWRadAtm", #forcings
                                                 "fieldCapacity", "HRUarea", "scalarSurfaceTemp", #others
                                                 "scalarCanopyIce", "scalarCanopyLiq", "scalarSnowDepth", #Canopy
                                                 "scalarBelowCanopySolar", "scalarWindspdCanopyBottom", "scalarSenHeatGround", "scalarSenHeatTotal",
                                                 "scalarLatHeatGround", "scalarLatHeatTotal", "scalarSnowSublimation", "scalarThroughfallSnow", "scalarThroughfallRain",
                                                 "scalarRainPlusMelt" , "scalarInfiltration", "scalarExfiltration",  #Soil
                                                 "scalarSWE", "scalarSurfaceRunoff", "scalarSoilDrainage",
                                                 "mLayerTemp_SoilLayer1", "mLayerTemp_SoilLayer2", "mLayerTemp_SoilLayer3", "mLayerTemp_SoilLayer4", "mLayerTemp_SoilLayer5","mLayerTemp_SoilLayer6","mLayerTemp_SoilLayer7","mLayerTemp_SoilLayer8",
                                                 "mLayerHeight_SoilLayer1","mLayerHeight_SoilLayer2", "mLayerHeight_SoilLayer3", "mLayerHeight_SoilLayer4", "mLayerHeight_SoilLayer5", "mLayerHeight_SoilLayer6", "mLayerHeight_SoilLayer7", "mLayerHeight_SoilLayer8"),
                                         y_lab=c("h", "mm h-1", "°C", "m s-1", "mm h-1", "W m-2", "W m-2", #forcings
                                                 " ", " ", "°C", #others
                                                 "mm", "mm", "mm", #canopy
                                                 "W m-2", "m s-1",  "W m-2",  "W m-2",
                                                 "W m-2",  "W m-2", "mm h-1", "mm h-1", "mm h-1",
                                                 "mm s-1", "mm s-1", "mm s-1", #Soil
                                                 "kg m-2", "mm h-1", "mm h-1",
                                                 "°C", "°C", "°C", "°C", "°C", "°C", "°C", "°C",
                                                 "m", "m", "m", "m", "m", "m", "m", "m" ))

  #######################################################################
  #Individual plots
  #######################################################################
  plot_list = list()
  for (yvar in names(sumdata_all)[-1]) {
    p = ggplot(sumdata_all, aes_string(x = stime, y = yvar)) +
      geom_line()+
      scale_x_datetime(date_labels = "%b %y")+
      # ylab(units_list[i])+
      ylab(sumdata_units_lookup$y_lab[match(yvar, sumdata_units_lookup$var_names)])+ #Matching units with variable using a loopup table
      ggtitle(yvar)+
      theme_bw()+


      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank())
    plot_list[[yvar]] = p
  }

  # create pdf where each page is a separate plot. ,
  pdf(outfile,  width=7, height=1.5)
  for (yvar in names(sumdata_all)[-1]) {
    print(plot_list[yvar])
  }
  dev.off()
}


# # #ALTERNATIVE
# layerindex1 <- midTotoStartIndex+nSnow #Soil layer nearest to atmoshphere
# layerindex2 <- midTotoStartIndex+nSnow+1
# layerindex3 <- midTotoStartIndex+nSnow+2
# layerindex4 <- midTotoStartIndex+nSnow+3
# layerindex5 <- midTotoStartIndex+nSnow+4
# layerindex6 <- midTotoStartIndex+nSnow+5
# layerindex7 <- midTotoStartIndex+nSnow+6
# layerindex8 <- midTotoStartIndex+nSnow+7 #Bottom-most soil layer
#
#
# #Extracting individual soil layers
# layerlist <- list(layerindex1, layerindex2, layerindex3,
#                   layerindex4, layerindex5, layerindex6,
#                   layerindex7, layerindex8)
#
#
# mLayerVolFracLiqold <- data.frame(mLayerVolFracLiq[layerindex1], mLayerVolFracLiq[layerindex2],
#                                   mLayerVolFracLiq[layerindex3], mLayerVolFracLiq[layerindex4],
#                                   mLayerVolFracLiq[layerindex5], mLayerVolFracLiq[layerindex6],
#                                   mLayerVolFracLiq[layerindex7], mLayerVolFracLiq[layerindex8]
# )
