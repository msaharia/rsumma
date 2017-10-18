summaout <- nc_open('data_raw/benchmarking/PNW_3L_1H.nc') #Open the SUMMA output NetCDF file
ncvars <- names(summaout[['var']]) #Get all the variables in the netCDF file

########################################################################
# Extract time indices
########################################################################
stime <- ncvar_get(summaout, 'time') #seconds since 1990
stime <- as.POSIXct(stime, origin="1990-01-01 00:00", tz = "GMT")

hruIndex = 1
ncvars <- ncvars[1:5]
t <- sapply(ncvars, function(x) ncvar_get(summaout, x)[hruIndex,], USE.NAMES = TRUE)
sumdata_all <- data.frame(stime, t)


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

devtools::use_data(sumdata_units_lookup)

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
pdf('figures/temporaltesting.pdf',  width=7, height=1.5)
for (yvar in names(sumdata_all)[-1]) {
  print(plot_list[yvar])
}
dev.off()

nc_close(summaout)
