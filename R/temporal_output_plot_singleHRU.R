kpacks <- c('raster','sp','maptools','rgdal', 'ncdf4',
            'lubridate','reshape2', 'tidyverse')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

########################################################################
# Config
########################################################################
ncname = 'data_raw/PNW_2016-2017_G00001-00010_1.nc'
hruIndex = 1
ncfname <- file.path(ncname,fsep = .Platform$file.sep) #Get full file path of the NetCDF file
summa_forc <- nc_open(ncfname) #Open the NetCDF file
summa_atts <- ncatt_get(summa_forc, 0) #Get the attributes of the netCDF file

########################################################################
# Functions to extract a particular variable and the time indices
########################################################################
stime <- ncvar_get(summa_forc, 'time') #seconds since 1990
stime <- as.POSIXct(stime, origin="1990-01-01 00:00", tz = "GMT")

# Forcings
pptrate <- ncvar_get(summa_forc, 'pptrate')*3600 #Convert from kgm-2s-1 to mm/hour
scalarRainfall <- ncvar_get(summa_forc, 'scalarRainfall')*3600 # "computed rainfall rate (instant)" # Convert from kgm-2s-1 to mm/hour
SWRadAtm <- ncvar_get(summa_forc, 'SWRadAtm')
airtemp <- ncvar_get(summa_forc, 'airtemp')-272.15 #Convert to Centigrade

#Canopy
scalarCanopyIce <- ncvar_get(summa_forc, 'scalarCanopyIce') #mass of ice on the vegetation canopy (instant), kg m-2
scalarCanopyLiq <- ncvar_get(summa_forc, 'scalarCanopyLiq')
scalarSnowDepth <- ncvar_get(summa_forc, 'scalarSnowDepth')*1000 #From m to mm
scalarBelowCanopySolar <- ncvar_get(summa_forc, 'scalarBelowCanopySolar')
scalarWindspdCanopyBottom <- ncvar_get(summa_forc, 'scalarWindspdCanopyBottom')*3600000 #m/s to mm/h
scalarSenHeatTotal <- ncvar_get(summa_forc, 'scalarSenHeatTotal')
scalarSenHeatGround <- ncvar_get(summa_forc, 'scalarSenHeatGround')
scalarLatHeatTotal <- ncvar_get(summa_forc, 'scalarLatHeatTotal')

#Soil
scalarSnowSublimation <- ncvar_get(summa_forc, 'scalarSnowSublimation')*3600 #Convert from kgm-2s-1 to mm/hour
scalarThroughfallSnow <- ncvar_get(summa_forc, 'scalarThroughfallSnow')*3600 #Convert from kgm-2s-1 to mm/hour
scalarThroughfallRain <- ncvar_get(summa_forc, 'scalarThroughfallRain')*3600 #Convert from kgm-2s-1 to mm/hour
scalarRainPlusMelt <- ncvar_get(summa_forc, 'scalarRainPlusMelt')*3600000 #m/s to mm/h
scalarInfiltration <- ncvar_get(summa_forc, 'scalarInfiltration')*3600000 #m/s to mm/h
scalarSWE <- ncvar_get(summa_forc, 'scalarSWE')
scalarSurfaceRunoff <- ncvar_get(summa_forc, 'scalarSurfaceRunoff')*3600000 #m/s to mm/h
scalarSoilDrainage <- ncvar_get(summa_forc, 'scalarSoilDrainage')*3600000 #m/s to mm/h

########################################################################
# Soil Moisture
########################################################################
#Dimension
midTotoStartIndex <- ncvar_get(summa_forc, 'midTotoStartIndex') #start index of the midToto vector for a given timestep
nSnow <- ncvar_get(summa_forc, 'nSnow') #Number of snow layers at every time step
nLayers <- ncvar_get(summa_forc, 'nLayers') #Number of snow layers at every time step
mLayerVolFracLiq <- ncvar_get(summa_forc, 'mLayerVolFracLiq') #Soil Moisture
mLayerVolFracWat <- ncvar_get(summa_forc, 'mLayerVolFracWat') #Volumetric fraction of total water in each layer
mLayerTemp <- ncvar_get(summa_forc, 'mLayerTemp')-272.15 #Temperature in each layer

#Extracting individual soil layers
layerindex1 <- midTotoStartIndex+nSnow #Soil layer nearest to atmoshphere
layerindex2 <- midTotoStartIndex+nSnow+1
layerindex3 <- midTotoStartIndex+nSnow+2
layerindex4 <- midTotoStartIndex+nSnow+3
layerindex5 <- midTotoStartIndex+nSnow+4
layerindex6 <- midTotoStartIndex+nSnow+5
layerindex7 <- midTotoStartIndex+nSnow+6
layerindex8 <- midTotoStartIndex+nSnow+7 #Bottom-most soil layer

mLayerVolFracLiq <- data.frame(mLayerVolFracLiq[layerindex1], mLayerVolFracLiq[layerindex2],
           mLayerVolFracLiq[layerindex3], mLayerVolFracLiq[layerindex4],
           mLayerVolFracLiq[layerindex5], mLayerVolFracLiq[layerindex6],
           mLayerVolFracLiq[layerindex7], mLayerVolFracLiq[layerindex8]
)

mLayerVolFracWat <- data.frame(mLayerVolFracWat[layerindex1], mLayerVolFracWat[layerindex2],
                               mLayerVolFracWat[layerindex3], mLayerVolFracWat[layerindex4],
                               mLayerVolFracWat[layerindex5], mLayerVolFracWat[layerindex6],
                               mLayerVolFracWat[layerindex7], mLayerVolFracWat[layerindex8]
)

mLayerTemp <- data.frame(mLayerTemp[layerindex1], mLayerTemp[layerindex2],
                               mLayerTemp[layerindex3], mLayerTemp[layerindex4],
                               mLayerTemp[layerindex5], mLayerTemp[layerindex6],
                               mLayerTemp[layerindex7], mLayerTemp[layerindex8]
)

names(mLayerVolFracLiq) <- c("mLayerVolFracLiq_SoilLayer1", "mLayerVolFracLiq_SoilLayer2",
               "mLayerVolFracLiq_SoilLayer3", "mLayerVolFracLiq_SoilLayer4",
              "mLayerVolFracLiq_SoilLayer5", "mLayerVolFracLiq_SoilLayer6",
              "mLayerVolFracLiq_SoilLayer7", "mLayerVolFracLiq_SoilLayer8")

names(mLayerVolFracWat) <- c("mLayerVolFracWat_SoilLayer1", "mLayerVolFracWat_SoilLayer2",
                             "mLayerVolFracWat_SoilLayer3", "mLayerVolFracWat_SoilLayer4",
                             "mLayerVolFracWat_SoilLayer5", "mLayerVolFracWat_SoilLayer6",
                             "mLayerVolFracWat_SoilLayer7", "mLayerVolFracWat_SoilLayer8")


names(mLayerTemp) <- c("mLayerTemp_SoilLayer1", "mLayerTemp_SoilLayer2",
                             "mLayerTemp_SoilLayer3", "mLayerTemp_SoilLayer4",
                             "mLayerTemp_SoilLayer5", "mLayerTemp_SoilLayer6",
                             "mLayerTemp_SoilLayer7", "mLayerTemp_SoilLayer8")

########################################################################
# Create dataframes for plotting
########################################################################
#sumdata <- data.frame(stime, pptrate, scalarRainfall, scalarSoilDrainage, scalarInfiltration)
#sumdata_long <- melt(sumdata, id='stimevec')

sumdata_all <- data.frame(stime, pptrate, scalarRainfall, SWRadAtm, airtemp, #forcings
                      scalarCanopyIce, scalarCanopyLiq, scalarSnowDepth, #Canopy
                      scalarBelowCanopySolar, scalarWindspdCanopyBottom, scalarSenHeatTotal, scalarSenHeatGround,
                      scalarLatHeatTotal, scalarSnowSublimation, scalarThroughfallSnow, scalarThroughfallRain,
                      scalarRainPlusMelt , scalarInfiltration,  #Soil
                      scalarSWE, scalarSurfaceRunoff, scalarSoilDrainage,
                      mLayerVolFracLiq, mLayerVolFracWat, mLayerTemp, check.names=FALSE)

# sumdata_all_long <- melt(sumdata_all, id='stime')

########################################################################
# Individual plots
########################################################################
var_list = names(sumdata_all)
units_list = c("Time", "mm h-1",  "mm h-1", "W m-2", "°C",
               "mm", "mm", "mm",
               "W m-2", "mm h-1", "W m-2", "W m-2",
                  "W m-2", "mm hr-1", "mm h-1", "mm h-1",
               "mm h-1", "mm h-1", "kg m-2", "mm h-1",
               "mm h-1",
               " "," "," "," "," "," "," "," ",
              " "," "," "," "," "," "," "," ",
               "°C","°C","°C","°C","°C","°C","°C","°C")

#kg m-2 converted to mm

# sumdata_all <- sumdata_all[2:nrow(sumdata_all),]
# Make plots.
plot_list = list()
for (i in 2:length(var_list)) {
  p = ggplot(sumdata_all, aes_string(x = stime, y = var_list[i])) +
    #ylim(-9999, 10000) +
    #scale_y_continuous(limits = c(-999, NA))+
    # coord_cartesian(ylim = c(325, 500))+
    geom_line()+
    scale_x_datetime(date_labels = "%b %y")+
    ylab(units_list[i])+
    ggtitle(paste(var_list[i]))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x=element_blank())
  plot_list[[i]] = p
}

# create pdf where each page is a separate plot. ,
pdf("figures/summaoutput_wateryear_H9561_v2.pdf",  width=7, height=1.5)
for (i in 2:length(var_list)) {
  print(plot_list[[i]])
}
dev.off()

# ########################################################################
# # Multiline plot
# ########################################################################
# ggplot(sumdata_long, aes(x = stimevec, y = value, color = variable)) +
#   geom_line() +
#   #scale_color_hue(labels = c("PPT Rate", "asd","ass","assrqqq"))+
#   ylab('SUMMA Output Variables') +
#   xlab('Time')+
#   theme_bw()
#



