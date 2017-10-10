multiline <- function(summavar){
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

  if(summavar %in% names(L3) == TRUE){
    summadf <- cbind(L3['stime'], L3[summavar], L8[summavar])
    colnames(summadf)[2] <- paste(colnames(summadf)[2],"3L", sep="_")
    colnames(summadf)[3] <- paste(colnames(summadf)[3],"8L", sep="_")

    summadf %>%
      gather(variable, value, -stime) %>%
      ggplot(aes(x=stime, y=value, colour=variable))+
        geom_line() +
        scale_color_manual(values =c("red","green"), labels=c("3 Layer","8 Layer"))+
        ylab(sumdata_units_lookup$y_lab[match(summavar, sumdata_units_lookup$var_names)])+
        ggtitle(summavar)+

        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.x=element_blank()) %>%
      return()

  } else{
    L8 %>%
      ggplot(aes_string(x='stime', y=summavar))+
        geom_line(colour='green') +
        ylab(sumdata_units_lookup$y_lab[match(summavar, sumdata_units_lookup$var_names)])+
        ggtitle(paste0(summavar, ' (8 Layer)', sep=""))+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank()) %>%
    return()

  }
}

L3_H1 <- temporalplot('data_raw/benchmarking/PNW_3L_1H.nc', "figures/summaoutput_3L_1H.pdf")
L8 <- temporalplot('data_raw/20150101_20161231_GRU9561/PNW_2015-2016_H9561_1.nc')


annualRunoff_L3 <- sum(L3['scalarSurfaceRunoff']) + sum(L3['scalarSoilDrainage'])
annualRunoff_L8 <- sum(L8['scalarSurfaceRunoff']) + sum(L8['scalarSoilDrainage'])

runoffRatio_L3 <- annualRunoff_L3/sum(L3['pptrate'])
runoffRatio_L8 <- annualRunoff_L8/sum(L8['pptrate'])

maxSWE_L3 <- max(L3['scalarSWE'])
maxSWE_L8<- max(L8['scalarSWE'])

annualRunoff <- c(annualRunoff_L3, annualRunoff_L8)
maxSWE <- c(maxSWE_L3, maxSWE_L8)
runoffRatio <- c(runoffRatio_L3, runoffRatio_L8)

calcvars <- data.frame(rbind(annualRunoff, maxSWE, runoffRatio)) %>%
  setNames(c("3 Layer", "8 Layer"))


nams <- colnames(L8)[-1]

x <- lapply(nams, multiline)

# create pdf where each page is a separate plot. ,
pdf('figures/out.pdf',  width=7, height=1.5)
grid.table(calcvars)
for (i in seq(1,length(L8))) {
  print(x[i])
}
dev.off()

