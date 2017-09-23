kpacks <- c('ncdf4')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#Old initial conditions netCDF file
ncname <- 'data_raw/PNW_3L/summa_zInitialCond_PNW.nc'
icond <- ncdf4::nc_open(ncname)

#New dimensions for IC file
ifcdim <- icond$dim[['ifcToto']]
hrudim <- icond$dim[['hru']]
scaldim <- icond$dim[['scalarv']]
midtdim <- ncdim_def("MidToto", "", 1:3) #3 layers
midsdim <- icond$dim[['midSoil']]

mv <- -9999

#Define the data
iLayerHeight = ncvar_def( "iLayerHeight", "units", list(ifcdim, hrudim), mv, prec="double")
scalarCanopyTemp = ncvar_def( "scalarCanopyTemp", "units", list(scaldim, hrudim), mv, prec="double")
nSoil = ncvar_def( "nSoil", "units", list(scaldim, hrudim), mv, prec="short")
mLayerVolFracLiq = ncvar_def( "mLayerVolFracLiq", "units", list(midtdim, hrudim), mv, prec="double")
mLayerTemp = ncvar_def( "mLayerTemp", "units", list(midtdim, hrudim), mv, prec="double")
scalarCanopyIce = ncvar_def( "scalarCanopyIce", "units", list(scaldim, hrudim), mv, prec="double")
scalarSfcMeltPond = ncvar_def( "scalarSfcMeltPond", "units", list(scaldim, hrudim), mv, prec="double")
scalarSWE = ncvar_def( "scalarSWE", "units", list(scaldim, hrudim), mv, prec="double")
scalarCanairTemp = ncvar_def( "scalarCanairTemp", "units", list(scaldim, hrudim), mv, prec="double")
scalarSnowDepth = ncvar_def( "scalarSnowDepth", "units", list(scaldim, hrudim), mv, prec="double")
mLayerVolFracIce = ncvar_def( "mLayerVolFracIce", "units", list(midtdim, hrudim), mv, prec="double")
dt_unit = ncvar_def( "dt_unit", "units", list(scaldim, hrudim), mv, prec="double")
mLayerDepth = ncvar_def( "mLayerDepth", "units", list(midtdim, hrudim), mv, prec="double")
mLayerMatricHead = ncvar_def( "mLayerMatricHead", "units", list(midsdim, hrudim), mv, prec="double")
scalarAquiferStorage = ncvar_def( "scalarAquiferStorage", "units", list(scaldim, hrudim), mv, prec="double")
scalarSnowAlbedo = ncvar_def( "scalarSnowAlbedo", "units", list(scaldim, hrudim), mv, prec="double")
nSnow = ncvar_def( "nSnow", "units", list(scaldim, hrudim), mv, prec="short")
scalarCanopyLiq = ncvar_def( "scalarCanopyLiq", "units", list(scaldim, hrudim), mv, prec="double")

#Create the NetCDF file
nc = nc_create("data_raw/PNW_3L/test.nc", list(iLayerHeight, scalarCanopyTemp, nSoil, mLayerVolFracLiq,
                               mLayerTemp, scalarCanopyIce, scalarSfcMeltPond, scalarSWE,
                               scalarCanairTemp, scalarSnowDepth, mLayerVolFracIce, dt_unit,
                               mLayerDepth, mLayerMatricHead, scalarAquiferStorage, scalarSnowAlbedo,
                               nSnow, scalarCanopyLiq), force_v4=TRUE)

#Get variable data
mLayerDepthdat <- ncvar_get(icond, 'mLayerDepth')[,1:3] #3 Layer to be adjusted later

#Write data to the NetCDF file
ncvar_put(nc, mLayerDepth, mLayerDepthdat)




