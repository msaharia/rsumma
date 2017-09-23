source('R/summa_master_config.R')

########################################################################
# Bookkeeping
########################################################################
runStart <- lubridate::ymd_hm(startDate) #Start date of the entire simulation
startTimes <<- runStart + months(0:(nMonths-1)) #The start time of every simulation batch
endTimes <<- runStart + months(1:nMonths) - lubridate::dhours(1) #The end time of every simulation batch

zDecNames <- paste0('summa_zDecisions_', format(startTimes, format="%Y%m%d"), '_',
                    format(endTimes, format="%Y%m%d")) #zDecision file names
fileManNames <- paste0('summa_fileManager_', format(startTimes, format="%Y%m%d"), '_',
                       format(endTimes, format="%Y%m%d")) #fileManager file names

##################################################### ###################
# Creating zDecisions files
########################################################################
zDecisionsTemplate <- readLines('data_raw/PNW2/summa_zDecisions_template.txt')
Map(function(startTimes, endTimes, zDecNames_var){
  filled_zDecisionsTemplate <- gsub("starttime", paste0("'", format(startTimes, "%F %T"), "'"), zDecisionsTemplate, fixed = TRUE)
  filled_zDecisionsTemplate <- gsub("endtime", paste0("'", format(endTimes, "%F %T"), "'"), filled_zDecisionsTemplate, fixed = TRUE)
  writeLines(filled_zDecisionsTemplate, file.path("data_raw/PNW2/zDecisionFiles/", zDecNames_var))
  filled_zDecisionsTemplate    #return vector written to file
},
start = startTimes, #Start times
end = endTimes, #End times
zDecNames_var = paste0(zDecNames, ".txt") #zDecision file names
)

########################################################################
# Creating file Manager files
########################################################################
fileManagerTemplate <- readLines('data_raw/PNW2/summa_fileManager_template.txt')
Map(function(startTimes, endTimes, fileManNames_var){
  filled_fileManagerTemplate <- gsub("par_zDecisions", paste0("'", fileManNames_var, "'"), fileManagerTemplate, fixed = TRUE)
  writeLines(filled_fileManagerTemplate, file.path("data_raw/PNW2/",  fileManNames_var))
  filled_fileManagerTemplate    #return vector written to file
},
fileManNames_var = paste0("fileManagerFiles/", fileManNames, ".txt") #filemanager file names
)

########################################################################
# Create output folders
########################################################################


########################################################################
# Run SUMMA, re-create fileManagers with new Restart files
########################################################################
#CREATE THE JOB LIST
grustart = seq(1,10,20) #Creates the start GRU of every run
configFiles = paste0('data_raw/PNW2/fileManagerFiles/', fileManNames)
joblist <- paste(summaexe, "-g", grustart, nGRU, "-m", configFiles)









