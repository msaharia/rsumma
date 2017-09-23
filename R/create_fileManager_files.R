#Function for creating file Manager files

fileMans <- function(zDecNames, fileManNames){
  fileManNames <- paste0('summa_fileManager_', format(startTimes, format="%Y%m%d"), '_',
                         format(endTimes, format="%Y%m%d"), ".txt") #fileManager file names

  readLines('data_raw/PNW2/summa_fileManager_template.txt') %>%
    gsub(pattern = "par_zDecisions", replace = zDecNames) %>% #Replaces the zDecisions parameter in the template
    write.table(., paste0("data_raw/PNW2/fileManagerFiles/", fileManNames), #Writes out a decision file for every batch
                row.names=F, col.names=F, quote = F)

}

x <- mapply(fileMans, zDecNames, fileManNames)
