#' Create zDecision files for every month automatically
#'
#' @param zDecFile zDecisions template file
#'
#' @examples
#' zDecNames <- zDecs('data_raw/PNW2/summa_zDecisions_template.txt')
zDecs <- function(zDecFile){
  runStart <- lubridate::ymd_hm(startDate) #Start date of the entire simulation
  startTimes <<- runStart + months(0:(nMonths-1)) #The start time of every simulation batch
  endTimes <<- runStart + months(1:nMonths) - lubridate::dhours(1) #The end time of every simulation batch
  zDecNames <- paste0('summa_zDecisions_', format(startTimes, format="%Y%m%d"), '_',
                       format(endTimes, format="%Y%m%d"), ".txt") #zDecision file names

  #Meta-Function for creating the separate zDecisions files
  multx<-function(startTimes, endTimes, zDecNames){
    readLines(zDecFile) %>%
      gsub(pattern = "starttime", replace = format(startTimes, "%F %T")) %>% #Replaces the start time
      gsub(pattern = "endtime", replace = format(endTimes, "%F %T")) %>% #Replaces the end time
      write.table(., paste0("data_raw/PNW2/zDecisionFiles/", zDecNames), #Writes out a decision file for every batch
                  row.names=F, col.names=F, quote = F)
  }
  x <- mapply(multx, startTimes, endTimes, zDecNames)
  #pro <- list(startTimes, endTimes, zDecNames)
  return(zDecNames)
}

template <- readLines('data_raw/PNW2/summa_zDecisions_template.txt')
file_data <- Map(function(startTimes, endTimes, name){
  filled_template <- gsub("starttime", format(startTimes, "%F %T"), template, fixed = TRUE)
  filled_template <- gsub("endtime", format(endTimes, "%F %T"), filled_template, fixed = TRUE)
  writeLines(filled_template, file.path("data_raw/PNW2/zDecisionFiles/", name))
  filled_template    # return vector written to file
},
start = startTimes,
end = endTimes,
name = paste0('summa_zDecisions_', format(startTimes, format="%Y%m%d"), '_',
              format(endTimes, format="%Y%m%d"), ".txt") #zDecision file names
)


