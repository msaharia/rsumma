#' Create zDecision files for every month automatically
#'
#' @param zDecFile zDecisions template file
#' @param startDate Start date of the simulation
#' @param nMonths Number of months from the start date for which files are to be created
#'
#' @examples
#' zDecs('data_raw/PNW2/summa_zDecisions_template.txt', '2016-10-01 00:00', 20)
zDecs <- function(zDecFile, startDate, nMonths){
  runStart <- lubridate::ymd_hm(startDate) #Start date of the entire simulation
  startTimes <- runStart + months(0:(nMonths-1)) #The start time of every simulation batch
  endTimes <- runStart + months(1:nMonths) - lubridate::dhours(1) #The end time of every simulation batch
  zDecNames <<- paste0('summa_zDecisions_', format(startTimes, format="%Y%m%d"), '_',
                       format(endTimes, format="%Y%m%d"), ".txt") #zDecision file names

  #Meta-Function for creating the separate zDecisions files
  multx<-function(startTimes, endTimes, zDecNames){
    readLines(zDecFile) %>%
      gsub(pattern = "starttime", replace = startTimes) %>% #Replaces the start time
      gsub(pattern = "endtime", replace = endTimes) %>% #Replaces the end time
      write.table(.,paste0("data_raw/PNW2/zDecisionFiles/", zDecNames), #Writes out a decision file for every batch
                  row.names=F, col.names=F, quote = F)
  }
  x <- mapply(multx, startTimes, endTimes, zDecNames)

}



