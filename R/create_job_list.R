#' Parallelization of SUMMA run
#'
#' @param config SUMMA Confg file
#' @param nGRU Length of each GRU set for a SUMMA run
#'
#' @return
#' @export
#'
#' @examples
#' summa('/d3/msaharia/SHARP/summa/PNW2/summa_fileManager_PNW.txt', 20)
summa <- function(config, nGRU){
  #CONFIG
  summaexe = "/d3/msaharia/SHARP/summa/bin/summa.exe"
  decisions = "/d3/msaharia/SHARP/summa/PNW2/summa_zDecisions_PNW.txt"

  #CREATE THE JOB LIST
  grustart = seq(1,100,20) #Creates the start GRU of every run
  joblist <- paste(summaexe, "-g", grustart, nGRU, "-m", config)

  return(joblist)

  #PUT IT IN SLURM QUEUE

}


#PLAY

runStart <- lubridate::ymd_hm('2016-10-01 00:00') #Start of the entire run
st <- runStart + months(0:2)
et <- runStart + months(1:3) - lubridate::dhours(1)
mult_one<-function(st,et, num_of_files){
  readLines("data_raw/PNW2/summa_zDecisions_template.txt") %>%
    gsub(pattern = "starttime", replace = st) %>%
    gsub(pattern = "endtime", replace = et) %>%
    # write.table('data_raw/PNW2/summazd.txt', row.names = F, col.names = F, quote = F)
    write.table(.,paste("data_raw/PNW2/template_", num_of_files, ".txt",sep=""),
                row.names=F, col.names=F, quote = F)
}
x <- mapply(mult_one,st,et,1:3)

# write.table(x[,3],'data_raw/PNW2/summazd.txt', row.names = F, col.names = F)

paste0('summa_zDecisions_', format(st, format="%Y%m%d"), '_',format(et, format="%Y%m%d"), ".txt")





# # s <<- sapply(grustart, function(i) paste(summaexe, "g", i, nGRU, "-m", config))
# a <-summa('/d3/msaharia/SHARP/summa/PNW2/summa_fileManager_PNW.txt', 20)
# system(paste(a[1]))
