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

# # s <<- sapply(grustart, function(i) paste(summaexe, "g", i, nGRU, "-m", config))
# a <-summa('/d3/msaharia/SHARP/summa/PNW2/summa_fileManager_PNW.txt', 20)
# system(paste(a[1]))
