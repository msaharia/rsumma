#' Parallelization of SUMMA run
#'
#' @param config SUMMA Confg file
#' @param nGRU Length of each GRU set for a SUMMA run
#'
#' @return
#' @export
#'
#' @examples
#' summa('/d3/msaharia/SHARP/summa/PNW2/summa_fileManager_PNW.txt', 10)
summa <- function(config, nGRU){

  gru1 = 1
  grustart = seq(1,100,20)

  summaexe = '/d3/msaharia/SHARP/summa/bin/summa.exe'

  for(i in seq(from=1, to=100, by=20)){
    system(paste(summaexe, "-r d -g", i, nGRU, "-m", config))

    print(i)
  }
}

PNW_2016-12-25-00_spinup_G00061-00080_1.ncPNW_2016-12-25-00_spinup_G00061-00080_1.nc
