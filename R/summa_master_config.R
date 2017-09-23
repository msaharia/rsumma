# This is the configuration file for running parallel SUMMA jobs

#Templates
fileManagerTemplate <- 'data_raw/PNW2/summa_fileManager_template.txt'
zDecisionsTemplate <- 'data_raw/PNW2/summa_zDecisions_template.txt'

#Run Configurations
startDate <- '2016-10-01 00:00' #The start time of every simulation batch
nMonths <- 20

grustart <- 1
nGRU <- 20

#SUMMA
summaexe = '/d3/msaharia/SHARP/summa/bin/summa.exe'

