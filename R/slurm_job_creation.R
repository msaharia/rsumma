# cat("#!/bin/bash", file=slurmFile, append=TRUE, sep = "\n")
# cat("#SBATCH --nodes=2 # Set the number of nodes", file=slurmFile, append=TRUE, sep = "\n")
# cat("#SBATCH --ntasks-per-node=16   # Number of tasks per node", file=slurmFile, append=TRUE, sep = "\n")
# cat("#SBATCH --job-name=summa       # Job name", file=slurmFile, append=TRUE, sep = "\n")
# cat("#SBATCH --mail-type=ALL        # Mail events ", file=slurmFile, append=TRUE, sep = "\n")
# cat("#SBATCH --mail-user=manab@ucar.edu", file=slurmFile, append=TRUE, sep = "\n")
# cat("#SBATCH --output=serial_test_%j.ou", file=slurmFile, append=TRUE, sep = "\n")
# # cat("echo \"Running SUMMA on a single CPU core\" ", file=slurmFile, append=TRUE, sep = "\n")

#User supplied
summaexe <-  '/glade/u/home/manab/SHARP/summa/bin/summa.exe'
lenGRU <- 50                                                  #Length of every GRU set
strtGRU <-  seq(1, 11723, by = lenGRU)                            #Start ID of every GRU
lenJob <- 36                                                  #Numbe of Jobs per Joblist
fileMan <- '/glade/p/work/manab/SHARP/summa/PNW_3L/summa_fileManager_PNW3L_3H.txt'

#List of all run Commands
summaruncommands <- paste(summaexe, '-g', strtGRU, lenGRU , '-r d', '-m', fileMan, sep=" ")
summaruncommands <- append(head(summaruncommands, -1),
                           paste(summaexe, '-g', tail(strtGRU,1), 11723%%lenGRU ,
                                 '-r d', '-m', fileMan, sep=" "))


for(i in seq(from = 1, to = length(summaruncommands), by = lenJob)){
  #Create job lists
  jobfileName <- paste0('data_raw/PNW/joblists/summa_joblist_', i)
  cat(paste(summaruncommands[i:(i+lenJob-1)]), file=jobfileName, append=TRUE, sep = "\n")
  print(jobfileName)

  #Create job submission scripts
  pbsFile =  paste0('data_raw/PNW/pbsscripts/pbs_', i, ".sh")

  cat("#!/bin/bash -u", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -A P48500028", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -N summa_20150101_20161231_3L_3H", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -j oe", file=pbsFile, append=TRUE, sep = "\n")
  cat("##PBS -m abe", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -M manab@ucar.edu", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -q regular", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -l walltime=02:00:00", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -l select=1:ncpus=36:mpiprocs=36", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -o ./log/summa3L3H.out", file=pbsFile, append=TRUE, sep = "\n")
  cat("#PBS -e ./log/summa3L3H.err", file=pbsFile, append=TRUE, sep = "\n")
  cat("mkdir -p /glade/scratch/manab/temp", file=pbsFile, append=TRUE, sep = "\n")
  cat("export TMPDIR=/glade/scratch/manab/temp", file=pbsFile, append=TRUE, sep = "\n")
  cat("export MPI_SHEPHERD=true", file=pbsFile, append=TRUE, sep = "\n")
  cat(paste('mpiexec_mpt', 'launch_cf.sh', jobfileName), file=pbsFile, append=TRUE, sep = " ")
}

#Create individual job submission scripts



