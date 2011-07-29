#The functions in this file run multiple instances of the tree model

require(deSolve)                                                                #load relevant packages and function files
require(rootSolve)
require(gdata)
source("R/treephys_ODEs.r")
source("R/weibullcurves.R")
source("R/modelplots.R")
source("R/treephysinout.R")
#source("R/parameterfile.R")                                                         #load a parameter file file
parms=load.parmtable("parmfile.R")
states0=load.statetable("parmfile.R")
changes=load.changes("R/changeparms.R")
times=1:1000
deathcurve = matrix(0,nrow(changes),2)
colnames(deathcurve) = c("Time", "Mode")
filelist = matrix(0,nrow(changes),2)

for(i in 1:nrow(changes)) {  #create a loop for runs equal to the length of the changed parameter matrix
  for(j in 1:ncol(changes)) {  #this loop replaces the parameter values in the parms list with the one in this row of changes
    parms[colnames(changes)[j]] = changes[i,j]
  }
  modelout=0
  modelout = lsodar(y=states0, times=times, func=tree_odes, parms=parms,rootfunc=death_func)
  tablefile=paste("Outputs/Multirun" ,i, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R", sep="")
  filelist[i,1] = tablefile
  filelist[i,2] = date()
  write.fwf(as.data.frame(parms),file=tablefile,append=TRUE)
  write.fwf(as.data.frame(modelout),file=tablefile,append=TRUE)
  deathcurve[i,1] = nrow(modelout)
  deathcurve[i,2] = ifelse(tail(modelout[,"S"],1)<=parms$S.death,"S", ifelse(tail(modelout[,"K"],1)<=parms$K.death, "K", "N"))
}
deathcurve = cbind(changes, deathcurve)
filelist=cbind(filelist,changes)

run4 = as.matrix(read.table("Outputs/Multirun1_20110713_113003.R",skip=2,header=TRUE))
run4parms = as.list(read.table("Outputs/Multirun1_20110713_113003.R",header=TRUE,nrows=1))
plot.stress(run4,run4parms)

rle
