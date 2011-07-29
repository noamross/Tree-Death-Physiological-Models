runmodel = function(parmfile,outfile,plots=FALSE,saveplots=FALSE) {

  #Runs the model after loading parameters from file
  #Parameters:
  #parmfile = input file, see custom format
  #outfile = outputs, see custom format
  #plots = a vector of plot types (strings of functions) desired from the run
  #saveplots = a filename for plot outputs

  #load relevant data from file:

  parms=load.parmtable(parmfile)
  states0=load.statetable(parmfile)
  forcings=load.forcings(parmfile)
  comment=load.comment(parmfile)
  times=load.times(parmfile)
  cat(comment,sep="\n")

  #define forcing functions that interpolate between data points

  for(i in 1:(ncol(forcings)-1)) {
    assign(paste("func.",colnames(forcings)[i+1],sep=""), approxfun(x=forcings[,1], y=forcings[,(i+1)],method="constant", rule=2, f=0),envir = .GlobalEnv)
  }

  #run the model
  
  modelout = lsodar(y=states0, times=times, func=tree_odes, parms=parms,rootfunc=death_func)
  
  #save outputs
  
  save.run(parms=parms,modelout=modelout,outfile=outfile,comment=comment)
  
  #display or save selected plots
  
  if(plots[1]!=FALSE) for(i in 1:length(plots)) {
    if(saveplots!=FALSE){pdf(paste(file="Outputs/", saveplots, "_", plots[i], "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))}else{dev.new()}
        get(plots[i])(modelout,parms)
    if(saveplots!=FALSE){dev.off()}
      }
}
  
multirun = function(parmfile,changefile,outfile,plots=FALSE,saveplots=FALSE) {

  #Runs the model after loading parameters from file
  #Parameters:
  #parmfile = input file, see custom format
  #outfile = outputs, see custom format
  #plots = a vector of plot types (strings of functions) desired from the run
  #saveplots = a filename for plot outputs

  #load relevant data from file:

  parms=load.parmtable(parmfile)
  states0=load.statetable(parmfile)
  forcings=load.forcings(parmfile)
  comment=load.comment(parmfile)
  changes=load.changes(changefile)
  times=load.times(parmfile)
  cat(comment,sep="\n")
  filelist = matrix(0,nrow(changes),1)

  for(i in 1:nrow(changes)) {  #create a loop for runs equal to the length of the changed parameter matrix
    for(j in 1:ncol(changes)) {  #this loop replaces the parameter values in the parms list with the one in this row of changes
      if(!is.na(match(colnames(changes)[j],names(parms)))){parms[colnames(changes)[j]] = changes[i,j]}
      if(!is.na(match(colnames(changes)[j],colnames(forcings)))){forcings[,colnames((changes))[j]] = changes[i,j]}
      if(!is.na(match(colnames(changes)[j],names(states0)))){states0[colnames(changes)[j]] = changes[i,j]}
    }

    for(k in 1:(ncol(forcings)-1)) {
      assign(paste("func.",colnames(forcings)[k+1],sep=""), approxfun(x=forcings[,1], y=forcings[,(k+1)],method="constant", rule=2, f=0),envir = .GlobalEnv)
    }
 
    modelout=0
    modelout = lsodar(y=states0, times=times, func=tree_odes, parms=parms,rootfunc=death_func)
    if(dim(modelout)[1]<length(times)){
      modelout=rbind(modelout,matrix(NA,length(times)-dim(modelout)[1],dim(modelout)[2]))
      modelout[,"time"] = times
    }
    runfile = save.run(parms=parms,modelout=modelout,outfile=paste(outfile,i,sep=""),comment=comment)
    filelist[i,1] = runfile
#    filelist[i,2] = date()
  
  }
  filelist=cbind(filelist,changes)
  colnames(filelist)[1:2] = c("File","Date")
  multifile = paste("Outputs/", outfile, "_runs",".R",sep="")
  write(comment,file=multifile)
  write.fwf(as.data.frame(filelist), file=multifile, quote=TRUE, append=TRUE)
}