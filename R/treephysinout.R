save.run = function(parms,modelout,comment,outfile) {
  
  #Saves the run in my custom format
  #modelout = output matrix of a run
  #parms = list of parameters
  #outfile = the start of the fileneame
  
  tablefile=paste("Outputs/", outfile, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R", sep="")
  write(comment,file=tablefile)
  write.fwf(as.data.frame(parms),file=tablefile,append=TRUE,quote=TRUE)
  write.fwf(as.data.frame(modelout),file=tablefile,append=TRUE,quote=TRUE)
  return(tablefile)
}

load.times = function(file){
  eval(parse(text=scan(file,nlines=1,skip=3,what="character")[2]))
}

load.parmtable = function(file){
  z = as.matrix(read.table(file, header=TRUE,sep=" ", skip=5, strip.white=TRUE, fill=TRUE))
  parmtable = z[1:(match("State",z)-1),]
  parms = as.list(as.numeric(parmtable[,2]))
  names(parms) = parmtable[,1]
  return(parms)
}

load.statetable = function(file){
  z = as.matrix(read.table(file, header=TRUE,sep=" ", skip=5, strip.white=TRUE, fill=TRUE))
  statetable = z[(match("State",z)+1):(match("Time",z)-1),]
  colnames(statetable) = z[(match("State",z)),]
  states0=as.numeric(statetable[,2])
  names(states0)=statetable[,1]
  return(states0)
}

load.forcings = function(file){
  z = as.matrix(read.table(file, header=TRUE,sep=" ", skip=5, strip.white=TRUE, fill=TRUE))
  forcingtable = z[(match("Time",z)+1):dim(z)[1],1:(match("",z[match("Time",z),])-1)]
  colnames(forcingtable) = z[(match("Time",z)),1:(match("",z[match("Time",z),])-1)]
  forcings=as.numeric(forcingtable)
  dim(forcings)=dim(forcingtable)
  colnames(forcings)=colnames(forcingtable)
  return(forcings)
}

load.comment = function(file) {
    comment = readLines(file,n=2)
    return(comment)
}


load.changes = function(file) {
  changes = as.matrix(read.table(file,header=TRUE,sep=" ",skip=0,strip.white=TRUE))
  return(changes)
}


load.output = function(file) {
  modelout=as.matrix(read.table(file,skip=4,header=TRUE))
  return(modelout)
}

load.outparms = function(file) {
  parms=as.list(read.table(file,skip=2,header=TRUE,nrows=1))
  return(parms)
}  

load.multi = function(file) {
  multifile = as.matrix(read.table(file,skip=2,header=TRUE))
  multifile[,1] = trim(multifile[,1])
  multidata = as.list(rep(0,dim(multifile)[1]))
  names(multidata) = multifile[,1]
  for(i in 1:dim(multifile)[1]) {
    multidata[[i]] = load.output((multifile[i,1]))
    }
  multidata = abind(multidata,along=3)
  return(multidata)
}

load.multiparms = function(file) {
  multifile = as.matrix(read.table(file,skip=2,header=TRUE))
  multifile[,1] = trim(multifile[,1])
  multiparms = vector("list",length=dim(multifile)[1])
  for(i in 1:dim(multifile)[1]) {
    multiparms[[i]] = do.call(cbind,load.outparms(multifile[i,1]))
    }
  multiparms = do.call(rbind,multiparms)
  return(multiparms)
}