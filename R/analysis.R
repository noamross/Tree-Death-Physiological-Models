deathtime = function(multidata) {
  deathtime = rep(0,dim(multidata)[3])
  for(i in 1:dim(multidata)[3]) {
    row = match(NA,multidata[,2,i]) - 1
    if(is.na(row)){deathtime[i]=tail(multidata[,"time",i],1)}else{deathtime[i]=multidata[row,"time",i]}
  }
  return(deathtime)
}

deathtype = function(multidata, multiparms) {
  deathtypes = rep(0,dim(multidata)[3])
  for(i in 1:length(deathtypes)) {
    row = match(NA,multidata[,2,i]) - 1
    if(is.na(row)){deathtypes[i]="none"}
      else{deathtypes[i] = ifelse(multidata[row,"S",i] <= multiparms[i,"S.death"],"starvation", ifelse(multidata[row,"K",i] <= multiparms[i,"K.death"], "embolism", NA))}
  }
  return(deathtypes)
}

deathmatrix = function(multidata,multiparms,par1,par2) {
  parmstates = cbind(multiparms,t(multidata[1,,]))
  axis1 = sort(unique(parmstates[,par1]))
  axis2 = sort(unique(parmstates[,par2]))
  deathmatrix = matrix(NA,length(axis1),length(axis2),dimnames=list(assign(par1,axis1),assign(par2,axis2)))
  deathtimes = deathtime(multidata)
  for(i in 1:length(axis1)) {
    for(j in 1:length(axis2)) {
     deathmatrix[i,j] = deathtimes[intersect(which(parmstates[,par1]==axis1[i]),which(parmstates[,par2]==axis2[j]))]
    }
  }
  return(deathmatrix)
}
