#Various functions for plotting outputs of the tree physiology model

Psiplot = function(modelout, parms, savefile=FALSE) {
#Create a plot of relevant pressures from model outputs
#Parameters:
#modelout = output from treemodel
#parms = parameter list from treemodel
#save = if true, saves the image to a pdf

  Psi_50 = Psi.50(parms$k.K,parms$l.K)                                          #Calculate Psi_50 and Psi_e for display on the plot
  Psi_e = Psi.e(parms$k.K,parms$l.K)

  maxpressure = max(modelout[,"X"],Psi_50,Psi_e)                                 #Set the max and min ranges for the plot
  minpressure = min(modelout[,"X"],Psi_50,Psi_e)

   if(savefile!=FALSE) pdf(paste(file="Outputs/", savefile, "_Psiplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))

  plot(modelout[,"time"],modelout[,"X"],xlab="Time",ylab="Xylem Pressure (MPa)",
    ylim=c(minpressure, maxpressure),type="l",mgp=c(2,1,0))
  
  abline(h=c(Psi_50,Psi_e),col="red",lty=2)
  text(0, Psi_50, expression(Psi[50]), adj=c(0,-0.1),cex=1.2)
  text(0, Psi_e, expression(Psi[e]), adj=c(0,-0.1),cex=1.2)
  
  if(savefile!=FALSE) dev.off()
}

gridplot = function(modelout, parms, savefile=FALSE) {
#Create a plot of relevant pressures from model outputs
#Parameters 
#modelout = output from treemodel
#parms = parameter list from treemodel
#save = if true, saves the image to a pdf
  
   if(savefile!=FALSE) pdf(paste(file="Outputs/", savefile, "_gridplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))
  
  par(mfrow=c(2,4)) #make a 2X4 grid of graphs
  plot(times, modelout[,"Psi_s"], type="l",ylab="Soil Water Potential")
  plot(times, modelout[,"D"], type="l",ylab="Leaf Surface Water Deficit")
  plot(times, modelout[,"Psi_l"], type="l",ylab="Leaf Water Potential")
  plot(times, modelout[,"X"], type="l",ylab="Xylem Pressure")
  lines(times, modelout[,"Psi_l"], type="l")
  plot(times, modelout[,"G"], type="l",ylab="Stomatal Conductance")
  plot(times, modelout[,"K"], type="l",ylab="Xylem Conductance")
  plot(times, modelout[,"P"], type="l", col="green", ylab="Carbon Flows",ylim=c(min(modelout[,"P"],modelout[,"R"],modelout[,"B"],modelout[,"M"]),max(modelout[,"P"],modelout[,"R"],modelout[,"B"],modelout[,"M"])))
  lines(times, modelout[,"R"], type="l", col="red")
  lines(times, modelout[,"B"], type="l", col="black")
  lines(times, modelout[,"M"], type="l", col="blue")
  legend("topright",legend=c("P","R","B","M"), col=c("green", "red", "black", "blue"),lty=c(1,1,1,1))
  plot(times, modelout[,"S"], type="l", ylab="Non-Photosynthetic Carbohydrates")
  
  if(savefile!=FALSE) dev.off()
  
}

stressplot = function(modelout, parms, savefile=FALSE) {
  
  #Create a plot of the two main stress variables: carbohydrates and conductance
  #Parameters 
  #modelout = output from treemodel
  #parms = parameter list from treemodel
  #save = if true, saves the image to a pdf

   if(savefile!=FALSE) pdf(paste(file="Outputs/", savefile, "_stressplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))
  
  par(mar=c(5.1, 4.1, 4.1, 4.1))
  plot(modelout[,"time"], modelout[,"S"],type="l", xlab="Time",ylab="Carbohydrates (S) (g)",mgp=c(2,1,0))
  par(new=TRUE)
  plot(modelout[,"time"], modelout[,"K"],type="l",col="red",lty=1,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,max(modelout[,"K"])))
  axis(4,col.ticks="red",col.axis="red",col="red")
  mtext("Conductance (K) (mmol s-1 MPa-1)",side=4,line=3,col="red")
  
  if(savefile!=FALSE) dev.off()
  
}

carbonplot = function(modelout,parms,savefile=FALSE) {
#Create a plot of the carbon fluxes in the tree
#Parameters 
#modelout = output from treemodel
#parms = parameter list from treemodel
#save = if true, saves the image to a pdf

  maxcarb = max(modelout[,"P"])
  mincarb = -max(modelout[,"R"],modelout[,"B"],modelout[,"M"])

   if(savefile!=FALSE) pdf(paste(file="Outputs/", savefile, "_carbonplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))

  plot(modelout[,"time"],modelout[,"P"],ylim=c(mincarb,maxcarb),xlab="Time",
    ylab="Carbon flux (grams/day)",col="green",type="l",mgp=c(2,1,0))
  text(0,modelout[1,"P"],"P",adj=c(0.5,0.5))
  lines(modelout[,"time"],-modelout[,"R"],col="red")
  text(0,-modelout[1,"R"],"R",adj=c(0.5,-0.1))
#  lines(modelout[,"time"],-modelout[,"M"],col="orange")
#  text(0,-modelout[1,"M"],"M",adj=c(0.5,-0.1))
#  lines(modelout[,"time"],-modelout[,"B"],col="blue")
#  text(0,-modelout[1,"B"],"B",adj=c(0.5,-0.1))
  abline(h=0)
  
  if(savefile!=FALSE) dev.off()
}

stomataplot = function(modelout,parms,savefile=FALSE) {
  
  if(savefile!=FALSE) pdf(paste(file="Outputs/", savefile, "_stomataplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))
    
  plot(modelout[,"time"],modelout[,"G"],type="l",ylab="Stomatal Conductance",xlab="Time",ylim=c(0,max(modelout[,"G"])), mgp=c(2,1,0))

  if(savefile!=FALSE) dev.off()
}

fourplot = function(modelout,parms,savefile=FALSE) {

    if(savefile!=FALSE) pdf(paste(file="Outputs/", savefile, "_fourplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))
    
    par(mfrow=c(2,2))
    Psiplot(modelout,parms)
    carbonplot(modelout,parms)
    stressplot(modelout,parms)
    stomataplot(modelout,parms)
    
    if(savefile!=FALSE) dev.off()
}



#-----Multirun plots
deathcurve = function(multidata, multiparms, parameter, save=FALSE) { 
# This function plots the time to death and mode of death as a function of changing parameter valuer
# deathcurve = output from multirun script of many simulations with changing parameter values

  if(save) pdf(paste(file="Outputs/Deathcurve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))  

  deathtimes = deathtime(multidata)
  deathtypes = deathtype(multidata,multiparms)
  if(is.na(match(parameter,colnames(multiparms)))){
    parm = multidata[1,parameter,]
  }
  else{
    parm = multiparms[,parameter]
  }
  colors = rep(0,length(deathtimes))
  colors[which(deathtypes=="none")] = "green"
  colors[which(deathtypes=="embolism")] = "blue"
  colors[which(deathtypes=="starvation")] = "red"
  par(mgp=c(1.9,.5,0),tck = -.01, las = 1)
  plot(parm, deathtimes, type="p", col=colors, pch=19, xlab=parameter, ylab="Time to Death (days)",main="Time to death and mode of death under drought")
  lines(parm,deathtimes,type="c",col="black")
  legend(x="topright", legend=c("Carbon Starvation","Xylem embolism","None"),pch=19,col=c("red","blue","green"))
 
  if(save) dev.off()

}  

deathsurface = function(multidata,multiparms,par1,par2,save=FALSE) {
  
  if(save) pdf(paste(file="Outputs/Deathsurf_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))  
  
  parmstates = cbind(multiparms,t(multidata[1,,]))
  x = parmstates[,par1]
  y = parmstates[,par2]
  deathtimes = deathtime(multidata)
  colors = rep(0,length(deathtimes))
  deathtypes = deathtype(multidata,multiparms)
  colors[which(deathtypes=="none")] = "green"
  colors[which(deathtypes=="embolism")] = "blue"
  colors[which(deathtypes=="starvation")] = "red"
#  scatterplot3d(get(par1),get(par2),deathtimes,color=colors,type="h",pch=19)
  plot3d(x,y,deathtimes,col=colors,type="h",xlab=par1,ylab=par2)
  
  if(save)dev.off()
  
}


dpath = function(modelout,parms,save=FALSE) {
  #This function plots the plants' path in carbohydrate/conductance space  
  if(!is.list(parms)){parms = as.list(parms)}
  if(save) pdf(paste(file="Outputs/dpath_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))
  
  plot(modelout[,"S"], modelout[,"K"], type="l",xlab="Carbohydrate Stores (g)",ylab="Conductance")
  points(modelout[1,"S"], modelout[1,"K"],pch=19)
  if((tail(modelout[,"S"],1)<=parms$S.death)||(tail(modelout[,"K"],1)<=parms$K.death)) points(tail(modelout[,"S"],1), tail(modelout[,"K"],1),pch=19)
#  abline(h=parms$K.death,v=parms$S.death)
  
}

dpaths = function(multidata, multiparms, save=FALSE) {
  if(save) pdf(paste(file="Outputs/dpaths_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))
#  X = max(multidata[,"S",], na.rm=TRUE)
#  Y = max(multidata[,"K",], na.rm=TRUE)
  cols = rainbow(dim(multidata)[3])
  matplot(multidata[,"S",],multidata[,"K",],type="l",col=cols,lty=1,lwd=1.5, xlab="Carbohydrate Stores (g)",ylab="Conductance")
  endpts = matrix(NA,dim(multidata)[3],2)
  for(i in 1:dim(multidata)[3]) {
    endpts[i,1] = multidata[match(NA,multidata[,"S",i])-1,"S",i]
    endpts[i,2] = multidata[match(NA,multidata[,"K",i])-1,"K",i]
  }
    points(endpts,pch=19,col=cols)
    abline(h=multiparms[,"K.death"],v=multiparms[,"S.death"],col="blue",lwd=0.5)
    #text()
#    legend(x="right",title=paste("Value of ",colnames(filelist)[3],sep=""), legend=filelist[,3],col=cols,lty=1)
  }
  
  