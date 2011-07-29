#This is a run file file the tree physiology model 

require(deSolve)                                                                #load relevant packages and function files
require(rootSolve)
require(gdata)
source("R/treephys_ODEs.r")
source("R/weibullcurves.R")
source("R/modelplots.R")
source("R/parameterfile.R")                                                         #load a parameter file file

modelout = 0 

plot_weibull(parms$k.K,parms$l.K)

modelout = lsoda(states0, times, tree_odes, parms)
par(mfrow=c(2,2))
plot.Psis(modelout,parms)
plot.carbon(modelout,parms)
plot.stress(modelout,parms)
plot(modelout[,"time"],modelout[,"G"],type="l",ylab="Stomatal Conductance",xlab="Time",mgp=c(2,1,0))

%plot(modelout[,"time"],modelout[,"Psi_l"],type="l",ylab="Leaf Water Potential",xlab="Time")
save(times, states0, parms, modelout,     #save parameters and output data in ASCII file named "Modelrun_YYYYMMDD_HHMM.R"
     file=paste("Outputs/BiggerG_" ,format(Sys.time(), "%Y%m%d_%H%M%S"), ".R", sep=""), 
     ascii=TRUE)
tablefile=paste("Outputs/Tabledata_" ,format(Sys.time(), "%Y%m%d_%H%M%S"), ".R", sep="")
write.fwf(as.data.frame(parms),file=tablefile,append=TRUE)
write.fwf(as.data.frame(modelout),file=tablefile,append=TRUE)
loaded=as.matrix(read.table(tablefile,skip=2,header=TRUE))
loadedparms=as.list(read.table(tablefile,header=TRUE,nrows=1))


par(mfrow=c(2,3))
plot.stress(modelout1,parms)
text(700,8,expression(G[min] == 0.06),cex=1.3)
plot.stress(modelout15,parms)
text(700,8,expression(G[min] == 0.05),cex=1.3)
plot.stress(modelout2,parms)
text(700,8,expression(G[min] == 0.045),cex=1.3)
plot.stress(modelout3,parms)
text(700,1,expression(G[min] == 0.03),cex=1.3)
plot.stress(modelout4,parms)
text(700,1,expression(G[min] == 0.01),cex=1.3)
plot.stress(modelout5,parms)
text(700,1,expression(G[min] == 0.00),cex=1.3)
mtext(side=1,"Time",outer=TRUE, padj=-2)
mtext(side=2,"Carbohydrates (S) (g)",outer=TRUE, padj=2)
mtext(side=4,"Conductance (K) (mmol s-1 MPa-1)",col="red",outer=TRUE,padj=-2)

plot.stress = function(modelout, parms) {
 par(mar=c(4,4,3,4))
 plot(modelout[,"time"], modelout[,"S"],type="l",lwd=2, xlab="",ylab="",mgp=c(2,1,0),ylim=c(1,50))
# legend("topright",c("Carbs","Conductance"),lty=c(1,2),cex=0.75,col=c("black","red"))
 par(new=TRUE)
 plot(modelout[,"time"], modelout[,"K"],type="l",col="red",lty=1,lwd=2,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,max(modelout[,"K"])))
 axis(4,col.ticks="red",col.axis="red",col="red")
 mtext("",side=4,line=3,col="red")

}

Carbohydrates (S) (g)
Time
Conductance (K) (mmol s-1 MPa-1)
