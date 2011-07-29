plot_weibull = function(k,l) {
#Plot the weibull failure function for stem embolism given the weibull parameters
# Parameters
# k = weibull shape function
# l = weibull scale function
  w = function(X){exp(-(-X/l)^k)}                                               #define weibull function
  dw = function(X){(k/l)*((-X/l)^(k-1))*exp(-(-X/l)^k)}                         #define weibull derivative
                                                                                                                               
  Psi_50 = Psi.50(k,l)                                                          #calculate point of 50% failure                                    
  Psi_e = Psi.e(k,l)                                                            #calculate point of beginning of failure (where x_50 tangent hits 1)
                                                                                                                               
  x = seq(2*Psi_50,0,length.out=100)                                            #create a range from zero to twice x_50                            
  y = w(x)                                                                      #calculate weibull curve                                           
  z = dw(x)                                                                     #calculate weibull derivative curve                                
  
  #set up plot:
  xlabel = expression(paste("Xylem pressure (",-Delta*Psi,")"))                        #name X axis 
  plot(x,y,type="l",ylim=c(min(y,z),1.1*max(y,z)),xlim=rev(range(x)),           #plot weibull curve
    xlab=xlabel,ylab="Fraction of max. conductance (K)",tck=0,labels=FALSE,
    cex.lab=1.75,mgp=c(0.75,1,0),lwd=3)
  lines(x,z,lwd=3)                                                                    #plot derivative
  points(Psi_50,w(Psi_50),pch=16, cex=2)                                                      #mark point of 50% embolism
  points(Psi_e, w(Psi_e),pch=16, cex=2)                                                       #mark point of beginning of failure
  lines(x,(x-Psi_50)*dw(Psi_50)+0.5,lty=2, lwd=3)                                          #plot tangent to x_50             
  abline(h=c(0,1),col="red",lty=2)                                              #make horizontal lines at 1 and 0
# abline(v=x_50,col="blue",lty=2)                                               #make vertical line at x_50
  text(Psi_50, 0.5, expression(Psi[50]),adj=c(1.1,1.1), cex=2)       
  text(Psi_e, w(Psi_e), expression(Psi[e]), adj=c(1.1,1.1), cex=2)                                                                #label Psi_e
  text(0,0, expression(over(d*K,d*Psi)),adj=c(0,-0.2), cex=2)                                     #label derivative curve
  text(0,1,"K",adj=c(0.5,-0.1), cex=2)                                                              #label weibull curve
}