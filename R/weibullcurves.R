#functions related to calculating and plotting weibull failure curves for xylem embolism

Psi.50 = function(k,l) { 
#Find Psi_50, the point of 50% failure for a weibull curve
# Parameters
# k = weibull shape function
# l = weibull scale function
  
  Psi_50 = -l*(log(2))^(1/k)                                                    #calculate point of 50% failure      
  return(Psi_50)
  }
  
Psi.e = function(k,l) {
#Find Psi_e for a weibull curve, the start of failure, defined as the intercept of the tangent of Psi_50
# Parameters
# k = weibull shape function
# l = weibull scale function
  Psi_50 = Psi.50(k,l)
  Psi_e = (0.5/((k/l)*((-Psi_50/l)^(k-1))*exp(-(-Psi_50/l)^k)))+Psi_50
  return(Psi_e)
}

weibullplot = function(modelout,parms,savefile=FALSE) {
  if(savefile!=FALSE) pdf(paste(file="Outputs/", savefile, "_Psiplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))
  plot_weibull(parms$k.K,parms$l.K)
  if(savefile!=FALSE) dev.off()
}

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
  xlabel = expression(paste("Xylem pressure (",Psi,")"))                        #name X axis 
  plot(x,y,type="l",ylim=c(min(y,z),1.1*max(y,z)),xlim=rev(range(x)),           #plot weibull curve
    xlab=xlabel,ylab="Fraction of max. conductance (K)")
  lines(x,z)                                                                    #plot derivative
  points(Psi_50,w(Psi_50))                                                      #mark point of 50% embolism
  points(Psi_e, w(Psi_e))                                                       #mark point of beginning of failure
  lines(x,(x-Psi_50)*dw(Psi_50)+0.5,lty=2)                                          #plot tangent to x_50             
  abline(h=c(0,1),col="red",lty=2)                                              #make horizontal lines at 1 and 0
# abline(v=x_50,col="blue",lty=2)                                               #make vertical line at x_50
  text(Psi_50, 0.5, substitute(Psi[50] == a, list(a=round(Psi_50,2))),            #label Psi_50
    adj=c(-0.1,-0.1))       
  text(Psi_e, w(Psi_e), substitute(Psi[e] == a, list(a=round(Psi_e,2))),
    adj=c(1.1,1.1))                                                                #label Psi_e
  text(0,0, expression(over(d*X,d*Psi)),adj=c(0.5,-0.1))                                     #label derivative curve
  text(0,1,"X",adj=c(0.5,-0.1))                                                              #label weibull curve
}

find_weibull = function(Psi_50, Psi_e) {
# Find the weibull parameters k and lambda for xylem failure based on measures of failure
# Parameters
# Psi_50 = the pressure at which 50% xylem failure (in conductance) occures
# Psi_e = the pressure at which xylem failure begins, calculated as where the tangent to Psi_50 hits 1
  
  out=optim(c(1,1),weibull_errs,x_a=Psi_50,x_b=Psi_e,a=0.5)                     #find optimal k and l values by fitting weibull to 2 points
  return(c(k=out$par[1],l=out$par[2]))                                                         #return k and l as a vector
  
}

weibull_errs = function(kl,x_a,x_b,a) {
# Return the sum of squared errors for fit of weibull curve against guess of Psi_e and Psi_50
# Parameters
# kl = a vector of the guessed shape (k) and scale (l) parameters of the weibull curve
# x_a = the input value of Psi_50
# x_b = the input value of Psi-E

  k = kl[1]                                                                     #pull k and l out of vector
  l = kl[2]
  w_a = exp(-(-x_a/l)^k)                                                        #calculate Psi_50 for guessed curve
  dw_a = (k/l)*((-x_a/l)^(k-1))*exp(-(-x_a/l)^k)                                #calculate Psi_e for guessed curve
  err = (w_a - a)^2 + (dw_a - (a/(x_b-x_a)))^2                                  #calculate the sum of the square errors of both guesses
  return(err)                                                                   #return SSQ
}


