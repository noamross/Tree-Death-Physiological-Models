#Functions and Code for Tree Death Physiology Model
#Noam Ross Started February 15, 2011

#Set run variables
times = 1:500 #times at which to solve the model;
states0 = c( #a vector of initial state variables
  W = -1, #soil water potential, MPa
  D = -2, #leaf surface moisture saturation deficit, MPa
  S = 3, #total non-structural carbohydrates, g
  K = 1 #xylem conductance, mmol s-1 MPa-1 
)


parms = list(
  alpha = 1, #photsynthetic rate per stomatal conductance, in g s MPa mmol-1 d-1
  beta = 0.1, #repair coefficient in g g-1 d-1
  gamma = 0, #growth coefficient in g g-1 d-1
  theta = 0.005, #repair efficacy, in mmol s-1 MPa-1 d-1 g-1
  kmax = 10, #maximum hydraulic contuctance, in mmol s-1 MPa-1
  m = 0.0, #minimum maintenance respiration carb allocation, in g d-1;
  l.B = 1, #scale parameter for growth weibull function
  k.B = 1, #shape parameter for growth weibull function
  l.K = 8, #scale parameter for conductance weibull function, from Pinyon branches in Linton eet al. 1998
  k.K = 2, #shape paramter for conductance weibull function, from Pinyon branches in Linton et al. 1998 
  #Gmin = 0 , #minimum stomatal conductance, mmol s-1 MPa-1
  gmax = 1,  #maximum stomatal conductance, mmol s-1 MPa-1
  #l.G = 1 , #scale parameter for stomatal closing weibull function
  #k.G = 5 , #shape parameter for stomatal closing weibull function
  gs = 0.5, #stomatal closure factor
  wpg = 0.5 #water potential of gravity, MPa
)
  

tree_odes = function(time, states, parms) {
#A function of the ODEs for the tree model, designed to be solved by lsoda
#Vars:
# time:   current time state of the model
# states: a vector of state variables
# parms:  a list of parameters for the model
  
  with(c(as.list(states), parms), { #extract parameters from 'parms' vector
    X = (W - D*gmax/K - wpg)/(1 + D/(K*gs)) # Calculate xylem pressure
    #G = gmax - (-X/gs) # Calculate stomatal flow.  
    G = gmax - (-X/gs) # Calculate stomatal flow;
    
    
    dW = 0  #no change in soil water potential or 
    dD = 0  #atmospheric water deficit for now
    
    P = alpha * G                       #calculate photosynthesis and carbon allocation to:
    R = beta * S * (1 - K/kmax)         #respiration from xylem repair
    B = gamma * S * exp(-(-X/l.B)^k.B)  #biomass growth - (Slow growth is associated with high mortality Pedersen 1998te)
    M = m                             #maintenance respiration
    
    dS = P - R - M - B  #daily change in TNC, g d-1
   # dK = -K*(k.K/l.K)*((-X/l.K)^(k.K-1))*exp(-((-X/l.K)^k.K)) + theta*R #daily change in xylem conductance, mmol s-1 MPa-1 d-1 
    dK = -K*(k.K/l.K)*((-X/l.K)^(k.K-1))*exp(-((-X/l.K)^k.K)) + theta*R #daily change in xylem conductance, mmol s-1 MPa-1 d-1 
    
    return(list(c(dW,dD,dS,dK),c(X=X, G=G, P=P, R=R, B=B, M=M)))   #return a list of of both derivatives and other state variables
  })
}

require(deSolve)
modelout = lsoda(states0, times, tree_odes, parms)
save(times, states0, parms, modelout,     #save parameters and output data in ASCII file named "Modelrun_YYYYMMDD_HHMM.R"
     file=paste("Outputs/Modelrun_" ,format(Sys.time(), "%Y%m%d_%H%M%S"), ".R", sep=""), 
     ascii=TRUE)

pdf(paste(file="Outputs/Modelrun_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep=""))#create a pdf file to store outputs
par(mfrow=c(2,4)) #make a 2X4 grid of graphs
plot(times, modelout[,"W"], type="l",ylab="Soil Water Potential")
plot(times, modelout[,"D"], type="l",ylab="Leaf Surface Water Deficit")
plot(times, modelout[,"X"], type="l",ylab="Xylem Pressure")
plot(times, modelout[,"G"], type="l",ylab="Stomatal Conductance")
plot(times, modelout[,"K"], type="l",ylab="Xylem Conductance")
plot(times, modelout[,"P"], type="l", col="green", ylab="Carbon Flows")
lines(times, modelout[,"R"], type="l", col="red")
lines(times, modelout[,"B"], type="l", col="black")
lines(times, modelout[,"M"], type="l", col="blue")
legend("topright",legend=c("P","R","B","M"), col=c("green", "red", "black", "blue"),lty=c(1,1,1,1))
plot(times, modelout[,"S"], type="l", ylab="Non-Photosynthetic Carbohydrates")

#par(mfrow=c(1,1))  #set up a single graph
plot(modelout[,"X"], modelout[,"K"], type="l", xlab="Xylem Pressure", ylab="Conductance")  #plot xylem pressure against conductance

dev.off() #close the pdf

par(mfrow=c(2,4)) #make a 2X4 grid of graphs
plot(times, modelout[,"W"], type="l",ylab="Soil Water Potential")
plot(times, modelout[,"D"], type="l",ylab="Leaf Surface Water Deficit")
plot(times, modelout[,"X"], type="l",ylab="Xylem Pressure")
plot(times, modelout[,"G"], type="l",ylab="Stomatal Conductance")
plot(times, modelout[,"K"], type="l",ylab="Xylem Conductance")
plot(times, modelout[,"P"], type="l", col="green", ylab="Carbon Flows")
lines(times, modelout[,"R"], type="l", col="red")
lines(times, modelout[,"B"], type="l", col="black")
lines(times, modelout[,"M"], type="l", col="blue")
legend("topright",legend=c("P","R","B","M"), col=c("green", "red", "black", "blue"),lty=c(1,1,1,1))
plot(times, modelout[,"S"], type="l", ylab="Non-Photosynthetic Carbohydrates")

#par(mfrow=c(1,1))  #set up a single graph
plot(modelout[,"X"], modelout[,"K"], type="l", xlab="Xylem Pressure", ylab="Conductance")  #plot xylem pressure against conductance

