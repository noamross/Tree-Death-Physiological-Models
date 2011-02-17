#Functions and Code for Tree Death Physiology Model
#Noam Ross Started February 15, 2011
#Test web edit

#Set run variables
times = 1:120 #times at which to solve the model
states0 = c( #a vector of initial state variables
  W = -1, #soil water potential, MPa
  D = -2, #leaf surface moisture saturation deficit, MPa
  S = 5, #total non-structural carbohydrates, g
  K = 1 #xylem conductance, mmol s-1 MPa-1 
)


parms = list(
  alpha = 1, #photsynthetic rate per stomatal conductane, in g s MPa mmol-1 d-1
  beta = 1, #repair coefficient in g g-1 d-1
  gamma = 1, #growth coefficient in g g-1 d-1
  theta = 1, #repair efficacy, in mmol s-1 MPa-1 d-1 g-1
  kmax = 10, #maximum hydraulic contuctance, in mmol s-1 MPa-1
  m = 0, #minimum maintenance respiration carb allocation, in g d-1
  l.B = 1, #scale parameter for growth weibull function
  k.B = 1, #shape parameter for growth weibull function
  l.K = 1, #scale parameter for conductance weibull function
  k.K = 1, #shape paramter for conductance weibull function
  #Gmin = 0 , #minimum stomatal conductance, mmol s-1 MPa-1
  gmax = 1,  #maximum stomatal conductance, mmol s-1 MPa-1
  #l.G = 1 , #scale parameter for stomatal closing weibull function
  #k.G = 5 , #shape parameter for stomatal closing weibull function
  gs = 1, #stomatal closure factor
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
    G = gmax - (-X/gs) # Calculate stomatal flow.  
    
    dW = 0  #no change in soil water potential or 
    dD = 0  #atmospheric water deficit for now
    
    P = alpha * G                       #calculate photosynthesis and carbon allocation to:
    R = beta * S * (1 - K/kmax)         #respiration from xylem repair
    B = gamma * S * exp(-(-X/l.B)^k.B)  #biomass growth
    M = m                               #maintenance respiration
    
    dS = P - R - M - B  #daily change in TNC, g d-1
    dK = -(k.K/l.K)*((-X/l.K)^(k.K-1))*exp(-(-X/l.K)^k.K) + theta*R #daily change in xylem conductance, mmol s-1 MPa-1 d-1 
    
    return(list(c(dW,dD,dS,dK),c(X=X, G=G, P=P, R=R, B=B, M=M))) 
  })
}

require(deSolve)
modelout = lsoda(states0, times, tree_odes, parms)
par(mfrow=c(3,3))
plot(modelout[,1], modelout[,4], type="l")