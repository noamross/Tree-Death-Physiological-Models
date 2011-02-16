#Functions and Code for Tree Death Physiology Model
#Noam Ross Started February 15, 2011

#Set run variables
times = #times at which to solve the model
states0 = c( #a vector of initial state variables
  W = , #soil water potential, MPa
  D = , #leaf surface moisture saturation deficit, MPa
  S = , #total non-structural carbohydrates, g
  K = , #xylem conductance, mmol s-1 MPa-1 
)
  soil water potential )

parms = list(
  alpha = , #photsynthetic rate per stomatal conductane, in g s MPa mmol-1 d-1
  beta = , #repair coefficient in g g-1 d-1
  gamma = , #growth coefficient in g g-1 d-1
  theta = , #repair efficacy, in mmol s-1 MPa-1 d-1 g-1
  kmax = , #maximum hydraulic contuctance, in mmol s-1 MPa-1
  m = , #minimum maintenance respiration carb allocation, in g d-1
  l.B = , #scale parameter for growth weibull function
  k.B = , #shape parameter for growth weibull function
  l.K = , #scale parameter for conductance weibull function
  k.K = , #shape paramter for conductance weibull function
  #Gmin = 0 , #minimum stomatal conductance, mmol s-1 MPa-1
  gmax = 1,  #maximum stomatal conductance, mmol s-1 MPa-1
  #l.G = 1 , #scale parameter for stomatal closing weibull function
  #k.G = 5 , #shape parameter for stomatal closing weibull function
  gs = ,#stomatal closure factor
)
  

tree_odes = function(time, states, parms) {
#A function of the ODEs for the tree model, designed to be solved by lsoda
#Vars:
# time:   current time state of the model
# states: a vector of state variables
# parms:  a list of parameters for the model
  
  with(as.list(parms), { #extract parameters from 'parms' vector
    X = (W - D*gmax/K - wpg)/(1 + D/(K*gs)) # Calculate xylem pressure
    G = gmax - (-X/gs) # Calculate stomatal flow.  These are at
    
    dW = 0  #no change in soil water potential or 
    dD = 0  #atmospheric water deficit for now
    
    P = alpha * G                       #calculate photosynthesis and carbon allocation to:
    R = beta * S * (1 - K/kmax)         #respiration from xylem repair
    B = gamma * s * exp(-(-X/l.B)^k.B)  #biomass growth
    M = m                               #maintenance respiration
    
    dS = P - R - M - B  #daily change in TNC, g d-1
    dK = -(k.K/l.K)*((-X/l.K)^(k.K-1))*exp(-(-X/l.K)^k.K) + theta*R #daily change in xylem conductance, mmol s-1 MPa-1 d-1 
    
    return(list(c(dW,dD,dS,dK),c(X=X, G=G, P=P, R=R, B=B, M=M))) 
  })
}

lsoda(states0, times, tree_odes, parms)