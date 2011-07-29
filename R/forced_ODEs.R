#Functions and Code for Tree Death Physiology Model
#Noam Ross Started February 15, 2011

leaf = function(Psi_l, parms){
#A function used to find the root of combined equations for leaf water potential, stomatal conductance, and ABA concentration
#Called by 'uniroot' in the tree ODE function, returns a value for Psi_l, leaf water potential
#Parameters:
#Psi_l: Guess of leaf water potential
#parms: The model parameter list
  with(parms,{
    zeroval = G.min - ((K/D)*(Psi_s - Psi_l - h*rho*g)) + omega*exp(((zeta*a*Psi_s)/(K*(Psi_s - Psi_l - h*rho*g) + b))*exp(-tau*Psi_l))
    return(zeroval)
  })
}

tree_odes = function(time, states, parms) {
#A function of the ODEs for the tree model, designed to be solved by lsodar
#Vars:
# time:   current time state of the model
# states: a vector of state variables
# parms:  a list of parameters for the model
  
  with(c(as.list(states), parms), { #extract parameters from 'parms' vector
    Psi_s = func.Psi_s(time)
    D = func.D(time)
    Psi_l = uniroot(leaf,c(-30,(b/K + Psi_s - h*rho*g)),parms = c(as.list(states),parms,Psi_s=Psi_s,D=D))$root #Calculate leaf water potential
    G = (K/D)*(Psi_s - Psi_l - h*rho*g)
    
    ABA = (-a*Psi_s)/(G*D + b)
    X = -(Psi_s - Psi_l - h*rho*g) # Calculate xylem pressure
    #G = gmax - (-X/gs) # Calculate stomatal flow.  
    #G = gmax - (-X/gs) # Calculate stomatal flow;
    
    P = alpha * G                       #calculate photosynthesis and carbon allocation to:
    R = beta * S * (1 - K/kmax)         #respiration from xylem repair
    B = gamma * S * exp(-(-X/l.B)^k.B)  #biomass growth - (Slow growth is associated with high mortality Pedersen 1998te)
    M = m                             #maintenance respiration
    
    dS = P - R - M - B  #daily change in TNC, g d-1
   # dK = -K*(k.K/l.K)*((-X/l.K)^(k.K-1))*exp(-((-X/l.K)^k.K)) + theta*R #daily change in xylem conductance, mmol s-1 MPa-1 d-1 
    dK = -kmax*(k.K/l.K)*((-X/l.K)^(k.K-1))*exp(-((-X/l.K)^k.K)) + theta*R #daily change in xylem conductance, mmol s-1 MPa-1 d-1 
    
    return(list(c(dS,dK),c(Psi_s=Psi_s, D=D, Psi_l=Psi_l,G=G,ABA=ABA,X=X, P=P, R=R, B=B, M=M)))   #return a list of of both derivatives and other state variables
  })
}

death_func = function(time, states, parms) {
#This is the root function for lsodar, which is used to stop the simulation
#after minimum levels of carbohydrates or conductance are reached
  with(c(as.list(states), parms), { #extract parameters from 'parms' and 'states' vectors
   return((S-S.death)*(K-K.death)) #this function should equal zero when either state reaches its critical minimum 
  })
}
