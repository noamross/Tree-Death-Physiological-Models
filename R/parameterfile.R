#initial states and parameters for running the tree

times = 1:1000 #times at which to solve the model;
states0 = c( #a vector of initial state variables
  Psi_s = -4, #soil water potential, MPa
  D = 4 , #leaf surface moisture saturation deficit, MPa
  S = 1, #total non-structural carbohydrates, g
  K = 10 #xylem conductance, mmol s-1 MPa-1 
)


parms = list(
  alpha = 3, #photsynthetic rate per stomatal conductance, in g s MPa mmol-1 d-1
  beta = 0.5, #repair coefficient in g g-1 d-1
  gamma = 0, #growth coefficient in g g-1 d-1
  theta = 0.075, #repair efficacy, in mmol s-1 MPa-1 d-1 g-1
  kmax = 10, #maximum hydraulic contuctance, in mmol s-1 MPa-1
  m = 0.0, #minimum maintenance respiration carb allocation, in g d-1;
  l.B = 1, #scale parameter for growth weibull function
  k.B = 1, #shape parameter for growth weibull function
  l.K = 0.45, #scale parameter for conductance weibull function, from Pinyon branches in Linton eet al. 1998
  k.K = 3.85, #shape paramter for conductance weibull function, from Pinyon branches in Linton et al. 1998 
  G.min = 0.06 , #minimum stomatal conductance, mmol s-1 MPa-1
  h = 1, #tree height
  rho = 1, #density of water
  g = 1, #accelleration of gravity
  zeta = 1, #stomatal sensitivity to ABA
  omega = 1, #range of stomatal opening
  tau = 0.95, #stomatal sensitivity to leaf water potential
  a = 0.1, #ABA production sensitivity to soil water potential
  b = 5 #ABA consumption rate
  )
  
