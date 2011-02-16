#Starting workspace for my tree physiology model
#Noam Ross February 8, 2011

#Define State Variables
S = #Total available non-structural carbohydrates, grams
K = #Total hydraulic conductance through the xylem, in mmol s-1 MPa-1
W = 2 #soil water potential, MPa
X = W - (E / K) - wpg  #xylem pressure, mPa
Ds = 1 #leaf surface moisture saturation deficit, MPa

#Define System Conditions
wpg = 1 #gravitational water potential, MPa, also height*density*graviational force


#Define Instantaneous relationships and processes

E = G*D #evapotranspiration rate, in mmol s-1
P = alpha * G #photosynthetic rate, in g d-1
R = beta * S * (1 - (K / Kmax)) #allocation of carbs to xylem repair, g d-1
B = gamma * S * exp(-(-X/l.B)^k.B) # allocation of carbs to growth, g d-1
# TOO COMPLEX FOR NOW: G = Gmin + (Gmax - Gmin)*exp(-(-wp.delta/l.G)^k.G) #total stomatal conductance, mmol s-1 MPa-1
G = Gmax - (-wp.delta*gs)   #total stomatal conductance, mmol s-1 MPa-1

#Define Parameters
alpha = #photsynthetic rate per stomatal conductane, in g s MPa mmol-1 d-1
beta = #repair coefficient in g g-1 d-1
gamma = #growth coefficient in g g-1 d-1
theta = #repair efficacy, in mmol s-1 MPa-1 d-1 g-1
kmax = #maximum hydraulic contuctance, in mmol s-1 MPa-1
m = #minimum maintenance respiration carb allocation, in g d-1
l.B = #scale parameter for growth weibull function
k.B = #shape parameter for growth weibull function
l.K = #scale parameter for conductance weibull function
k.K = #shape paramter for conductance weibull function
#Gmin = 0 #minimum stomatal conductance, mmol s-1 MPa-1
gmax = 1 #maximum stomatal conductance, mmol s-1 MPa-1
#l.G = 1 #scale parameter for stomatal closing weibull function
#k.G = 5 #shape parameter for stomatal closing weibull function
gs = #stomatal closure factor

#notes on weibull function - has long tail, which may not be true for biological system.  
#partial death of trees
#describe the stomata feedback system - 
#who would know - Lou Gross.  
#yoh iwasa - masting physiological models
#also, hastings, lyles, rosenstock on masting
#sandy leipold at USFS


#define differential equations
dS = P - R - M - B  #daily change in TNC, g d-1
dK = -(k.K/l.K)*((-wp.delta/l.K)^(k.K-1))*exp(-(-wp.delta)) + theta*R #daily change in xylem conductance, mmol s-1 MPa-1 d-1 



