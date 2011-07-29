#Two lines of
#comment then a space

times "1:1000"

Parameter Value Units Description LaTeX
alpha 5 "g s MPa mmol-1 d-1" "photsynthetic rate per stomatal conductance" $\\alpha$
beta 0.5 "g g-1 d-1" "repair coefficient" $\\beta$
gamma 0 "g g-1 d-1" "growth coefficient" $\\gamma$
theta 0.075 "mmol s-1 MPa-1 d-1 g-1" "repair efficacy" $\\theta$
kmax 10 "mmol s-1 MPa-1" "maximum hydraulic contuctance" $k_{max}$
m 0 "g d-1" "minimum maintenance respiration carb allocation" $m$
l.B 1 "" "scale parameter for growth weibull function" $l_B$
k.B 1 "" "shape parameter for growth weibull function" $k_B$
l.K 3.26 "" "scale parameter for conductance weibull function" $l_K$
k.K 2.21 "" "shape paramter for conductance weibull function" $k_K$
G.min 0.05 "mmol s-1 MPa-1" "minimum stomatal conductance" $G_{min}$
h 2 "m" "tree height" $h$
rho 0.1 "kg cm-3" "density of water" $\\rho$
g 9.81 "m s-2" "accelleration of gravity" $g$
zeta 1 "" "stomatal sensitivity to ABA" $\\zeta$
omega 1 "" "range of stomatal opening" $\\omega$
tau 0.95 "" "stomatal sensitivity to leaf water potential" $\\tau$
a 0.5 "" "ABA production sensitivity to soil water potential" $a$
b 5 "" "ABA consumption rate" $b$
S.death 0.01 "g" "Minimum viable carbohydrate level" $S_death$
K.death 0.01 "mmol s^-1 MPa^-1" "Minimum viable xylem conductabce" $K_death$

State Value Units Description LaTeX
S 1 "g" "total non-structural carbohydrates" $S$
K 10 "mmol s^-1 MPa^-1" "xylem conductance" $K$

Time Psi_s D
0 -0.5 2
1000 -0.5 2
