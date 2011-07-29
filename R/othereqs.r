#assorted equations used in the tree physiology project
cp
vpd = function(temp, RH) {
#This calculates the water vapor deficit, in MPa, from temperature and relative humidity
#temp = temperature in celsius
#RH = relative humidity, as a fraction

  t = temp + 273.15  #convert to kelvin
  A = -1.88e4; B = -13.1; C = -1.5e-2; D = 8e-7; E = -1.69e-11; F = 6.456 #set constants
  vp_sat = exp((A/t) + B + (C*t) + (D*(t^2)) + E*(t^3) +F*log(t)) #calculate saturation pressure (kPA)
  vp_air = vp_sat*RH #calculate the water vapor pressure (kpA)
  vpd = (vp_sat - vp_air) #calculate the vapor pressure deficit (MPa)
  return(vpd)
  
}

vpsat = function(t) {
  vp_sat = exp((A/t) + B + (C*t) + (D*(t^2)) + E*(t^3) +F*log(t)) 
}

vpd2 = function(temp, RH) {
#This calculates the water vapor deficit, in MPa, from temperature and relative humidity
#temp = temperature in celsius
#RH = relative humidity, as a fraction

  t = temp*9/5 + 32 + 459.67  #convert deg C to deg R
  A = -1.044e4; B = -1.129; C = -2.702e-2; D = 1.289e-5; E = -2.478e-9; F = 6.456
  vp_sat = exp((A/t) + B + (C*t) + (D*(t^2)) + (E*(t^3)) + (F*log(t))) #calculate saturation pressure psi
  vp_air = vp_sat*RH #calculate the water vapor pressure psi
  vpd = (vp_sat - vp_air) #calculate the vapor pressure deficit (MPa)
  return(vpd)
  
}