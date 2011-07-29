

parms = list(
  G_min = 0,
  omega = 1,
  zeta = 5,
  tau = 0.1,
  a = 1,
  b = 0.75,
  D = -2,
  K = 1,
  Psi_s = 1,
  h = 1,
  rho = 1,
  g = 0.5)



leaf = function(Psi_l, parms){
  with(parms,{ 
    Psil = G_min - ((K/D)*(Psi_s - Psi_l - h*rho*g)) + omega*exp((zeta*(a*Psi_s)/(K*(Psi_s - Psi_l - h*rho*g) + b))*exp(-tau*Psi_l))
    G = (K/D)*(Psi_s - Psil - h*rho*g)
    ABA = (a*Psi_s)/(G*D + b)
    return(Psil, G, ABA)
  })
}


leaf1 = function(Psi_l, parms){
  with(parms,{
    Psil = G_min - ((K/D)*(Psi_s - Psi_l - h*rho*g)) + omega*exp((zeta*(a*Psi_s)/(K*(Psi_s - Psi_l - h*rho*g) + b))*exp(-tau*Psi_l))
    return(Psil)
  })
}





x = seq(parms$D,parms$Psi_s,by=0.01)
out = leaf(x,parms)
par(mfrow=c(1,3))
plot(x,out$Psil, type="l")
root = uniroot(leaf1,c(parms$D,parms$Psi_s),parms)
abline(h=0,col="red",lty=2)
abline(v=root$root,col="blue",lty=2)
plot(x,out$G,type="l")
abline(v=root$root,col="blue",lty=2)
plot(x,out$ABA,type="l")
abline(v=root$root,col="blue",lty=2)

