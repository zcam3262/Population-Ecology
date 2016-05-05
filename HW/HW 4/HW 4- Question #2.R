## Homework 4- Question 2
library(deSolve)
    ##Function
comp <- function(t, y, p) {
  N1 <- y[1]
  N2 <- y[2]
  with(as.list(p), {
    dN1.dt <- (r1 * N1 / K1) * (1 - N1 - a12 * N2)
    dN2.dt <- (r2 * N2 / K2) * (1 - N2 - a21 * N1)
    return(list(c(dN1.dt, dN2.dt)))})}
    ##Paremeters (Short term, 20 days)
t.1<- 1:20
y0<-c('N1'=0.1, 'N2'=0.1)
p<-c('r1'=0.1, 'r2'=0.6,
     'K1'=2, 'K2'=1,
     'a12'=0.15, 'a21'=0.3)
    ##Simulate
sim<-ode(y=y0, times=t.1, func=comp, parms=p, method='lsoda')
sim<-as.data.frame(sim)
    ##Plot
plot(N1~ time, type='l', col='blue', bty='l', data=sim, ylim = c(0, 2))
points(N2~ time, type='l', lty=2, col='red', data=sim)

#Short term results: Species 2 would outcompete Species 1

    ##Paremeters (Long term, 100 days)
t.2<- 1:100
    ##Simulate
sim.2<-ode(y=y0, times=t.2, func=comp, parms=p, method='lsoda')
sim.2<-as.data.frame(sim.2)
    ##Plot
plot(N1~ time, type='l', col='blue', bty='l', data=sim.2, ylim = c(0, 2))
points(N2~ time, type='l', lty=2, col='red', data=sim.2)

# Long term results: Species 1 outcompetes species 2 in the long run.
    #since some species have a low carrying capacity and high growth rate (species 2)
    #they are more likely to succeed early on, but ultimately be out competed by a
    #species with a lower growth rate but higher carrying capacity (species 1).