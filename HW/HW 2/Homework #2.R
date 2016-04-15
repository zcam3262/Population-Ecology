                                ##Homework 2

      ##Problem 2.a------------------------------------------------  
      
  ##Establishing the Function
  log.growth <- function(t, y, p) {
    N <- y[1]
    with(as.list(p), {
      dN.dt <- r * N*(1-(N/K)^theta)
      return(list(dN.dt))})}
  
  ##Assign Variables and Parameters
      p.1<-c('r'=0.25, 'K'=100, 'theta'=1)
      
y0<-c('N'=runif(1, min=0.01, max=0.1))
t<-1:100
  
  ##Load DeSolve
  library(deSolve)
  
  ##Simulate
sim<-ode(y=y0, times=t, func=log.growth, parms=p.1, method='lsoda')
sim<-as.data.frame(sim)
  
  ##Plot simulation
plot(N~t, data=sim, type='l',col='green')
  
      ##Problem 2.b------------------------------------------------  

  ##Assigning New Parameters (1)
      p.2<-c('r'=0.25, 'K'=50, 'theta'=1)
  
  ##Simulation
sim.2<-ode(y=y0, times=t, func=log.growth, parms=p.2, method='lsoda')
sim.2<-as.data.frame(sim.2)
  ##Plot
plot(N~t, data=sim.2, type='l', col='red')
    
  ##Assigning New Parameters (2)
      p.3<-c('r'=0.25, 'K'=25, 'theta'=1)
  
  ##Simulation
sim.3<-ode(y=y0, times=t, func=log.growth, parms=p.3, method='lsoda')
sim.3<-as.data.frame(sim.3)
  
  ##Plot
plot(N~t, data=sim.3, type='l', col='purple')
  
  ##Plotting all on same axis
plot(N~t, data=sim, type='l', lwd='2', col='green')
points(N~t, data=sim.2, type='l',lwd='3', col='red')
points(N~t, data=sim.3, type='l',lwd='4', col='purple')

  
?diff

  ##Deriving
sim$deriv <- c(diff(sim$N), NA) 
sim.2$deriv <- c(diff(sim.2$N), NA)
sim.3$deriv <- c(diff(sim.3$N), NA)

  ##Plotting
plot(deriv ~ N, data = sim, type = 'l', col = 'green', bty='l')
lines(deriv ~ N, data = sim.2, type = 'l', col = 'red', bty = 'l')
lines(deriv ~ N, data = sim.3, type = 'l', col = 'purple', bty = 'l')

      ##Problem 2.c------------------------------------------------  

  ##Maximuum growth rate
max(sim$deriv, na.rm = TRUE)
max(sim.2$deriv, na.rm = TRUE)
max(sim.3$deriv, na.rm = TRUE)

  ##When Maximuum growth rate occurs
which(sim$deriv == max(sim$deriv, na.rm = TRUE))
which(sim.2$deriv == max(sim.2$deriv, na.rm = TRUE))
which(sim.3$deriv == max(sim.3$deriv, na.rm = TRUE))
  
  ##Population size at maximuum growth rate
sim$N[which(sim$deriv == max(sim$deriv, na.rm = TRUE))]
sim.2$N[which(sim.2$deriv == max(sim.2$deriv, na.rm = TRUE))]
sim.3$N[which(sim.3$deriv == max(sim.3$deriv, na.rm = TRUE))]

  ##Plotting Population size that yields maximum growth rate vs. Carrying capacity

##X value:
N<-c(sim$N[which(sim$deriv == max(sim$deriv, na.rm = TRUE))],
     sim.2$N[which(sim.2$deriv == max(sim.2$deriv, na.rm = TRUE))],
     sim.3$N[which(sim.3$deriv == max(sim.3$deriv, na.rm = TRUE))])
##Y value:
K<-c(100, 50, 25)
#Plot
plot(K~N, type='b', col='orange')

      ##Problem 3------------------------------------------------  

  ##Assigning different values of theta
p.a<-c('r'=0.25, 'K'=100, 'theta'=0.5)
p.b<-c('r'=0.25, 'K'=100, 'theta'=1)
p.c<-c('r'=0.25, 'K'=100, 'theta'=1.8)

  ##Simulation
sim.a<-ode(y=y0, times=t, func=log.growth, parms=p.a, method='lsoda')
sim.a<-as.data.frame(sim.a)
sim.b<-ode(y=y0, times=t, func=log.growth, parms=p.b, method='lsoda')
sim.b<-as.data.frame(sim.b)
sim.c<-ode(y=y0, times=t, func=log.growth, parms=p.c, method='lsoda')
sim.c<-as.data.frame(sim.c)

  ##Plot
plot(N~t, data=sim.a, type='l', lwd='1', col='magenta')
points(N~t, data=sim.b, type='l',lwd='2', col='blue')
points(N~t, data=sim.c, type='l',lwd='3', col='yellow')