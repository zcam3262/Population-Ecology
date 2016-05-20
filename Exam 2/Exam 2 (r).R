##Exam 2
library(deSolve)
##Number 1
predprey.1 <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  with(as.list(p), {
    dH.dt <- (r *(1-(H/K))-(b * H * Z))
    dZ.dt <- (C * H * Z)-(m * Z)
    return(list(c(dH.dt, dZ.dt)))})}
p.1 <- c('r' = 1, 'b' = 1,
         'C' = 1, 'K'=1,
         'm'=0.1)
y0 <-c('H' =1 , 'Z' = 0.1)
##SIM
sim.1 <- ode(y = y0, times = t, func = predprey.1, parms = p.1, method = 'lsoda')
sim.1 <- as.data.frame(sim.1)

##Plot
plot(H ~ time, data = sim, type = 'l', col = 'green', bty = 'l', ylim=c(0,2))
points(Z ~ time, data = sim, type = 'l', col = 'purple', lty = 2)

## Number 2
## write a function for predator-prey
predprey <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  P <- y[3]
  with(as.list(p), {
    dH.dt <- (r *(1-(H/K))-(b * H * Z))
    dZ.dt <- (C * H * Z)-(m * Z) - (d*Z*P)
    dP.dt <- (e * Z * P)-(n * P)
    return(list(c(dH.dt, dZ.dt, dP.dt)))})}

##Establishing parameters/Fixed

p <- c('r' = 1, 'b' = 1,
       'C' = 1, 'K'=1,
       'm'=0.1, 'e'=1,
       'd'=1, 'n'=0.1)
y0 <- c('H' =1 , 'Z' = 0.1, 'P'=0.1)
t <- 1:100

##Sim
sim <- ode(y = y0, times = t, func = predprey, parms = p, method = 'lsoda')
sim <- as.data.frame(sim)


##Plot
plot(H ~ time, data = sim, type = 'l', col = 'green', bty = 'l', ylim=c(0,2))
points(Z ~ time, data = sim, type = 'l', col = 'purple', lty = 2)
points(P ~ time, data =sim, type='l', col= 'red', lty=3)