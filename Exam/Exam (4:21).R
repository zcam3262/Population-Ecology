      ##Exam Question 3
##Function
log.growth <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1 - (N / K)^theta)
    return(list(dN.dt))})}
##Parameters
p.t <- c('r' = 0.2, 'K' = 1.05, 'theta' = 1.05)
y0 <- c('N' = 0.01)
        t <- 1:100

p.g <- c('r' = 0.28, 'K' = 0.75, 'theta' = 1.25)

p.p <- c('r' = 0.15, 'K'=1, 'theta'=1)

library(deSolve)

##Simulations
sim.t <- ode(y = y0, times = t, func = log.growth, parms = p.t, method = 'lsoda')
sim.g <- ode(y = y0, times = t, func = log.growth, parms = p.g, method = 'lsoda')
sim.p <- ode(y = y0, times = t, func = log.growth, parms = p.p, method = 'lsoda')

sim.t <- as.data.frame(sim.t)
sim.g <- as.data.frame(sim.g)
sim.p <- as.data.frame(sim.p)


##Plotting
plot(N ~ time, data = sim.t, type = 'l', lwd = 2, bty = 'l', col = 'red')
  points(N ~ time, data = sim.g, type = 'l', lwd = 1, bty = 'l', col = 'purple')
  points(N ~ time, data = sim.p, type = 'l', lwd = 3, bty = 'l', col = 'yellow')
  
##Deriving
  
  sim.t$deriv <- c(diff(sim.t$N), NA)
  plot(deriv ~ N, data = sim.t, type = 'l', col = 'red', bty = 'l')
  
  sim.g$deriv <- c(diff(sim.g$N), NA)
  points(deriv ~ N, data = sim.g, type = 'l', col = 'purple')
  
  sim.p$deriv <- c(diff(sim.p$N), NA)
  points(deriv ~ N, data = sim.p, type = 'l', col = 'yellow')
  
##Finding max growth
  max.growths <- c(max(sim.t$deriv, na.rm = TRUE),
                   max(sim.g$deriv, na.rm = TRUE),
                   max(sim.p$deriv, na.rm = TRUE))
  
  Ks <- c(p.t['K'], p.g['K'], p.p['K'])
  
  plot(max.growths ~ Ks, pch = 21, bg = 'green', type = 'b', lty = 1, bty = 'l')
  
  ## Thus since grapes have the highest max growth rate, AND are valued at the most 
  #             (2 cents/grape)the would be the best crop for the farmer to grow