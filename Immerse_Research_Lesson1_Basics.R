install.packages("deSolve")
install.packages("rodeo")
library(deSolve)
library(rodeo)

rm(list=ls())
data(vars, pars, pros, funs, stoi)

nBox <- 2
model <- rodeo$new(vars=vars, pars=pars, funs=funs, pros=pros, stoi=stoi, dim=c(nBox))
model$compile(fortran=FALSE)

monod <- function(c,h) {c/(c+h)}

rp <- function(x) {rep(x, nBox)}
v <- cbind(bac=rp(0.01), sub=rp(0))
model$setVars(v)
p <- cbind(mu=rp(0.5), half=rp(0.1), yield=rp(0.1), vol=c(3000,1000), 
           flow=rp(50), sub_in=rp(1))
model$setPars(p)
p

out <- model$dynamics(time=0:150, fortran=F)

layout(matrix(1:model$lenVars(), nrow=1))

for (vn in model$namesVars()) {
  matplot(out[,"time"], out[,paste(vn, 1:nBox, sep=".")], 
          type="l", xlabel="time", ylab=vn, lty=1:nBox, col=1:nBox)
  legend("right", bty="n", lty=1:nBox, col=1:nBox, legend=paste("box", 1:nBox))

}
layout(1)