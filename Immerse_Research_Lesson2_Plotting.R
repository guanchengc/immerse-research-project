install.packages("demodelr")
install.packages("ggplot2")
library(ggplot2)
library(demodelr)
library(dplyr)
library(purrr)

# Deterministic and stochastic parts of the equation
deterministic_log <- c(dx ~ r*x*(1-x/K))
stochastic_log <- c(dx~1)

# Identify our initial conditions and our parameters
init_logistic <- c(x=3)
logistic_parameters <- c(r=0.8, K=100)

#Identify the time steps and the time duration of the simulation
deltaT_logistic <- 0.05
timesteps_logistic <- 200

#sd of the stochastic noise
D_logistic <- 1

# simulation
logistic_out <- demodelr::euler_stochastic(deterministic_rate=deterministic_log,
                                 stochastic_rate=stochastic_log,
                                 initial_condition=init_logistic,
                                 parameters=logistic_parameters,
                                 deltaT=deltaT_logistic,
                                 n_steps=timesteps_logistic,
                                 D=D_logistic)

#plot
ggplot(data=logistic_out) + geom_line(aes(x=t,y=x))

#sim
n_sims <- 200

logistic_run <- rerun(n_sims) %>%
  set_names(paste("sim", 1:n_sims)) %>%
  map(~demodelr::euler_stochastic(deterministic_rate=deterministic_log,
                                  stochastic_rate=stochastic_log,
                                  initial_condition=init_logistic,
                                  parameters=logistic_parameters,
                                  deltaT=deltaT_logistic,
                                  n_steps=timesteps_logistic,
                                  D=D_logistic)) %>%
  map_dfr(~.x, .id="simulation")

ggplot(data=logistic_run) +
  geom_line(aes(x=t, y=t, color=simulation))+
  ggtitle("Plot for logistic SDE")+
  grids(color="none")