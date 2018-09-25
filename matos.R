library(tidyverse)
library(magrittr)

##############################################
## Matos' compensation w/ linear and        ##
## logarithmic utilities for monetary gain. ##
##############################################

# pp: probability of Matos correctly answering the question.
linear_compensation <- function(pp)
{
  uu <- ((300)*(1-pp) + (10^6*pp)-5*10^5)/10^6
  uu*(uu > 0)
}

log_compensation <- function(pp)
{
  uu <- (exp((log(300)*(1-pp)+log(10^6)*pp))-5*10^5)/10^6
  uu*(uu>0)
}

n = 10^3
pp = (0:n)/n
dt_1 = tibble(prob = pp,
              comp = linear_compensation(pp),
              utility = "no risk aversion")
dt_2 = tibble(prob = pp,
              comp = log_compensation(pp),
              utility = "high risk aversion")
dt = rbind(dt_1, dt_2)

dt %>%
  ggplot(aes(x = prob, y = comp, group = utility)) +
  geom_line(aes(linetype = utility)) +
  geom_hline(yintercept = .125, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(0.5, 0.915)) +
  scale_y_continuous(breaks = c(0, 0.125, 0.5)) +
  xlab("Probability of correct answer") +
  ylab("Compensation (in millions of R$)")
ggsave("./figures/compensation_matos_1.pdf")

############################################
## Matos' compensation as a function of   ##
## risk and probability of correct answer ##
############################################

nn = 10^2
dt = expand.grid(prob = (1:nn)/nn , 
                 risk = (1:(nn-1))/nn) %>%
  as.tibble()


#[Kadane, 2011] Utility function
#mm: monetary gain, tt: utility function parameter.
k_utility <- function(mm, tt) (-1 * mm^(1-tt))/(tt-1)
#uu: utility gain, tt: utility function parameter.
k_inv_utility <- function(uu,tt) (-1 * (tt-1)*uu)^(1/(1-tt))

k_compensation <- function(pp,tt)
{
  uu <- pp*k_utility(10^6, tt) + (1-pp)*k_utility(300, tt)
  mm <- k_inv_utility(uu, tt)
  diff <- mm - .5 * 10^6
  diff*(diff > 0)/10^6
}

dt %>%
  mutate(Compensation = map2_dbl(prob, risk, k_compensation)) %>%
  ggplot(aes(x = prob, y = risk)) +
  geom_raster(aes(fill = Compensation), interpolate = TRUE) +
  scale_fill_gradient2(low = "white", mid = "grey", high = "black", 
                       midpoint=.25) +
  scale_x_continuous(breaks = c(0, .25, .5, .75, 1)) +
  xlab("Probability of correct answer (p)") +
  ylab(expression(paste("Proportion of risk aversion (",
                        theta,
                        ")", sep = ""))) +
  theme_classic()
ggsave("./figures/compensation_matos_2.pdf")
