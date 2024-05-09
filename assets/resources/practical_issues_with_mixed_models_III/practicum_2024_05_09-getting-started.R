library(tidyverse)
library(lme4)
library(mvtnorm)

N_item <- 200
N_participant <- 300
dat <- expand_grid(Wordlikeness=c("low","high"),Item=1:N_item,Participant=1:N_participant) %>%
  filter(xor(Wordlikeness=="low",Item <= (N_item/2)))

#xtabs(~ Participant + Item + Wordlikeness, dat)
set.seed(3)
beta_low <- 450
beta_high <- 600
Sigma_participant <- matrix(c(50^2,0,0,50^2),2,2)
sigma_item <- 20
sigma_epsilon <- 200
b_participant <- rmvnorm(N_participant,c(0,0),Sigma_participant)
b_item <- rnorm(N_item,0,sigma_item)


dat$RT <- with(dat,pmax(beta_low*ifelse(Wordlikeness=="low",1,0) + 
                 beta_high*ifelse(Wordlikeness=="high",1,0) +
                 b_participant[cbind(Participant,1)] * ifelse(Wordlikeness=="low",1,0) +
                 b_participant[cbind(Participant,2)] * ifelse(Wordlikeness=="high",1,0) +
                 b_item[Item] +
                 rnorm(nrow(dat),0,sigma_epsilon),
                 rnorm(nrow(dat),100,20))
               )
ggplot(dat,aes(x=RT)) + geom_density()

## accuracies
bias <- -1.5
rt_effect <- 0.005
eta <- function(RT) bias + rt_effect * RT

## visualize accuracy~rt relationship
dat_plot <- tibble(RT=seq(0,1500))
dat_plot$p_correct <- plogis(eta(dat_plot$RT))
ggplot(dat_plot,aes(x=RT,y=p_correct)) + geom_line()

dat$accuracy <- with(dat,rbinom(nrow(dat),1,plogis(eta(RT))))
xtabs(~Wordlikeness + accuracy,dat)