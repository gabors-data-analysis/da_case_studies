#######################################
  # Ch03
#
  # simulation
# v1.0 2019-07-25
# v1.1 2020-03-09 axes edited + minor edits
# v1.2 2020-03-31 power law edited
# v1.3 2020-04-22 names ok + FIXME 

# FIXME (hist y ayis) not at zero but visible


######################################

#clear environment
rm(list = ls())

# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

# location folders
output <- paste0(dir,"da_case_studies/ch03-simulations/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))
source(paste0(func, "da_helper_functions.R"))


library(tidyverse)

# set the seed
set.seed(16460)

# sample size
N <- 100000
obs <- N

# Bernoulli
bernoulli <- as.data.frame(rbinom(obs, 1, 0.7))
colnames(bernoulli) <- "bernoulli"

g_bernoulli<- ggplot(data = bernoulli, aes (x = bernoulli, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.1, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  expand_limits(x = 0.01, y = 0.01) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_bg() 
g_bernoulli
#save_fig("theoretical_Bernoulli_R", output, size = "small")
save_fig("ch03-figure-rb7-1a-bernoulli", output, size = "small")


# Binomial
# with smaller sample
Nbinom <- 20
binomial <- as.data.frame(rbinom(obs,Nbinom,0.4))
colnames(binomial) <- "binomial"

g_binom<-ggplot(data = binomial, aes (x = binomial, y = (..count..)/sum(..count..))) +
  geom_histogram_da(binwidth = 0.5, type="percent") +
  labs(y = "Percent") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = c(0.01,0.01), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 16), breaks=seq(0, 16, by=2)) + 
  theme_bg() 
g_binom
#save_fig("theoretical_binomial_R", output, size = "small")
save_fig("ch03-figure-rb7-1b-binomial", output, size = "small")


# uniform [0,1]

uniform <- as.data.frame(runif(obs, 0, 1))
colnames(uniform) <- "uniform"

g_uniform<-ggplot(data = uniform, aes (x = uniform, y = (..count..)/sum(..count..))) +
  geom_histogram(bins =50, center=1,
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 1), breaks=seq(0, 1, by=0.1)) + 
  scale_y_continuous(expand = c(0.001,0.001), labels = scales::percent_format(accuracy = .1)) +
  coord_cartesian(clip = "off") +
    theme_bg() 
g_uniform
#save_fig("theoretical_uniform_R", output, size = "small")
save_fig("ch03-figure-rb7-1c-uniform", output, size = "small")

# normal
normal <- as.data.frame(rnorm(obs, 0,1))
colnames(normal) <- "normal"

g_normal<-ggplot(data = normal, aes (x = normal, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.2, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 0.1), breaks=seq(0, 0.1, by=0.02)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(-5, 5), breaks=seq(-4, 4, by=1)) + 
  expand_limits(x = 0.01, y = 0.01) +
  coord_cartesian(clip = "off") +
  theme_bg() 
g_normal
#save_fig("theoretical_normal_R", output, size = "small")
save_fig("ch03-figure-rb7-2a-normal", output, size = "small")

# lognoromal
# take the exponential of the randomly generated normal above
lognormal <- as.data.frame(exp(normal))
colnames(lognormal) <- "lognormal"

g_lognormal<-ggplot(data = subset(lognormal, lognormal <10), aes (x = lognormal, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.1, boundary=0.0,
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 0.08), breaks=seq(0, 0.08, by=0.02)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 8), breaks=seq(0, 8, by=1)) + 
  expand_limits(x = 0, y = 0) +
  coord_cartesian(clip = "off") +
  theme_bg() 
g_lognormal
#save_fig("theoretical_lognormal_R", output, size = "small")
save_fig("ch03-figure-rb7-2b-lognormal", output, size = "small")


# power-law
alpha <- 3
xmin <- 1
x <- seq(1, obs, 1)
powerlaw <-  xmin * (x ^ (-alpha))
histrange <- quantile(powerlaw, .75)
powerlaw <- powerlaw / sum(powerlaw)

powerlaw <- as.data.frame(powerlaw)


g_power<-ggplot(data = subset(powerlaw, powerlaw < histrange), aes (x = powerlaw, y = (..count..)/sum(..count..))) +
  geom_histogram(bins=50, boundary=0.5,
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  scale_x_continuous(labels = fancy_scientific) +
  theme_bg() 
g_power
#save_fig("theoretical_powerlaw_R", output, size = "small")
save_fig("ch03-figure-rb7-2c-powerlaw", output, size = "small")


