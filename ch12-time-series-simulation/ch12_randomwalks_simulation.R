############################################################
#
# DATA ANALYSIS TEXTBOOK
# CHAPTER 12
# time series simulations

# v1.1 2019-12-23 line edits
# v1.2 2020-03-25 graph edits
# v1.3 2020-04-22 names ok
# v1.4 2020-04-27 minor edits

############################################################  
# WHAT THIS CODES DOES:
# random walk simulation
# serial correlation simulations

# CLEAR MEMORY
rm(list=ls())



# Libraries
library(tidyverse)
library(ggplot2)


############################################################  
# SET YOUR OWN PATH HERE
############################################################  
# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

#location folders
output <- paste0(dir,"da_case_studies/ch12-time-series-simulation/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))


# PART 1
# Random walk simulation 
# Generate k random walks across time {0, 1, ... , T}


# set parameters
set.seed (10)
T <- 100  # number of obs
k <- 5    # nr of random walks generated
initial.value <- 0

# create a function
GetRandomWalk <- function() {
  # Add a standard normal at each step
  initial.value + c(0, cumsum(rnorm(T)))
}

# Matrix of random walks
values <- replicate(k, GetRandomWalk())

# visualize
rws <- as.data.frame(values)
rws <- rws %>%
  mutate(time=as.numeric(rownames(.)))

rws <- rws %>% 
  gather(var, value, V1:V5) 

rws_plot <- ggplot(rws,aes(time, value, color=var)) + 
  geom_line (show.legend = FALSE, size =0.8) +
  theme_bg() +
  scale_color_manual(values = c(color[1], color[2], color[3], color[4], color[5])) +
  labs(x = "Time period", 
       y = "Value of the simulated variable") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,100), breaks=seq(0,100,10)) 
rws_plot
#save_fig("R_07_randomwalks", "small")
save_fig("ch12-figure-1-randomwalks", output, "small")


# PART 2
# Serially correlated vs serially uncorrelated series
# simulation exercies



# rnorm(n, mean = 0, sd = 1)

# serially uncorrelated series/white noise
set.seed(2016)
uncorr <- as.data.frame(ts(rnorm(100, mean=0, sd=1)) )

uncorr <- uncorr %>%
  mutate(t=as.numeric(rownames(.)))
uncorr

whitenoise <- ggplot(uncorr,aes(t, x)) + 
  geom_line (show.legend = FALSE, size =0.6, color=color[1]) +
  geom_hline(yintercept=0, 
             color = color[2], size=1)+
  labs(x = "Time period", 
       y = "Value of the simulated variable") +
  theme_bg() +
  scale_y_continuous(expand = c(0.01,0.01)) + 
  scale_x_continuous(expand = c(0.01,0.01),breaks=seq(0,100,10)) 
whitenoise
#save_fig("R_07_serialcorr_whitenoise", "small")
save_fig("ch12-figure-9a-serialcorr-whitenoise", output, "small")


# serially correlated series, pho=0.8
set.seed(2016)
rho=0.8
E <- rnorm(100, 0, 1)
x <- numeric()
x[1] <- E[1]
for(i in 2:100) x[i] <- rho*x[i-1] + E[i]

E <- as.data.frame(E)

corr08 <- E %>%
  mutate(t=as.numeric(rownames(.)))

corr08_graph <- ggplot(corr08,aes(t, x)) + 
  geom_line (show.legend = FALSE, size =0.6, color=color[1]) +
  geom_hline(yintercept=0, 
             color = color[2], size=1)+
  labs(x = "Time period", 
       y = "Value of the simulated variable") +
    theme_bg() +
  scale_y_continuous(expand = c(0.01,0.01)) + 
  scale_x_continuous(expand = c(0.01,0.01),breaks=seq(0,100,10)) 
corr08_graph

#save_fig("R_07_serialcorr_corr08", "small")
save_fig("ch12-figure-9b-serialcorr-corr08", output, "small")






