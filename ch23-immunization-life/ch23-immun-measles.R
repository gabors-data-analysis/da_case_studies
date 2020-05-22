# ***************************************************************
# * ch23 
# *
# * Case Study immunization against measels and child mortality age 0-5
# * whole World

# * Data: world-bank_immunizaton-panle

# v2.0. 2020-04-19
# v2.1. 2020-04-20 minor graph edits
# v2.2 2020-04-22 names ok
# v2.3 2020-04-22 labels edited

# **************************************************************

# * WHAT THIS CODES DOES:
# * looks at continents for aggregate trends
# * country level models
# **************************************************************


# Clear memory
rm(list=ls())

library(tidyverse)
library(purrr)
library(haven)
library(stargazer)


# CHECK WORKING DIRECTORY - CHANGE IT TO YOUR WORKING DIRECTORY
# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


# dir <- "/Users/zholler/Documents/Private/"

#location folders
data_in <- paste0(dir,"da_data_repo/world-bank-immunization/clean/")
data_out <- paste0(dir,"da_case_studies/ch23-immunization-life/")
output <- paste0(dir,"da_case_studies/ch23-immunization-life/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))
source(paste0(func, "da_helper_functions.R"))

# Read in data ------------------------------------------------------------

data <- read_dta(paste0(data_in, "world-bank_immunization-continents.dta"))

# **************************************************
# * info graph on measles vaccination continent aggregates

p1 <- ggplot(data, aes(x = year, y = imm6)) +
  geom_line(color = color[1], size = 0.7) +
  geom_line(aes(x = year, y = imm7), color = color[2], size = 0.7) +
  geom_text(data = data[12,], aes(label = "South Asia"), hjust = 1.2, vjust = 1, size=2) +
  geom_text(data = data[16,], aes(y = imm7, label = "Sub-Saharan Africa"), hjust = 0.4, vjust = 1.5, size=2) +
  labs(y = "Immunization rate (percent)", x="Date (year)") + 
  scale_y_continuous(expand=c(0,0), breaks = seq(50, 100, by = 10), limits = c(50, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(1998, 2018, by = 5), limits = c(1998, 2018)) +
  theme_bg()

for (i in seq(1,5)) {
	p1 <- p1 + geom_line(aes_string(x = "year", y = paste0("imm",i)), color = "grey", size=0.5)
}
p1
save_fig("ch23-figure-2a-tsimmun", output, size = "small")

p2 <- ggplot(data, aes(x = year, y = surv6)) +
  geom_line(color = color[1], size = 0.7) +
  geom_line(aes(x = year, y = surv7), color = color[2], size = 0.7) +
  geom_text(data = data[11,], aes(label = "South Asia"), hjust = 0, vjust = 1.5, size=2) +
  geom_text(data = data[15,], aes(y = surv7, label = "Sub-Saharan Africa"), hjust = 0.2, vjust = 1.5, size=2) +
  labs(y = "Child survival rate (percent)", x="Date (year)") + 
  scale_y_continuous(expand=c(0,0), breaks = seq(80, 100, by = 5), limits = c(80, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(1998, 2018, by = 5), limits = c(1998, 2018)) +
  theme_bg()
for (i in seq(1,5)) {
	p2 <- p2 + geom_line(aes_string(x = "year", y = paste0("surv",i)), color = "grey", size=0.5)
}
p2
save_fig("ch23-figure-2b-tssurvival", output, size = "small")

#Ch23_figures/immun-FD1.tex
#Ch23_figures/immun-FESE.tex
#Ch23_figures/immun-FD2.tex
#Ch23_figures/immun-FE.tex



