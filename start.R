# basix
install.packages("tidyverse")
install.packages("pacman")
library(tidyverse)
library(pacman)

#ch00
pacman::p_load(urca, sandwich, stargazer,stringr)
pacman::p_load(scales, data.table, knitr )

#Part I
pacman::p_load(lspline, cowplot, arm, pastecs, DataCombine, janitor)
pacman::p_load(haven, Hmisc, xtable, binsreg)

# Part II
pacman::p_load(viridis, grid, gridExtra)


#Part III
pacman::p_load(lmtest, caret, glmnet, skimr, directlabels, 
               prophet, timeDate)

# Part IV
p_load(MatchIt, Matching, gmodels)

