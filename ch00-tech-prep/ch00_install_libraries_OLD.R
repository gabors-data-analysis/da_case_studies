################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES (Central Europen University) and  Gabor KEZDI (University of Michigan)
# Cambridge University Press 2020

# License: Free to share, modify and use for educational purposes. 
# Not to be used for business purposes
# 
#
###############################################################################################x



# Required packages
# You shall run at the start, it will install the packages we use
# v1.3 2019-12-05
# v1.4 2019-12-23
# v1.5 2019-12-24
# v1.6 2019-12-29
# v1.7 2020-01-15
# v1.8 2020-03-24 move to R 3.6.3




# This is based on R version 3.6.3.
# make sure you have updated to this.
# also have Rtools downloaded and installed.

# if you have an older version, this is what you should do.
# installing/loading the package:
  #   if(!require(installr)) {
  #   install.packages("installr"); require(installr)} #load / install+load installr
  #   updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
  #   updateR(F, T, T, F, T, F, T) # install, move, update.package, quit R.

# you may consider a clear re-install. Get rid of existing version of R, clean or directories used (can check with ".libPaths() ") and install R again. Rstudio will find new version automatically.

# make sure we have the installer package
install.packages("pacman")
install.packages("tidyverse")
install.packages('BiocManager')
# basics
pacman::p_load(plyr,ggplot2, magrittr, data.table, pander, reshape2, haven, broom,
               foreign, pastecs, arm, DataCombine)

pacman::p_load(shiny, markdown, knitr, devtools, plotly)

#stats
pacman::p_load(plm, lmtest, sandwich, stargazer, segmented, lmtest, lspline, rms, boot, mfx, desc, urca, Hmisc )
pacman::p_load(estimatr, binsreg, AER, lfe, huxtable, margins, dyn)

pacman::p_load(kableExtra, vars, janitor, summarytools)



#dev
pacman::p_load(DescTools, DescTools, rvest, skimr, pdp, aTSA )
pacman::p_load(extrafont)

#extra
pacman::p_load(sjPlot, sjstats, scales, multiwayvcov, readstata13, bit64, xtable, car, clubSandwich)
# graphs
pacman::p_load(grid, gridExtra, ggstatsplot, lsr, ggpmisc)
pacman::p_load(ggraph, igraph)
pacman::p_load(cairoDevice, Cairo, dagitty)
pacman::p_load(RColorBrewer, viridis, cowplot, directlabels)
pacman::p_load(dynlm, quantmod, fredr)

# ML
pacman::p_load(caret,h2o, glmnet, MASS, rpart, gbm, randomForest, ROCR, rpart, rattle, ranger, prophet, e1071) 
pacman::p_load(FeatureHashing, Matrix, xgboost, pROC, stringr, dummies, Metrics, kernlab, mlbench, moments, rpart.plot, RColorBrewer, party, partykit)
pacman::p_load(MatchIt, gmodels, hdm)

# TS
pacman::p_load(fpp3) # includes fable, tstibble

# Problems, issues
# some packages require dependencies that behave slightly differently on unix and windows - e.g Shiny requires additional libraries on windows, while not on unix
# wierd bug with "undefined variables" - only on R windows. If you keep seing this add #!diagnostics off as the first line of your code
# rJava package has many  problems on OS and therfore opening excel files on a Mac causes many troubles





