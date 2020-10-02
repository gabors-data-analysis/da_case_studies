################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# VARIOUS CHAPTERS
# Simulations, graphs
# version 0.91 2020-10-02


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(ggplot2)
library(tidyr)
library(cowplot)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "da_illustrations/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


# CHAPTER 06

# ch6-figure-2  ------------------------------------------------------
df <- 200
x = seq(-2.5, 2.5, 0.01)
d = dt(x, df)

data <- data.frame(x = x, d = d)
ggplot(data = data, aes(x=x, y=d)) +
  geom_line(color = color[1]) +
  geom_area(data = data[data$x< -2,], fill = color[1], alpha = 0.8) +
  geom_area(data = data[data$x> 2,], fill = color[1], alpha = 0.8) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = data[data$x == 0, "d"]), linetype = "dashed") +
  geom_segment(aes(x = -2, y = 0, xend = -2, yend = data[data$x == -2, "d"]), linetype = "dashed") +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = data[data$x == 2, "d"]), linetype = "dashed") +
  annotate(geom = "text", x = -1, y = 0.05 , label = "plain(Do~not~reject)~H[0]", parse = TRUE, size = 2.5) +
  annotate(geom = "text", x = 1, y = 0.05 , label = "plain(Do~not~reject)~H[0]", parse = TRUE, size = 2.5) +
  annotate(geom = "segment", x = -2.3, y = 0.01, xend = -2.3, yend = 0.1, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -2.3, y =  0.1, label = "plain(Reject)~H[0]", vjust = -1,  parse = TRUE, size = 2.5) +
  annotate(geom = "segment", x = 2.3, y = 0.01, xend = 2.3, yend = 0.1, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 2.3, y =  0.1, label = "plain(Reject)~H[0]", vjust = -1,  parse = TRUE, size = 2.5) +
  labs(y = "", x = "") +
  scale_y_continuous(limits = c(0,0.4), breaks = c(), expand=c(0,0)) +
  scale_x_continuous(breaks = c(-2,0,2)) +
  theme_bg() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
save_fig("ch06-figure-2-hyptest-nulltrue", output, size = "small")

# ch6-figure-3  ------------------------------------------------------
x = seq(-3, 6, 0.01)
d = dt(x, df)
d2 = dt(x, df, ncp = 2.6)

data <- data.frame(x = x, d = d, d2 = d2)
ggplot(data = data, aes(x=x, y=d2)) +
  geom_line(size = 0.5, color = color[1]) +
  geom_area(data = data[data$x< 2,], fill = color[3], alpha=0.8) +
  geom_line(aes(x=x, y=d), size = 0.5, color = color[3]) +
  geom_area(aes(x=x, y=d2), data = data[data$x> 2 , ], fill = color[5], alpha=0.7) +
  
  geom_vline(xintercept = -2, linetype = "dashed") +
  geom_vline(xintercept = 2, linetype = "dashed", color = color[3]) +
  annotate(geom = "text", x = 2.3, y = 0.42 , label = "Alternative is true",  size = 2.0, hjust = -0.1, color = color[1]) +
  annotate(geom = "text", x = -1.5,   y = 0.42 , label = "If Null were true", size = 2.0, hjust = -0.1, color = color[3]) +
  annotate(geom = "text", x = -1.0,   y = 0.48 , label = "Critical values", size = 2.0, hjust = -0.1, color = color[3]) +
  annotate(geom = "segment", x = -1.3, y = 0.48, xend = -2, yend = 0.48, size=0.5, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "segment", x = 1.3, y = 0.48, xend = 2, yend = 0.48, size=0.5, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 1.2, y = 0.06 , label = "FN", size = 2.5, hjust = -0.1, color = "black") +
  annotate(geom = "text", x = 3.5, y = 0.06 , label = "TP", size = 2.5, hjust = -0.1, color = "black") +
  labs(y = "", x = "") +
  scale_y_continuous(expand=c(0,0),limits = c(0,0.50), breaks = c()) +
  scale_x_continuous( limits = c(-3,6), breaks = c(-2,0,2,4,6)) +
  theme_bg() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
save_fig("ch06-figure-3-hyptest-nulltrue-alttrue", output, size = "small")


# CHAPTER 13


# ch13-figure-1  ------------------------------------------------------

x = seq(-10, 10, 0.01)
l1 = abs(x)
l2 = abs(x) + (x^2)/5
l3 = ifelse(x<=0, abs(x) + (x^2)/10, abs(x) + (x^2)/4.4)

data <- data.frame(x = x, l1 = l1, l2 = l2, l3 = l3)
p <- ggplot(data = data, aes(x=x, y=l1)) +
  geom_line(aes(color = "l1"), size = 1.5) +
  geom_line(aes(x=x, y=l2, color = "l2"), size = 1.5) +
  geom_line(aes(x=x, y=l3, color = "l3"), size = 1.5) +
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(ylim=c(0,30)) +
  scale_color_manual(values=color[1:3],
                     labels = c("symmetric, linear", "symmetric, convex", "asymmetric, convex"), name="") +
  labs(y = "Loss", x = "Prediction error") +
  theme_bg() +
  theme(legend.position = c(0.5, 0.95), legend.text = element_text(size=6))
p
save_fig("ch13-figure-1-lossfunctions", output, size = "small", plot = p)
# have to overwrite png otherwise legend is different
png(filename = paste0(output, "ch13-figure-1-lossfunctions.png"),
         width = mywidth_small, height = myheight_small, units = "cm",
         pointsize = 6, res = 1200)
print(p)
dev.off()



# ch13-figure-2  ------------------------------------------------------
x = seq(1, 20, 1)
original = c(100,90,81,73,67,62,58,55,52,50,48,47,46,45,44,43,42,41,40,39)
live = c(100,91,82,74,68,64,60,57,54,52,51,51,52,54,57,59,62,64,66,68)

data <- data.frame(x = x, original = original, live = live)
ggplot(data = data, aes(x=x, y=live)) +
  geom_line(size = 1, color = color[1]) +
  geom_line(aes(x=x, y=original), size = 1, color = color[2]) +
  geom_linerange(aes(x = x, ymin = original, ymax = live), size = 1.5, color = "grey") +
  annotate(geom = "segment", xend = 17, yend = data[data$x == 17, "live"],
           x =15, y = data[data$x == 15, "live"] + 8, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x =15, y = data[data$x == 15, "live"] + 8, label = "live data", size = 2.5, hjust =1.2) +
  annotate(geom = "segment", xend = 16, yend = data[data$x == 16, "original"],
           x =14, y = data[data$x == 14, "original"] - 8, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x =14, y = data[data$x == 14, "original"] - 8, label = "original data", size = 2.5, hjust =1.2) +
  scale_y_continuous(expand=c(0.01,0.01) ,limits = c(0,100)) +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(1,20), breaks = seq(1,20,1), labels=NULL) +
  labs(y = "Model RMSE", x = "Less complex                    --------->                       More complex") +
  theme_bg()+
  background_grid(major="y", minor="none")
save_fig("ch13-figure-2-overfitting-complexity", output, size = "small")


# CHAPTER 22


# ch22-figure-1  ------------------------------------------------------
x = seq(0, 10, 1)
y = seq(0, 10, 1)

data <- data.frame(x = x, y = y)
p <- ggplot(data = data, aes(x=x, y=y)) +
  geom_segment(aes(x = 2, y = 1, xend = 8, yend = 2, color = "1")) +
  geom_segment(aes(x = 2, y = 3, xend = 8, yend = 5.5, color = "2")) +
  geom_segment(aes(x = 2, y = 3, xend = 8, yend = 4, color = "2"), linetype = "dashed") +
  annotate(geom = "segment", xend = 6, yend = 4.7, x = 5, y = 4.7, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",  x = 5, y = 4.7, label = "Observed", hjust = 1.1, size = 2.5) +
  annotate(geom = "segment", xend = 6, yend = 3.7, x = 5, y = 2.5, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",  x = 5, y = 2.5, label = "Potential untreated", hjust = 1.1, size = 2.5) +
  annotate(geom = "text",  x = 8.4, y = 1.5, label = "alpha", size = 2.5, parse = TRUE) +
  annotate(geom = "text",  x = 8.4, y = 4.8, label = "beta", size = 2.5, parse = TRUE) +
  annotate(geom = "text",  x = 9.3, y = 4.3, label = "alpha + beta", size = 2.5, parse = TRUE) +
  annotate(geom = "segment", xend = 8.2, yend = 1, x = 8.2, y = 2,  arrow = arrow(length = unit(2, "mm"), type = "closed", ends = "both") ) +
  annotate(geom = "segment", xend = 8.2, yend = 4, x = 8.2, y = 5.5,  arrow = arrow(length = unit(2, "mm"), type = "closed", ends = "both") ) +
  annotate(geom = "segment", xend = 8.8, yend = 3, x = 8.8, y = 5.5,  arrow = arrow(length = unit(2, "mm"), type = "closed", ends = "both") ) +
  scale_y_continuous(expand = c(0,0),limits = c(0,6), labels =c()) +
  scale_x_continuous(breaks = c(2,8), limits = c(0,10), labels = c("Before","After")) +
  scale_color_manual(values=color[1:2], name="", labels = c("Untreated", "Treated")) +
  labs(y = "", x = "") +
  theme_bg() +
  theme(legend.text = element_text(size=6), panel.border = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.15, 0.95),)
p
save_fig("ch22-figure-1-diff-in-diffs-graph", output, size = "small", plot = p)
# have to overwrite png otherwise legend is different
png(filename = paste0(output, "ch22-figure-1-diff-in-diffs-graph.png"),
    width = mywidth_small, height = myheight_small, units = "cm",
    pointsize = 6, res = 1200)
print(p)
dev.off()




# CHAPTER 18


# test sets within 
# t = 1 to 24
# holdout + 5-fold cv
# holdout: 4 periods
# test: 4 periods
# training: 4x4 periods

png(filename = paste0(output, "ch18-figure-1-ts-cv-holdout1.png"),
    width = mywidth_large, height = myheight_large, units="cm", res=1200, pointsize = 6) 

#postscript(file =paste0(output, "ch18-figure-1-ts-cv-holdout1.eps"), onefile=FALSE, width = mywidth_small, height = myheight_small)

par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,26),ylim=c(0.5,1),
     xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
text(26,.95,"time")
holdout <- 21:24
for(j in 1:5)
{
  test    <- (1+(j-1)*4):(4+(j-1)*4)
  if(j<5)
    trainafter <- (5+(j-1)*4):20
  if(j>1)
    trainbefore <- 1:((j-1)*4)
  
  arrows(0,1-j/20,25,1-j/20,0.05)
  
  points(holdout,rep(1-j/20,length(holdout)),pch=21,col=color[3], cex=1.5, bg=color[4])
  points(test,rep(1-j/20,length(test)),pch=19,cex=1.5, col=color[1])
  if(j<5)
    points(trainafter,rep(1-j/20,length(trainafter)),pch=19,cex=1.5, col=color[2])
  if(j>1)
    points(trainbefore,rep(1-j/20,length(trainbefore)),pch=19,cex=1.5, col=color[2])
}
dev.off()

# TODO
# transform to ggplot, 
# make dots size=1.5
# graph should have annotation: last line of dots - valahogy kijelkolni oket egy ilyen jellel {, csak vizszintesen
# green part: "training set", purple: "test set", yellow: "holdout set" - 

###############################################
# rolling windows
# t = 1 to 32
# holdout + 5-fold cv
# holdout: 4 periods
# test: 4 periods
# training: 8 periods
# rest is not used


png(filename = paste0(output, "ch18-figure-2-ts-cv-holdout2.png"),
    width = mywidth_large, height = myheight_large, units="cm", res=1200, pointsize = 6) 

#postscript(file =paste0(output, "ch18-figure-2-ts-cv-holdout2.eps"), onefile=FALSE, width = mywidth_small, height = myheight_small)

par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,34),ylim=c(0.5,1),
     xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
text(34,.95,"time")
holdout <- 29:32
for(j in 1:5)
{
  train    <- (1+(j-1)*4):(8+(j-1)*4)
  test     <- (9+(j-1)*4):(12+(j-1)*4)
  if(j<5)
    notusedafter <- (13+(j-1)*4):28
  if(j>1)
    notusedbefore <- 1:((j-1)*4)
  
  arrows(0,1-j/20,33,1-j/20,0.05)
  
  points(holdout,rep(1-j/20,length(holdout)),pch=21,col=color[3], cex=1.5, bg=color[4])
  points(train,rep(1-j/20,length(train)),pch=19,cex=1.5, col=color[2])
  points(test,rep(1-j/20,length(test)),pch=19,cex=1.5, col=color[1])
  if(j<5)
    points(notusedafter,rep(1-j/20,length(notusedafter)),pch=19,cex=1.5, col="grey")
  if(j>1)
    points(notusedbefore,rep(1-j/20,length(notusedbefore)),pch=19,cex=1.5, col="grey")
}
dev.off()


# TODO
# transform to ggplot, 
# make dots size=1.5
# graph should have annotation: last line of dots - valahogy kijelölni őket egy ilyen jellel {, csak vizszintesen
# green part: "training set", purple: "test set", yellow: "holdout set", grey "unused" 

