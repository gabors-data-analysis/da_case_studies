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

# v1.5 2020-01-05 change to large size asked by CUP
# v1.6 2020-01-26 cut undeeded stuff
# v1.7 2020-03-10 ads inch for eps_
# v1.8 2020-03-24 ads new save_fig function
# v1.9 2020-04-04 minor color edits
# v1.10 2020-04-05 ads tree saver
# v1.11 2020-04-09 color edits again
# v2.0 2020-06-08 final touches
# v2.1 2020-08-04 redo hist freq w boundary forced=0
# v2.2 2020-08-24 library check


library(tidyverse)
library(scales)

# -----------------------------
# in Windows this may be needed
# Sys.setlocale("LC_ALL","English")


# -----------------------------------------------------------------------------------------------------------------------
####################################################
# Define colors
####################################################

# colors based on viridis RGB scheme may be used for online
# color <- c("#3D4D8AFF",            "#43BF71FF",            "#440154FF",            "#FDE725FF",            "#23888EFF" )

# this is derived from CMYK color scheme by Cambridge University Press for print
color <- c("#3a5e8cFF", "#10a53dFF", "#541352FF", "#ffcf20FF", "#2f9aa0FF")
#blue  #3a5e8c   # purple #541352    teal #2f9aa0  yellow #ffcf20   # green #10a53d
show_col(color)

color.outline = "#FFFFFF"
#color.outline = "grey90"
color.regline = "#000000"
color.statline = "#4D4D4D"
color.stat = "#000000"
color.background = "grey80"
color.fill = "#000000"

color.fill = "#0000FF"
color.fill2 ="#ADD8E6"
color.fill3 = "#FF8C00"


# The function sets the basic design of the graphs (axis, grids, titles etc.)
theme_bg <- function() {

    # Generate color palette
  palette <- c("white", 
               "grey70", 
               "black",
               "grey50") # global

color.background
  color.background = palette[1]
  color.grid.major = palette[4]
  color.grid.minor = palette[2]
  color.axis.text = palette[3]
  color.axis.title = palette[3]
  color.title = palette[3]

#  palette_brewer <- brewer.pal("Blues", n=9)
  color.fill <- palette[1]
  color.line <- palette[3]

  # Chart elements

  theme_bw() +

    # Chart region
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=palette[2],size=.01)) +
    # Axis
    
    theme(axis.line=element_line(color=color.line,size=.2)) +
    
    # Grids
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.1)) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.1)) +
    # theme(axis.ticks=element_blank()) +

        # Legend

    theme(legend.position=c(0.8,0.2),
          panel.grid.minor =element_blank()) +
    theme(legend.background = element_rect(fill="transparent")) +
    theme(legend.text = element_text(size=4,color=color.axis.title )) +

    # Title & axis labels

    theme(plot.title=element_text(color=color.title, size=6, vjust=1.25, hjust=0.5, face = "plain")) +
    theme(axis.text.x=element_text(size=6,color=color.axis.text, face = "plain")) +
    theme(axis.text.y=element_text(size=6,color=color.axis.text, face = "plain")) +
    theme(axis.title.x=element_text(size=6,color=color.axis.title, vjust=0, face = "plain")) +
    theme(axis.title.y=element_text(size=6,color=color.axis.title, vjust=1.25, face = "plain")) +

    # Margins

    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
}




####################################################
# create hand made functions for most common geoms.
####################################################

theme_colors= color

geom_point_da <- function(color = theme_colors[1], size = 1,  shape = 16, alpha = 0.7, 
                          show.legend=F, na.rm = TRUE){
  geom_point(color = color, size = size, shape = shape, alpha = alpha, 
             show.legend = show.legend, na.rm = na.rm)
}

geom_smooth_da <- function(method="lm", color=theme_colors[2], se=F, size=0.7){
  geom_smooth(method=method,color=color,se=se, size=size)
}

geom_segment_da <- function(color=theme_colors[2], size=0.7, na.rm=TRUE){
  geom_segment(color=color,size=size,na.rm=na.rm)
}

geom_line_da <- function(color=theme_colors[1], size=0.4, na.rm=TRUE){
  geom_line(color=color,size=size,na.rm=na.rm)
}


#geom_histogram_da <- function(type='percent',boundary=0,
#                              color = color.outline, fill = theme_colors[1],closed='left',
#                              size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE){
  

geom_histogram_da <- function(type='percent',boundary=0, binwidth=NULL, bins=NULL,
                              color = color.outline, fill = theme_colors[1],
                              size = 0.2, alpha = 0.8,  show.legend=F, na.rm=TRUE,closed='left'){
  if(type=='percent'){
    geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth = binwidth, bins=bins, boundary=0,
                   color = color, fill = fill, alpha = alpha,closed=closed)
  }
  else if(type == 'frequency'){
    geom_histogram(binwidth = binwidth, bins=bins, boundary=0,
                   color = color, fill = fill, alpha = alpha,closed=closed)
  }
  else {
    stop("Unknown type value for geom_histogram_da.")
  }
  
}

scale_y_continuous_da<-function(type='percent',name='Percent',expand = c(0.01,0.01),
                                labels = scales::percent_format(accuracy = 1)){
  if(type=='percent'){
    scale_y_continuous(name = name,expand = expand, labels = labels)
  }
  else if(type == 'frequency'){
    scale_y_continuous(name = 'Frequency',expand = expand, labels = waiver())
  }
  else {
    stop("Unknown type value for scale_y_continuous_da.")
  }
  
}






# -----------------------------------------------------------------------------------------------------------------------
####################################################
# Define size
####################################################

mywidth_small = 7.5
mywidth_large = 12
mywidth_verylarge = 15

aspect=0.75
myheight_small      = mywidth_small     * aspect
myheight_large      = mywidth_large     * aspect
myheight_verylarge  = mywidth_verylarge * aspect

# eps in inch
inch=2.54

eps_myheight_small     = myheight_small/inch
eps_mywidth_small      = mywidth_small/inch
eps_myheight_large     = myheight_large/inch
eps_mywidth_large      = mywidth_large/inch
eps_myheight_verylarge = myheight_verylarge/inch
eps_mywidth_verylarge  = mywidth_verylarge/inch





# -----------------------------------------------------------------------------------------------------------------------
####################################################
# Define save_fig
####################################################



## Function to save figures.
save_fig <- function(filename, filepath, size,plot=last_plot()){
  filename_png<-paste0(filename,'.png')
  filename_eps<-paste0(filename,'.eps')
  if (size=="small"){
    width=mywidth_small
    height=myheight_small
    eps_width=eps_mywidth_small
    eps_height=eps_myheight_small
    psize = 6
  } else if (size == "large") {
    width=mywidth_large
    height=myheight_large
    eps_width=eps_mywidth_large
    eps_height=eps_myheight_large
    psize = 10
  } else if (size == "verylarge") {
    width=mywidth_verylarge
    height=myheight_verylarge
    eps_width=eps_mywidth_verylarge
    eps_height=eps_myheight_verylarge
    psize = 12
  }
  else{
    print("Unknown size option. Try 'small', 'large', or 'verylarge'")
  }
  ggsave(paste0(filepath, filename_png),plot=plot, width=width, height=height,units = "cm", dpi = 1200)
  cairo_ps(filename = paste0(filepath, filename_eps),
           width = eps_width, height = eps_height, pointsize = psize,
           fallback_resolution = 1200)
  print(plot)
  dev.off()
}


