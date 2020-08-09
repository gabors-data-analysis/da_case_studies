################################################################################
# DATA ANALYSIS TEXTBOOK
# CH 12 TIME SERIES
# STOCK RETURN AND MARKET RISK
#
# STOCKS-SP500
# v1.0 2018-10-01 first run
# v1.3 2019-08-15 new take with many options
# v1.4 2019-12-04 new version for textbokk
# v1.5 2019-12-08 small edits, now has u.r. test
# v1.6 2020-02-06 graph edits re axis
# v1.7 2020-03-24 more graph edits re axis
# v1.8 2020-04-22 names ok
# v1.9 2020-04-25 names ok
# v2.0 2020-04-28 graph label edits
# v2.1 2020-04-30 graph label edits
# v2.2 2020-08-09 4,5,6 y axis edits


################################################################################

# WHAT THIS CODES DOES:
# combines data, aggregates to monthly level
# describes patterns
# many ts graphs
# runs regressions 


library(lubridate)
library(ggplot2)
library(cowplot)
library(scales)
library(DataCombine)
library(stargazer)
library(sandwich)
library(dyn) 
library(lmtest)
library(estimatr)
library(huxtable)
library(plotly)
library(htmlwidgets)
library(tidyverse)
library(dplyr)
library(xtable)
library(aTSA)


# CLEAR MEMORY
rm(list=ls())


################################################################################
# SET YOUR DIRECTORY HERE
################################################################################
# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

data_in <- paste0(dir,"da_data_repo/stocks-sp500/raw/")
data_out <- paste0(dir,"da_data_repo/stocks-sp500/clean/")

output   <- paste0(dir,"da_case_studies/ch12-stock-returns-risk/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))



#####################################################################
# cleaner
#####################################################################


#location folders
stock_data<-read_csv(paste0(data_in,"ready_sp500_45_cos.csv"))
# filter on MSFT
MSFT <- stock_data %>% filter(ticker == "MSFT")

# format date to daily
p_MSFT <-MSFT %>% select(ref.date,price.close) %>% rename(date=ref.date)

# ready_sp500_index.csv
sp500_index<-read_csv(paste0(data_in,"ready_sp500_index.csv"))

# format date to daily
p_SP500 <- sp500_index %>% select(ref.date,price.close) %>% rename(date=ref.date)

# join these two, ie date, and closing price for MSFT and SP500
data_daily <- inner_join(p_SP500,p_MSFT,by="date") %>% rename(p_SP500=price.close.x,p_MSFT=price.close.y)

# filter to keep from date("31/12/1997","DMY") to date("31/12/2018","DMY")
data_daily <- data_daily %>% filter(date>="1997-12-31" & date<="2018-12-31") 

# 21ys + last day of 1997
data_daily <- data_daily %>% mutate(year = year(date),month=month(date))



write_csv(data_daily, paste0(data_out, "stock-prices-daily.csv"))



#####################################################################
# analysis
#####################################################################



#############################
#   PART I: graphs
#############################

data_daily<- data_daily %>% mutate(lnp_MSFT=log(p_MSFT),lnp_SP500=log(p_SP500))

  # daily graphs
p1<-ggplot(data=data_daily,aes(x=date)) +
  geom_line(aes(y = p_MSFT),color = color[1], size = 0.5)+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,120), breaks = seq(0,120,20)) +  
  scale_x_date(breaks = as.Date(c("1998-01-01","2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01")),
               limits = as.Date(c("1998-01-01","2018-12-31")), labels = date_format("1%b%Y"),
               minor_breaks = "1 year") +
  labs(y = "Microsoft stock price (US dollars)",x= "Date (day)")+
  theme_bg() 
p1
#save_fig("ch12-msft-1_R", "small", plot=p1)
save_fig("ch12-figure-2a-msft-day", output, "small", plot=p1)


p2<-ggplot(data=data_daily,aes(x=date)) +
  geom_line(aes(y = p_SP500),color = color[1], size = 0.5)+
  scale_y_continuous(limits = c(500,3000), breaks = seq(500,3000,500)) +  
  scale_x_date(breaks = as.Date(c("1998-01-01","2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01")),
               limits = as.Date(c("1998-01-01","2018-12-31")), labels = date_format("1%b%Y"),
               minor_breaks = "1 year") +
  labs(y = "S&P 500 stock market index",x= "Date (day)")+
  theme_bg() 
p2
#save_fig("ch12-sp500-1_R", "small", plot=p2)
save_fig("ch12-figure-2b-sp500-day",output, "small", plot=p2)

  
# Phillips Perron unit root test (with long NW lags). 
  # Take the p-value for Z-rho with drift, no trend.
pp.test(data_daily$p_MSFT, lag.short = F)
pp.test(data_daily$p_SP500, lag.short = F)  
  

# * DAILY YIELD
data_daily <- data_daily %>% 
            mutate(l.p_MSFT=lag(p_MSFT),l.p_SP500=lag(p_SP500)) %>% 
            mutate(d.p_MSFT=p_MSFT-l.p_MSFT,d.p_SP500=p_SP500-l.p_SP500)

data_daily <- data_daily %>% mutate(PctRetMSFT=(d.p_MSFT/l.p_MSFT)*100, PctRetSP500=(d.p_SP500/l.p_SP500)*100,)


# create monthly version of data by taking the last day of each month
data_monthly<-data_daily %>% select(date,year,month,p_SP500,p_MSFT) %>% group_by(ym = strftime(date, "%Y-%m")) %>% 
                                                        filter(date==max(date)) %>% ungroup()

data_monthly <- data_monthly %>% mutate(l.p_MSFT=lag(p_MSFT),l.p_SP500=lag(p_SP500)) %>% 
  mutate(d.p_MSFT=p_MSFT-l.p_MSFT,d.p_SP500=p_SP500-l.p_SP500)


data_monthly <- data_monthly %>% mutate( PctRetMSFT= (d.p_MSFT/l.p_MSFT)*100, 
                             PctRetSP500 = (d.p_SP500/l.p_SP500)*100
                            )

data_monthly<-data_monthly %>% mutate(d.lnp_MSFT=log(p_MSFT)-log(lag(p_MSFT)),
                        d.lnp_SP500=log(p_SP500) - log(lag(p_SP500)))



p3<-ggplot(data=data_monthly,aes(x=date)) +
  geom_line(aes(y = p_MSFT),color = color[1], size = 0.5)+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,120), breaks = seq(0,120,20)) +  
  scale_x_date(breaks = as.Date(c("1998-01-01","2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01")),
               limits = as.Date(c("1998-01-01","2018-12-31")), labels = date_format("%b%Y"),
               minor_breaks = "1 year") +
  labs(y = "Microsoft stock price (US dollars)",x = "Date (month)")+
  theme_bg() 
p3
#save_fig("ch12-stocks-msft-2_R","small", plot=p3)
save_fig("ch12-figure-3a-msft-mo", output, "small", plot=p3)


p4<-ggplot(data=data_monthly,aes(x=date)) +
  geom_line(aes(y = p_SP500),color = color[1], size = 0.5)+
  scale_y_continuous(limits = c(500,3000), breaks = seq(500,3000,500)) +  
  scale_x_date(breaks = as.Date(c("1998-01-01","2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01")),
               limits = as.Date(c("1998-01-01","2018-12-31")), labels = date_format("%b%Y"),
               minor_breaks = "1 year") +
  labs(y = "S&P500 stock market index",x = "Date (month)")+
  theme_bg() 
p4
#save_fig("ch12-stocks-sp500-2_R","small", plot=p4)
save_fig("ch12-figure-3b-sp500-mo", output, "small", plot=p4)


#unit root test

pp.test(data_monthly$p_MSFT)
pp.test(data_monthly$p_SP500)    
  

p4a <- ggplot(data=data_monthly,aes(x=date)) +
  geom_line(aes(y = PctRetMSFT),color = color[1], size = 0.4)+
  geom_hline(yintercept = 1.13,color=color[3], size=0.8) +
  labs(y = "Microsoft monthly returns (percent)",x = "Date (month)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(-45,45), breaks = seq(-40,40, by=20)) +
  scale_x_date(breaks = as.Date(c("1998-01-01","2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01")),
               limits = as.Date(c("1998-01-01","2018-12-31")), labels = date_format("%b%Y"),
               minor_breaks = "1 year")  +
  theme_bg() 
p4a
#save_fig("ch12-stocks-msft-3_R","small")
save_fig("ch12-figure-4a-msft-moret",output, "small", plot=p4a)

p4b<-ggplot(data=data_monthly,aes(x=date)) +
  geom_line(aes(y = PctRetSP500),color = color[1], size = 0.4)+
  geom_hline(yintercept = 0,47,color=color[3], size=0.8) +
  labs(y = "S&P500 index monthly returns (percent)",x = "Date (month)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(-45,45), breaks = seq(-40,40, by=20)) +
  scale_x_date(breaks = as.Date(c("1998-01-01","2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01")),
               limits = as.Date(c("1998-01-01","2018-12-31")), labels = date_format("%b%Y"),
               minor_breaks = "1 year") +
  theme_bg() 
  
p4b
#save_fig("ch12-stocks-sp500-3_R","small" )
save_fig("ch12-figure-4b-sp500-moret",output, "small", plot=p4b)

#unit root test
pp.test(data_monthly$PctRetMSFT)
pp.test(data_monthly$PctRetSP500)  


##Descriptive statistics
sm<- data_monthly %>% select(PctRetMSFT,PctRetSP500) %>% 
                      rename('Monthly returns on Microsoft (percent)'=PctRetMSFT) %>%
                      rename('Monthly returns on the S&P500 (percent)'=PctRetSP500) %>%
                      gather(key = "index", value = "pct_return") %>%
                      group_by(index) %>%
                      summarize_all(list(~min(.,na.rm=TRUE),~max(.,na.rm=TRUE),~mean(.,na.rm=TRUE),~sd(.,na.rm=TRUE),~n()))

xt<-xtable(sm,align='llccccc')
names(xt) <- c('Variables','Min','Max','Mean','Sd','N')
print(xt, type = "latex",digits = 1,include.rownames = FALSE, file = paste0(output,"table.tex"))

# CORRELATIONS

# scatterplot

p5<-ggplot(data=data_monthly,aes(x=PctRetSP500,y = PctRetMSFT)) +
  geom_point_da()+
  geom_smooth_da(method='lm')+
  labs(x="S&P500 index monthly returns (percent)",y="Microsoft stock monthly returns (percent)")  +
  theme_bg() +
  geom_segment(aes(x = -20, y = -20, xend = 20, yend = 20), color=color[3], size=0.5, linetype="dashed")+
  geom_segment(aes(x = 10, y = 0, xend = 17, yend = 17), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 10, y = -3, size=2, label = "45 degree line for beta=1")+
  geom_segment(aes(x = -10, y = -17, xend = -15, yend = -17), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = -5, y = -17, size=2, label = "reg line, beta=1.26")
p5
#save_fig("ch12-stocks-scatter_R","small", plot=p9)
save_fig("ch12-figure-5-stocks-scatter",output, "small", plot=p5)


p6a<-ggplot(data=data_monthly %>% select(date, PctRetMSFT, PctRetSP500) %>%  
             gather(key = "index", value = "pct_return", -date))+
  geom_line(aes(x=date,y = pct_return,color=index,size=index))+
  scale_size_manual( values = c(0.3,0.25),guide=FALSE) + 
  scale_color_manual(name = "", values=c(color[1], color[2]), 
                     labels = c("Microsoft", "S&P500")) +
  labs(x = 'Date (month)',y = "Monthly returns (percent)")+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(-45,45), breaks = seq(-40,40,20)) +  
  scale_x_date(breaks = as.Date(c("1998-01-01","2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01")),
             limits = as.Date(c("1998-01-01","2018-12-31")), labels = date_format("%b%Y"),
               minor_breaks = "1 year") +
  theme_bg() +
  theme(legend.position=c(0.65,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
p6a
#save_fig("ch12-stocks-together-1_R","small", plot=p7)
save_fig("ch12-figure-6a-stocks-together-1",output, "small", plot = p6a)

p6b<-ggplot(data=data_monthly %>% select(date, PctRetMSFT, PctRetSP500) %>%  
         gather(key = "index", value = "pct_return", -date))+ 
  geom_line(aes(x=date,y = pct_return,color=index),size = 0.5)+
  scale_color_manual(name = "", values=c(color[1], color[2], color[3]), 
                     labels = c("MSFT", "S&P500")) +
  labs(x = 'Date (month)',y = "Monthly returns (percent)")+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(-12,12), breaks = seq(-12,12,4)) +  
  scale_x_date(breaks = as.Date(c("2017-01-01","2017-07-01","2018-01-01","2018-07-01","2019-01-01")),
               limits = as.Date(c("2017-01-01","2019-02-01")), labels = date_format("%b%Y"),
               minor_breaks = "1 month") +
  theme_bg() +
  theme(legend.position=c(0.65,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
p6b
#save_fig("ch12-stocks-together-2_R","small", plot=p8)
save_fig("ch12-figure-6b-stocks-together-2",output, "small", plot=p6b)




##############################   
#   Regressions
##############################   
# NB  we shall use newey west SE! (but in textbook at this point have not covered yet)
reg1 <- lm_robust(PctRetMSFT ~ PctRetSP500, data=data_monthly, se_type = "HC1")

reg2 <- lm_robust(d.lnp_MSFT ~ d.lnp_SP500, data=data_monthly, se_type = "HC1")

reg3 <- lm_robust(PctRetMSFT ~ PctRetSP500, data=data_daily, se_type = "HC1")

# -> combine in single table
huxreg(PctRetMSFT=reg1,d.lnp_MSFT=reg2,PctRetMSFT=reg3,
       statistics = c(N = "nobs", R2 = "r.squared"),
       stars = c(`**` = 0.01, `*` = 0.05))
# 
# 
# 

# CANDLESTICK GRAPH (not in textbook, but cool)
p_candle <- stock_data %>%
  filter (ticker == "MSFT") %>%
  mutate (date = as.Date (ref.date)) %>%
  filter (date > "2007-12-27" & date < "2012-01-01") %>%
  group_by (date=substr(ref.date,1,7)) %>%
  mutate (Open = first (price.adjusted, order_by = date),
          Close = last (price.adjusted, order_by = date)) %>%
  summarize (Open = mean(Open), 
             Close = mean(Close), 
             High = max(price.adjusted),
             Low = min (price.adjusted)) %>%
  plot_ly(x = ~date, open = ~Open, high = ~High, low = ~Low, close = ~Close, type = "candlestick", color=color[1]) %>%
  layout(title = "Microsoft monthly aggregated adjusted closing price",
         xaxis = list(rangeslider = list(visible = F)))
p_candle
htmlwidgets::saveWidget(as_widget(g02), paste0(output,"ch12_g02.html"))
dev.off()

