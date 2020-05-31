# ***************************************************************
# * Airline merger
# * Ch 22
# * ANALYSIS
# * Examining pre-treatment trends in avg ln price
# ***************************************************************


# Clear memory
rm(list=ls())

source("global.R")

use_case_dir <- file.path("ch22-airline-merger-prices/")
loadLibraries(use_case_dir)

data_in <- paste(data_dir,"airline-tickets-usa","clean", sep = "/")

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

# workfile to identify treated and untreated markets
data_work <- read_rds(paste0(data_out,"ch22-airline-workfile.rds")) %>%
	filter(balanced==1 & year == 2011) %>%
	arrange(market, year) %>%
	select(origin, finaldest, return, treated, smallmkt)

# * use year-quarter panel data 
# *  and merge to it treated-untreated 
# *	(keep matched ones; no unmatched from "using")
data <- read_dta(paste(data_in, "originfinal-panel.dta", sep="/"))
data <- merge(data, data_work, by = c("origin", "finaldest", "return"))


# * aggreagete data to create average price by treated-untreated and year-quarter
# * and draw time series graphs of log avg price
# * all markets
data_agg <- data %>%
	group_by(treated, year, quarter) %>%
	summarise(avgprice = weighted.mean(avgprice, passengers))  %>%
	ungroup()

data_agg <- data_agg %>%
	mutate(
		date = as.yearqtr(paste(year, quarter, sep="-")),
		lnavgprice = log(avgprice)
	)


ggplot(data_agg, aes(x = date, y = lnavgprice, color = factor(treated))) +
  geom_line(data = filter(data_agg, treated==1), size = 1.3) +
  geom_line(data = filter(data_agg, treated==0), size = 1.3) +
  annotate("text", x = as.yearqtr("2013-1"), y = 5.14, label = "Treated markets", size=3, color = color[2]) + 
  annotate("text", x = as.yearqtr("2013-1"), y = 5.46, label = "Unreated markets", size=3, color = color[1]) +
  geom_vline(xintercept = as.yearqtr("2012-1"), color = color[3], size = 0.9, linetype="longdash")+
  geom_vline(xintercept = as.yearqtr("2015-3"), color = color[3], size = 0.9, linetype="longdash") +
  annotate("text", x = as.yearqtr("2011-1"), y = 5.57, label = "Announcement", size=2.5, color = color[3]) + 
  annotate("text", x = as.yearqtr("2014-3"), y = 5.58, label = "Merger happens", size=2.5, color = color[3]) +
  scale_y_continuous(limits = c(5, 5.6), breaks = seq(5, 5.6, 0.1)) +
  scale_color_manual(values=color[1:2], name="") +
  labs(y = "ln(average price)", x="") +
  scale_x_yearqtr(format = "%YQ%q") +
  theme_bg() +
  theme(axis.text.x=element_text(size=9)) +
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.title.x=element_text(size=9)) +
  theme(axis.title.y=element_text(size=9)) +
  theme(legend.position="none")
save_fig("ch22-figure-2-pretrends-all", output, size = "large")

# small markets
data_agg <- data %>%
	filter(smallmkt==1) %>%
	group_by(treated, year, quarter) %>%
	summarise(avgprice = weighted.mean(avgprice, passengers))  %>%
	ungroup()

data_agg <- data_agg %>%
	mutate(
		date = as.yearqtr(paste(year, quarter, sep="-")),
		lnavgprice = log(avgprice)
	)

ggplot(data_agg, aes(x = date, y = lnavgprice, color = factor(treated))) +
  geom_line(data = filter(data_agg, treated==1),  size = 0.7) +
  geom_line(data = filter(data_agg, treated==0), size = 0.7) +
  annotate("text", x = as.yearqtr("2013-1"), y = 5.59, label = "Unreated markets", size=2, color = color[1]) +
  annotate("text", x = as.yearqtr("2013-1"), y = 5.49, label = "Treated markets", size=2, color = color[2]) +
  geom_vline(xintercept = as.yearqtr("2012-1"), color = color[3], size = 0.6, linetype="longdash")+
  geom_vline(xintercept = as.yearqtr("2015-3"), color = color[3], size = 0.6, linetype="longdash") +
  scale_color_manual(values=color[1:2], labels = c("Untreated markets", "Treated markets"), name="") +
  labs(y = "ln(average price)", x="") +
  scale_x_yearqtr(format = "%YQ%q") +
  theme_bg() +
  theme(legend.position="none")
save_fig("ch22-figure-3a-pretrends-small", output, size = "small")

 
# large markets
data_agg <- data %>%
	filter(smallmkt==0) %>%
	group_by(treated, year, quarter) %>%
	summarise(avgprice = weighted.mean(avgprice, passengers))  %>%
	ungroup()

data_agg <- data_agg %>%
	mutate(
		date = as.yearqtr(paste(year, quarter, sep="-")),
		lnavgprice = log(avgprice)
	)

ggplot(data_agg, aes(x = date, y = lnavgprice, color = factor(treated))) +
  geom_line(data = filter(data_agg, treated==1),  size = 0.7) +
  geom_line(data = filter(data_agg, treated==0), size = 0.7) +
  annotate("text", x = as.yearqtr("2013-1"), y = 4.3, label = "Unreated markets", size=2, color = color[1]) +
  annotate("text", x = as.yearqtr("2013-1"), y = 4.9, label = "Treated markets", size=2, color = color[2]) +
  geom_vline(xintercept = as.yearqtr("2012-1"), color = color[3], size = 0.6, linetype="longdash")+
  geom_vline(xintercept = as.yearqtr("2015-3"), color = color[3], size = 0.6, linetype="longdash") +
  scale_color_manual(values=color[1:2], labels = c("Untreated markets", "Treated markets"), name="") +
  labs(y = "ln(average price)", x="") +
  scale_x_yearqtr(format = "%YQ%q") +
  theme_bg() +
  theme(legend.position="none")
save_fig("ch22-figure-3b-pretrends-large", output, size = "small")

