# ***************************************************************
# * Airline merger
# * Ch 22
# v 2.1 2020-04-19
# v 2.2 2020-04-20 graph edits
# v 2.3 2020-04-22 names ok
# v 2.4 2020-04-27 adding shares w csv from stata
# v 2.4 2020-04-30 label edits

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

# CREATE Workfile : only before and after period

data <- read_dta(file.path(data_in, "originfinal-panel.dta"))

# ***************************************************************************
# *   market = origin X final destination 
# *  (note final destination is:
# * airport at end of one-way routes if 4 or fewer
# * airport in middle of return routes if there is middle & 9 or fewer



# * before = 2011 (all year)
# * after  = 2016 (all year)
# * workfile 1: drop all other years
data <- data %>%
  filter(year==2011 | year==2016)

# * create total number of passengers from shares 
# * so we can get aggreagate shares
data_agg <- data %>%
  mutate(
    ptotalAA = shareAA*passengers,
    ptotalUS = shareUS*passengers,
    ptotallargest = sharelargest*passengers
  ) %>%
  group_by(origin, finaldest, return, year) %>%
  summarise(
    airports = first(airports), 
    return_sym = first(return_sym), 
    stops = first(stops),
    ptotalAA = sum(ptotalAA), 
    ptotalUS = sum(ptotalUS),
    ptotallargest = sum(ptotallargest),
    passengers = sum(passengers),
    itinfare = sum(itinfare)
  ) %>%
  ungroup()

data_agg <- data_agg %>%
  mutate(
    after = as.numeric(year == 2016), 
    before = as.numeric(year == 2011),
    avgprice = itinfare/passengers,
    shareAA = ptotalAA/passengers,
    shareUS = ptotalUS/passengers,
    sharelargest = ptotallargest/passengers,
    AA = as.numeric(shareAA > 0), #share variables never missing
    US = as.numeric(shareUS > 0),
    AA_and_US = as.numeric(shareAA > 0 & shareUS > 0),
    AA_or_US = as.numeric(shareAA > 0 | shareUS > 0)
  )

# create numeric ID for market
data_agg <- data_agg %>%
  arrange(origin, finaldest) %>%
  mutate(market = factor(paste(origin, finaldest, return, sep = "_")))

# tell Stata it's xt data with time difference of 5 yearas
# local d = 2016-2011
# xtset market year, delta(`d')
# xtdes

# passengers before and after
data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    pass_bef = mean(ifelse(before == 1, passengers, NA), na.rm = TRUE), 
    pass_aft = mean(ifelse(after == 1, passengers, NA), na.rm = TRUE)
  ) %>%
  ungroup()


# * balanced vs unbalanced part of panel
data_agg <- data_agg %>%
  group_by(market) %>%
  mutate(balanced = as.numeric(n() == 2)) %>%
  ungroup()

data_agg %>%
  group_by(balanced) %>%
  summarise(sum(passengers), n())

# Define treated and untreated markets
# treated: both AA and US present in the before period
# untreated: neither AA nor US present in the before period
# drop if only AA or only US in before period (neither treated nor untreated)

data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    treated = max(as.numeric(AA_and_US == 1 & before == 1)), 
    untreated = max(as.numeric(AA_or_US == 0 & before == 1)),
    smallmkt = max(as.numeric(passengers < 5000 & before == 1))
  ) %>%
  ungroup()

data_agg <- data_agg %>%
  mutate(lnavgp = ifelse(is.infinite(log(avgprice)), NA, log(avgprice))) %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(d_lnavgp = lnavgp - lag(lnavgp)) %>%
  ungroup()


# save "$data_out/ch22-airline-workfile.dta" ,replace

# **************************************************************************
# * DESCRIBE
# * and create d_ln(y)
# **************************************************************************

# use "$data_out/ch22-airline-workfile.dta" ,replace

# describe yearly data
data_agg %>%
  select(year, passengers) %>%
  group_by(year) %>%
  summarise_each(funs(N = length, 
                      q25 = quantile(., 0.25), 
                      median = median, 
                      q75 = quantile(., 0.75), 
                      q90 = quantile(., 0.90),
                      mean = mean, 
                      sum = sum)
                    )

data_agg %>%
  filter(origin=="JFK" & finaldest=="LAX") %>%
  select(market, origin, finaldest, return, year, passengers)

data_agg %>%
  filter(year == 2011) %>%
  select(smallmkt, passengers) %>%
  group_by(smallmkt) %>%
  summarise_each(funs(N = length, 
                      min = min, 
                      max = max,
                      median = median,
                      mean = mean, 
                      sum = sum))

# describe balanced
data_agg %>%
  group_by(year, balanced) %>%
  summarise(n(), sum(passengers), mean(passengers))


# describe treatment
data_agg %>%
  group_by(year, treated, untreated) %>%
  summarise(n(), sum(passengers), mean(passengers))

# describe outcome
data_agg %>%
  filter(before == 1) %>%
  select(avgprice) %>%
  summarise_all(funs(N = length, 
                      min = min, 
                      max = max,
                      median = median,
                      mean = mean, 
                      sum = sum))

data_agg %>%
  filter(avgprice==0) %>%
  select(passengers) %>%
  summarise_all(funs(N = length,
                      mean = mean, 
                      sum = sum))


# **************************************************************************
# * ANALYSIS
# * Basic diff-in-diffs regtrssion
# *  weighted by # passengers on market, in before period
# **************************************************************************

# keep balanced
data_balanced <- data_agg %>%
  filter(balanced == 1) 

fd <- lm(d_lnavgp ~ treated, weights = data_balanced$pass_bef, data = data_balanced)
fd_small <- lm(d_lnavgp ~ treated, weights = filter(data_balanced, smallmkt==1)$pass_bef, data = filter(data_balanced, smallmkt==1))
fd_large <- lm(d_lnavgp ~ treated, weights = filter(data_balanced, smallmkt==0)$pass_bef, data = filter(data_balanced, smallmkt==0))


summary(fd)
summary(fd_small)
summary(fd_large)

stargazer_r(list(fd, fd_small, fd_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=file.path(output,"airlines-reg1.tex")
)

# Corresponding diff-in-diffs table
data_balanced %>%
  group_by(after, treated) %>%
  summarise(weighted.mean(lnavgp, pass_bef, na.rm = TRUE), n())

# **************************************************************************
# * ANALYSIS
# * Examining pre-treatment trends in avg ln price
# **************************************************************************

# workfile to identify treated and untreated markets

data_helper <- read_dta(paste0(data_out, "ch22-airline-workfile.dta")) %>%
	filter(balanced==1 & year == 2011) %>%
	arrange(market, year) %>%
	select(origin, finaldest, return, treated, smallmkt)

# * use year-quarter panel data 
# *  and merge to it treated-untreated 
# *	(keep matched ones; no unmatched from "using")

data <- read_dta(paste0(data_in, "originfinal-panel.dta"))
data <- merge(data, data_helper, by = c("origin", "finaldest", "return"))

# gen yq=yq(year,quarter)
# format yq %tq

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


p1<-ggplot(data_agg, aes(x = date, y = lnavgprice, color = factor(treated))) +
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
#save_fig("pretrends-all", output, size = "small")
p1
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
#save_fig("pretrends-small", output, size = "small")
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
#save_fig("pretrends-large", output, size = "small")
save_fig("ch22-figure-3b-pretrends-large", output, size = "small")

#ch22-table-2-airlines-reg1
#ch22-table-3-airlines-tab
#ch22-table-4-airlines-reg2
#ch22-table-5-airlines-reg3
#ch22-table-6-airlines-reg4

#########################################
# create graph 4 to be replaced
shares <- read_csv(paste0(data_out,"sharehistogram.csv"))
#stata: hist share_bef  [w=pass_bef], bin(20) percent 

#share_bef - this is the variable the distribution of which we need
#pass_bef - this variable is used for weighting

sh<- ggplot(data=shares, aes(x=share_bef,  y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.05, boundary=0, aes(weight = pass_bef),
                 color = color.outline, fill = color[1], alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "Market share of AA and US combined, at baseline", y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0,0.5), breaks = seq(0, 0.50, by = 0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_bg()
sh
save_fig("ch22-figure-4-airlines-sharehist", output, size = "small")


# **************************************************************************
# * ANALYSIS
# * Diff-in-diffs regerssion with confounder variables
# *  weighted by # passengers on market, in before period
# **************************************************************************

data_agg <- read_dta(paste0(data_out, "ch22-airline-workfile.dta"))

data_balanced <- data_agg %>%
  filter(balanced==1) %>%
  arrange(market, year) 

# potential confouders: # passengers before, share of largest carrier before
data_balanced <- data_balanced %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    lnpass_bef = mean(ifelse(before == 1, log(passengers), NA), na.rm = TRUE), 
    sharelarge_bef = mean(ifelse(before == 1, sharelargest, NA), na.rm = TRUE)
  ) %>%
  ungroup()

formula2 <- as.formula(d_lnavgp ~ treated + lnpass_bef + return + stops + sharelarge_bef)
fd2 <- lm(formula2, weights = data_balanced$pass_bef, data = data_balanced)
fd2_small <- lm(formula2, weights = filter(data_balanced, smallmkt==1)$pass_bef, data = filter(data_balanced, smallmkt==1))
fd2_large <- lm(formula2, weights = filter(data_balanced, smallmkt==0)$pass_bef, data = filter(data_balanced, smallmkt==0))

summary(fd2)
summary(fd2_small)
summary(fd2_large)

stargazer_r(list(fd2, fd2_small, fd2_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=file.path(output,"airlines-reg2.tex")
)

# **************************************************************************
# * ANALYSIS
# * Diff-in-diffs regerssion with quantitative treatment
# *  weighted by # passengers on market, in before period
# **************************************************************************


data_balanced <- data_balanced %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    share_bef = mean(ifelse(before == 1, shareAA + shareUS, NA), na.rm = TRUE), 
    sharelarge_bef = mean(ifelse(before == 1, sharelargest, NA), na.rm = TRUE)
  ) %>%
  ungroup()

data_balanced %>%
  group_by(share_bef==0, share_bef==1) %>%
  summarise(sum(passengers), mean(passengers), n())

ggplot(data_balanced, aes(x = share_bef, y = ..density.., weight = pass_bef)) + 
  geom_histogram(bins = 20) +
  theme_bg()

# outsheet market year share_bef pass_bef using "$data_out/sharehistogram.csv",comma replace

formula3 <- as.formula(d_lnavgp ~ share_bef + lnpass_bef + return + stops + sharelarge_bef)
fd3 <- lm(formula3, weights = data_balanced$pass_bef, data = data_balanced)
fd3_small <- lm(formula3, weights = filter(data_balanced, smallmkt==1)$pass_bef, data = filter(data_balanced, smallmkt==1))
fd3_large <- lm(formula3, weights = filter(data_balanced, smallmkt==0)$pass_bef, data = filter(data_balanced, smallmkt==0))

summary(fd3)
summary(fd3_small)
summary(fd3_large)

stargazer_r(list(fd3, fd3_small, fd3_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=file.path(output,"airlines-reg3.tex")
)

# **************************************************************************
# * ANALYSIS
# * Diff-in-diffs on pooled cross-sections regeression 
# * use entire unbalanced panel 
# *   - errr... after only is dropped here see later
# *  weighted by # passengers on market, in before period
# **************************************************************************

data_agg <- read_dta(paste0(data_out, "ch22-airline-workfile.dta"))

data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    lnpass_bef = mean(ifelse(before == 1, log(passengers), NA), na.rm = TRUE), 
    sharelarge_bef = mean(ifelse(before == 1, sharelargest, NA), na.rm = TRUE)
  ) %>%
  ungroup()

data_agg %>%
  group_by(balanced, before) %>%
  summarise(sum(passengers), n())


# treatment group defined if observed before only or both before and after
data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    treatment = mean(ifelse(before == 1, AA_and_US, NA), na.rm = TRUE)
  ) %>%
  ungroup()


data_agg %>%
  group_by(is.na(treatment), balanced) %>%
  summarise(n(), sum(passengers))


# conditioning on observed confounders
formula4 <- as.formula(lnavgp ~ (treatment + lnpass_bef + return + stops + sharelarge_bef)*after )
fd4 <- lm(formula4, weights = data_agg$pass_bef, data = data_agg)
fd4_small <- lm(formula4, weights = filter(data_agg, smallmkt==1)$pass_bef, data = filter(data_agg, smallmkt==1))
fd4_large <- lm(formula4, weights = filter(data_agg, smallmkt==0)$pass_bef, data = filter(data_agg, smallmkt==0))

summary(fd4)
summary(fd4_small)
summary(fd4_large)

stargazer_r(list(fd4, fd4_small, fd4_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=file.path(output,"airlines-reg4.tex")
)
