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

source("global.R")

use_case_dir <- file.path("ch23-immunization-life/")
loadLibraries(use_case_dir)

data_in <- paste(data_dir,"world-bank-immunization","clean", sep = "/")

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

# Read in data ------------------------------------------------------------

data <- read_dta(paste(data_in, "world-bank_immunization-continents.dta", sep = "/"))

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

# **************************************************
# * regressions on countries

data_panel <- read_dta(paste(data_in, "world-bank_immunization-panel.dta", sep = "/"))

data_panel <- data_panel %>%
  filter(!(is.na(imm) | is.na(gdppc))) %>%
  mutate(c = factor(c)) %>%
  group_by(c) %>%
  mutate(balanced = min(year) == 1998 & max(year) == 2017 & length(unique(year)) == 20) %>%
  ungroup() 

data_balanced <- data_panel %>%
  filter(balanced == TRUE)

data_balanced <- data_balanced %>%
  arrange(c, year) %>%
  group_by(c) %>%
  mutate(
    lnpop=log(pop),
    d_surv = surv- lag(surv),
    d_imm = imm - lag(imm),
    d2_imm = d_imm - lag(d_imm), 
    d_lngdppc= lngdppc- lag(lngdppc),
    d_lnpop = lnpop - lag(lnpop),
    avgpop = mean(pop) #for weights in xtreg fe
  ) %>%
  ungroup()

# *****************************************************
# * FE REGRESSSIONS

fe_lm <- lm(surv ~ imm + c + factor(year),
                data = data_balanced, 
                weights = data_balanced$avgpop
              )
summary(fe_lm)$coefficients[c('imm'),]

fe_lm2 <- plm(surv ~ imm + lngdppc + lnpop + c + factor(year),
                data = data_balanced, 
                weights = data_balanced$avgpop)
summary(fe_lm2)

# *************************
# ** CLUSTER SE VS BIASED SE 

coeftest(fe_lm2)[c('imm'),]
## robust significance test, cluster by group
## (robust vs. serial correlation)
coeftest(fe_lm2, vcov.=vcovHC)[c('imm'),]
# coeftest(zz, vcov.=function(x) vcovHC(x, method="arellano", type="HC1"))

# *************************
# * FD REGRESSIONS - clustered se?

local maxlag = 5
local maxlag_1 = maxlag-1

# * basic FD 
fD_lm <- lm(d_surv ~ d_imm,
                data = data_balanced, 
                weights = data_balanced$pop
              )
coeftest(fD_lm)[c('d_imm'),]
 
# * FD, 5 lags
lags_helper <- paste(paste0("lag(d_imm,", c(0:5), ")"), collapse = " + ")
fd_lm_5_formula <- as.formula(paste0("d_surv ~ ", lags_helper))

fd_lm_5 <- lm(fd_lm_5_formula,
                data = data_balanced, 
                weights = data_balanced$pop
              )


# * FD, 5 lags, cumul
lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
fd_lm_5_cumul_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper))
fd_lm_5_cumul <- lm(fd_lm_5_cumul_formula,
                data = data_balanced, 
                weights = data_balanced$pop
              )

# * FD, 5 lags, cumul, lead
lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
lead_helper <- paste(paste0("lead(d_imm,", c(1:3), ")"), collapse = " + ")

fd_lm_5_cumul_lead_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper, " + ", lead_helper))
fd_lm_5_cumul_lead <- lm(fd_lm_5_cumul_lead_formula,
                data = data_balanced, 
                weights = data_balanced$pop
              )

# *************************
# * AGGREG TREND, CONFOUNDERS, CTRY TRENDS
# * FD, 5 lags, cumul, aggreg trend
lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
fd_lm_5_cumul_trend_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper, "+ factor(year)"))
fd_lm_5_cumul_trend <- lm(fd_lm_5_cumul_trend_formula,
                data = data_balanced, 
                weights = data_balanced$pop
              ) 

# * FD, 5 lags, cumul, aggreg trend, confounders 
lags_helper <- paste(paste0("lag(d2_imm, ", c(0:4), ")"), collapse = " + ")
lags_helper2 <- paste(paste0("lag(d_lngdppc, ", c(0:5), ")"), collapse = " + ")
lags_helper3 <- paste(paste0("lag(d_lnpop, ", c(0:5), ")"), collapse = " + ")

fd_lm_5_cumul_trend_c_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", 
  lags_helper, "+",
  lags_helper2, "+",
  lags_helper3, "+",
  "+ factor(year)"))
fd_lm_5_cumul_trend_c <- lm(fd_lm_5_cumul_trend_c_formula,
                data = data_balanced, 
                weights = data_balanced$pop
              ) 
 
# * check: cumulative coeffs on the confounders
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper2," =0"))
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper3," =0"))

# * check: it's not the number of obsrevations
data_balanced_filtered <- data_balanced %>%
  filter(!is.na(d_lngdppc))
fd_lm_5_cumul_trend2 <- lm(formula = fd_lm_5_cumul_trend_formula,
                data = data_balanced_filtered, 
                weights = data_balanced_filtered$pop
              )

# * FD, 5 lags, cumul, aggreg trend, , country linear trend
# areg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm ///
#   L(0/`maxlag').d_lngdppc L(0/`maxlag').d_lnpop ///
#   i.year [w=pop], cluster(c) absorb(c)
 

