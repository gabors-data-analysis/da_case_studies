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
    avgpop = mean(pop), #for weights in xtreg fe
    year = factor(year)
  ) %>%
  ungroup()



# *****************************************************
# * FE REGRESSSIONS

fe_lm <- lm_robust(surv ~ imm + year,
                data = data_balanced, 
                weights = avgpop, 
                se_type = "stata", 
                fixed_effect =  ~ c ,
                clusters = c)

fe_lm2 <- lm_robust(surv ~ imm + lngdppc + lnpop + year,
                data = data_balanced, 
                weights = avgpop, 
                se_type = "stata", 
                fixed_effect =  ~ c ,
                clusters = c)


# ch23-table-2-immun-fe
huxreg(fe_lm, fe_lm2, 
  statistics = c(N = "nobs", R2 = "r.squared"), 
  coefs = c("imm", "lngdppc", "lnpop"))

# *************************
# ** CLUSTER SE VS BIASED SE 

fe_lm3 <- lm_robust(surv ~ imm + lngdppc + lnpop + year,
                data = data_balanced, 
                weights = avgpop, 
                se_type = "stata", 
                fixed_effect =  ~ c )

# ch23-table-3-immun-fese
huxreg(list("Clustered SE" = fe_lm2, "Simple SE" = fe_lm3), 
  statistics = c(N = "nobs", R2 = "r.squared"), 
  coefs = c("imm", "lngdppc", "lnpop"))

# *************************
# * FD REGRESSIONS 

# * basic FD 
fd_lm <- lm_robust(d_surv ~ d_imm,
                data = data_balanced, 
                weights = pop,
                se_type = "stata", 
                clusters = c)
 
# * FD, 5 lags
lags_helper <- paste(paste0("lag(d_imm,", c(0:5), ")"), collapse = " + ")
fd_lm_5_formula <- as.formula(paste0("d_surv ~ ", lags_helper))

fd_lm_5 <- lm_robust(fd_lm_5_formula,
                data = data_balanced, 
                weights = pop,
                se_type = "stata", 
                clusters = c
              )


# * FD, 5 lags, cumul
lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
fd_lm_5_cumul_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper))
fd_lm_5_cumul <- lm_robust(fd_lm_5_cumul_formula,
                data = data_balanced, 
                weights = pop,
                se_type = "stata", 
                clusters = c
              )

# * FD, 5 lags, cumul, lead
lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
lead_helper <- paste(paste0("lead(d_imm,", c(1:3), ")"), collapse = " + ")

fd_lm_5_cumul_lead_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper, " + ", lead_helper))
fd_lm_5_cumul_lead <- lm_robust(fd_lm_5_cumul_lead_formula,
                data = data_balanced, 
                weights = pop,
                se_type = "stata", 
                clusters = c
              )

# h23-table-4-immun-fd1
huxreg(fd_lm, fd_lm_5, fd_lm_5_cumul, fd_lm_5_cumul_lead,
  statistics = c(N = "nobs", R2 = "r.squared")
)

# *************************
# * AGGREG TREND, CONFOUNDERS, CTRY TRENDS
# * FD, 5 lags, cumul, aggreg trend

lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
fd_lm_5_cumul_trend_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper, "+ year"))
fd_lm_5_cumul_trend <- lm_robust(fd_lm_5_cumul_trend_formula,
                data = data_balanced, 
                weights = pop,
                se_type = "stata", 
                clusters = c
              ) 

# * FD, 5 lags, cumul, aggreg trend, confounders 
lags_helper <- paste(paste0("lag(d2_imm, ", c(0:4), ")"), collapse = " + ")
lags_helper2 <- paste(paste0("lag(d_lngdppc, ", c(0:5), ")"), collapse = " + ")
lags_helper3 <- paste(paste0("lag(d_lnpop, ", c(0:5), ")"), collapse = " + ")

fd_lm_5_cumul_trend_c_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", 
  lags_helper, "+",
  lags_helper2, "+",
  lags_helper3, "+",
  "+ year"))
fd_lm_5_cumul_trend_c <- lm_robust(fd_lm_5_cumul_trend_c_formula,
                data = data_balanced, 
                weights = pop,
                se_type = "stata", 
                clusters = c
              ) 
 
# * check: cumulative coeffs on the confounders
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper2," =0"))
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper3," =0"))

# * check: it's not the number of obsrevations
data_balanced_filtered <- data_balanced %>%
  filter(!is.na(d_lngdppc))
fd_lm_5_cumul_trend2 <- lm_robust(formula = fd_lm_5_cumul_trend_formula,
                data = data_balanced_filtered, 
                weights = pop,
                se_type = "stata", 
                clusters = c
              )

# * FD, 5 lags, cumul, aggreg trend, cofounders, country linear trend
fd_lm_5_cumul_trend_c_country_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", 
  lags_helper, "+",
  lags_helper2, "+",
  lags_helper3, "+",
  "+ year + c"))

fd_lm_5_cumul_trend_c_country <- lm_robust(fd_lm_5_cumul_trend_c_country_formula,
                data = data_balanced, 
                weights = pop,
                se_type = "stata", 
                clusters = c
              ) 

# ch23-table-5-immun-fd2
huxreg(fd_lm_5_cumul_trend, fd_lm_5_cumul_trend_c, fd_lm_5_cumul_trend_c_country,
  statistics = c(N = "nobs", R2 = "r.squared"), 
  omit_coefs = c(paste("year", levels(data_balanced$year), sep= ""), paste("c", levels(data_balanced$c), sep= ""))
)



 

