# how it's done log

# not to share

basic_log <- c("ln_accommodates", "ln_beds", "f_property_type", "f_room_type","ln_days_since", "flag_days_since")
poly_log <- c("ln_accommodates2","ln_days_since2","ln_days_since3")

# Create models in logs, models: 1-8
modellog1 <- " ~ ln_accommodates"
modellog2 <- paste0(" ~ ",paste(basic_log,collapse = " + "))
modellog3 <- paste0(" ~ ",paste(c(basic_log, basic_add),collapse = " + "))
modellog4 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log),collapse = " + "))
modellog5 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1),collapse = " + "))
modellog6 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2),collapse = " + "))
modellog7 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities),collapse = " + "))
modellog8 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities,X3),collapse = " + "))


model7_log <- model_results_cv[["modellog7"]][["model_work_data"]]

# FIXME: a problem
# Logged target variable
# model log 7 on log price
Ylog <- data_holdout[["ln_price"]]
#predictionlog_test <- predict(model7_log, newdata = data_holdout)
#predictionlog_test2 <- exp(predictionlog_test) * exp((rmselog)^2/2)
#d <- data.frame(ylev=Ylev, ylog=Ylog, predlev=predictionlev_holdout[,"fit"] , predlog=predictionlog_test2)
# repeat for log (not in book)
model_result_plot_logs <- ggplot(data = t1_logs, aes(x = factor(nvars), y = value, color=var, group = var)) +
  geom_line(color=c(color[1],color[2],color[3])) +
  scale_y_continuous(
    name = "RMSE",
    sec.axis = sec_axis(~ . * 100 , name = "BIC"),
    limits = c(10, 45)) +
  scale_x_discrete( name = "Number of vars", expand=c(0, 1)) +
  geom_dl(aes(label = var), method = list("top.points", cex=0.7)) +
  scale_colour_discrete(guide = 'none') +
  theme_bg()
