# Calibration curve -----------------------------------------------------------
# how well do estimated vs actual event probabilities relate to each other?

# backend notes
# need ungroup()  as select remembers groups https://github.com/tidyverse/dplyr/issues/1511



### we should do a fn here

calib_fn(share, stayshealthy, pred_lpm )

calib_fn <- function (data, actual, predicted) {
  
  
}


actual_vs_predicted <- share %>%
  ungroup() %>% 
  dplyr::select(actual = stayshealthy, 
                predicted = pred_logit) 

num_groups <- 10

calibration_d <- actual_vs_predicted %>%
  mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
  group_by(predicted_score_group) %>%
  dplyr::summarise(mean_actual = mean(actual), mean_predicted = mean(predicted), num_obs = n())

g_calib <- ggplot(calibration_d,aes(x = mean_actual, y = mean_predicted)) +
  geom_point(color=color[1], size=1.5, alpha=0.8) +
  geom_line(color=color[1], size=1, alpha=0.8) +
  geom_abline(intercept = 0, slope = 1, color=color[2]) +
  labs(x = "Actual event probability", y = "Predicted event probability") +
  #  ylim(0, 1) + xlim(0, 1) +
  theme_bg()+
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  background_grid(major = "xy", minor = "none", size.major = 0.2) 
g_calib
save_fig("calib_logit", output, "small")
