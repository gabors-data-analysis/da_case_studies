#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 16
# CH16A Predicting apartment prices with random forest
# using the airbnb dataset
# version 0.9 2020-09-09
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

library(xtable)
library(vip)
library(lime)
library(DALEX)
library(DALEXtra)
library(tidyverse)
library(caret)
library(ranger)
library(rattle)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xgboost)
library(doParallel)



# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies")

# set data dir, data used
source("ch00-tech-prep/set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/xy/Dropbox/gabors_data_analysis/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "ch16-airbnb-random-forest/"
data_in <- use_case_dir
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

# Parallel Processing Setup
n_cores = parallel::detectCores() - 1

cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

# Speed Flag: Set to TRUE for quick testing, FALSE for full analysis
FAST_RUN <- TRUE 


#-----------------------------------------------------------------------------------------

#########################################################################################
#
# PART I
# Loading and preparing data ----------------------------------------------
#
#########################################################################################

# !!! make sure you have run ch16-airbnb-prepare-london.R before

# Used area
area <- "london"
data <- read_csv(paste0(data_in, "airbnb_", area, "_workfile.csv")) %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(price))


count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}

count_missing_values(data)

# Sample definition and preparation ---------------------------------------

# We focus on normal apartments, n<8
data <- data %>% filter(n_accommodates < 8)



# basic descr stat -------------------------------------------
skimr::skim(data)
summary(data$price)
Hmisc::describe(data$price)
describe(data$f_room_type)
describe(data$f_property_type)
table(data$f_number_of_reviews)

# create train and holdout samples -------------------------------------------
# train is where we do it all, incl CV

set.seed(2801)

if (FAST_RUN) {
  set.seed(1234)
  try <- createDataPartition(data$price, p = 0.1, list = FALSE)
  data <- data[try, ]
}

train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

# Define models: simpler, extended -----------------------------------------------------------

# Basic Variables inc neighborhood
basic_vars <- c(
  "n_accommodates", "n_beds", "n_days_since",
  "f_property_type","f_room_type", "f_bathroom", "f_cancellation_policy", "f_bed_type",
  "f_neighbourhood_cleansed")

# reviews
reviews <- c("n_number_of_reviews", "flag_n_number_of_reviews" ,"n_review_scores_rating", "flag_review_scores_rating")

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

#interactions for the LASSO
# from ch14
X1  <- c("n_accommodates*f_property_type",  "f_room_type*f_property_type",  "f_room_type*d_familykidfriendly",
         "d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
# with boroughs
X2  <- c("f_property_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed" )


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1,X2)


#########################################################################################
#
# PART II
# RANDOM FORESTS -------------------------------------------------------
#
#########################################################################################



# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE,
                              allowParallel = TRUE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(5, 7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)


# simpler model for model A (1)
set.seed(1234)
system.time({
rf_model_1 <- train(
  formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity",
  num.threads = 1
)
})
rf_model_1

# set tuning for benchmark model (2)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
rf_model_2 <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity",
  num.threads = 1
)
})

rf_model_2


# evaluate random forests -------------------------------------------------

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  )
)
summary(results)

# Save outputs -------------------------------------------------------

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file= paste0(output,"rf_tuning_modelB.tex"))


# Turning parameter choice 1
result_1 <- matrix(c(
                     rf_model_1$finalModel$mtry,
                     rf_model_2$finalModel$mtry,
                     rf_model_1$finalModel$min.node.size,
                     rf_model_2$finalModel$min.node.size
                     ),
                    nrow=2, ncol=2,
                    dimnames = list(c("Model A", "Model B"),
                                    c("Min vars","Min nodes"))
                   )
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_turning_choices.tex"))

# Turning parameter choice 2
result_2 <- matrix(
  c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`)),
  nrow=2, ncol=1,
  dimnames = list(c("Model A", "Model B"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_rmse.tex"))


#########################################################################################
#
# PART III
# MODEL DIAGNOSTICS -------------------------------------------------------
#
#########################################################################################


#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
calculate_grouped_importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(rf.obj$variable.importance[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10


rf_model_2_var_imp <- rf_model_2$finalModel$variable.importance/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


##############################
# 1) full varimp plot, above a cutoff
##############################

# to have a quick look
plot(varImp(rf_model_2))

cutoff = 600
rf_model_2_var_imp_plot <- ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot
#save_fig("rf_varimp1",output, "large")
save_fig("ch16-figure-1-rf-varimp-base",output, "large")

##############################
# 2) full varimp plot, top 10 only
##############################


# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot_b
#save_fig("rf_varimp1_b",output, "small")
save_fig("ch16-figure-2b-rf-varimp-top10",output, "small")


##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model_2$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_cancellation_policy_varnames <- grep("f_cancellation_policy",varnames, value = TRUE)
f_bed_type_varnames <- grep("f_bed_type",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_cancellation_policy = f_cancellation_policy_varnames,
               f_bed_type = f_bed_type_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = "f_bathroom",
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds")

rf_model_2_var_imp_grouped <- calculate_grouped_importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
theme_bw() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_grouped_plot
#save_fig("rf_varimp_grouped1",output, "small")
save_fig("ch16-figure-2a-rf-varimp-group",output, "small")

# First we did this by summing up the individual importances of factors - this is not correct, but it's in the first edition.
# Simply summing up the individual importances of each dummy could underestimate the importance of the qualitative variable. 
# To address this issue, we can employ a model-agnostic feature importance technique. 
# One such technique is permutation feature importance, which randomly shuffles the values of variables and measures how much the fit of the prediction is decreased.

# Permutation Importance
rf_model_2_var_imp_perm <- vip(rf_model_2, method = "permute", 
                               target = "price", metric = "rmse",
                               geom = "point",
                               pred_wrapper = predict, 
                               nsim = 5,
                               train = data_train,
                               parallel = TRUE)

rf_model_2_var_imp_perm_plot <- rf_model_2_var_imp_perm +
  geom_segment(aes(x = Variable, xend = Variable, y = 0, yend = Importance), 
               color = color[1], size = 0.7) +
  geom_point(color = color[1], size = 1.5) +
  ggtitle("Top 10 most important variable calculated with permutation importance") +
  theme_bw()

rf_model_2_var_imp_perm_plot
save_fig("ch16-figure-2c-rf-varimp-perm", output, "small")

# ------------------------------------------------------------------------------



#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################

pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  geom_line(color=color[1], size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
theme_bw()
pdp_n_acc_plot
#save_fig("rf_pdp_n_accom", output, "small")
save_fig("ch16-figure-3a-rf-pdp-n-accom", output, "small")

# Partial Dependence Plot with ICE curves
pdp_n_acc_ice <- pdp::partial(rf_model_2, pred.var = "n_accommodates", 
                               pred.grid = distinct_(data_holdout, "n_accommodates"), 
                               train = data_train, ice = TRUE)

pdp_n_acc_ice_plot <- pdp_n_acc_ice %>%
  autoplot(alpha = 0.05, color = color[2]) +
  geom_line(data = pdp_n_acc, aes(x = n_accommodates, y = yhat), color = color[1], size = 1) +
  geom_point(data = pdp_n_acc, aes(x = n_accommodates, y = yhat), color=color[1], size=2) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bw()
pdp_n_acc_ice_plot
save_fig("ch16-figure-3c-rf-pdp-n-accom-ice", output, "small")


pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
pdp_n_roomtype_plot <- pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  ylab("Predicted price") +
  xlab("Room type") +
  scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  theme_bw()
pdp_n_roomtype_plot
#save_fig("rf_pdp_roomtype", output, "small")
save_fig("ch16-figure-3b-rf-pdp-roomtype", output, "small")

pdp_n_roomtype_ice <- pdp::partial(rf_model_2, pred.var = "f_room_type", 
                                    pred.grid = distinct_(data_holdout, "f_room_type"), 
                                    train = data_train, ice = TRUE)

pdp_n_roomtype_ice_plot <- ggplot(pdp_n_roomtype_ice, aes(x = f_room_type, y = yhat, group = yhat.id)) +
  geom_line(alpha = 0.05, color = color[2]) +
  geom_line(data = pdp_n_roomtype, aes(x = f_room_type, y = yhat, group = 1), color = color[1], size = 1) +
  geom_point(data = pdp_n_roomtype, aes(x = f_room_type, y = yhat, group = 1), color = color[1], size = 2) +
  ylab("Predicted price") +
  xlab("Room type") +
  theme_bw()
pdp_n_roomtype_ice_plot
save_fig("ch16-figure-3d-rf-pdp-roomtype-ice", output, "small")

# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))

describe(data_holdout_w_prediction$n_accommodates)


calculate_rmse <- function(data, group_var = NULL) {
  if (is.null(group_var)) {
    out <- data %>%
      summarise(
        rmse = RMSE(predicted_price, price),
        mean_price = mean(price),
        rmse_norm = rmse / mean_price
      )
  } else {
    out <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        rmse = RMSE(predicted_price, price),
        mean_price = mean(price),
        rmse_norm = rmse / mean_price
      )
  }
  return(out)
}

# Apartment size
sub_size <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  calculate_rmse("is_low_size")

# Borough
sub_borough <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c("Westminster", "Camden", "Kensington and Chelsea", "Tower Hamlets", "Hackney", "Newham")) %>%
  calculate_rmse("f_neighbourhood_cleansed")

# Property type
sub_prop <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Apartment", "House")) %>%
  calculate_rmse("f_property_type")

# All holdout
sub_all <- calculate_rmse(data_holdout_w_prediction) %>%
  mutate(all = "All") %>%
  select(all, everything())

# Assemble table
header_row <- function(text) {
  c(text, rep("", 3))
}

result_3 <- rbind(
  header_row("Apartment size"),
  as.matrix(sub_size),
  header_row("Type"),
  as.matrix(sub_prop),
  header_row("Borough"),
  as.matrix(sub_borough),
  as.matrix(sub_all)
) %>%
  as.data.frame() %>%
  mutate(across(c(rmse, mean_price, rmse_norm), as.numeric))

result_3

# Save output
options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs = TRUE, linesep = "", 
      digits = c(0, 2, 1, 2), col.names = c("", "RMSE", "Mean price", "RMSE/price")) %>%
  cat(., file = paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)

##########################################

##########################################
## LIME ##
##########################################

explainer_lime <- lime(data_train[predictors_2], rf_model_2)
explanation_lime <- lime::explain(data_holdout[3, predictors_2], explainer_lime, n_features =15)
plot_features(explanation_lime)
save_fig("ch16-figure-4-rf-lime", output, "small")

##########################################
## SHAP VALUES ##
##########################################

# Using DALEX for SHAP
explainer_rf <- DALEX::explain(rf_model_2, 
                        data = data_train[predictors_2], 
                        y = data_train$price,
                        label = "Random Forest",
                        colorize = FALSE)

# Local SHAP (Waterfall)
set.seed(1234)
bd_rf <- predict_parts(explainer_rf, 
                       new_observation = data_holdout[3, ], 
                       type = "shap")
plot(bd_rf)
save_fig("ch16-figure-5a-rf-shap-waterfall", output, "small")

# Global Importance (Beeswarm alternative in DALEX)
mp_rf <- model_parts(explainer_rf)
plot(mp_rf)
save_fig("ch16-figure-5b-rf-shap-global", output, "small")

#########################################################################################
#
# PART IV
# HORSERACE: compare with other models -----------------------------------------------
#
#########################################################################################



# OLS with dummies for area
# using model B

set.seed(1234)
system.time({
ols_model <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "lm",
  trControl = train_control
)
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

# * LASSO
# using extended model w interactions

set.seed(1234)
system.time({
lasso_model <- train(
  formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
  data = data_train,
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
  trControl = train_control
)
})

lasso_coeffs <- coef(
    lasso_model$finalModel,
    lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable")

colnames(lasso_coeffs) <- c("variable","lasso_coefficient") # the column has a name "1", to be renamed

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_non_null, by = "variable", all=TRUE)
regression_coeffs %>%
  write.csv(file = paste0(output, "regression_coeffs.csv"))

# CART
set.seed(1234)
system.time({
cart_model <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "rpart",
  tuneLength = 10,
  trControl = train_control
)
})

fancyRpartPlot(cart_model$finalModel, sub = "")

# GBM  -------------------------------------------------------
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = c(200, 400, 500), # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model


# much more tuning

# the next will be in final model, loads of tuning
if (FAST_RUN) {
  gbm_grid2 <- expand.grid(interaction.depth = c(1, 5, 9),
                           n.trees = c(100, 300, 500),
                           shrinkage = c(0.1, 0.2),
                           n.minobsinnode = c(10, 20))
} else {
  gbm_grid2 <-  expand.grid(interaction.depth = c(1, 3, 5, 7, 9, 11), # complexity of the tree
                            n.trees = (1:10)*50, # number of iterations, i.e. trees
                            shrinkage = c(0.02, 0.05, 0.1, 0.15, 0.2), # learning rate: how quickly the algorithm adapts
                            n.minobsinnode = c(5,10,20,30) # the minimum number of training set samples in a node to commence splitting
  )
}


set.seed(1234)
system.time({
  gbm_model2 <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                      data = data_train,
                      method = "gbm",
                      trControl = train_control,
                      verbose = FALSE,
                      tuneGrid = gbm_grid2)
})
gbm_model2


# XGBoost -------------------------------------------------------
# Note: xgboost 2.0+ has compatibility issues with caret's xgbTree.
# We implement native CV and training for better stability.

xgb_features <- model.matrix(price ~ . - 1, data = data_train[, c("price", predictors_2)])
dtrain <- xgb.DMatrix(data = xgb_features, label = data_train$price)

xgb_params <- list(
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.05,
  colsample_bytree = 0.8,
  subsample = 0.8
)

set.seed(1234)
xgb_cv <- xgb.cv(
  params = xgb_params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = 0
)

# Extract optimal results
eval_log <- xgb_cv$evaluation_log
test_rmse_col <- grep("test.*rmse.*mean", names(eval_log), value = TRUE)[1]
best_nrounds <- which.min(eval_log[[test_rmse_col]])
best_cv_rmse <- eval_log[[test_rmse_col]][best_nrounds]

xgb_model_native <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)

# Wrapper for horserace integration
xgb_predict_wrapper <- list(
  model = xgb_model_native,
  predictors = predictors_2,
  cv_rmse = best_cv_rmse
)

predict.xgb_wrapper <- function(object, newdata, ...) {
  xgb_newdata <- model.matrix(~ . - 1, data = newdata[, object$predictors])
  predict(object$model, newdata = xgb.DMatrix(data = xgb_newdata))
}
class(xgb_predict_wrapper) <- "xgb_wrapper"

cat("XGBoost training complete. Best nrounds:", best_nrounds, "CV RMSE:", round(best_cv_rmse, 3), "\n")


# and get prediction rmse and add to next summary table

# ---- compare these models

# Caret-based models (compatible with resamples())
caret_models <- list(
  "OLS" = ols_model,
  "LASSO (model w/ interactions)" = lasso_model,
  "CART" = cart_model,
  "Random forest (smaller model)" = rf_model_1,
  "Random forest" = rf_model_2,
  "GBM (basic tuning)"  = gbm_model,
  "GBM (broad tuning)" = gbm_model2
)

# Extract CV RMSE for caret models
summary_results <- resamples(caret_models) %>% summary()
cv_rmse <- imap(caret_models, ~{
  mean(summary_results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist()

# Extract Holdout RMSE for caret models
holdout_rmse <- map(caret_models, ~{
  RMSE(predict(.x, newdata = data_holdout), data_holdout[["price"]])
}) %>% unlist()

# Create summary table
model_performance <- data.frame(
  "CV RMSE" = cv_rmse,
  "Holdout RMSE" = holdout_rmse
)

# Add XGBoost performance
model_performance["XGBoost", ] <- c(
  xgb_predict_wrapper$cv_rmse,
  RMSE(predict(xgb_predict_wrapper, newdata = data_holdout), data_holdout[["price"]])
)

# Save consolidated table
kable(x = model_performance, format = "latex", digits = 3, booktabs = TRUE, linesep = "") %>%
  cat(., file = paste0(output, "horse_race_of_models.tex"))

# Stop parallel cluster
stopCluster(cl)
registerDoSEQ()

#ch16-table-1-rf-models-turning-choices
#ch16-table-2-performance-across-subsamples