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


################################
# DA helped functions
# adds various functions used in several chapters



# v2.0. 2020-04-06 major extension, now includes all functions for ch14-ch17 and ch24
# v2.1. 2020-04-09 minor edits to tree
# v2.2. 2020-04-30 minor edits ch24
# v2.3 2020-08-24 library check


library(tidyverse)
library(urca)
library(stargazer)
library(sandwich)
library(stringr)


calculate_se <- function(lm_model, se = 'robust', max_lag) {
    if (!se %in% c('traditional', 'robust', 'newey-west')) stop("se should be one of traditional, robust or newey-west (default is robust).")
    if (!require(sandwich)) stop("Required sandwich package is missing.")

    if (se == 'robust') {
        sqrt(diag(vcovHC(lm_model, type="HC1")))
    } else if (se == 'newey-west') {
        sqrt(diag(NeweyWest(lm_model, lag = max_lag, prewhite = FALSE)))
    } else {
        sqrt(diag(vcov(lm_model)))
    }
}

summary_r <- function(model, se = 'robust', max_lag = 0, ...) {

    sumry <- summary(model)
    table <- coef(sumry)
    table[, 2] <- calculate_se(model, se, max_lag)
    table[, 3] <- table[,1]/table[, 2]
    table[, 4] <- 2*pt(abs(table[, 3]), df.residual(model), lower.tail=FALSE)

    sumry$coefficients <- table
    p <- nrow(table)
    if (p > 1) {
        if (se == 'robust') {
            hyp <- cbind(0, diag(p - 1))
            sumry$fstatistic[1] <- linearHypothesis(model, hyp, white.adjust="hc1")[2, "F"]
        } else if (se == 'newey-west') {
            sumry$fstatistic[1] <- NA
        }
    }

    print(sumry)
    cat("Number of observations:", length(residuals(model)), "\n\n")

    if (se == 'robust') {
        cat("Note: Heteroscedasticity-consistent standard errors (adjustment HC1)\n")
    } else if (se == 'newey-west') {
        cat("Note: Newey-West standard errors - maximum lag:", max_lag, "\n")
    }


}

stargazer_r <- function(list_of_models, type="text", align=TRUE, no.space=TRUE,
                        keep.stat=c("n", "rsq"),
                        se = 'robust', max_lag = 0, ...) {
    if (!require(stargazer)) stop("Required stargazer package is missing.")

    if (class(type) != "character") stop("Different models should be given in a list.")
    if (class(list_of_models) != "list") list_of_models <- list(list_of_models)
    if (!length(se) %in% c(1, length(list_of_models))) stop("For parameter se you should give one string (if you want to apply it to all models) or a list of strings (if you want to apply different types of standard error for the different models). The string could take traditional, robust, and newey-west (default is robust).")

    if (length(se) == 1) {
        note <- paste(capwords(se[[1]]), "standard errors in parentheses")
        se <- as.list(rep(se[[1]], length(list_of_models)))
    } else {
        note <- "Standard errors in parentheses"
    }

    if (length(max_lag) == 1) {
        max_lag <- as.list(rep(max_lag[[1]], length(list_of_models)))
        if (all(se == 'newey-west')) {
            note <- paste(note, "- max lag:", max_lag[[1]])
        }
    }

    if (any(se == 'newey-west')) keep.stat <- "n"

    list_se_robust <- lapply(
        seq_along(list_of_models),
        function(j) {
            if (class(list_of_models[[j]])[[1]] %in% c('lm', 'plm')) {
                calculate_se(list_of_models[[j]], se = se[[j]], max_lag = max_lag[[j]])
            } else {
                NULL
            }
        }
    )

    args <- list(...)
    if (!is.null(args[['out']])) type="html"

    stargazer(
        list_of_models,
        se = list_se_robust,
        report ="vcs*",
        notes = note,
        type = type, align = align, keep.stat = keep.stat, no.space = no.space,
        ...
    )
}


pperron <- function(x, model = c('constant', 'trend'), type = "Z-tau") {
    if (!require(urca)) stop("Required urca package is missing.")

    results <- ur.pp(x, type = type, model = model)
    print(results)

    model <- match.arg(model)
    if (model == 'trend') trend = 'ct' else trend = 'c'
    cat(
        "MacKinnon approximate p-value for Z-tau:",
        punitroot(results@teststat, trend = trend),
        "\n\n"
    )
}

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = "-" )
    sapply(strsplit(s, split = "-"), cap, USE.NAMES = !is.null(names(s)))
}

lags <- function(variable, lags) {
    # create lags of a variable for a regression as string
    var_string <- deparse(substitute(variable))
    paste(
        lapply(
            lags,
            function(i) {
                paste0("lag(", var_string, ",", i, ")")
            }
        ),
        collapse = "+"
    )
}

d <- function(x) {
    c(NA, diff(x))
}

Arima <- function(..., transform.pars = FALSE) {
    model <- arima(...)

    # rename to be consistent with lm
    names(model$coef) <- gsub('intercept', '(Intercept)', names(model$coef))
    row.names(model$var.coef) <- gsub('intercept', '(Intercept)', row.names(model$var.coef))
    colnames(model$var.coef) <- gsub('intercept', '(Intercept)', colnames(model$var.coef))

    model
}

mse_log <- function (pred, y, corr) {
  # Mean Squared Error for log models
  (mean((exp(y) - exp(pred) * exp(corr^2/2))^2, na.rm=T ))
}

mse_lev <- function(pred, y) {
  # Mean Squared Error for log models
  (mean((pred - y)^2, na.rm=T))
}


########## 2020-03-31
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  l <- gsub("0e\\+00","0",l)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2)
  l <- gsub("e\\+","e",l)
  #convert 1x10^ or 1.000x10^ -> 10^
  l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
  # return this as an expression
  parse(text=l)
}

create_output_if_doesnt_exist<- function(folder){
  if (!dir.exists(folder)){
    dir.create(folder)
    print(paste(folder,"folder created."))}
  else{
    print(paste(folder,"folder already exists"))}
}

loadLibraries <- function(use_case_dir) {
  lines <- trimws(readLines(file.path(use_case_dir,"R_packages.txt")))
  loaded <- lapply(lines[lines != ""], function(x) library(x, character.only = TRUE))
}


# new stuff for prediction parts
#v1.0. 2020 01
#v1.1 2020-03-26 group_by()


# ch14
price_diff_by_variables <- function(df, factor_var, dummy_var){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)

  # Process your data frame and make a new dataframe which contains the stats
  
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)

  stats <- df %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(Mean = mean(price, na.rm=TRUE),
                     se = sd(price)/sqrt(n()))

  stats[,2] <- lapply(stats[,2], factor)

  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9))+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
                  position=position_dodge(width = 0.9), width = 0.25)+
    ylab('Mean Price')+
    theme_bw()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line()) +
    scale_fill_grey()
}


price_diff_by_variables2 <- function(df, factor_var, dummy_var, factor_lab, dummy_lab){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)

  # Process your data frame and make a new dataframe which contains the stats
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)

  stats <- df %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(Mean = mean(price, na.rm=TRUE),
                     se = sd(price)/sqrt(n()))

  stats[,2] <- lapply(stats[,2], factor)

  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9), alpha=0.8)+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
                  position=position_dodge(width = 0.9), width = 0.25)+
    scale_color_manual(name=dummy_lab,
                       values=c(color[2],color[1])) +
    scale_fill_manual(name=dummy_lab,
                      values=c(color[2],color[1])) +
    ylab('Mean Price')+
    xlab(factor_lab) +
    theme_bg()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          legend.position = "top",
          #legend.position = c(0.7, 0.9),
          legend.box = "vertical",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 5, face = "bold"),
          legend.key.size = unit(x = 0.4, units = "cm")
        )
}

#ch15
save_tree_plot <- function(cart_model, filename, filepath, size, tweak = 1.2) {
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
    psize = 12
  } else if (size == "verylarge") {
    width=mywidth_verylarge
    height=myheight_verylarge
    eps_width=eps_mywidth_verylarge
    eps_height=eps_myheight_verylarge
    psize = 14
  }
  else{
    print("Unknown size option. Try 'small', 'large', or 'verylarge'")
  }
  png(filename =paste0(filepath, filename_png),
      width = width, height = height, units = "cm", res = 1200, pointsize = psize)
  rpart.plot(cart_model, tweak=tweak, digits=2, extra=107, under = TRUE)
  dev.off()

  cairo_ps(filename = paste0(filepath, filename_eps),
           width = eps_width, height = eps_height, pointsize = psize,
           fallback_resolution = 1200)
  rpart.plot(cart_model, tweak=tweak, digits=2, extra=107, under = TRUE)
  dev.off()
}






#ch17
# helper functions for classification code: ch17-predicting-firm-exit
# v.1.0. 2019-11-xx
# v.1.3 2019-12-21 slight graph changes
# v.1.4 2019-12-24 adds coloring, size
# v.1.5 2020-04-07 graphical features changed + FIXME

twoClassSummaryExtended <- function (data, lev = NULL, model = NULL)
{
  lvls <- levels(data$obs)
  rmse <- sqrt(mean((data[, lvls[1]] - ifelse(data$obs == lev[2], 0, 1))^2))
  c(defaultSummary(data, lev, model), "RMSE" = rmse)
}



#createLossPlot <- function(r, best_coords, file_name,  mywidth_large=12, myheight_large = 9) {
createLossPlot <- function(r, best_coords, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {
  t <- best_coords$threshold[1]
  sp <- best_coords$specificity[1]
  se <- best_coords$sensitivity[1]
  n <- rowSums(best_coords[c("tn", "tp", "fn", "fp")])[1]

  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  all_coords <- all_coords %>%
    mutate(loss = (fp*FP + fn*FN)/n)
  l <- all_coords[all_coords$threshold == t, "loss"]

  loss_plot <- ggplot(data = all_coords, aes(x = threshold, y = loss)) +
    geom_line(color=color[1], size=0.7) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_vline(xintercept = t , color = color[2] ) +
    annotate(geom = "text", x = t, y= min(all_coords$loss),
             label=paste0("best threshold: ", round(t,2)),
             colour=color[2], angle=90, vjust = -1, hjust = -0.5, size = 7) +
    annotate(geom = "text", x = t, y= l,
             label= round(l, 2), hjust = -0.3, size = 7) +
    theme_bg()
  save_fig(file_name, output, "small")

  #  ggsave(plot = loss_plot, paste0(file_name,".png"), width=mywidth_small, height=myheight_small, dpi=1200)
  #  cairo_ps(filename = paste0(file_name,".eps"), width = mywidth_small, height = myheight_small, pointsize = 12, fallback_resolution = 1200)
  #  print(loss_plot)
  #  dev.off()

  loss_plot
}



#createRocPlotWithOptimal <- function(r, best_coords, file_name,  mywidth_large=12, myheight_large = 9) {
createRocPlotWithOptimal <- function(r, best_coords, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {

  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  t <- best_coords$threshold[1]
  sp <- best_coords$specificity[1]
  se <- best_coords$sensitivity[1]

  roc_plot <- ggplot(data = all_coords, aes(x = specificity, y = sensitivity)) +
    geom_line(color=color[1], size=0.7) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_x_reverse(breaks = seq(0, 1, by = 0.1)) +
    geom_point(aes(x = sp, y = se)) +
    annotate(geom = "text", x = sp, y = se,
             label = paste(round(sp, 2),round(se, 2),sep = ", "),
             hjust = 1, vjust = -1, size = 7) +
    theme_bg()
  #  + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
  #          axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
  save_fig(file_name, output, "small")

  #  ggsave(plot = roc_plot, paste0(file_name, ".png"),         width=mywidth_small, height=myheight_small, dpi=1200)
  # cairo_ps(filename = paste0(file_name, ".eps"),           width = mywidth_small, height = myheight_small, pointsize = 12,           fallback_resolution = 1200)
  #print(roc_plot)
  #dev.off()

  roc_plot
}
# createRocPlot <- function(r, file_name,  mywidth_large=12, myheight_large = 9) {
createRocPlot <- function(r, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {
  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)

  roc_plot <- ggplot(data = all_coords, aes(x = fpr, y = tpr)) +
    geom_line(color=color[1], size = 0.7) +
    geom_area(aes(fill = color[4], alpha=0.4), alpha = 0.3, position = 'identity', color = color[1]) +
    scale_fill_viridis(discrete = TRUE, begin=0.6, alpha=0.5, guide = FALSE) +
    xlab("False Positive Rate (1-Specifity)") +
    ylab("True Positive Rate (Sensitivity)") +
    geom_abline(intercept = 0, slope = 1,  linetype = "dotted", col = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0, 0.01)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0.01, 0)) +
    theme_bg()
  #+    theme(axis.text.x = element_text(size=13), axis.text.y = element_text(size=13),
  #        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))
  save_fig(file_name, output, "small")

  #ggsave(plot = roc_plot, paste0(file_name, ".png"),      width=mywidth_small, height=myheight_small, dpi=1200)
  #cairo_ps(filename = paste0(file_name, ".eps"),    #        width = mywidth_small, height = myheight_small, pointsize = 12,    #       fallback_resolution = 1200)
  #print(roc_plot)
  #dev.off()

  roc_plot
}


# CH24
# HELPER FUNCTIONS

#1.0 2020-03-24
#1.1 2020-03-31 alter graph colors


getPointsGraph <- function(data, colors) {
  data %>%
    group_by(t_event) %>%
    summarise(mean = mean(points)) %>%
    group_by(t_event_6 = cut(t_event, c(-13,-6,0, 1,7,13), right = FALSE)) %>%
    mutate(group_mean = mean(mean)) %>%
    ggplot(data = ., aes(x = t_event, y = mean)) +
    geom_point(color = color[1]) +
    geom_line(aes(x = t_event, y = group_mean, group = t_event_6), size = 1, color = color[1]) +
    geom_vline(xintercept = 0, color = color[3], size=1.5, linetype = "dashed" ) +
    labs(y = "Average points", x = "Event time: games before/after manager change") +
    scale_x_continuous(breaks = c(-12, -6, -1, 1, 6, 12), limits = c(-12, 12)) +
    scale_y_continuous(expand=c(0.01,0.01), breaks = seq(0, 1.6, 0.2), limits = c(0, 1.7)) +
    annotate("text", x = 4, y = 0.1, label = "after", size=2.5)+
    annotate("text", x = -6, y = 0.1, label = "before", size=2.5)+
    theme_bg()
}

chooseRandomPseudo <- function(data, seed = 27845) {
  set.seed(seed)
  data %>%
    filter(pseudo == 1) %>%
    group_by(team, season) %>%
    group_modify(~ {.x[sample(nrow(.x),1),]}) %>%
    ungroup() %>%
    select(team, season, gameno, pseudo) %>%
    right_join(select(data, -pseudo)) %>%
    mutate(pseudo = replace_na(pseudo, 0))
}

getPointsGraphWithPseudo <- function(data, colors) {
  data_plot <- data %>%
    group_by(t_event, countinterv, countpseudo)%>%
    summarise(mean = mean(points)) %>%
    group_by(t_event_6 = cut(t_event, c(-13,-6,0, 1,7,13), right = FALSE), countinterv, countpseudo) %>%
    mutate(group_mean = mean(mean))

  ggplot(data = data_plot, aes(x = t_event, y = mean, color = factor(countpseudo))) +
    geom_point() +
    geom_line(data = filter(data_plot, countpseudo == 0),
              aes(x = t_event, y = group_mean, group = t_event_6), size = 1) +
    geom_line(data = filter(data_plot, countpseudo == 1),
              aes(x = t_event, y = group_mean, group = t_event_6), size = 1) +
    geom_vline(xintercept = 0, color = color[3], size=1.5, linetype = "dashed" ) +
    labs(y = "Average points", x = "Event time: games before/after intervention or pseudo intervention") +
    scale_x_continuous(breaks = c(-12, -6, -1, 1, 6, 12), limits = c(-12, 12)) +
    scale_y_continuous(expand=c(0.01,0.01), breaks = seq(0, 1.6, 0.2), limits = c(0, 1.7)) +
    scale_color_manual(values=color[1:2], labels = c("intervention", "pseudo intervention"), name="") +
    annotate("text", x = 4, y = 0.1, label = "after", size=2.5)+
    annotate("text", x = -6, y = 0.1, label = "before", size=2.5)+
    theme(legend.position = c(0.5,0.1),           legend.background = element_blank(),           legend.box.background = element_rect(color = "white"))+
    theme_bg()
}

create_calibration_plot <- function(data, file_name, prob_var, actual_var, y_lab = "Actual event probability" , n_bins = 10, breaks = NULL) {
  
  if (is.null(breaks)) {
    breaks <- seq(0,1,length.out = n_bins + 1)
  }

  binned_data <- data %>%
    mutate(
      prob_bin = cut(!!as.name(prob_var), 
                    breaks = breaks,
                    include.lowest = TRUE)
    ) %>%
    group_by(prob_bin, .drop=FALSE) %>%
    summarise(mean_prob = mean(!!as.name(prob_var)), mean_actual = mean(!!as.name(actual_var)), n = n())

    p <- ggplot(data = binned_data) +
      geom_line(aes(mean_prob, mean_actual), color=color[1], size=0.6, show.legend = TRUE) +
      geom_point(aes(mean_prob,mean_actual), color = color[1], size = 1, shape = 16, alpha = 0.7, show.legend=F, na.rm = TRUE) +
      geom_segment(x=min(breaks), xend=max(breaks), y=min(breaks), yend=max(breaks), color=color[2], size=0.3) +
      theme_bg() +
      labs(x= "Predicted event probability",
           y= y_lab) +
      coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
      expand_limits(x = 0.01, y = 0.01) +
      scale_y_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) +
      scale_x_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) 

    save_fig(file_name, output, "small")
    p
}
