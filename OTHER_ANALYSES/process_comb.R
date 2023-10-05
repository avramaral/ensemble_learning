library("tidyverse")
library("patchwork")

groups <- 1:8

results_comb_files <- paste("OTHER_ANALYSES/RESULTS/naive_ensemble_group_", groups, ".RDS", sep = "")

lls <- rep(0, length(groups))
results_comb <- list()
sum_mean   <- NULL
sum_median <- NULL
for (i in 1:length(groups)) {

  results_comb[[i]] <- readRDS(file = results_comb_files[i])
  lls[i] <- length(results_comb[[i]]$wis_truth)
  
  for (l in 1:lls[i]) {
    tmp <- results_comb[[i]]$wis_truth[[l]]$df_wis
    tmp_sum_mean   <- sum(tmp[tmp$model == "Mean", "wis"])
    tmp_sum_median <- sum(tmp[tmp$model == "Median", "wis"])
    
    if (length(sum_mean) == 0) {
      sum_mean   <- c(i, tmp_sum_mean)
      sum_median <- c(i, tmp_sum_median)
    } else {
      sum_mean   <- rbind(sum_mean,   c(i, tmp_sum_mean))
      sum_median <- rbind(sum_median, c(i, tmp_sum_median))
    }
  }
}

sum_mean <- as.data.frame(sum_mean)
rownames(sum_mean) <- 1:nrow(sum_mean)
colnames(sum_mean) <- c("group", "wis")

sum_median <- as.data.frame(sum_median)
rownames(sum_median) <- 1:nrow(sum_median)
colnames(sum_median) <- c("group", "wis")

plot_comb <- function (data, name = "Mean", ylim = c(75, 200), col = "red", ...) {
  p <- ggplot(data) + 
         geom_point(aes(x = group, y = wis), shape = 21, size = 2, color = col, fill = NA, stroke = 1) +
         scale_y_continuous(breaks = seq(ylim[1], ylim[2], length.out = 6)) +
         scale_x_continuous(breaks = seq(min(data$group), max(data$group))) +
         labs(x = "Number of models", y = "WIS", title = name) +
         expand_limits(y = ylim) +
         theme_bw() +
         theme(legend.position = "none",
               plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
               axis.title.x = element_text(size = 14),
               axis.title.y = element_text(size = 14),
               axis.text = element_text(size = 12),
               text = element_text(family = "LM Roman 10"))
  
  p
}

p1 <- plot_comb(data = sum_mean, name = "Mean", col = "black")
p2 <- plot_comb(data = sum_median, name = "Median", col = "black")

p_total <- p1 + p2 + plot_annotation(theme = theme(plot.margin = margin()))
ggsave(plot = p_total, filename = paste("OTHER_ANALYSES/result_comb.png", sep = ""), width = 3500, height = 1400, units = "px")

##################################################
# REAL-TIME MODEL SELECTION SCORE
##################################################

ens_method <- "ranked_unweighted"
unweighted_method <- "mean"

skip_recent_days <- FALSE

training_size <- 90
uncertain_size <- 40

quant <- TRUE # Weights depend (or not) on the quantiles
horiz <- FALSE # Weights depend (or not) on the horizons 

state <- "DE"
age <- "00+"

##################################################

wis_mean   <- data.frame(n_models = 1:8, value = rep(0, 8))
wis_median <- data.frame(n_models = 1:8, value = rep(0, 8))

count <- 1
for (n_ensemble_models in 1:8) {
  wis_truth_file_mean <- paste("OTHER_ANALYSES/RESULTS/FITTED_OBJECTS/WIS/wis_truth_", ens_method, "_mean_",  n_ensemble_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
  wis_truth_mean <- readRDS(wis_truth_file_mean)
  wis_mean[count, "value"] <- sum(wis_truth_mean$df_wis[, "wis"])
  
  wis_truth_file_median <- paste("OTHER_ANALYSES/RESULTS/FITTED_OBJECTS/WIS/wis_truth_", ens_method, "_median_",  n_ensemble_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
  wis_truth_median <- readRDS(wis_truth_file_median)
  wis_median[count, "value"] <- sum(wis_truth_median$df_wis[, "wis"])
  
  count <- count + 1
}

common_col <- "red"
common_fil <- "red" # NA
shape_plot <- 21 # 23
p1_updated <- p1 + geom_point(data = wis_mean  , aes(x = n_models, y = value), color = common_col, fill = common_fil, size = 2, shape = shape_plot, stroke = 1) + geom_hline(yintercept = sum_mean[  nrow(sum_mean  ), 2], linetype = "dashed")
p2_updated <- p2 + geom_point(data = wis_median, aes(x = n_models, y = value), color = common_col, fill = common_fil, size = 2, shape = shape_plot, stroke = 1) + geom_hline(yintercept = sum_median[nrow(sum_median), 2], linetype = "dashed")
p_total_updated <- p1_updated + p2_updated + plot_annotation(theme = theme(plot.margin = margin())) + plot_annotation(title = ifelse(skip_recent_days, "Skip 40-day window", "Use 'best guess' for the 40-day window"))
# p_total_updated
ggsave(plot = p_total_updated, filename = paste("OTHER_ANALYSES/result_comb_real_time_", skip_recent_days, ".png", sep = ""), width = 3500, height = 1200, units = "px")


