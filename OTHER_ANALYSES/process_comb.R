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

p1 <- plot_comb(data = sum_mean, name = "Mean")
p2 <- plot_comb(data = sum_median, name = "Median", col = "blue")

p_total <- p1 + p2 + plot_annotation(theme = theme(plot.margin = margin()))
ggsave(plot = p_total, filename = paste("OTHER_ANALYSES/result_comb.png", sep = ""), width = 3500, height = 1400, units = "px")

