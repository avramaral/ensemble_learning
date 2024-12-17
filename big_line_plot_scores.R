source("header.R")
source("utils.R")
source("aux.R")

strata <- "all"
state <- "DE"
age <- "00+"

skip_recent_days <- FALSE
probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# Load data sets
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

KIT_frozen_baseline <- data %>% filter(model == "KIT-frozen_baseline")

# Only necessary for `filter_data()` and `compute_wis_truth()`
training_size <- 90
uncertain_size <- 40
original_models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 

filtered_data <- filter_data(data = data, truth_data = truth_data, models = original_models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

if (strata == "all") {
  r <- range(data$forecast_date)
  KIT_frozen_baseline <- KIT_frozen_baseline %>% filter(forecast_date >= r[1], forecast_date <= r[2], age_group %in% age, location %in% state)
  baseline <- KIT_frozen_baseline %>% filter(location == "DE", age_group == "00+", quantile == 0.5) %>% select(target_end_date, target, value)
  baseline_00 <- baseline %>% filter(target == "0 day ahead inc hosp") %>% select(target_end_date, value)
}

##################################################

# 1: Mean
data_unweighted_mean <- readRDS("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_DE_age_00+.RDS")
data_unweighted_mean <- data_unweighted_mean %>% filter(model == "Mean", !is.na(quantile))
data_unweighted_mean <- data_unweighted_mean %>% filter(forecast_date >= "2021-11-30", forecast_date <= "2022-04-29")
data_unweighted_mean <- data_unweighted_mean %>% arrange(forecast_date, quantile)
data_unweighted_mean$model <- "Mean"

# 2: DISW-4
data_DISW_4 <- readRDS("RESULTS/FITTED_OBJECTS/new_method_wis_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS")
data_DISW_4 <- data_DISW_4$new_data %>% filter(model == "DISW", !is.na(quantile))
data_DISW_4 <- data_DISW_4 %>% filter(forecast_date >= "2021-11-30", forecast_date <= "2022-04-29")
data_DISW_4 <- data_DISW_4 %>% arrange(forecast_date, quantile)
data_DISW_4$model <- "DISW 4"

# 3: AISW-4
data_AISW_4 <- readRDS("RESULTS/FITTED_OBJECTS/new_method_pinball_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS")
data_AISW_4 <- data_AISW_4$new_data %>% filter(model == "ISW", !is.na(quantile))
data_AISW_4 <- data_AISW_4 %>% filter(forecast_date >= "2021-11-30", forecast_date <= "2022-04-29")
data_AISW_4 <- data_AISW_4 %>% arrange(forecast_date, quantile)
data_AISW_4$model <- "AISW 4"

# Select-4-Mean-2
data_select_4_mean <- readRDS("RESULTS/FITTED_OBJECTS/new_method_ranked_unweighted_Mean_4_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS")
data_select_4_mean <- data_select_4_mean$new_data %>% filter(model == "ISW", !is.na(quantile))
# data_select_4_mean <- data_select_4_mean %>% filter(forecast_date >= "2021-11-30", forecast_date <= "2022-04-29")
data_select_4_mean <- data_select_4_mean %>% arrange(forecast_date, quantile) 
data_select_4_mean$model <- "Select-4 Mean 2"

##################################################
# Compute scores (bar plot; i.e., useless here)
# Compute WIS for all 4 selected ensemble methods
##################################################

ensemble_data <- rbind(data_unweighted_mean, data_DISW_4, data_AISW_4, data_select_4_mean)
ensemble_data_simplified <- ensemble_data %>% filter(forecast_date >= "2022-01-09", forecast_date <= "2022-04-29") # Based on DISW, which already discards imcomplete observations 
ensemble_models <- c("Mean", "DISW 4", "AISW 4", "Select-4 Mean 2")
ensemble_colors <- c("#009E73", "#E6ADD8", "#ADD8E6", "#FDDA0D")
horizon <- -28:0

skip_first_days <- 30 

if (FALSE) {
  # Notice that `start_date` and `end_date` are manually defined
  wis_truth <- compute_wis_truth(data = ensemble_data_simplified, truth_data = truth_data, models = ensemble_models, horizon = horizon, start_date = as.Date("2022-01-09"), end_date = as.Date("2022-04-29"), skip_first_days = skip_first_days)

  df_wis <- wis_truth$df_wis
  wis_summ <- wis_truth$wis_summ
  
  (total_wis <- df_wis %>% group_by(model) %>% summarise(total_wis = sum(wis)))
}

##################################################
# Compute scores (line plot)
# Compute WIS for all 4 selected ensemble methods
##################################################

#######
# Others required quantities for plotting **only**
#######
hh <- 0 
extra_skip <- 30
baseline_tmp <- baseline_00
average <- FALSE # Should daily WIS be aggregated in a moving average?
tmp_char <- "raw_commonY_" # Originally depends on the `average`
#######
#######

data <- rbind(data, ensemble_data)
r[1] <- r[1] + 1 # So all plots are comparable

# ATTENTION: one should load the function `plot_postprocessed_models_mod()` first (it is at the end of this file)!
ensemble_plots <- list()
for (i in 1:length(ensemble_models)) {
  model <- ensemble_models[i]
  color <- ensemble_colors[i]
  names(color) <- model
  
  print(paste("Model: ", model, " (", sprintf("%02d", i), "/", sprintf("%02d", length(ensemble_models)), ").", sep = ""))
  ensemble_plots[[i]] <- plot_postprocessed_models_mod(data = data, 
                                                       nowcasts = ensemble_data, 
                                                       truth_data = truth_data, 
                                                       model = model, 
                                                       r = r, 
                                                       training_size = training_size, 
                                                       uncertain_size = uncertain_size, 
                                                       baseline_tmp = baseline_tmp, 
                                                       hh = hh, 
                                                       # ens_method = ens_method, 
                                                       extra_skip = extra_skip, 
                                                       skip_recent_days = skip_recent_days,
                                                       average = average)
}
  
p_total <- ensemble_plots[[1]]$p_total / ensemble_plots[[2]]$p_total / ensemble_plots[[3]]$p_total / ensemble_plots[[4]]$p_total
ggsave(filename = "PLOTS/POSTPROCESS/ensemble_line_plot.jpeg", plot = p_total, width = 3500, height = 5600, units = c("px"), dpi = 300, bg = "white")







# Modified `plot_postprocessed_models()`
plot_postprocessed_models_mod <- function (data, nowcasts, truth_data, model, r, training_size, uncertain_size, baseline_tmp, ...) {
  
  cmb_models <- model
  cmb_colors <- color
  mm <- model
  df_nowcast <- nowcasts   |> filter(target == paste(hh, " day ahead inc hosp", sep = ""), type == "quantile", quantile %in% probs, model == mm) |> select(target_end_date, quantile, value, model)
  df_nowcast <- df_nowcast |> rename(date = target_end_date)
  df_true_dt <- truth_data |> filter(date >= range(df_nowcast$date)[1], date <= range(df_nowcast$date)[2])
  
  for (i in 1:nrow(df_true_dt)) { 
    for (j in 1:length(unique(df_nowcast$model))) {
      df_nowcast <- df_nowcast |> add_row(date = df_true_dt$date[i], quantile = 0, value = df_true_dt$truth[i], model = unique(df_nowcast$model)[j]) 
    }
  }
  df_nowcast <- df_nowcast |> pivot_wider(names_from = quantile, values_from = value)
  
  x_dates <- as.Date(c("2021-11-15", "2022-04-29"))
  
  alphas <- setNames(c(0.75, 0.4), c("50%", "95%"))
  line_colors <- setNames(c("red", "lightgray"), c("Final", "At time\nof nowcast"))
  p1 <- ggplot(df_nowcast) +
    geom_ribbon(aes(x = date, ymin = `0.025`, ymax = `0.975`, alpha = "95%"), fill = "skyblue3") +
    geom_ribbon(aes(x = date, ymin = `0.25` , ymax = `0.75`, alpha = "50%"),  fill = "skyblue3") +
    geom_line(aes(x = date, y = `0.5`), linetype = "solid", linewidth = 0.5, color = "royalblue4") + 
    geom_line(aes(x = date, y = `0`, color = "Final"),  linetype = "solid", linewidth = 0.5)  + 
    geom_line(data = baseline_tmp, aes(x = target_end_date, y = value, color = "At time\nof nowcast"),  linetype = "solid", linewidth = 0.5)  + 
    labs(x = NULL, y = "COVID-19 7-day hospitalization incidence in Germany", title = paste("Horizon: ", hh, " days (", model, ")", sep = "")) +
    scale_alpha_manual(
      name = "Nowcasts with \nprediction intervals", values = alphas,
      guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
    ) +
    scale_color_manual(
      name = "Truth", values = line_colors,
      guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)
    ) +
    scale_y_continuous(breaks = c(0, 5000, 10000, 15000), limits = c(0, 17500)) +
    xlim(x_dates) + 
    theme_bw() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.key.size = unit(0.65, "lines"),
      strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
      axis.title.y = element_text(size = 11),
      axis.text = element_text(size = 11),
      axis.ticks = element_line(colour = "black", linewidth = 0.25),
      panel.grid.major = element_line(linewidth = 0.15),
      panel.grid.minor = element_line(linewidth = 0.1),
      plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
      legend.margin = margin(0, 0, 0, 5),
      legend.box.spacing = unit(0, "pt"),
      legend.background = element_rect(fill = "transparent"),
      text = element_text(family = "LM Roman 10")
    )  
  
  ##################################################
  
  # P2
  
  # tmp_data_1 <- data[data$model == model, ]
  # tmp_data_2 <- nowcasts[nowcasts$model == model, ]
  # tmp_data_2$model <- name_method
  # tmp_data_T <- rbind(tmp_data_1, tmp_data_2)
  tmp_data_T <- nowcasts[nowcasts$model == model, ]
  
  
  wis_truth <- compute_wis_truth(data = tmp_data_T, truth_data = truth_data, models = cmb_models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = uncertain_size, verbose = FALSE)
  
  df_wis <- wis_truth$df_wis
  wis_summ <- wis_truth$wis_summ
  
  
  df_wis_horizon <- data.frame(model = rep(cmb_models, length(horizon)), horizon = rep(horizon, each = length(cmb_models)), wis = 0)
  count <- 1
  for (i in 1:length(wis_summ)) {
    for (j in 1:length(cmb_models)) {
      df_wis_horizon$wis[count] <- wis_summ[[i]][j]
      count <- count + 1
    }
  }
  
  #df_wis_horizon[df_wis_horizon$model == name_method, ]$model <- "Post-processed"
  df_wis_horizon$model <- factor(df_wis_horizon$model, levels = c(model))
  names(cmb_colors) <- c(names(cmb_colors)[1])
  
  ##################################################
  
  # P3
  
  cmb_wis <- list()
  
  count <- 1
  b <- txtProgressBar(min = 0, max = length(horizon), initial = 0)
  for (h in horizon) {
    cmb_wis[[as.character(h)]] <- compute_wis_data(data = tmp_data_T, truth_data = truth_data, start_date = r[1], end_date = r[2], horizon = h, models = cmb_models, probs = probs)
    
    count <- count + 1
    setTxtProgressBar(b, count)
  }
  close(b)
  
  total_days <- ifelse(skip_recent_days, (training_size - uncertain_size), training_size)
  wis_days <- compute_wis_days(wis = cmb_wis, models = cmb_models, start_date = r[1], end_date = r[2], total_days = total_days, average = average)
  
  if (skip_recent_days) { r_1 <- (r[1] - 40 + 1); r_e <- 0 } else { r_1 <- r[1] + 1; r_e <- 0 }
  r_2 <- r[2]
  
  if (average) {
    y_lab_temp <- "WIS (Averaged over horizons and M.W. up to 90 days)"
  } else {
    y_lab_temp <- "WIS (Averaged over horizons)"
  }
  
  # wis_days[wis_days$model == name_method, ]$model <- "Post-processed"
  
  ##################################################
  
  max_y <- 400 # max(df_wis_horizon$wis, wis_days$value)
  
  p2 <- ggplot(data = df_wis_horizon, aes(x = horizon, y = wis, color = model)) +
    geom_line(linewidth = 1) +
    scale_color_manual(NULL, values = cmb_colors) +
    labs(x = "Horizon (days)", y = "WIS (Averaged over time points)", color = "Model") +
    scale_x_continuous(breaks = 0:5 * -5, minor_breaks = -28:0) +
    expand_limits(y = c(0, max_y)) +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 11),
          axis.text = element_text(size = 11),
          text = element_text(size = 11, family = "LM Roman 10"))
  
  
  p3 <- ggplot(data = wis_days, aes(x = forecast_date, y = value, color = model)) +
    geom_line(linewidth = 1, alpha = rep(ifelse(unique(wis_days$forecast_date) < (r_1 - 1 + 30 + uncertain_size), 0.25, 1), 1)) +
    geom_vline(xintercept = as.numeric(r_1 - 1 + 30 + uncertain_size), linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r_1 - 1 + 30 + uncertain_size - 40), linetype = "dashed") + 
    scale_color_manual(NULL, values = cmb_colors) +
    expand_limits(y = c(0, max_y)) +
    xlim(c(r_1, r_2)) +
    labs(x = "", y = y_lab_temp) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 11),
          axis.text = element_text(size = 11),
          text = element_text(size = 11, family = "LM Roman 10"))
  
  ##################################################
  
  p_total <- p1 + p2 + p3 + plot_layout(width = c(2, 2, 2)) + plot_annotation(theme = theme(plot.margin = margin()))
  list(p1 = p1, p2 = p2, p3 = p3, p_total = p_total)
}
