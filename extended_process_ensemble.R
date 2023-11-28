# args <- commandArgs(trailingOnly = TRUE)
# state_idx        <- as.numeric(args[1])
# age_idx          <- as.numeric(args[2])

##################################################

source("header.R")
source("utils.R")
source("aux.R")

training_size <- 90
uncertain_size <- 40

state_idx <- 17
age_idx <- 7

strata <- "all"

reparameterize <- TRUE

##################################################
# LOAD AND PRE-PROCESS DATA (FOR ALL ENSEMBLE MODELS)
# `state_idx` and `age_idx` must be selected (if such data exist)
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

KIT_frozen_baseline <- data %>% filter(model == "KIT-frozen_baseline")

state <- unique(data$location)
state <- c(state, "DE")
state <- state[2:length(state)][state_idx]

age <- unique(data$age_group)
age <- c(age, "00+")
age <- age[2:length(age)][age_idx]

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 
models_orig <- models
colors_orig <- colors

filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

models <- c("DISW 1", "DISW 2", "DISW 3", "DISW 4", "AISW 1", "AISW 2", "AISW 3", "AISW 4", "Mean", "Median")
DISW_colors <- colorRampPalette(c("#8B0000", "#E6ADD8"))(4)
AISW_colors <- colorRampPalette(c("#00008B", "#ADD8E6"))(4)
colors <- c(DISW_colors, AISW_colors, "#009E73", "#60D1B3")

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
naive_ensemble <- readRDS(file = naive_ensemble_file)
data <- rbind(data, naive_ensemble)

r <- range(data$forecast_date)

##################################################

reparameterize_file <- ifelse(reparameterize, "new_", "")

# DISW

DISW_1_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_wis_size_", training_size, "_skip_TRUE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
DISW_2_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_wis_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
DISW_3_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "all_quant_method_wis_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
DISW_4_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_wis_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_TRUE.RDS", sep = "")

DISW_1 <- readRDS(file = DISW_1_file)
DISW_2 <- readRDS(file = DISW_2_file)
DISW_3 <- readRDS(file = DISW_3_file)
DISW_4 <- readRDS(file = DISW_4_file)

DISW_1_ens <- DISW_1$ensemble
DISW_2_ens <- DISW_2$ensemble
DISW_3_ens <- DISW_3$ensemble
DISW_4_ens <- DISW_4$ensemble

DISW_1 <- DISW_1$new_data
DISW_2 <- DISW_2$new_data
DISW_3 <- DISW_3$new_data
DISW_4 <- DISW_4$new_data

DISW_1$model <- models[1]
DISW_2$model <- models[2]
DISW_3$model <- models[3]
DISW_4$model <- models[4]

# AISW

AISW_1_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_pinball_size_", training_size, "_skip_TRUE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
AISW_2_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_pinball_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
AISW_3_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "all_quant_method_pinball_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
AISW_4_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_pinball_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_TRUE.RDS", sep = "")

AISW_1 <- readRDS(file = AISW_1_file)
AISW_2 <- readRDS(file = AISW_2_file)
AISW_3 <- readRDS(file = AISW_3_file)
AISW_4 <- readRDS(file = AISW_4_file)

AISW_1_ens <- AISW_1$ensemble
AISW_2_ens <- AISW_2$ensemble
AISW_3_ens <- AISW_3$ensemble
AISW_4_ens <- AISW_4$ensemble

AISW_1 <- AISW_1$new_data
AISW_2 <- AISW_2$new_data
AISW_3 <- AISW_3$new_data
AISW_4 <- AISW_4$new_data

AISW_1$model <- models[5]
AISW_2$model <- models[6]
AISW_3$model <- models[7]
AISW_4$model <- models[8]

##########
##########

naive_ensemble <- naive_ensemble[!(naive_ensemble$type == "mean"), ]

ensemble_data <- rbind(naive_ensemble, DISW_1, DISW_2, DISW_3, DISW_4, AISW_1, AISW_2, AISW_3, AISW_4)

r <- range(DISW_1$forecast_date)
ensemble_data <- ensemble_data[(ensemble_data$forecast_date >= r[1]) & (ensemble_data$forecast_date <= r[2]), ]
ensemble_data$model <- factor(x = ensemble_data$model, levels = models)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# COMPUTE SCORE
# Compute WIS for all post-processed models, given truth final data
##################################################

# Extra gap
skip_first_days <- 30
compute_wis_file <- paste("RESULTS/FITTED_OBJECTS/WIS/wis_size_", training_size, "_state_", state, "_age_", age, "_extra_gap_", skip_first_days, ".RDS", sep = "")

if (file.exists(compute_wis_file)) {
  wis_truth <- readRDS(file = compute_wis_file)
} else {
  
  # Make all models comparable: `skip_first_days = 0 + X` (X = 30; i.e., an extra gap), since I already filtered the `ensemble_data` before
  wis_truth <- compute_wis_truth(data = ensemble_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)
  saveRDS(object = wis_truth, file = compute_wis_file)
}

df_wis <- wis_truth$df_wis
if (TRUE) {
  df_wis$model <- as.character(df_wis$model)
  for (i in 1:nrow(df_wis)) {
    if (df_wis[i, "model"] == "ISW 1") { df_wis[i, "model"] <- "AISW 1" }   
    if (df_wis[i, "model"] == "ISW 2") { df_wis[i, "model"] <- "AISW 2" }   
    if (df_wis[i, "model"] == "ISW 3") { df_wis[i, "model"] <- "AISW 3" }   
    if (df_wis[i, "model"] == "ISW 4") { df_wis[i, "model"] <- "AISW 4" }   
  }
  df_wis$model <- factor(x = df_wis$model, levels = c("Mean", "Median", "DISW 1", "DISW 2", "DISW 3", "DISW 4", "AISW 1", "AISW 2", "AISW 3", "AISW 4"))
} 
wis_summ <- wis_truth$wis_summ

# Bar plot
wis_bar <- plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 100, skip_space = TRUE)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)
# wis_line_horizon <- wis_line_horizon + annotate("text", x = -18, y = 300, label = "1: shared across horizon and omit recent data     ", family = "mono", size = 3) +
#                                        annotate("text", x = -18, y = 280, label = "2: shared across horizon and plug-in point nowcast", family = "mono", size = 3) + 
#                                        annotate("text", x = -18, y = 260, label = "3: shared across horizon and full set of quantiles", family = "mono", size = 3) +
#                                        annotate("text", x = -18, y = 240, label = "4: varying over horizons and plug-in point nowcast", family = "mono", size = 3)

tmp_ttl <- "" # "WIS (weighted ensemble)"
p_total <- wis_bar + wis_line_horizon + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
saveRDS(object = p_total, file = paste("PLOTS/ENSEMBLE/WIS_all_ensemble.RDS", sep = "")) 

ggsave(filename = paste("PLOTS/ENSEMBLE/WIS_all_ensemble.jpeg", sep = ""), plot = p_total, width = 3500, height = 1400, units = c("px"), dpi = 300, bg = "white") 

if (TRUE) { # Plot all strata
  
  p_all    <- readRDS("PLOTS/ENSEMBLE/WIS_all_ensemble.RDS")    &  plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5, size = 18))
  p_ages   <- readRDS("PLOTS/ENSEMBLE/WIS_ages_ensemble.RDS")   &  plot_annotation(title = "Age groups")     & theme(plot.title = element_text(hjust = 0.5, size = 18))
  p_states <- readRDS("PLOTS/ENSEMBLE/WIS_states_ensemble.RDS") &  plot_annotation(title = "States")         & theme(plot.title = element_text(hjust = 0.5, size = 18))
  
  t_all    <- grid::textGrob("National level", gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  t_ages   <- grid::textGrob("Age groups",     gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  t_states <- grid::textGrob("States",         gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  
  c_all    <- (wrap_elements(panel = t_all)    / p_all)    + plot_layout(heights = c(1, 10))
  c_ages   <- (wrap_elements(panel = t_ages)   / p_ages)   + plot_layout(heights = c(1, 10))
  c_states <- (wrap_elements(panel = t_states) / p_states) + plot_layout(heights = c(1, 10))
  
  p_total3 <- wrap_elements(c_all    + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 22.5)))) /
              wrap_elements(c_states + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 22.5)))) /
              wrap_elements(c_ages   + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 22.5))))
  ggsave(filename = "PLOTS/ENSEMBLE/WIS_ensemble_3.jpeg", plot = p_total3, width = 3500, height = 4600, units = c("px"), dpi = 300, bg = "white") 
}

##################################################
# PLOT WEIGHTS
# The range `rr` has to be set according to the original data forecast dates
##################################################

rr <- range(data$forecast_date)
skip_first_days_1 <- uncertain_size + 1
skip_first_days_2 <- 1

quant <- TRUE
horiz <- TRUE

w_hat_file <- paste("RESULTS/w_hat_size_", training_size, "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

if (file.exists(w_hat_file)) {
  w_hat <- readRDS(file = w_hat_file)

  w_hat_DISW_1 <- w_hat$w_hat_DISW_1
  w_hat_DISW_2 <- w_hat$w_hat_DISW_2
  w_hat_DISW_3 <- w_hat$w_hat_DISW_3
  w_hat_DISW_4 <- w_hat$w_hat_DISW_4
  w_hat_AISW_1 <- w_hat$w_hat_AISW_1
  w_hat_AISW_2 <- w_hat$w_hat_AISW_2
  w_hat_AISW_3 <- w_hat$w_hat_AISW_3
  w_hat_AISW_4 <- w_hat$w_hat_AISW_4

} else {
  w_hat_DISW_1 <- weights_tibble(ensemble = DISW_1_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_1, probs = probs, ens_model = "DISW", horiz = horiz)
  w_hat_DISW_2 <- weights_tibble(ensemble = DISW_2_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "DISW", horiz = horiz)  
  w_hat_DISW_3 <- weights_tibble(ensemble = DISW_3_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "DISW", horiz = horiz)
  w_hat_DISW_4 <- weights_tibble(ensemble = DISW_4_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "DISW", horiz = horiz)
  w_hat_AISW_1 <- weights_tibble(ensemble = AISW_1_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_1, probs = probs, ens_model = "AISW", horiz = horiz, excep = TRUE)
  w_hat_AISW_2 <- weights_tibble(ensemble = AISW_2_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "AISW", horiz = horiz, excep = TRUE)
  w_hat_AISW_3 <- weights_tibble(ensemble = AISW_3_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "AISW", horiz = horiz, excep = TRUE)
  w_hat_AISW_4 <- weights_tibble(ensemble = AISW_4_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "AISW", horiz = horiz)
  
  w_hat <- list(w_hat_DISW_1 = w_hat_DISW_1, w_hat_DISW_2 = w_hat_DISW_2, w_hat_DISW_3 = w_hat_DISW_3, w_hat_DISW_4 = w_hat_DISW_4, w_hat_AISW_1 = w_hat_AISW_1, w_hat_AISW_2 = w_hat_AISW_2, w_hat_AISW_3 = w_hat_AISW_3, w_hat_AISW_4 = w_hat_AISW_4)
  saveRDS(object = w_hat, file = w_hat_file)
}

# Available options for `aggregate_total` and `indep_quant`
# Aggregated: `aggregate_total = TRUE` and `indep_quant = FALSE` (or `aggregate_total = TRUE` and `indep_quant = TRUE`)
# Per quantile: `aggregate_total = FALSE` and `indep_quant = TRUE`
# Per horizon: `aggregate_total = FALSE` and `indep_quant = FALSE`

aggregate_total <- TRUE
indep_quant <- TRUE

formatted_w_hat_file <- paste("RESULTS/w_hat_per_horizon_size_", training_size, "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

if (file.exists(formatted_w_hat_file) & (!(aggregate_total | indep_quant))) { # Read files `per horizon` (as this takes longer to run)
  formatted_w_hat <- readRDS(file = formatted_w_hat_file)

  formatted_w_hat_DISW_1 <- formatted_w_hat$formatted_w_hat_DISW_1
  formatted_w_hat_DISW_2 <- formatted_w_hat$formatted_w_hat_DISW_2
  formatted_w_hat_DISW_3 <- formatted_w_hat$formatted_w_hat_DISW_3
  formatted_w_hat_DISW_4 <- formatted_w_hat$formatted_w_hat_DISW_4
  formatted_w_hat_AISW_1 <- formatted_w_hat$formatted_w_hat_AISW_1
  formatted_w_hat_AISW_2 <- formatted_w_hat$formatted_w_hat_AISW_2
  formatted_w_hat_AISW_3 <- formatted_w_hat$formatted_w_hat_AISW_3
  formatted_w_hat_AISW_4 <- formatted_w_hat$formatted_w_hat_AISW_4
} else {
  print("DISW_1")
  formatted_w_hat_DISW_1 <- format_w_hat(w_hat = w_hat_DISW_1, aggregate_total = aggregate_total, indep_quant = indep_quant)
  print("DISW_2")
  formatted_w_hat_DISW_2 <- format_w_hat(w_hat = w_hat_DISW_2, aggregate_total = aggregate_total, indep_quant = indep_quant)
  print("DISW_3")
  formatted_w_hat_DISW_3 <- format_w_hat(w_hat = w_hat_DISW_3, aggregate_total = aggregate_total, indep_quant = indep_quant)
  print("DISW_4")
  formatted_w_hat_DISW_4 <- format_w_hat(w_hat = w_hat_DISW_4, aggregate_total = aggregate_total, indep_quant = indep_quant)
  print("AISW_1")
  formatted_w_hat_AISW_1 <- format_w_hat(w_hat = w_hat_AISW_1, aggregate_total = aggregate_total, indep_quant = indep_quant)
  print("AISW_2")
  formatted_w_hat_AISW_2 <- format_w_hat(w_hat = w_hat_AISW_2, aggregate_total = aggregate_total, indep_quant = indep_quant)
  print("AISW_3")
  formatted_w_hat_AISW_3 <- format_w_hat(w_hat = w_hat_AISW_3, aggregate_total = aggregate_total, indep_quant = indep_quant)
  print("AISW_4")
  formatted_w_hat_AISW_4 <- format_w_hat(w_hat = w_hat_AISW_4, aggregate_total = aggregate_total, indep_quant = indep_quant)

  if (!(aggregate_total | indep_quant)) {
    saveRDS(object = list(formatted_w_hat_DISW_1 = formatted_w_hat_DISW_1,
                          formatted_w_hat_DISW_2 = formatted_w_hat_DISW_2,
                          formatted_w_hat_DISW_3 = formatted_w_hat_DISW_3,
                          formatted_w_hat_DISW_4 = formatted_w_hat_DISW_4,
                          formatted_w_hat_AISW_1 = formatted_w_hat_AISW_1,
                          formatted_w_hat_AISW_2 = formatted_w_hat_AISW_2,
                          formatted_w_hat_AISW_3 = formatted_w_hat_AISW_3,
                          formatted_w_hat_AISW_4 = formatted_w_hat_AISW_4), file = formatted_w_hat_file)
  }
}

# Plotting
extra_skip <- 30
if (aggregate_total) { # Aggregated
  plot_D1 <- plotting_summarized_weights(w_hat = formatted_w_hat_DISW_1$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
  plot_D2 <- plotting_summarized_weights(w_hat = formatted_w_hat_DISW_2$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
  plot_D3 <- plotting_summarized_weights(w_hat = formatted_w_hat_DISW_3$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
  plot_D4 <- plotting_summarized_weights(w_hat = formatted_w_hat_DISW_4$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
  plot_A1 <- plotting_summarized_weights(w_hat = formatted_w_hat_AISW_1$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
  plot_A2 <- plotting_summarized_weights(w_hat = formatted_w_hat_AISW_2$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
  plot_A3 <- plotting_summarized_weights(w_hat = formatted_w_hat_AISW_3$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
  plot_A4 <- plotting_summarized_weights(w_hat = formatted_w_hat_AISW_4$new_w_hat, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
} else {
  if (indep_quant) { # Per quantile
    plot_D1 <- plotting_quantile_weights(w_hat = formatted_w_hat_DISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
    plot_D2 <- plotting_quantile_weights(w_hat = formatted_w_hat_DISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
    plot_D3 <- plotting_quantile_weights(w_hat = formatted_w_hat_DISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
    plot_D4 <- plotting_quantile_weights(w_hat = formatted_w_hat_DISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip)
    plot_A1 <- plotting_quantile_weights(w_hat = formatted_w_hat_AISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
    plot_A2 <- plotting_quantile_weights(w_hat = formatted_w_hat_AISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
    plot_A3 <- plotting_quantile_weights(w_hat = formatted_w_hat_AISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
    plot_A4 <- plotting_quantile_weights(w_hat = formatted_w_hat_AISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25)
  } else { # Per horizon
    plot_D1 <- plotting_horizon_weights(w_hat = formatted_w_hat_DISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip)
    plot_D2 <- plotting_horizon_weights(w_hat = formatted_w_hat_DISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip)
    plot_D3 <- plotting_horizon_weights(w_hat = formatted_w_hat_DISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip)
    plot_D4 <- plotting_horizon_weights(w_hat = formatted_w_hat_DISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip)
    plot_A1 <- plotting_horizon_weights(w_hat = formatted_w_hat_AISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25)
    plot_A2 <- plotting_horizon_weights(w_hat = formatted_w_hat_AISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25)
    plot_A3 <- plotting_horizon_weights(w_hat = formatted_w_hat_AISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25)
    plot_A4 <- plotting_horizon_weights(w_hat = formatted_w_hat_AISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25)
  }
}

ggsave(filename = paste("PLOTS/weights_D1.jpeg", sep = ""), plot = plot_D1, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 
ggsave(filename = paste("PLOTS/weights_D2.jpeg", sep = ""), plot = plot_D2, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 
ggsave(filename = paste("PLOTS/weights_D3.jpeg", sep = ""), plot = plot_D3, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 
ggsave(filename = paste("PLOTS/weights_D4.jpeg", sep = ""), plot = plot_D4, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 
ggsave(filename = paste("PLOTS/weights_A1.jpeg", sep = ""), plot = plot_A1, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 
ggsave(filename = paste("PLOTS/weights_A2.jpeg", sep = ""), plot = plot_A2, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 
ggsave(filename = paste("PLOTS/weights_A3.jpeg", sep = ""), plot = plot_A3, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 
ggsave(filename = paste("PLOTS/weights_A4.jpeg", sep = ""), plot = plot_A4, width = 3500, height = 2500, units = c("px"), dpi = 300, bg = "white") 


