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

models <- c("Mean", "Median", "DISW 1", "DISW 2", "DISW 3", "DISW 4", "ISW 1", "ISW 2", "ISW 3", "ISW 4")
colors <- c("#009E73", "#60D1B3", rainbow(8))

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

DISW_1$model <- models[3]
DISW_2$model <- models[4]
DISW_3$model <- models[5]
DISW_4$model <- models[6]

# ISW

ISW_1_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_pinball_size_", training_size, "_skip_TRUE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
ISW_2_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_pinball_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
ISW_3_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "all_quant_method_pinball_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_FALSE.RDS", sep = "")
ISW_4_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_pinball_size_", training_size, "_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_TRUE.RDS", sep = "")

ISW_1 <- readRDS(file = ISW_1_file)
ISW_2 <- readRDS(file = ISW_2_file)
ISW_3 <- readRDS(file = ISW_3_file)
ISW_4 <- readRDS(file = ISW_4_file)

ISW_1_ens <- ISW_1$ensemble
ISW_2_ens <- ISW_2$ensemble
ISW_3_ens <- ISW_3$ensemble
ISW_4_ens <- ISW_4$ensemble

ISW_1 <- ISW_1$new_data
ISW_2 <- ISW_2$new_data
ISW_3 <- ISW_3$new_data
ISW_4 <- ISW_4$new_data

ISW_1$model <- models[7]
ISW_2$model <- models[8]
ISW_3$model <- models[9]
ISW_4$model <- models[10]

##########
##########

naive_ensemble <- naive_ensemble[!(naive_ensemble$type == "mean"), ]

ensemble_data <- rbind(naive_ensemble, DISW_1, DISW_2, DISW_3, DISW_4, ISW_1, ISW_2, ISW_3, ISW_4)

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
wis_summ <- wis_truth$wis_summ

# Bar plot
wis_bar <- plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 100)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)
wis_line_horizon <- wis_line_horizon + annotate("text", x = -18, y = 300, label = "1: shared across horizon and omit recent data     ", family = "mono", size = 3) +
                                       annotate("text", x = -18, y = 280, label = "2: shared across horizon and plug-in point nowcast", family = "mono", size = 3) + 
                                       annotate("text", x = -18, y = 260, label = "3: shared across horizon and full set of quantiles", family = "mono", size = 3) +
                                       annotate("text", x = -18, y = 240, label = "4: varying over horizons and plug-in point nowcast", family = "mono", size = 3)
  
p_total <- wis_bar + wis_line_horizon + plot_annotation(title = "WIS (weighted ensemble)", theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
ggsave(filename = paste("PLOTS/WIS_weighted_ensemble.jpeg", sep = ""), plot = p_total, width = 3500, height = 1400, units = c("px"), dpi = 300, bg = "white") 

##################################################
# PLOT WEIGHTS
# The range `rr` has to be set according to the original data forecast dates
##################################################

rr <- range(data$forecast_date)
skip_first_days_1 <- uncertain_size + 1
skip_first_days_2 <- 1

w_hat_file <- paste("RESULTS/w_hat_size_", training_size, "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

if (file.exists(w_hat_file)) {
  w_hat <- readRDS(file = w_hat_file)

  w_hat_DISW_1 <- w_hat$w_hat_DISW_1
  w_hat_DISW_2 <- w_hat$w_hat_DISW_2
  w_hat_ISW_1  <- w_hat$w_hat_ISW_1
  w_hat_ISW_2  <- w_hat$w_hat_ISW_2

} else {
  w_hat_DISW_1 <- weights_tibble(ensemble = DISW_1_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_1, probs = probs, ens_model = "DISW", horiz = horiz)
  w_hat_DISW_2 <- weights_tibble(ensemble = DISW_2_ens, r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "DISW", horiz = horiz)
  w_hat_ISW_1  <- weights_tibble(ensemble = ISW_1_ens , r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_1, probs = probs, ens_model = "ISW" , horiz = horiz)
  w_hat_ISW_2  <- weights_tibble(ensemble = ISW_2_ens , r = rr, models = models_orig, horizon = horizon, skip_first_days = skip_first_days_2, probs = probs, ens_model = "ISW" , horiz = horiz)
  #w_hat_DISW_1 <- w_hat_DISW_2 <- w_hat_ISW_1 <- w_hat_ISW_2
  
  w_hat <- list(w_hat_DISW_1 = w_hat_DISW_1, w_hat_DISW_2 = w_hat_DISW_2, w_hat_ISW_1 = w_hat_ISW_1, w_hat_ISW_2 = w_hat_ISW_2)
  saveRDS(object = w_hat, file = w_hat_file)
}

# Available options for `aggregate_total` and `indep_quant`
# Aggregated: `aggregate_total = TRUE` and `indep_quant = FALSE` (or `aggregate_total = TRUE` and `indep_quant = TRUE`)
# Per quantile: `aggregate_total = FALSE` and `indep_quant = TRUE`
# Per horizon: `aggregate_total = FALSE` and `indep_quant = FALSE`

aggregate_total <- FALSE
indep_quant <- TRUE

formatted_w_hat_file <- paste("RESULTS/w_hat_per_horizon_size_", training_size, "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

if (file.exists(formatted_w_hat_file) & (!(aggregate_total | indep_quant))) { # Read files `per horizon` (as this takes longer to run)
  formatted_w_hat <- readRDS(file = formatted_w_hat_file)

  formatted_w_hat_DISW_1 <- formatted_w_hat$formatted_w_hat_DISW_1
  formatted_w_hat_DISW_2 <- formatted_w_hat$formatted_w_hat_DISW_2
  formatted_w_hat_ISW_1  <- formatted_w_hat$formatted_w_hat_ISW_1
  formatted_w_hat_ISW_2  <- formatted_w_hat$formatted_w_hat_ISW_2
} else {
  formatted_w_hat_DISW_1 <- format_w_hat(w_hat = w_hat_DISW_1, aggregate_total = aggregate_total, indep_quant = indep_quant)
  formatted_w_hat_DISW_2 <- format_w_hat(w_hat = w_hat_DISW_2, aggregate_total = aggregate_total, indep_quant = indep_quant)
  formatted_w_hat_ISW_1  <- format_w_hat(w_hat = w_hat_ISW_1 , aggregate_total = aggregate_total, indep_quant = indep_quant)
  formatted_w_hat_ISW_2  <- format_w_hat(w_hat = w_hat_ISW_2 , aggregate_total = aggregate_total, indep_quant = indep_quant)

  if (!(aggregate_total | indep_quant)) {
    saveRDS(object = list(formatted_w_hat_DISW_1 = formatted_w_hat_DISW_1,
                          formatted_w_hat_DISW_2 = formatted_w_hat_DISW_2,
                          formatted_w_hat_ISW_1  = formatted_w_hat_ISW_1 ,
                          formatted_w_hat_ISW_2  = formatted_w_hat_ISW_2 ), file = formatted_w_hat_file)
  }
}

# Plotting

if (aggregate_total) { # Aggregated
  plotting_summarized_weights(w_hat = formatted_w_hat_DISW_1$new_w_hat, r = rr, models = models_orig, colors = colors_orig)
  plotting_summarized_weights(w_hat = formatted_w_hat_DISW_2$new_w_hat, r = rr, models = models_orig, colors = colors_orig)
  plotting_summarized_weights(w_hat = formatted_w_hat_ISW_1$new_w_hat , r = rr, models = models_orig, colors = colors_orig)
  plotting_summarized_weights(w_hat = formatted_w_hat_ISW_2$new_w_hat , r = rr, models = models_orig, colors = colors_orig)
} else {
  if (indep_quant) { # Per quantile
    plotting_quantile_weights(w_hat = formatted_w_hat_DISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig)
    plotting_quantile_weights(w_hat = formatted_w_hat_DISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig)
    plotting_quantile_weights(w_hat = formatted_w_hat_ISW_1$new_w_hat_total , r = rr, models = models_orig, colors = colors_orig)
    plotting_quantile_weights(w_hat = formatted_w_hat_ISW_2$new_w_hat_total , r = rr, models = models_orig, colors = colors_orig)
  } else { # Per horizon
    plotting_horizon_weights(w_hat = formatted_w_hat_DISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "DISW (St.1.)")
    plotting_horizon_weights(w_hat = formatted_w_hat_DISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "DISW (St.2.)")
    plotting_horizon_weights(w_hat = formatted_w_hat_ISW_1$new_w_hat_total , r = rr, models = models_orig, colors = colors_orig, big_title = "ISW (St.1.)")
    plotting_horizon_weights(w_hat = formatted_w_hat_ISW_2$new_w_hat_total , r = rr, models = models_orig, colors = colors_orig, big_title = "ISW (St.2.)")
  }
}

