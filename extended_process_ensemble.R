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

models <- c("Mean", "Median", "Post-Mean", "Post-Median", "Mean-Post", "Median-Post", "DISW 1", "DISW 2", "DISW 3", "DISW 4", "AISW 1", "AISW 2", "AISW 3", "AISW 4", "Select-4 Mean", "Select-4 Median")
DISW_colors <- colorRampPalette(c("#8B0000", "#E6ADD8"))(4)
AISW_colors <- colorRampPalette(c("#00008B", "#ADD8E6"))(4)
colors <- c( "#009E73", "#60D1B3", "#FF4500", "#FF7F50", "#9400D3", "#9370DB", DISW_colors, AISW_colors, "#FDDA0D", "#FFFF3F")

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
naive_ensemble <- readRDS(file = naive_ensemble_file)
data <- rbind(data, naive_ensemble)

r <- range(data$forecast_date)

##################################################

reparameterize_file <- ifelse(reparameterize, "new_", "")

# Post-naive

post_naive_file <-  "DATA/UNTRAINED_ENSEMBLE/POST_PROCESSED/new_postprocessed_naive_ensemble_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS"
post_naive <- readRDS(file = post_naive_file)

post_mean   <- post_naive |> filter(model == "Mean")
post_median <- post_naive |> filter(model == "Median")

post_mean$model   <- models[5]
post_median$model <- models[6]

# Naive-post

mean_post_file   <- "RESULTS/FITTED_OBJECTS/POST_PROCESSED/new_post-processing_model_Mean_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS"
median_post_file <- "RESULTS/FITTED_OBJECTS/POST_PROCESSED/new_post-processing_model_Median_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS"

mean_post   <- readRDS(file = mean_post_file)
median_post <- readRDS(file = median_post_file)

mean_post_ens   <- mean_post$ensemble
median_post_ens <- median_post$ensemble

mean_post   <- mean_post$new_data
median_post <- median_post$new_data

mean_post$model   <- models[3]
median_post$model <- models[4]

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

DISW_1$model <- models[7]
DISW_2$model <- models[8]
DISW_3$model <- models[9]
DISW_4$model <- models[10]

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

AISW_1$model <- models[11]
AISW_2$model <- models[12]
AISW_3$model <- models[13]
AISW_4$model <- models[14]

# Select 4

m <- 4
select_4_mean_file   <- paste("RESULTS/FITTED_OBJECTS/new_method_ranked_unweighted_Mean_",   m ,"_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS", sep = "")
select_4_median_file <- paste("RESULTS/FITTED_OBJECTS/new_method_ranked_unweighted_Median_", m ,"_size_90_skip_FALSE_state_DE_age_00+_quant_TRUE_horiz_TRUE.RDS", sep = "")

select_4_mean   <- readRDS(file = select_4_mean_file)
select_4_median <- readRDS(file = select_4_median_file)

select_4_mean_ens   <- select_4_mean$ensemble
select_4_median_ens <- select_4_median$ensemble

select_4_mean   <- select_4_mean$new_data
select_4_median <- select_4_median$new_data

select_4_mean$model   <- models[15]
select_4_median$model <- models[16]

##########
##########

naive_ensemble <- naive_ensemble[!(naive_ensemble$type == "mean"), ]

ensemble_data <- rbind(naive_ensemble, post_mean, post_median, mean_post, median_post, DISW_1, DISW_2, DISW_3, DISW_4, AISW_1, AISW_2, AISW_3, AISW_4, select_4_mean, select_4_median)

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

###

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

###

coverage_file <- paste("RESULTS/FITTED_OBJECTS/COVERAGE/coverage_size_", training_size, "_state_", state, "_age_", age, "_extra_gap_", skip_first_days, ".RDS", sep = "")

if (file.exists(coverage_file)) {
  coverage_models <- readRDS(file = coverage_file)
} else {
  coverage_models <- compute_coverage(data = ensemble_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)
  saveRDS(object = coverage_models, file = coverage_file)
}

###

# Bar plot
wis_bar <- plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 100, skip_space = FALSE)

# Coverage plot
coverage_bar <- plot_coverage(coverage_models = coverage_models, models = models, colors = colors)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)

tmp_ttl <- ""
p_total <- wis_bar + wis_line_horizon + coverage_bar + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
ggsave(filename = paste("PLOTS/ENSEMBLE/WIS_all_ensemble.jpeg", sep = ""), plot = p_total, width = 4750, height = 2000, units = c("px"), dpi = 300, bg = "white") 
saveRDS(object = p_total, file = paste("PLOTS/ENSEMBLE/WIS_all_ensemble.RDS", sep = ""))

if (FALSE) { # Plot all strata
  
  p_all    <- readRDS("PLOTS/ENSEMBLE/WIS_all_ensemble.RDS")    &  plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5, size = 18))
  p_ages   <- readRDS("PLOTS/ENSEMBLE/STRATIFIED/WIS_ages_ensemble.RDS")   &  plot_annotation(title = "Age groups")     & theme(plot.title = element_text(hjust = 0.5, size = 18))
  p_states <- readRDS("PLOTS/ENSEMBLE/STRATIFIED/WIS_states_ensemble.RDS") &  plot_annotation(title = "States")         & theme(plot.title = element_text(hjust = 0.5, size = 18))
  
  t_all    <- grid::textGrob("National level", gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  t_ages   <- grid::textGrob("Age groups",     gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  t_states <- grid::textGrob("States",         gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  
  c_all    <- (wrap_elements(panel = t_all)    / p_all)    + plot_layout(heights = c(1, 10))
  c_ages   <- (wrap_elements(panel = t_ages)   / p_ages)   + plot_layout(heights = c(1, 10))
  c_states <- (wrap_elements(panel = t_states) / p_states) + plot_layout(heights = c(1, 10))
  
  p_total2 <- wrap_elements(c_states + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) /
              wrap_elements(c_ages   + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0))))
  
  p_total3 <- wrap_elements(c_all    + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) /
              wrap_elements(c_states + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) /
              wrap_elements(c_ages   + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) + plot_layout(heights = c(1.5, 1, 1))
  
  ggsave(filename = "PLOTS/ENSEMBLE/WIS_ensemble_2.jpeg", plot = p_total2, width = 4500, height = 3066, units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = "PLOTS/ENSEMBLE/WIS_ensemble_3.jpeg", plot = p_total3, width = 4500, height = 5366, units = c("px"), dpi = 300, bg = "white") 
}


if (FALSE) {
  
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
  
  aggregate_total <- FALSE
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
  
  
  v2 <- TRUE
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
      plot_D1 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_DISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, sub_tt = "") 
      plot_D2 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_DISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, sub_tt = "") 
      plot_D3 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_DISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, sub_tt = "") 
      plot_D4 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_DISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, sub_tt = " (Averaged over horizons)") 
      plot_A1 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_AISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25, sub_tt = "") 
      plot_A2 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_AISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25, sub_tt = "") 
      plot_A3 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_AISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25, sub_tt = "") 
      plot_A4 <- plotting_quantile_weights_v2(w_hat = formatted_w_hat_AISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, extra_skip = extra_skip, y_max = 1.25, sub_tt = " (Averaged over horizons)") 
    } else { # Per horizon
      plot_D1 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_DISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, all_same = TRUE)
      plot_D2 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_DISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, all_same = TRUE)
      plot_D3 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_DISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, all_same = TRUE)
      plot_D4 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_DISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, all_same = FALSE)
      plot_A1 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_AISW_1$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25, all_same = TRUE)
      plot_A2 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_AISW_2$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25, all_same = TRUE)
      plot_A3 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_AISW_3$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25, all_same = TRUE)
      plot_A4 <- plotting_horizon_weights_v2(w_hat = formatted_w_hat_AISW_4$new_w_hat_total, r = rr, models = models_orig, colors = colors_orig, big_title = "", extra_skip = extra_skip, y_max = 1.25, all_same = FALSE)
    }
  }

  dim_f <- function (all_same, ...) {
    if (v2 == TRUE) { wi <- 4600; he <- 1375 } else { wi <- 3500; he <- 2500 }
    
    if (!aggregate_total & !indep_quant) {
      if (v2 == TRUE) { 
        if (all_same) {
          wi <- 2080 # 1533
          he <- 1250
        } else {
          wi <- 4600; he <- 3500
        }
      } else { stop("To be defined.") }
    }
    c(wi, he)
  }

  dim_same <- dim_f(all_same = TRUE )
  dim_vary <- dim_f(all_same = FALSE)
  
  ggsave(filename = paste("PLOTS/weights_D1.jpeg", sep = ""), plot = plot_D1, width = dim_same[1], height = dim_same[2], units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PLOTS/weights_D2.jpeg", sep = ""), plot = plot_D2, width = dim_same[1], height = dim_same[2], units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PLOTS/weights_D3.jpeg", sep = ""), plot = plot_D3, width = dim_same[1], height = dim_same[2], units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PLOTS/weights_D4.jpeg", sep = ""), plot = plot_D4, width = dim_vary[1], height = dim_vary[2], units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PLOTS/weights_A1.jpeg", sep = ""), plot = plot_A1, width = dim_same[1], height = dim_same[2], units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PLOTS/weights_A2.jpeg", sep = ""), plot = plot_A2, width = dim_same[1], height = dim_same[2], units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PLOTS/weights_A3.jpeg", sep = ""), plot = plot_A3, width = dim_same[1], height = dim_same[2], units = c("px"), dpi = 300, bg = "white")
  ggsave(filename = paste("PLOTS/weights_A4.jpeg", sep = ""), plot = plot_A4, width = dim_vary[1], height = dim_vary[2], units = c("px"), dpi = 300, bg = "white")
  
}
