source("header.R")
source("utils.R")
source("aux.R")

ens_method <- "pinball" # DO NOT CHANGE IT. `post-processed` must use `pinball` 
skip_recent_days <- FALSE # Always `FALSE`

training_size <- 90
uncertain_size <- 40

quant <- TRUE
horiz <- TRUE 

state_idx <- 17
age_idx <- 7

##################################################
# LOAD AND PRE-PROCESS DATA (ALSO POST-PROCESSED DATA)
# `state_idx` and `age_idx` must be selected (if such data exist)
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

state <- unique(data$location)
state <- c(state, "DE")
state <- state[2:length(state)][state_idx]

age <- unique(data$age_group)
age <- c(age, "00+")
age <- age[2:length(age)][age_idx]

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 
ens_models <- models
ens_colors <- colors

filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

models <- c(models, "Mean", "Median")
colors <- c(colors, "#009E73", "#60D1B3")

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
naive_ensemble <- readRDS(file = naive_ensemble_file)
data <- rbind(data, naive_ensemble)

##################################################

count <- 1
for (m in models) {
  tmp_postprocessed_file <- paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/post-processing_model_", m, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
  tmp_postprocessed_data <- readRDS(file = tmp_postprocessed_file)
  tmp_postprocessed_data <- tmp_postprocessed_data$new_data 
  tmp_postprocessed_data$model <- m
  
  if (count == 1) {
    postprocessed_data <- tmp_postprocessed_data
  } else {
    postprocessed_data <- rbind(postprocessed_data, tmp_postprocessed_data)
  }
  
  count <- count + 1
}
cp_postprocessed_data <- postprocessed_data

r <- range(postprocessed_data$forecast_date)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# UNTRAINED ENSEMBLE
# Compute the naive ensemble using `mean` and `median` for the post-processed individual models
##################################################

postprocessed_naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/POST_PROCESSED/postprocessed_naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")

if (file.exists(postprocessed_naive_ensemble_file)) {
  postprocessed_naive_ensemble <- readRDS(file = postprocessed_naive_ensemble_file)
} else {
  tmp_postprocessed_data <- postprocessed_data[!(postprocessed_data$model %in% c("Mean", "Median")), ]
  
  postprocessed_naive_ensemble <- compute_naive_ensemble(data = tmp_postprocessed_data, loc = state, age_gr = age)
  saveRDS(object = postprocessed_naive_ensemble, file = postprocessed_naive_ensemble_file)
}

postprocessed_data <- postprocessed_data[!(postprocessed_data$model %in% c("Mean", "Median")), ]
postprocessed_data <- rbind(postprocessed_data, postprocessed_naive_ensemble)

##################################################
# COMPUTE SCORE
# Compute WIS for all post-processed models, given truth final data
##################################################

# Make all models comparable: `skip_first_days = 40` (`+ 1` excluded, since the range for the post-processes data is different)
wis_truth <- compute_wis_truth(data = postprocessed_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = uncertain_size)

df_wis <- wis_truth$df_wis
wis_summ <- wis_truth$wis_summ

# Bar plot
plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 200)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)

##################################################
# PLOT OTHER POST-PROCESSED MODELS
# Must set `hh` to the horizon for the left-most plot
##################################################

hh <- 0 # Horizon for the left-most plot

postprocessed_plots_file <- paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/PLOTS/postprocessed_plots_hh_", hh, "_state_", state, "_age_", age, ".RDS", sep = "")

if (file.exists(postprocessed_plots_file)) {
  postprocessed_plots <- readRDS(file = postprocessed_plots_file)
} else {
  postprocessed_plots <- list()
  for (i in 1:length(models)) {
    model <- models[i]
    color <- colors[i]
    
    print(paste("Model: ", model, " (", sprintf("%02d", i), "/", sprintf("%02d", length(models)), ").", sep = ""))
    postprocessed_plots[[i]] <- plot_postprocessed_models(data = data, nowcasts = cp_postprocessed_data, truth_data = truth_data, model = model, r = r, training_size = training_size, uncertain_size = uncertain_size, hh = hh, ens_method = ens_method)
  }

  saveRDS(object = postprocessed_plots, file = postprocessed_plots_file)
}
