args <- commandArgs(trailingOnly = TRUE)
state_idx        <- as.numeric(args[1])
age_idx          <- as.numeric(args[2])
ens_method       <- args[3]
skip_recent_days <- as.logical(args[4])
# post_processing  <- as.logical(args[5])
# post_select_mod  <- args[6] 

##################################################

source("header.R")
source("utils.R")
source("aux.R")

ens_method <- "wis" # c("wis", "pinball")
skip_recent_days <- FALSE

training_size <- 90
uncertain_size <- 40

exploratory_wis <- FALSE # Plotting score for all individual and naive ensemble models
ignore_naive_ensemble_data <- TRUE # Remove naive ensembles from the data objects, so the trained models do not take them as inputs

quant <- TRUE # Weights depend (or not) on the quantiles
horiz <- TRUE # Weights depend (or not) on the horizons 

post_processing <- FALSE
post_select_mod <- "Epiforecasts"

state_idx <- 17
age_idx <- 7

cluster_size <- 2

##################################################
# LOAD AND PRE-PROCESS DATA
# `state_idx` and `age_idx` must be selected 
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
# if (TRUE) {
#           data <- read_csv(file = "OTHER_DATA/data.csv.gz")
#   missing_data <- read_csv(file = "OTHER_DATA/missing.csv.gz")
#   missing_data <- missing_data |> mutate(target_end_date = forecast_date + as.numeric(gsub(" day ahead inc hosp", "", target))) |>
#                                   add_column(type = "quantile", value = NA, pathogen = "COVID-19", retrospective = FALSE) |>
#                                   select(location, age_group, forecast_date, target_end_date, target, type, quantile, value, pathogen, model, retrospective)
#   data <- rbind(data, missing_data)
# }
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

state <- unique(data$location)
state <- c(state, "DE")
state <- state[2:length(state)][state_idx]

age <- unique(data$age_group)
age <- c(age, "00+")
age <- age[2:length(age)][age_idx]

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 

if ((state == "DE") & (age == "00+")) {
  idx_missing_model <- -(1:length(models))
} else if ((state == "DE") & (age != "00+")) {
  idx_missing_model <- which(models == "RKI")
} else if ((state != "DE") & (age == "00+")) {
  idx_missing_model <- which(models == "ILM")
} else { stop("This combination of `state` and `age` does not exist.") }

models <- models[-idx_missing_model]
colors <- colors[-idx_missing_model]

ens_models <- models
ens_colors <- colors

filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

r <- range(data$forecast_date)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

# if (TRUE) {
#   avail_matrix <- readRDS(paste("OTHER_DATA/model_availability_matrix/", state, "-", age, ".RDS", sep = ""))
# }

##################################################
# UNTRAINED ENSEMBLE
# Compute the naive ensemble using `mean` and `median`
##################################################

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")

if (file.exists(naive_ensemble_file)) {
  naive_ensemble <- readRDS(file = naive_ensemble_file)
} else {
  naive_ensemble <- compute_naive_ensemble(data = data, loc = state, age_gr = age)
  saveRDS(object = naive_ensemble, file = naive_ensemble_file)
}

data <- rbind(data, naive_ensemble)
models <- c(models, "Mean", "Median")
colors <- c(colors, "#009E73", "#60D1B3")

if (post_processing) {
  idx_post <- match(x = post_select_mod, table = models) 
  if(is.na(idx_post)) { stop("Select a valid model to post-process.") }
  ens_models <- models[idx_post]
  ens_colors <- colors[idx_post]
}

##################################################
# FORMAT DATA 
# Create the objects `y`, `y_current`, `values`, and `current`
##################################################

retrieved_data_file <- paste("DATA/TRAINING/retrieved_data_training_size_", training_size, "_state_", state, "_age_", age, ".RDS", sep = "")

if (file.exists(retrieved_data_file)) {
  retrieved_data <- readRDS(file = retrieved_data_file)
} else {
  retrieved_data <- retrieve_data(data = data, truth_data = truth_data, naive_ensemble = naive_ensemble, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = 1, training_size = training_size)
  saveRDS(object = retrieved_data, file = retrieved_data_file)
}

y         <- retrieved_data$y
y_current <- retrieved_data$y_current 
values    <- retrieved_data$values
current   <- retrieved_data$current

if (post_processing) { # Select just one model (`ens_models`) for post-processing
  new_tmp_data <- process_data_ignore_models(current = current, values = values, idx_models = idx_post)

  current <- new_tmp_data$current
  values <- new_tmp_data$values
  } else {
  if (ignore_naive_ensemble_data) { # Remove `Mean` and `Median` from the data objects
    new_tmp_data <- process_data_ignore_models(current = current, values = values, idx_models = (1:length(ens_models)))
    
    current <- new_tmp_data$current
    values <- new_tmp_data$values
  }
}

if (skip_recent_days) {
  new_tmp_data <- process_data_skip_days(y = y, values = values, uncertain_size = uncertain_size)
  y      <- new_tmp_data$y
  values <- new_tmp_data$values 
}

##################################################
# COMPUTE SCORE
# Compute WIS for all models, given truth final data (exploratory analysis)
##################################################

if (exploratory_wis) {
  
  # Make all models comparable: `skip_first_days = 40 + 1`
  wis_truth <- compute_wis_truth(data = data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = (uncertain_size + 1))
  
  df_wis <- wis_truth$df_wis
  wis_summ <- wis_truth$wis_summ
  
  # Bar plot
  plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 210)
  
  # Line plot over the horizons
  df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
  plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)

}

##################################################
# COMPUTE SCORE (INCOMPLETE DATA)
# Compute WIS for all models, based on the yet-to-be-corrected nowcasts (used to compute weights)
##################################################

if (ens_method == "wis") {

  wis_file <- paste("DATA/TRAINING/WIS/wis_weights_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

  if (file.exists(wis_file)) {
    wis_training_list <- readRDS(file = wis_file)
  } else {
    wis_training_list <- compute_wis_training(data = values, truth_data = y, start_date = r[1], end_date = r[2], horizon = horizon, models = ens_models, training_size = training_size, skip_recent_days = skip_recent_days, quant = quant, horiz = horiz)
    saveRDS(object = wis_training_list, file = wis_file)
  }

  wis <- wis_training_list$wis
  wis_avg <- wis_training_list$wis_avg

  if (!horiz) {
    weights <- compute_weights(wis = wis)
  } else {
    weights <- lapply(X = wis, FUN = compute_weights)

    # Line plot over the horizons for training
    df_wis_horizon_training <- compute_wis_horizon_training(wis_avg = wis_avg, models = ens_models, quant = quant)
    plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_training, models = ens_models, colors = colors, quant = quant, legend = TRUE)
  }
}

##################################################
# FIT THE MODEL
# Estimate scale and weight parameters and compute new nowcasts (based on different methods)
##################################################

new_data <- create_new_tibble()

skip_first_days <- ifelse(skip_recent_days, uncertain_size + 1, 1)
days <- seq(r[1] + skip_first_days, r[2], by = "1 day")

ensemble <- list()
count <- 1

if (ens_method == "pinball") {
  cl <- makeCluster(cluster_size)
  registerDoParallel(cl = cl)
  clusterExport(cl, c("cost_function", "par_weights_scale", "compute_wis", "mpfr"), envir = environment()) # Include exported functions
}

for (k in 1:length(days)) {
  dt <- days[k]
  print(paste(dt, " (", sprintf("%03d", count), "/", sprintf("%03d", length(days)), ")", sep = ""))

  if (horiz | (ens_method == "wis")) {
    ensemble[[as.character(dt)]] <- list()

    if (horiz) { b <- txtProgressBar(min = 1, max = length(horizon), initial = 1) }
    for (i in 1:length(horizon)) {
      h <- horizon[i]

      current_ens <- current[[as.character(dt)]][[as.character(h)]][[1]]
      values_ens  <- values[[as.character(dt)]][[as.character(h)]]

      # Analyze `NaN` values
      # TBD

      if (ens_method == "wis") {
        if (horiz) {
          tmp_result <- compute_ensemble(ens_method = ens_method, current = current_ens, weights = weights[[as.character(h)]], k = k)
        } else {
          tmp_result <- compute_ensemble(ens_method = ens_method, current = current_ens, weights = weights, k = k)
        }
      } else if (ens_method == "pinball") {
        tmp_result <- compute_ensemble(ens_method = ens_method,
                                       y = y[[as.character(dt)]][[as.character(h)]],
                                       y_current = y_current[[as.character(dt)]][[as.character(h)]],
                                       values = values_ens,
                                       current = current_ens,
                                       ens_models = ens_models,
                                       lower = -5,
                                       upper =  5,
                                       quant = quant,
                                       horiz = horiz,
                                       probs = probs,
                                       short_grid_search = TRUE, ######
                                       by = 0.01)
      } else { stop("Choose a valid ensemble method.") }

      ensemble[[as.character(dt)]][[as.character(h)]] <- tmp_result

      # UPDATE `input_new_data()` to allow for the horizon independent estimations
      # Input the new data to the `new_data` tibble
      new_data <- input_new_data(new_data = new_data, ensemble = ensemble, ens_method = ens_method, h = h, state = state, age = age, day = dt, probs = probs)

      if (horiz) { setTxtProgressBar(b, i) }
    }
    if (horiz) { close(b) }
  } else {

    current_ens <- current[[as.character(dt)]]
    values_ens  <- values[[as.character(dt)]]

    if (ens_method == "pinball") {
      tmp_result <- compute_ensemble(ens_method = ens_method,
                                     y = y[[as.character(dt)]],
                                     y_current = y_current[[as.character(dt)]],
                                     values = values_ens,
                                     current = current_ens,
                                     ens_models = ens_models,
                                     lower = -5,
                                     upper =  5,
                                     quant = quant,
                                     horiz = horiz,
                                     probs = probs,
                                     short_grid_search = TRUE,
                                     by = 0.01)
    }  else { stop("Choose a valid ensemble method.") }

    ensemble[[as.character(dt)]] <- tmp_result

    # Input the new data to the `new_data` tibble
    new_data <- input_new_data(new_data = new_data, ensemble = ensemble, ens_method = ens_method, h = h, state = state, age = age, day = dt, horizon = horizon, probs = probs, horiz = horiz)

  }

  count <- count + 1
}

if (ens_method == "pinball") { unregister_dopar(); stopCluster(cl) }

fitted_model_file <- ifelse(post_processing,
                            paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/post-processing_model_", ens_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""),
                            paste("RESULTS/FITTED_OBJECTS/method_", ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))
saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = fitted_model_file)
