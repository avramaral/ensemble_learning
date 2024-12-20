##################################################
###### POST-PROCESSING ###########################
##################################################

# args <- commandArgs(trailingOnly = TRUE)
# skip_recent_days <- as.logical(args[1])
# horiz            <- as.logical(args[2])
# post_select_mod  <- as.character(args[3])
# method           <- as.character(args[4])
# cluster_size     <- as.numeric(args[5])

##################################################

##################################################
###### ENSEMBLE ##################################
##################################################

# args <- commandArgs(trailingOnly = TRUE)
# skip_recent_days <- as.logical(args[1])
# horiz            <- as.logical(args[2])
# method           <- as.character(args[3])
# cluster_size     <- as.numeric(args[4])

##################################################
###### HIGHEST RANKED ############################
##################################################

# args <- commandArgs(trailingOnly = TRUE)
# horiz             <- as.logical(args[1])
# n_ensemble_models <- as.numeric(args[2])
# unweighted_method <- as.character(args[3])

##################################################

##################################################
###### STRATIFIED ANALYSIS #######################
##################################################

# args <- commandArgs(trailingOnly = TRUE)
# ens_method   <- as.character(args[1])
# horiz        <- as.logical(args[2])
# strata       <- as.character(args[3])
# cluster_size <- as.numeric(args[4])

##################################################

source("header.R")
source("utils.R")
source("aux.R")

ens_method <- "wis" # c("wis", "pinball", "ranked_unweighted")
skip_recent_days <- FALSE # c(TRUE, FALSE)

training_size <- 90
uncertain_size <- 40

exploratory_wis <- FALSE # Plotting score for all individual and naive ensemble models
ignore_naive_ensemble_data <- TRUE # Remove naive ensembles from the data objects, so the trained models do not take them as inputs

quant <- TRUE # Weights depend (or not) on the quantiles
horiz <- TRUE # Weights depend (or not) on the horizons

post_processing <- FALSE
post_select_mod <- "KIT"

method <- "Mean" # c("Mean", "Median", "all_quant") # How to summarize the recent past

strata <- "all" # c("states", "ages", "all")

if (strata == "states") {
  state_idx <- 1:16
  age_idx <- 7
  cluster_size <- 64
  by <- 0.05
} else if (strata == "ages") {
  state_idx <- 17
  age_idx <- 1:6
  cluster_size <- 32
  by <- 0.025
} else {
  state_idx <- 17 
  age_idx <- 7  
  by <- 0.025
}

########################
# Ranked unweighted pars
n_ensemble_models <- 1 # 1:8
unweighted_method <- "Mean" # c("Mean", "Median")
########################

reparameterize <- TRUE # Model the difference if `TRUE`
cluster_size <- 8

##################################################
# LOAD AND PRE-PROCESS DATA
# `state_idx` and `age_idx` must be selected 
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

if ((length(state) == 1) & (length(age) == 1)) {
  if ((state == "DE") & (age == "00+")) {
    idx_missing_model <- -(1:length(models))
  } else if ((state == "DE") & (age != "00+")) {
    idx_missing_model <- which(models == "RKI")
  } else if ((state != "DE") & (age == "00+")) {
    idx_missing_model <- which(models == "ILM")
  } else { stop("This combination of `state` and `age` does not exist.") }
} else {
  if (length(age) != 1) {
    idx_missing_model <- which(models == "RKI")
  } else if (length(state) != 1) {
    idx_missing_model <- which(models == "ILM")
  } else { stop("Error.") }
}
 
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

KIT_frozen_baseline <- KIT_frozen_baseline %>% filter(forecast_date >= r[1], forecast_date <= r[2], age_group %in% age, location %in% state)
baseline <- KIT_frozen_baseline
if (length(state) == 16) { baseline <- fix_baseline(baseline) }

##################################################
# UNTRAINED ENSEMBLE
# Compute the naive ensemble using `mean` and `median`
##################################################

naive_ensemble_files <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")

if (length(naive_ensemble_files) == 1) {
  naive_ensemble_file <- naive_ensemble_files[1]
  
  if (file.exists(naive_ensemble_file)) {
    naive_ensemble <- readRDS(file = naive_ensemble_file)
  } else {
    naive_ensemble <- compute_naive_ensemble(data = data, loc = state, age_gr = age)
    saveRDS(object = naive_ensemble, file = naive_ensemble_file)
  }
  
  data <- rbind(data, naive_ensemble)
  models <- c(models, "Mean", "Median")
  colors <- c(colors, "#009E73", "#60D1B3")
  
  if (post_processing) { # Allows post-processing
    idx_post <- match(x = post_select_mod, table = models) 
    if(is.na(idx_post)) { stop("Select a valid model to post-process.") }
    ens_models <- models[idx_post]
    ens_colors <- colors[idx_post]
  }
  
} else { # Does not allow for post-processing
  
  for (i in 1:length(naive_ensemble_files)) {
    naive_ensemble_file <- naive_ensemble_files[i]
    
    if (file.exists(naive_ensemble_file)) { tmp_naive_ensemble <- readRDS(file = naive_ensemble_file) } else { stop("Create files first.") }

    if (i == 1) {
      naive_ensemble <- tmp_naive_ensemble
    } else {
      naive_ensemble <- rbind(naive_ensemble, tmp_naive_ensemble)
    }
  }
  
  data <- rbind(data, naive_ensemble)
  models <- c(models, "Mean", "Median")
  colors <- c(colors, "#009E73", "#60D1B3")
}

##################################################
# FORMAT DATA 
# Create the objects `y`, `y_current`, `values`, and `current`
##################################################

method_files <- ifelse(method == "all_quant", "all_quant_", "")
retrieved_data_files <- paste("DATA/TRAINING/retrieved_data_", method_files, "training_size_", training_size, "_state_", state, "_age_", age, ".RDS", sep = "")

if (length(retrieved_data_files) == 1) {
  retrieved_data_file <- retrieved_data_files[1]
  
  if (file.exists(retrieved_data_file)) {
    retrieved_data <- readRDS(file = retrieved_data_file)
  } else {
    retrieved_data <- retrieve_data(data = data, truth_data = truth_data, naive_ensemble = naive_ensemble, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = 1, training_size = training_size, method = method)
    saveRDS(object = retrieved_data, file = retrieved_data_file)
  }
  
  y         <- retrieved_data$y
  y_current <- retrieved_data$y_current 
  values    <- retrieved_data$values
  current   <- retrieved_data$current
  
  if (reparameterize) { # Reparameterize the model
    
    new_retrieved_data_file <- paste("DATA/TRAINING/new_retrieved_data_", method_files, "training_size_", training_size, "_state_", state, "_age_", age, ".RDS", sep = "")
    if (!file.exists(new_retrieved_data_file)) {
      
      if (method == "all_quant") { tmp_method <- method } else { tmp_method <- NULL }
      new_retrieved_data <- reparameterize_model(y = y, y_current = y_current, value = values, current = current, baseline = baseline, method = tmp_method) 
      
      saveRDS(object = new_retrieved_data, file = new_retrieved_data_file)
    } else {
      new_retrieved_data <- readRDS(file = new_retrieved_data_file)
    }
    
    y         <- new_retrieved_data$y
    y_current <- new_retrieved_data$y_current
    values    <- new_retrieved_data$values
    current   <- new_retrieved_data$current
    
  }
  
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
    new_tmp_data <- process_data_skip_days(y = y, values = values, uncertain_size = uncertain_size, method = method)
    y      <- new_tmp_data$y
    values <- new_tmp_data$values 
  }
  
} else { # Does not allow for post-processing (if so, checking the indexing--since there are missing models)
 
  y <- list()
  y_current <- list()
  values <- list()
  current <- list()
  
  for (i in 1:length(retrieved_data_files)) {
    print(paste(sprintf("%02d", i), " out of ", sprintf("%02d", length(retrieved_data_files)), sep = ""))
    
    retrieved_data_file <- retrieved_data_files[i]
    
    if (file.exists(retrieved_data_file)) { retrieved_data <- readRDS(file = retrieved_data_file) } else { stop("Create files first.") }
    
    y[[i]]         <- retrieved_data$y
    y_current[[i]] <- retrieved_data$y_current 
    values[[i]]    <- retrieved_data$values
    current[[i]]   <- retrieved_data$current
    
    if (reparameterize) { # Reparameterize the model
      
      parts_path <- strsplit(retrieved_data_file, "/")[[1]]
      new_retrieved_data_file <- paste(parts_path[1], "/", parts_path[2], "/new_", parts_path[3] ,sep = "")
      
      if (!file.exists(new_retrieved_data_file)) {
        
        if (length(state) > 1) {
          tmp_state <- state[i]
          tmp_age   <- age
        } else if (length(age) > 1) {
          tmp_state <- state
          tmp_age   <- age[i]
        } else { stop("Error.") }
        
        new_retrieved_data <- reparameterize_model(y = y[[i]], y_current = y_current[[i]], values = values[[i]], current = current[[i]], baseline = baseline, state = tmp_state, age = tmp_age) 
      
        saveRDS(object = new_retrieved_data, file = new_retrieved_data_file)
      } else {
        new_retrieved_data <- readRDS(file = new_retrieved_data_file)
      }
      
      y[[i]]         <- new_retrieved_data$y
      y_current[[i]] <- new_retrieved_data$y_current
      values[[i]]    <- new_retrieved_data$values
      current[[i]]   <- new_retrieved_data$current
      
    }
    
    if (ignore_naive_ensemble_data) { 
      new_tmp_data <- process_data_ignore_models(current = current[[i]], values = values[[i]], idx_models = (1:length(ens_models)))
      
      current[[i]] <- new_tmp_data$current
      values[[i]] <- new_tmp_data$values
    }

    
    if (skip_recent_days) {
      new_tmp_data <- process_data_skip_days(y = y[[i]], values = values[[i]], uncertain_size = uncertain_size)
      y[[i]]      <- new_tmp_data$y
      values[[i]] <- new_tmp_data$values 
    }
    
  }
  
  if (length(y) == 16) { tmp_names <- state } else if (length(y) == 6) { tmp_names <- age } else { tmp_names <- NULL }
  if (!is.null(tmp_names)) { names(y) <- names(y_current) <- names(values) <- names(current) <- tmp_names }
   
}

##################################################
# COMPUTE SCORE
# Compute WIS for all models, given truth final data (exploratory analysis)
##################################################

exploratory_wis <- TRUE
if (exploratory_wis) {
  
  if (strata == "all") { 
    tmp_models <- models[1:8]; tmp_colors <- colors[1:8] 
    tmp_models <- models;      tmp_colors <- colors
  } else { 
    tmp_models <- models[1:7]; tmp_colors <- colors[1:7] 
    tmp_models <- models[1:9]; tmp_colors <- colors
  }
  
  # Make all models comparable: `skip_first_days = 40 + 1`
  skip_first_days <- (uncertain_size + 1) + 30
  wis_truth <- compute_wis_truth(data = data, truth_data = truth_data, models = tmp_models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)

  df_wis <- wis_truth$df_wis
  wis_summ <- wis_truth$wis_summ

  if (TRUE) { tmp_add <- "full_" }
  coverage_file <- paste("DATA/TRAINING/COVERAGE/coverage_size_", tmp_add, training_size, "_skip_", as.character(skip_recent_days), "_strata_", strata, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
  
  if (file.exists(coverage_file)) {
    coverage_models <- readRDS(file = coverage_file)
  } else {
    coverage_models <- compute_coverage(data = data, truth_data = truth_data, models = tmp_models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)
    saveRDS(object = coverage_models, file = coverage_file)
  }
  
  
  ############################################
  # Compute WIS for the baseline model (NEW) #
  ############################################
  
  # Filtering is based on the `ensemble_data` object
  tmp_baseline_data <- KIT_frozen_baseline %>% filter(location %in% unique(data$location), age_group %in% unique(data$age_group), forecast_date >= range(data$forecast_date)[1], forecast_date <= range(data$forecast_date)[2])
  
  wis_baseline <- compute_wis_truth(data = tmp_baseline_data, truth_data = truth_data, models = "KIT-frozen_baseline", horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)
  df_wis_baseline <- wis_baseline$df_wis
  
  ############################################
  
  # Bar plot
  reference_pts <- df_wis %>% group_by(model) %>% mutate(total = sum(wis)) %>% ungroup() %>% select(total) %>% c() %>% unlist() %>% unname() %>% unique() %>% round(2)
  reference_pts_50 <- coverage_models$coverage_50 %>% unlist() %>% round(3) %>% unname()
  reference_pts_95 <- coverage_models$coverage_95 %>% unlist() %>% round(3) %>% unname()
  if (strata == "all") {
    # reference_pts <- c(182.23, 175.92, 125.06, 135.40, 93.67, 137.85, 143.98, 209.49, 81.95, 80.76)
    # reference_pts_50 <- c(0.17, 0.19, 0.71, 0.09, 0.20, 0.24, 0.27, 0.18, 0.42, 0.37) # 
    # reference_pts_95 <- c(0.49, 0.66, 1.00, 0.27, 0.53, 0.55, 0.65, 0.41, 0.90, 0.75) # 
    wis_bar <- plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = tmp_models, colors = tmp_colors, ylim_manual = 220, skip_space = FALSE, df_wis_baseline = df_wis_baseline)
    coverage_bar <- plot_coverage(coverage_models = coverage_models, models = tmp_models, colors = tmp_colors)
  } else {
    # Ages
    # reference_pts <- c(28.56, 31.77, 22.99, 30.98, 20.60, 23.59, 39.53, 16.28, 16.69)
    # reference_pts_50 <- c(0.327, 0.333, 0.646, 0.145, 0.232, 0.337, 0.192)
    # reference_pts_95 <- c(0.756, 0.760, 0.987, 0.410, 0.623, 0.764, 0.450)
    # States 
    # reference_pts <- c(14.99, 12.84, 15.26, 13.89, 17.02, 13.83, 20.37, 10.97, 11.19)
    # reference_pts_50 <- c(0.378, 0.575, 0.301, 0.309, 0.156, 0.368, 0.197)
    # reference_pts_95 <- c(0.781, 0.956, 0.658, 0.662, 0.316, 0.776, 0.456)
    if (strata == "states") { ylim_manual <- 25 } else if (strata == "ages") { ylim_manual <- 40 }
    wis_bar <- plot_wis_bar_stratified_simplified(df_wis = df_wis, wis_summ = wis_summ, models = tmp_models, colors = tmp_colors, ylim_manual = ylim_manual, skip_space = FALSE, df_wis_baseline = df_wis_baseline)
    coverage_bar <- plot_coverage_stratified(coverage_models = coverage_models, models = tmp_models, colors = tmp_colors)
  }

  # Line plot over the horizons
  df_wis_horizon_truth <- compute_wis_horizon_truth(models = tmp_models, horizon = horizon, wis_summ = wis_summ)
  wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = tmp_models, colors = tmp_colors)

  tmp_ttl <- "" # "WIS (original models)"
  p_total <- wis_bar + wis_line_horizon + coverage_bar + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
  saveRDS(object = p_total, file = paste("PLOTS/WIS_", tmp_add, strata, "_models.RDS", sep = ""))
  ggsave(filename = paste("PLOTS/WIS_", tmp_add, strata, "_models.jpeg", sep = ""), plot = p_total, width = 4600, height = 1500, units = c("px"), dpi = 300, bg = "white") 

  if (TRUE) { # Plot all strata

    p_all    <- readRDS(paste("PLOTS/WIS_", tmp_add, "all_models.RDS",    sep = "")) &  plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5, size = 18))
    p_ages   <- readRDS(paste("PLOTS/WIS_", tmp_add, "ages_models.RDS",   sep = "")) &  plot_annotation(title = "Age groups")     & theme(plot.title = element_text(hjust = 0.5, size = 18))
    p_states <- readRDS(paste("PLOTS/WIS_", tmp_add, "states_models.RDS", sep = "")) &  plot_annotation(title = "States")         & theme(plot.title = element_text(hjust = 0.5, size = 18))
    
    t_all    <- grid::textGrob("National level", gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
    t_ages   <- grid::textGrob("Age groups",     gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
    t_states <- grid::textGrob("States",         gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
     
    c_all    <- (wrap_elements(panel = t_all)    / p_all)    + plot_layout(heights = c(1, 10))
    c_ages   <- (wrap_elements(panel = t_ages)   / p_ages)   + plot_layout(heights = c(1, 10))
    c_states <- (wrap_elements(panel = t_states) / p_states) + plot_layout(heights = c(1, 10))
    
    p_total3 <-   wrap_elements(c_all    + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) /
                  wrap_elements(c_states + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) /
                  wrap_elements(c_ages   + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0))))
    ggsave(filename = paste("PLOTS/WIS_", tmp_add, "3.jpeg", sep = ""), plot = p_total3, width = 4700, height = 4600, units = c("px"), dpi = 300, bg = "white") 
  }
}

##################################################
# COMPUTE SCORE (INCOMPLETE DATA)
# Compute WIS for all models, based on the yet-to-be-corrected nowcasts (used to compute weights)
##################################################

if (ens_method == "wis") {

  method_files <- ifelse(method == "all_quant", "all_quant_", "")
  wis_files <- paste("DATA/TRAINING/WIS/wis_weights_", method_files, "size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

  if (length(wis_files) == 1) {
    wis_file <- wis_files[1]
    
    if (file.exists(wis_file)) {
      wis_training_list <- readRDS(file = wis_file)
    } else {
      wis_training_list <- compute_wis_training(data = values, truth_data = y, start_date = r[1], end_date = r[2], horizon = horizon, models = ens_models, training_size = training_size, skip_recent_days = skip_recent_days, quant = quant, horiz = horiz, method = method)
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
    
  } else { # Stratified analysis (there is no implementation to use all quantiles for the recent past)
    
    wis <- list()
    wis_avg <- list()
    
    for (i in 1:length(wis_files)) {
      wis_file <- wis_files[i]
      
      if (file.exists(wis_file)) { wis_training_list <- readRDS(file = wis_file) } else { stop("Create files first.") }
      
      wis[[i]] <- wis_training_list$wis
      wis_avg[[i]] <- wis_training_list$wis_avg
      
    }
    
    cmb_wis <- NULL
    if (is.list(wis[[1]])) {
      
      H <- length(wis[[i]])
      cmb_wis <- list()
      
      for (i in 1:length(wis_files)) {
        for (h in 1:H) {
          if (i == 1) {
            cmb_wis[[as.character(horizon[h])]] <- wis[[i]][[as.character(horizon[h])]]
          } else {
            cmb_wis[[as.character(horizon[h])]] <- cmb_wis[[as.character(horizon[h])]] + wis[[i]][[as.character(horizon[h])]]
          }
        }
      }
      for (i in 1:length(wis_files)) { cmb_wis[[as.character(horizon[h])]] <- (cmb_wis[[as.character(horizon[h])]] / length(wis_files)) }
      
    } else { # If `wis[[i]]` is not a list; i.e., if the weights are common across horizons
      
      S <- length(wis)
      for (s in 1:S) {
        if (s == 1) {
          cmb_wis <- wis[[1]]
        } else {
          cmb_wis <- cmb_wis + wis[[i]]
        }
      }
    }
    
    wis <- cmb_wis
    
    if (!horiz) {
      weights <- compute_weights(wis = wis)
    } else {
      weights <- lapply(X = wis, FUN = compute_weights)
    }
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
  clusterExport(cl, c("cost_function", "par_weights_scale", "compute_wis", "mpfr", "elementwise_avg", "elementwise_avg_3d", "quantile_distance"), envir = environment()) # Include exported functions
}

for (k in 1:length(days)) {
  dt <- days[k]
  print(paste(dt, " (", sprintf("%03d", count), "/", sprintf("%03d", length(days)), ")", sep = ""))

  if (horiz | (ens_method == "wis")) {
    ensemble[[as.character(dt)]] <- list()

    if (horiz) { b <- txtProgressBar(min = 1, max = length(horizon), initial = 1) }
    for (i in 1:length(horizon)) {
      h <- horizon[i]

      if ((length(state) > 1) | length(age) > 1) { # Stratified analysis
        
        n_strata <- max(length(state), length(age))
        if (n_strata == 16) { idx_strata <- state } else if (n_strata == 6) { idx_strata <- age }
        
        current_ens   <- list()
        values_ens    <- list()
        y_tmp         <- list()
        y_current_tmp <- list()
        
        for (j in 1:n_strata) {
          current_ens[[as.character(idx_strata[j])]]   <- current[[as.character(idx_strata[j])]][[as.character(dt)]][[as.character(h)]][[1]]
          values_ens[[as.character(idx_strata[j])]]    <-  values[[as.character(idx_strata[j])]][[as.character(dt)]][[as.character(h)]]
          y_tmp[[as.character(idx_strata[j])]]         <- y[[as.character(idx_strata[j])]][[as.character(dt)]][[as.character(h)]]
          y_current_tmp[[as.character(idx_strata[j])]] <- y_current[[as.character(idx_strata[j])]][[as.character(dt)]][[as.character(h)]]
        }
      } else { # National level
        current_ens   <- current[[as.character(dt)]][[as.character(h)]][[1]]
        values_ens    <-  values[[as.character(dt)]][[as.character(h)]]
        y_tmp         <- y[[as.character(dt)]][[as.character(h)]]
        y_current_tmp <- y_current[[as.character(dt)]][[as.character(h)]]
      }

      if (ens_method == "wis") {
        if (horiz) {
          tmp_result <- compute_ensemble(ens_method = ens_method, current = current_ens, weights = weights[[as.character(h)]], k = k)
        } else {
          tmp_result <- compute_ensemble(ens_method = ens_method, current = current_ens, weights = weights, k = k)
        }
      } else if (ens_method %in% c("pinball", "ranked_unweighted")) {
        tmp_result <- compute_ensemble(ens_method = ens_method,
                                       y = y_tmp,
                                       y_current = y_current_tmp,
                                       values = values_ens,
                                       current = current_ens,
                                       ens_models = ens_models,
                                       lower = -10,
                                       upper =  10,
                                       quant = quant,
                                       horiz = horiz,
                                       probs = probs,
                                       short_grid_search = TRUE, 
                                       by = by,
                                       method = method,
                                       n_ensemble_models = n_ensemble_models,
                                       unweighted_method = unweighted_method)
      } else { stop("Choose a valid ensemble method.") }

      ensemble[[as.character(dt)]][[as.character(h)]] <- tmp_result

      # Input the new data to the `new_data` tibble
      new_data <- input_new_data(new_data = new_data, ensemble = ensemble, ens_method = ens_method, h = h, state = state, age = age, day = dt, probs = probs)

      if (horiz) { setTxtProgressBar(b, i) }
    }
    
    if (!horiz) { ensemble[[as.character(dt)]] <- format_ensemble_wis(ensemble = ensemble[[as.character(dt)]], state = state, age = age) }
    
    if (horiz) { close(b) }
  } else {
    
    if ((length(state) > 1) | length(age) > 1) { # Stratified analysis
      
      n_strata <- max(length(state), length(age))
      if (n_strata == 16) { idx_strata <- state } else if (n_strata == 6) { idx_strata <- age }
      
      current_ens   <- list()
      values_ens    <- list()
      y_tmp         <- list()
      y_current_tmp <- list()
      
      for (j in 1:n_strata) {
        current_ens[[as.character(idx_strata[j])]]   <- current[[as.character(idx_strata[j])]][[as.character(dt)]]
        values_ens[[as.character(idx_strata[j])]]    <- values[[as.character(idx_strata[j])]][[as.character(dt)]]
        y_tmp[[as.character(idx_strata[j])]]         <- y[[as.character(idx_strata[j])]][[as.character(dt)]]
        y_current_tmp[[as.character(idx_strata[j])]] <- y_current[[as.character(idx_strata[j])]][[as.character(dt)]]
      }
    } else { # National level
      current_ens   <- current[[as.character(dt)]]
      values_ens    <- values[[as.character(dt)]]
      y_tmp         <- y[[as.character(dt)]]
      y_current_tmp <- y_current[[as.character(dt)]]
    }    

    if (ens_method %in% c("pinball", "ranked_unweighted")) {
      tmp_result <- compute_ensemble(ens_method = ens_method,
                                     y = y_tmp,
                                     y_current = y_current_tmp,
                                     values = values_ens,
                                     current = current_ens,
                                     ens_models = ens_models,
                                     lower = -10,
                                     upper =  10,
                                     quant = quant,
                                     horiz = horiz,
                                     probs = probs,
                                     short_grid_search = TRUE,
                                     by = by,
                                     method = method,
                                     n_ensemble_models = n_ensemble_models,
                                     unweighted_method = unweighted_method)
    }  else { stop("Choose a valid ensemble method.") }

    ensemble[[as.character(dt)]] <- tmp_result

    # Input the new data to the `new_data` tibble
    new_data <- input_new_data(new_data = new_data, ensemble = ensemble, ens_method = ens_method, h = h, state = state, age = age, day = dt, horizon = horizon, probs = probs, horiz = horiz)

  }

  count <- count + 1
}

if (ens_method == "pinball") { unregister_dopar(); stopCluster(cl) }

reparameterize_file <- ifelse(reparameterize, "new_", "")
ranked_file <- ifelse(ens_method == "ranked_unweighted", paste("_", unweighted_method, "_", n_ensemble_models, sep = ""), "")

######################################################
# Backup for non-processed `new_data` and `ensemble` #
######################################################
if ((length(state) != 1) | (length(age) != 1)) {
  if (length(state) != 1) {
    saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = paste("RESULTS/FITTED_OBJECTS/BACKUP_", reparameterize_file, "ALL_STATES_method_", ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))
  } else if (length(age) != 1) {
    saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = paste("RESULTS/FITTED_OBJECTS/BACKUP_", reparameterize_file, "ALL_AGES_method_",   ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))
  }
}
######################################################
######################################################

lim_days <- c(days[1], days[length(days)])
baseline <- baseline %>% filter(type == "quantile", forecast_date >= lim_days[1], forecast_date <= lim_days[2])
new_data <- add_baseline_new_data(new_data = new_data, baseline = baseline, reparameterize = reparameterize)
if ((length(state) > 1) | length(age) > 1) { # Stratified (only needed due to `wis`)
  ensemble <- add_baseline_ensemble(ensemble = ensemble, baseline = baseline, reparameterize = reparameterize, horiz = horiz)
} else {
  ensemble <- add_baseline_ensemble(ensemble = ensemble, baseline = baseline, reparameterize = reparameterize, horiz = (horiz | (ens_method == "wis")))
}

if ((length(state) == 1) & (length(age) == 1)) {
  fitted_model_file <- ifelse(post_processing,
                              paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/", reparameterize_file, method_files, "post-processing_model_", ens_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""),
                              paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, method_files, "method_", ens_method, ranked_file, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))
  saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = fitted_model_file)
} else {
  if (length(state) != 1) {
    saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "ALL_STATES_method_", ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))
  } else if (length(age) != 1) {
    saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "ALL_AGES_method_",   ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))
  }
} 
