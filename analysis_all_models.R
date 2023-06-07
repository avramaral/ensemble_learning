source("header.R")
source("utils.R")
source("aux.R")

###########################################################################
# LOAD THE NECESSARY DATA FILES AND SET BASIC QUANTITIES
###########################################################################

skip_40 <- FALSE

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

naive_ensemble <- readRDS(file = "TMP/naive_ensemble.RDS")

filtered_data <- filter_data(data = data, truth_data = truth_data, loc = "DE", age_gr = "00+", extra_delay = 7)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 
r <- range(data$forecast_date)

data <- data |> filter(model %in% models)
data <- rbind(data, naive_ensemble)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

y         <- readRDS(file = "TMP/y.RDS")
y_current <- readRDS(file = "TMP/y_current.RDS") 
values    <- readRDS(file = "TMP/values.RDS")
current   <- readRDS(file = "TMP/current.RDS")

###########################################################################
# ESTIMATE THE WEIGHTS AND COMPUTE THE NEW ENSEMBLE NOWCASTS
###########################################################################

# Create new data frame for saving the newly generated nowcasts (RE-RUN every time you compute the new nowcast, so you can start with an empty data frame)
new_data <- data.frame(location = character(), age_group = character(), forecast_date = as.Date(character()), target_end_date = as.Date(character()), target = character(), type = character(), quantile = double(), value = double(), pathogen = character(), model = character(), retrospective = logical(), stringsAsFactors = FALSE)
new_data <- as_tibble(new_data)

method <- "pinball" # c("mean", "median", "wis", "pinball")
quant <- TRUE # Weights estimated independently for quantile

# c(122, 0)
skip_days <- c(32, 0) # Number of skipped days in the beginning and the end of the total time range, respectively
days <- seq(r[1] + skip_days[1], r[2] - skip_days[2], by = "1 day") 

# horizon <- c(0) ##################
ensemble <- list()
count <- 1

cl <- makeCluster(round(detectCores() * 0.1, 0))
registerDoParallel(cl = cl)
clusterExport(cl, c("cost_function", "par_weights_scale", "cost_function_weights_ind", "compute_wis", "mpfr"), envir = environment())

for (k in 1:length(days)) { #length(days)) { 
  dt <- days[k]
  ensemble[[as.character(dt)]] <- list()
  
  print(paste(dt, " (", sprintf("%03d", count), "/", sprintf("%03d", length(days)), ")", sep = ""))
  b <- txtProgressBar(min = 1, max = length(horizon), initial = 1) 
  
  for (i in 1:length(horizon)) {
    h <- horizon[i]
    
    # Estimate the weights and theta, and compute the nowcasts based on the current values
    ensemble[[as.character(dt)]][[as.character(h)]] <- compute_ensemble(values = values[[as.character(dt)]][[as.character(h)]],
                                                                        current = current[[as.character(dt)]][[as.character(h)]],
                                                                        y = y[[as.character(dt)]][[as.character(h)]],
                                                                        y_current = y_current[[as.character(dt)]][[as.character(h)]],
                                                                        method = method,
                                                                        lower = -10, # -5 
                                                                        upper = 10, 
                                                                        quant = quant, # Estimate quantities for the quantiles independently
                                                                        models = models, 
                                                                        boo_wis = TRUE, # TRUE, # Consider WIS in the cost function
                                                                        hh = TRUE,
                                                                        probs = probs,
                                                                        two_pars = TRUE,
                                                                        theta_weights = TRUE,
                                                                        by = 0.01)
    
    # Input the new data to the "new_data" tibble
    for (q in 1:length(probs)) {
      value <- ensemble[[as.character(dt)]][[as.character(h)]]$nowcast[q]
      new_data <- new_data |> add_row(location = "DE", age_group = "00+", forecast_date = dt, target_end_date = (dt + h), target = paste(h, " day ahead inc hosp", sep = ""), type = "quantile", quantile =  probs[q], value = value, pathogen = "COVID-19", model = method, retrospective = FALSE)
    }
    setTxtProgressBar(b, i)
  }
  close(b)
  
  count <- count + 1
}

stopCluster(cl)

# saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = paste("TMP/FITTED_OBJECTS/all_models_grid_theta_all_horizons.RDS", sep = ""))
# fitted_object <- readRDS(file = paste("TMP/FITTED_OBJECTS/all_models_grid_theta_all_horizons.RDS", sep = ""))
# new_data <- fitted_object$new_data
# ensemble2 <- fitted_object$ensemble

# In case you want to load the results for the above fitted procedure for all days
if (FALSE) {
  fitted_object <- readRDS(file = "TMP/FITTED_OBJECTS/TWO_MODELS_ensemble_pinball_quant_TRUE.RDS")
  new_data <- fitted_object$new_data
  ensemble <- fitted_object$ensemble
  
  w_hat <- readRDS(file = "TMP/FITTED_OBJECTS/TWO_MODELS_W_HAT_ensemble_pinball_quant_TRUE.RDS")
}

###########################################################################
# SUMMARIZE THE COMPUTED WEIGHTS AND PLOT THEM OVER FORECAST DATES
###########################################################################

w_hat <- weights_tibble(ensemble = ensemble, r = r, models = models, horizon = horizon, skip_first_days = skip_days[1], skip_last_days = skip_days[2], probs = probs)
for (q in 1:length(probs)) {
  tmp_plot <- plotting_weights(w_hat = w_hat, h = 0, q = probs[q]) # Choose the horizon "h" and quantile "q"
  print(tmp_plot)
}

###########################################################################
# COMPUTE AND PLOT WIS IN THE "ASSESSMENT PHASE"
###########################################################################

cmb_data <- rbind(data, new_data)

cmb_models <- c(models, "mean", "median", method)
cmb_colors <- c(colors, "#009E73", "#60D1B3", "#FF0000")

cmb_wis <- list()
cmb_wis_cm <- list()

quant <- TRUE # Scores computed independently for quantile

for (h in horizon) {
  print(paste("Horizon: ", h, sep = ""))
  cmb_wis[[as.character(h)]] <- compute_wis_data(data = cmb_data, truth_data = truth_data, start_date = r[1], end_date = r[2], horizon = h, models = cmb_models, skip_first_days = skip_days[1], skip_last_days = skip_days[2], quant = quant, training = FALSE, total_days = ifelse(skip_40, 50, 90), probs = probs) 
  if (!quant) {
    cmb_wis_cm[[as.character(h)]] <- cmb_wis[[as.character(h)]] |> colMeans(na.rm = TRUE)
  } else {
    cmb_wis_cm[[as.character(h)]] <- apply(X = cmb_wis[[as.character(h)]], MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)
  }
}

# Plotting WIS per horizon
plotting_WIS(horizon = horizon, wis_cm = cmb_wis_cm, colors = cmb_colors, models = cmb_models, quant = quant) 

# [DOUBLE-CHECK IT] Plotting WIS per forecast date aggregated by horizon
if (quant) {
  wis_days <- list()
  for (q in 1:length(probs)) {
    wis_days[[q]] <- compute_wis_days(wis = cmb_wis, models = cmb_models, q = q, skip_first_days = skip_days[1], skip_last_days = skip_days[2], total_days = ifelse(skip_40, 50, 90), average = FALSE)
    tmp_plot <- plotting_wis_days(wis_days = wis_days[[q]], q = probs[q])
    print(tmp_plot)
  }
} else {
  wis_days <- compute_wis_days(wis = cmb_wis, models = cmb_models, skip_first_days = skip_days[1], skip_last_days = skip_days[2], total_days = ifelse(skip_40, 50, 90))
  tmp_plot <- plotting_wis_days(wis_days = wis_days, q = "aggregated")
  print(tmp_plot)
}
