args <- commandArgs(trailingOnly = TRUE)
skip_recent_days  <- as.logical(args[1])
n_ensemble_models <- as.numeric(args[2])
unweighted_method <- args[3]

source("header.R")
source("utils.R")
source("aux.R")

ens_method <- "ranked_unweighted"

# skip_recent_days <- FALSE

training_size <- 90
uncertain_size <- 40

ignore_naive_ensemble_data <- TRUE # Remove naive ensembles from the data objects, so the trained models do not take them as inputs

quant <- TRUE # Weights depend (or not) on the quantiles
horiz <- FALSE # Weights depend (or not) on the horizons 

state_idx <- 17
age_idx <- 7

# n_ensemble_models <- 8
# unweighted_method <- "mean"

model_name <- paste("Ranked unweighted (", unweighted_method, " ", n_ensemble_models, ")", sep = "")
print(model_name)

##################################################
# LOAD AND PRE-PROCESS DATA
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

r <- range(data$forecast_date)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# READ FORMATTED DATA 
##################################################

retrieved_data_file <- paste("DATA/TRAINING/retrieved_data_training_size_", training_size, "_state_", state, "_age_", age, ".RDS", sep = "")
retrieved_data <- readRDS(file = retrieved_data_file)
 
y         <- retrieved_data$y
y_current <- retrieved_data$y_current 
values    <- retrieved_data$values
current   <- retrieved_data$current

if (ignore_naive_ensemble_data) {
  new_tmp_data <- process_data_ignore_models(current = current, values = values, idx_models = (1:length(ens_models)))
  
  current <- new_tmp_data$current
  values <- new_tmp_data$values
}

if (skip_recent_days) {
  new_tmp_data <- process_data_skip_days(y = y, values = values, uncertain_size = uncertain_size)
  y      <- new_tmp_data$y
  values <- new_tmp_data$values 
}

##################################################
# COMPUTE SCORE (INCOMPLETE DATA)
##################################################

wis_file <- paste("DATA/TRAINING/WIS/wis_weights_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
wis_training_list <- readRDS(file = wis_file)
  
wis <- wis_training_list$wis
wis_avg <- wis_training_list$wis_avg

##################################################
# REAL-TIME UNTRAINED ENSEMBLE
##################################################

new_data <- create_new_tibble()

skip_first_days <- ifelse(skip_recent_days, uncertain_size + 1, 1)
days <- seq(r[1] + skip_first_days, r[2], by = "1 day")

ensemble <- list()
count <- 1

fitted_model_file <- paste("OTHER_ANALYSES/RESULTS/FITTED_OBJECTS/", ens_method, "_", unweighted_method, "_",  n_ensemble_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
if (!(file.exists(fitted_model_file))) {
  for (k in 1:length(days)) {
    dt <- days[k]
    print(paste(dt, " (", sprintf("%03d", count), "/", sprintf("%03d", length(days)), ")", sep = ""))
    
    if (horiz) {
      ensemble[[as.character(dt)]] <- list()
      
      for (i in 1:length(horizon)) {
        h <- horizon[i]
        
        current_ens <- current[[as.character(dt)]][[as.character(h)]][[1]]
        values_ens  <- values[[as.character(dt)]][[as.character(h)]]
        
        tmp_result <- compute_ensemble(ens_method = ens_method,
                                       y = y[[as.character(dt)]][[as.character(h)]],
                                       y_current = y_current[[as.character(dt)]][[as.character(h)]],
                                       values = values_ens,
                                       current = current_ens,
                                       n_ensemble_models = n_ensemble_models,
                                       unweighted_method = unweighted_method,
                                       ens_models = ens_models,
                                       quant = quant,
                                       horiz = horiz,
                                       probs = probs)
        
        ensemble[[as.character(dt)]][[as.character(h)]] <- tmp_result
        
        # Input the new data to the `new_data` tibble
        new_data <- input_new_data(new_data = new_data, ensemble = ensemble, ens_method = ens_method, h = h, state = state, age = age, day = dt, probs = probs)
      }
    } else { # Common across horizons
      
      current_ens <- current[[as.character(dt)]]
      values_ens  <- values[[as.character(dt)]]
      
      tmp_result <- compute_ensemble(ens_method = ens_method,
                                     y = y[[as.character(dt)]],
                                     y_current = y_current[[as.character(dt)]],
                                     values = values_ens,
                                     current = current_ens,
                                     n_ensemble_models = n_ensemble_models,
                                     unweighted_method = unweighted_method,
                                     ens_models = ens_models,
                                     quant = quant,
                                     horiz = horiz,
                                     probs = probs)
      
      ensemble[[as.character(dt)]] <- tmp_result
      
      # Input the new data to the `new_data` tibble
      new_data <- input_new_data(new_data = new_data, ensemble = ensemble, ens_method = ens_method, h = h, state = state, age = age, day = dt, horizon = horizon, probs = probs, horiz = horiz)
    }
    
    count <- count + 1
  }
  
  new_data$model <- model_name
  
  saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = fitted_model_file)
  
} else {
  
  tmp_result <- readRDS(file = fitted_model_file)
  ensemble <- tmp_result$ensemble
  new_data <- tmp_result$new_data
  
}

##################################################
# COMPUTE SCORE
##################################################

# new_data_2 <- readRDS(file = paste("OTHER_ANALYSES/RESULTS/naive_ensemble_group_", 8, ".RDS", sep = ""))
# new_data_2 <- new_data_2$naive_ensemble[[1]]
# new_data_2 %>% filter(location == "DE", age_group == "00+", !(is.na(quantile)), model == "Mean", forecast_date > as.Date("2021-11-29"))

wis_truth <- compute_wis_truth(data = new_data, truth_data = truth_data, models = model_name, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = ifelse(skip_recent_days, 0, 40))
df_wis <- wis_truth$df_wis

wis_truth_file <- paste("OTHER_ANALYSES/RESULTS/FITTED_OBJECTS/WIS/wis_truth_", ens_method, "_", unweighted_method, "_",  n_ensemble_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
saveRDS(object = wis_truth, file = wis_truth_file)
