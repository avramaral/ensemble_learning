##################################################
args <- commandArgs(trailingOnly = TRUE)
analyze_states <- as.logical(args[1])
cc             <- as.numeric(args[2])
horiz          <- as.logical(args[3])
##################################################

source("header.R")
source("utils.R")
source("aux.R")

s <- c("DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE", "DE-HH", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL", "DE-SN", "DE-ST", "DE-TH")
a <- c("00-04", "05-14", "15-34", "35-59", "60-79", "80+")

comb_state <- data.frame(state = s, age = "00+")
comb_age <- data.frame(state = "DE", age = a)

if (analyze_states) {
  state <- comb_state[cc, "state"]
  age <- comb_state[cc, "age"]
} else {
  state <- comb_age[cc, "state"]
  age <- comb_age[cc, "age"]
}

ens_method <- "wis" 
skip_recent_days <- FALSE

training_size <- 90
uncertain_size <- 40

ignore_naive_ensemble_data <- TRUE # Remove naive ensembles from the data objects, so the trained models do not take them as inputs

quant <- TRUE # Weights depend (or not) on the quantiles
# horiz <- TRUE # Weights depend (or not) on the horizons

method <- "Mean"

reparameterize <- TRUE 

##################################################
# LOAD AND PRE-PROCESS DATA
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

KIT_frozen_baseline <- data %>% filter(model == "KIT-frozen_baseline")

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 

if ((state == "DE") & (age != "00+")) { idx_missing_model <- which(models == "RKI") } else if ((state != "DE") & (age == "00+")) { idx_missing_model <- which(models == "ILM") } else { stop("Error.") }

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
##################################################

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
naive_ensemble <- readRDS(file = naive_ensemble_file)

data <- rbind(data, naive_ensemble)
models <- c(models, "Mean", "Median")
colors <- c(colors, "#009E73", "#60D1B3")

##################################################
# FORMAT DATA 
##################################################

retrieved_data_file <- paste("DATA/TRAINING/retrieved_data_training_size_", training_size, "_state_", state, "_age_", age, ".RDS", sep = "")
retrieved_data <- readRDS(file = retrieved_data_file)

y         <- retrieved_data$y
y_current <- retrieved_data$y_current 
values    <- retrieved_data$values
current   <- retrieved_data$current

if (reparameterize) {
  
  new_retrieved_data_file <- paste("DATA/TRAINING/new_retrieved_data_training_size_", training_size, "_state_", state, "_age_", age, ".RDS", sep = "")
  new_retrieved_data <- readRDS(file = new_retrieved_data_file)

  y         <- new_retrieved_data$y
  y_current <- new_retrieved_data$y_current
  values    <- new_retrieved_data$values
  current   <- new_retrieved_data$current
}

# Remove `Mean` and `Median` from the data objects
if (TRUE) {
  new_tmp_data <- process_data_ignore_models(current = current, values = values, idx_models = (1:length(ens_models)))
  
  current <- new_tmp_data$current
  values <- new_tmp_data$values
}

if (skip_recent_days) {
  new_tmp_data <- process_data_skip_days(y = y, values = values, uncertain_size = uncertain_size, method = method)
  
  y      <- new_tmp_data$y
  values <- new_tmp_data$values 
}
  
##################################################
# COMPUTE SCORE
##################################################

wis_file <- paste("DATA/TRAINING/WIS/wis_weights_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
    
wis_training_list <- compute_wis_training(data = values, truth_data = y, start_date = r[1], end_date = r[2], horizon = horizon, models = ens_models, training_size = training_size, skip_recent_days = skip_recent_days, quant = quant, horiz = horiz, method = method)
saveRDS(object = wis_training_list, file = wis_file)
