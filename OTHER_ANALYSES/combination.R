args <- commandArgs(trailingOnly = TRUE)
g <- as.numeric(args[1])

source("header.R")
source("utils.R")

all_models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ")

# https://stackoverflow.com/questions/40049313/generate-all-combinations-of-all-lengths-in-r-from-a-vector
combin <- do.call("c", lapply(seq_along(all_models), function(i) combn(all_models, i, FUN = list)))
n_mods <- data.frame()
for (i in 1:length(all_models)) { n_mods <- rbind(n_mods, c(i, length(which(lengths(combin) == i)))) }
colnames(n_mods) <- c("n_elements", "n_combinations")
n_mods$cumsum <- cumsum(n_mods$n_combinations)
idxs <- c(0, n_mods$cumsum)

select_combin <- combin[(idxs[g] + 1):(idxs[g + 1])]

##################################################

training_size <- 90
uncertain_size <- 40

state_idx <- 17
age_idx <- 7

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################

cp_data <- read_csv(file = "DATA/data.csv.gz")
cp_truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

state <- unique(cp_data$location)
state <- c(state, "DE")
state <- state[2:length(state)][state_idx]

age <- unique(cp_data$age_group)
age <- c(age, "00+")
age <- age[2:length(age)][age_idx]

##################################################

naive_ensemble <- list()
wis_truth <- list()

for (i in 1:length(select_combin)) {
  
  models <- select_combin[[i]]
  print(models)
  print(paste("(", sprintf("%02d", i), "/", sprintf("%02d", length(select_combin)), ")" ,sep = ""))
  
  # LOAD AND PRE-PROCESS DATA
  
  data <- cp_data
  truth_data <- cp_truth_data
  
  filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
  data <- filtered_data$data
  truth_data <- filtered_data$truth_data
  
  r <- range(data$forecast_date)
  
  # UNTRAINED ENSEMBLE
  
  naive_ensemble[[i]] <- compute_naive_ensemble(data = data, loc = state, age_gr = age)
  data <- rbind(data, naive_ensemble[[i]])
  models <- c(models, "Mean", "Median")
  
  # COMPUTE SCORE
  
  # Make all models comparable: `skip_first_days = 40 + 1`
  wis_truth[[i]] <- compute_wis_truth(data = data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = (uncertain_size + 1), verbose = FALSE)
    
}

saveRDS(object = list(naive_ensemble = naive_ensemble, wis_truth = wis_truth), file = paste("OTHER_ANALYSES/RESULTS/naive_ensemble_group_", g, ".RDS", sep = ""))
