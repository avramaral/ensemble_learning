source("header.R")
source("utils.R")
source("aux.R")

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

filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = 200)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

models <- c("Mean", "Median", "(60) DISW 4", "(90) DISW 4", "(ALL) DISW 4")
colors <- c("#009E73", "#60D1B3", "red", "blue", "yellow")

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
naive_ensemble <- readRDS(file = naive_ensemble_file)
data <- rbind(data, naive_ensemble)

r <- range(data$forecast_date)

##################################################

reparameterize_file <- ifelse(reparameterize, "new_", "")

# DISW
DISW_4_1_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_wis_size_60_skip_FALSE_state_",  state, "_age_", age, "_quant_TRUE_horiz_TRUE.RDS", sep = "")
DISW_4_2_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_wis_size_90_skip_FALSE_state_",  state, "_age_", age, "_quant_TRUE_horiz_TRUE.RDS", sep = "")
DISW_4_3_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, "method_wis_size_200_skip_FALSE_state_", state, "_age_", age, "_quant_TRUE_horiz_TRUE.RDS", sep = "")

DISW_4_1 <- readRDS(file = DISW_4_1_file)
DISW_4_2 <- readRDS(file = DISW_4_2_file)
DISW_4_3 <- readRDS(file = DISW_4_3_file)

DISW_4_1_ens <- DISW_4_1$ensemble
DISW_4_2_ens <- DISW_4_2$ensemble
DISW_4_3_ens <- DISW_4_3$ensemble

DISW_4_1 <- DISW_4_1$new_data
DISW_4_2 <- DISW_4_2$new_data
DISW_4_3 <- DISW_4_3$new_data

DISW_4_1$model <- models[3]
DISW_4_2$model <- models[4]
DISW_4_3$model <- models[5]

##########
##########

naive_ensemble <- naive_ensemble[!(naive_ensemble$type == "mean"), ]

ensemble_data <- rbind(naive_ensemble, DISW_4_1, DISW_4_2, DISW_4_3)

r <- range(DISW_4_1$forecast_date)
ensemble_data <- ensemble_data[(ensemble_data$forecast_date >= r[1]) & (ensemble_data$forecast_date <= r[2]), ]
ensemble_data$model <- factor(x = ensemble_data$model, levels = models)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# COMPUTE SCORE
# Compute WIS for all post-processed models, given truth final data
##################################################

# Extra gap
skip_first_days <- 30 + uncertain_size

# Make all models comparable with `skip_first_days`
wis_truth <- compute_wis_truth(data = ensemble_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)

df_wis <- wis_truth$df_wis
wis_summ <- wis_truth$wis_summ

coverage_file <- paste("RESULTS/FITTED_OBJECTS/COVERAGE/coverage_different_sizes_state_", state, "_age_", age, "_extra_gap_", skip_first_days, ".RDS", sep = "")

if (file.exists(coverage_file)) {
  coverage_models <- readRDS(file = coverage_file)
} else {
  coverage_models <- compute_coverage(data = ensemble_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)
  saveRDS(object = coverage_models, file = coverage_file)
}

# Bar plot
wis_bar <- plot_wis_bar_size(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 100, skip_space = FALSE)

# Coverage plot
coverage_bar <- plot_coverage(coverage_models = coverage_models, models = models, colors = colors)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)

tmp_ttl <- "" # "WIS (weighted ensemble)"
p_total <- wis_bar + wis_line_horizon + coverage_bar + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
saveRDS(object = p_total, file = paste("PLOTS/ENSEMBLE/DIFFERENT_TRAINING_SIZE/WIS_different_training.RDS", sep = "")) 

ggsave(filename = paste("PLOTS/ENSEMBLE/DIFFERENT_TRAINING_SIZE/WIS_different_training.jpeg", sep = ""), plot = p_total, width = 4700, height = 1500, units = c("px"), dpi = 300, bg = "white") 
