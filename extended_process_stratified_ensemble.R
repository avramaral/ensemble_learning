# args <- commandArgs(trailingOnly = TRUE)
# state_idx        <- as.numeric(args[1])
# age_idx          <- as.numeric(args[2])

##################################################

source("header.R")
source("utils.R")
source("aux.R")

training_size <- 90
uncertain_size <- 40

reparameterize <- TRUE

strata <- "states" # c("states", "ages")

if (strata == "states") {
  state_idx <- 1:16
  age_idx <- 7
} else if (strata == "ages") {
  state_idx <- 17
  age_idx <- 1:6
}

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

models <- c("Mean", "Median", "DISW 2", "DISW 4", "AISW 2", "AISW 4")
DISW_colors <- colorRampPalette(c("#8B0000", "#E6ADD8"))(4)[c(2, 4)]
AISW_colors <- colorRampPalette(c("#00008B", "#ADD8E6"))(4)[c(2, 4)]
colors <- c("#009E73", "#60D1B3", DISW_colors, AISW_colors)

# Naive ensemble
naive_ensemble_files <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")

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

r <- range(data$forecast_date)

##################################################

reparameterize_file <- ifelse(reparameterize, "new_", "")

tmp_name <- paste("ALL_", toupper(strata), sep = "")

# DISW

DISW_2_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, tmp_name, "_method_wis_size_", training_size, "_skip_FALSE_quant_TRUE_horiz_FALSE.RDS", sep = "")
DISW_4_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, tmp_name, "_method_wis_size_", training_size, "_skip_FALSE_quant_TRUE_horiz_TRUE.RDS",  sep = "")

DISW_2 <- readRDS(file = DISW_2_file)
DISW_4 <- readRDS(file = DISW_4_file)

DISW_2_ens <- DISW_2$ensemble
DISW_4_ens <- DISW_4$ensemble

DISW_2 <- DISW_2$new_data
DISW_4 <- DISW_4$new_data

DISW_2$model <- models[3]
DISW_4$model <- models[4]

# AISW

AISW_2_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, tmp_name, "_method_pinball_size_", training_size, "_skip_FALSE_quant_TRUE_horiz_FALSE.RDS", sep = "")
AISW_4_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, tmp_name, "_method_pinball_size_", training_size, "_skip_FALSE_quant_TRUE_horiz_TRUE.RDS", sep = "")

AISW_2 <- readRDS(file = AISW_2_file)
AISW_4 <- readRDS(file = AISW_4_file)

AISW_2_ens <- AISW_2$ensemble
AISW_4_ens <- AISW_4$ensemble

AISW_2 <- AISW_2$new_data
AISW_4 <- AISW_4$new_data

AISW_2$model <- models[5]
AISW_4$model <- models[6]

##########
##########

naive_ensemble <- naive_ensemble[!(naive_ensemble$type == "mean"), ]

ensemble_data <- rbind(naive_ensemble, DISW_2, DISW_4, AISW_2, AISW_4)

r <- range(DISW_2$forecast_date)
ensemble_data <- ensemble_data[(ensemble_data$forecast_date >= r[1]) & (ensemble_data$forecast_date <= r[2]), ]
ensemble_data$model <- factor(x = ensemble_data$model, levels = models)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# COMPUTE SCORE
# Compute WIS for all post-processed models, given truth final data
##################################################

# Extra gap
skip_first_days <- uncertain_size + 30

###

compute_wis_file <- paste("RESULTS/FITTED_OBJECTS/WIS/wis_size_", training_size, "_extra_gap_", skip_first_days, "_strata_", strata, ".RDS", sep = "")
if (file.exists(compute_wis_file)) {
  wis_truth <- readRDS(file = compute_wis_file)
} else {

  # Make all models comparable: `skip_first_days = 40 + X` (X = 30; i.e., an extra gap)
  wis_truth <- compute_wis_truth(data = ensemble_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)
  saveRDS(object = wis_truth, file = compute_wis_file)
}

###

coverage_file <- paste("RESULTS/FITTED_OBJECTS/COVERAGE/coverage_size_", training_size, "_extra_gap_", skip_first_days, "_strata_", strata, ".RDS", sep = "")
if (file.exists(coverage_file)) {
  coverage_models <- readRDS(file = coverage_file)
} else {
  coverage_models <- compute_coverage(data = ensemble_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)
  saveRDS(object = coverage_models, file = coverage_file)
}

###

df_wis <- wis_truth$df_wis
wis_summ <- wis_truth$wis_summ

# Bar plot
if (strata == "states") { ylim_manual <- 15 } else if (strata == "ages") { ylim_manual <- 20 }
wis_bar <- plot_wis_bar_ensemble(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = ylim_manual)

# Coverage plot
coverage_bar <- plot_coverage_ensemble(coverage_models = coverage_models, models = models, colors = colors)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)

if (FALSE) {
  wis_bar      <- wis_bar      + theme(plot.margin = margin(0, 5, 0, 42))
  coverage_bar <- coverage_bar + theme(plot.margin = margin(0, 5, 0, 42))
}

tmp_ttl <- "" # "WIS (weighted ensemble)"
p_total <- wis_bar + wis_line_horizon + coverage_bar + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
saveRDS(object = p_total, file = paste("PLOTS/ENSEMBLE/STRATIFIED/WIS_", strata,"_ensemble.RDS", sep = ""))

ggsave(filename = paste("PLOTS/ENSEMBLE/STRATIFIED/WIS_", strata,"_ensemble.jpeg", sep = ""), plot = p_total, width = 4600, height = 1500, units = c("px"), dpi = 300, bg = "white") 






