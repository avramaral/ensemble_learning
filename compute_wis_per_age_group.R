source("header.R")
source("utils.R")
source("aux.R")

skip_recent_days <- FALSE

training_size <- 90
uncertain_size <- 40

strata <- "ages"

if (strata == "ages") { state_idx <- 17; age_idx <- 1:6 } else { stop("Error. Check it!") }

#############################
# LOAD AND PRE-PROCESS DATA #
#############################

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

if (length(age) != 1) { idx_missing_model <- which(models == "RKI") } else { stop("Error. Check it 2!") }

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

######################
# UNTRAINED ENSEMBLE #
######################

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
models <- c(models, "Mean", "Median")
colors <- c(colors, "#009E73", "#60D1B3")

###############
# COMPUTE WIS #
###############

tmp_baseline_data <- KIT_frozen_baseline
tmp_baseline_data <- tmp_baseline_data[(tmp_baseline_data$forecast_date >= r[1]) & (tmp_baseline_data$forecast_date <= r[2]), ]
tmp_baseline_data <- tmp_baseline_data %>% filter(location %in% unique(data$location), age_group %in% unique(data$age_group)) %>% filter(!is.na(quantile))

tmp_models <- models[1:9]; tmp_colors <- colors[1:9] 

# Make all models comparable: `skip_first_days = 40 + 1`
skip_first_days <- (uncertain_size + 1) + 30

wis_truth <- list()
wis_relat <- list()
for (i in 1:length(horizon)) {
  print(paste("Horizon: ", horizon[i]), sep = "")
  wis_truth[[as.character(horizon[i])]] <- compute_wis_three_components_stratified(data = data,              truth_data = truth_data, models = tmp_models,                                     horizon = horizon[i], start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)
  wis_relat[[as.character(horizon[i])]] <- compute_wis_three_components_stratified(data = tmp_baseline_data, truth_data = truth_data, models = rep("KIT-frozen_baseline", length(tmp_models)), horizon = horizon[i], start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)
}

wis_total       <- array(data = 0, dim = c(length(horizon), as.numeric(diff(r) - skip_first_days + 1), length(models), length(age)))
wis_total_relat <- array(data = 0, dim = c(length(horizon), as.numeric(diff(r) - skip_first_days + 1), length(models), length(age)))
for (i in 1:length(horizon)) {
  wis_total[i, , , ]       <- wis_truth[[as.character(horizon[i])]]$wis
  wis_total_relat[i, , , ] <- wis_relat[[as.character(horizon[i])]]$wis
}

wis_total       <- wis_total       %>% apply(MARGIN = c(3, 4), FUN = mean)
wis_total_relat <- wis_total_relat %>% apply(MARGIN = c(3, 4), FUN = mean)
wis_total_relat <- wis_total / wis_total_relat

wis_ages       <- wis_total
wis_ages_relat <- wis_total_relat

# wis_total       <- array(data = 0, dim = c(length(horizon), as.numeric(diff(r) - skip_first_days + 1), length(models), length(age)))
# wis_total_relat <- array(data = 0, dim = c(length(horizon), as.numeric(diff(r) - skip_first_days + 1), length(models), length(age)))
# for (i in 1:length(horizon)) {
#   wis_total[i, , ]       <-  wis_truth[[as.character(horizon[i])]]$wis %>% apply(MARGIN = c(2, 3), FUN = mean)
#   wis_total_relat[i, , ] <- wis_total[i, , ] / (wis_relat[[as.character(horizon[i])]]$wis %>% apply(MARGIN = c(2, 3), FUN = mean))
#   # wis_total_relat[i, , ] <- (wis_truth[[as.character(horizon[i])]]$wis / wis_relat[[as.character(horizon[i])]]$wis) %>% apply(MARGIN = c(2, 3), FUN = mean)
# }
# wis_ages       <- wis_total       %>% apply(MARGIN = c(2, 3), FUN = mean)
# wis_ages_relat <- wis_total_relat %>% apply(MARGIN = c(2, 3), FUN = mean)

rownames(wis_ages) <- models
colnames(wis_ages) <- age

rownames(wis_ages_relat) <- models
colnames(wis_ages_relat) <- age

full_models <- c(models[1:5], "RKI",       models[6:9])
full_colors <- c(colors[1:5], "#3C4AAD00", colors[6:9])
  
wis_ages <- as_tibble(as.data.frame(wis_ages), rownames = "Model") %>% pivot_longer(cols = -Model, names_to = "Age_Group", values_to = "Value")
colnames(wis_ages) <- c("model", "age_group", "WIS")
wis_ages <- wis_ages %>% mutate(age_group = factor(age_group))
wis_ages <- wis_ages %>% mutate(model = factor(model, levels = full_models))

wis_ages_relat <- as_tibble(as.data.frame(wis_ages_relat), rownames = "Model") %>% pivot_longer(cols = -Model, names_to = "Age_Group", values_to = "Value")
colnames(wis_ages_relat) <- c("model", "age_group", "WIS")
wis_ages_relat <- wis_ages_relat %>% mutate(age_group = factor(age_group))
wis_ages_relat <- wis_ages_relat %>% mutate(model = factor(model, levels = full_models))

values <- setNames(full_colors, full_models)
pp_wis_age <- ggplot(wis_ages, aes(x = age_group, y = WIS, color = model)) +
  geom_jitter(size = 2, width = 0.175, height = 0) + 
  labs(title = NULL, x = "Age groups", y = "WIS (Avg. over time points and horizons)") +
  scale_color_manual(name = "Models", values = values, drop = FALSE) +
  coord_cartesian(ylim = c(-1, 101), xlim = c(0.5, 6.5), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 12, family = "LM Roman 10"))

pp_wis_age_relat <- ggplot(wis_ages_relat, aes(x = age_group, y = WIS, color = model)) +
  geom_jitter(size = 2, width = 0.175, height = 0) + 
  labs(title = NULL, x = "Age groups", y = "Relative WIS\n(Avg. over time points and horizons)") +
  scale_color_manual(name = "Models", values = values, drop = FALSE) +
  coord_cartesian(ylim = c(0, 0.4), xlim = c(0.5, 6.5), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 12, family = "LM Roman 10"))

pp_wis_age_comb <- pp_wis_age + pp_wis_age_relat + plot_layout(guides = "collect") & theme(legend.position = "right") 

ggsave(filename = paste("PLOTS/WIS_per_age_group.jpeg", sep = ""), plot = pp_wis_age_comb, width = 3000, height = 1070, units = c("px"), dpi = 300, bg = "white")


