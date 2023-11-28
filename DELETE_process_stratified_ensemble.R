source("header.R")
source("utils.R")
source("aux.R")

training_size <- 90
uncertain_size <- 40

quant <- TRUE
horiz <- TRUE 

group <- "AGG" # c("AGG", "AGE", "STA"), which stands for "aggregated," "age," and "state"

agg_idxs <- list(state_idx = 17, age_idx = 7)
age_idxs <- list(state_idx = 17, age_idx = 1:6)
sta_idxs <- list(state_idx = 1:16, age_idx = 7)

if (group == "AGG") { idxs <- agg_idxs } else if (group == "AGE") { 
                      idxs <- age_idxs } else if (group == "STA") { 
                      idxs <- sta_idxs } else { stop("Select a valid group.") } 

method <- "all_quant" # c("all_quant", "other")
method_file <- ifelse(method == "all_quant", "all_quant_", "")

##################################################
# LOAD AND PRE-PROCESS DATA (FOR ALL ENSEMBLE MODELS)
# `state_idx` and `age_idx` must be selected 
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

state <- unique(data$location)
state <- c(state, "DE")
state <- state[2:length(state)][idxs$state_idx]

age <- unique(data$age_group)
age <- c(age, "00+")
age <- age[2:length(age)][idxs$age_idx]

state_age <- data.frame(state = state, age = age)

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 
models_orig <- models
colors_orig <- colors

filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

models <- c("Mean", "Median", "DISW (St.1.)", "DISW (St.2.)", "ISW (St.1.)", "ISW (St.2.)")
colors <- c("#009E73", "#60D1B3", "#9400D3", "#9370DB", "#C71585", "#FF1493")
names(colors) <- models

for (i in 1:nrow(state_age)) {
  naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state_age[i, "state"], "_age_", state_age[i, "age"], ".RDS", sep = "")
  naive_ensemble_tmp  <- readRDS(file = naive_ensemble_file)
  if (i == 1) {
    naive_ensemble <- naive_ensemble_tmp
  } else {
    naive_ensemble <- rbind(naive_ensemble, naive_ensemble_tmp)
  }
}

data <- rbind(data, naive_ensemble)

##################################################

if (group == "AGG") { 
  group_file <- ""
  state_age_file <- paste("state_", state, "_age_", age, "_", sep = "")
} else if (group == "AGE") { 
  group_file <- "ALL_AGES_"
  state_age_file <- ""
} else if (group == "STA") { 
  group_file <- "ALL_STATES_" 
  state_age_file <- ""
} else { stop("Select a valid group.") } 

DISW_1_file <- paste("RESULTS/FITTED_OBJECTS/", group_file,              "method_wis_size_",     training_size, "_skip_TRUE_",  state_age_file, "quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
DISW_2_file <- paste("RESULTS/FITTED_OBJECTS/", group_file, method_file, "method_wis_size_",     training_size, "_skip_FALSE_", state_age_file, "quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
ISW_1_file  <- paste("RESULTS/FITTED_OBJECTS/", group_file,              "method_pinball_size_", training_size, "_skip_TRUE_",  state_age_file, "quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
ISW_2_file  <- paste("RESULTS/FITTED_OBJECTS/", group_file,              "method_pinball_size_", training_size, "_skip_FALSE_", state_age_file, "quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

DISW_1 <- readRDS(file = DISW_1_file)
DISW_2 <- readRDS(file = DISW_2_file)
ISW_1  <- readRDS(file = ISW_1_file)
ISW_2  <- readRDS(file = ISW_2_file)
# ISW_1 <- DISW_1
# ISW_2 <- DISW_2

DISW_1_ens <- DISW_1$ensemble
DISW_2_ens <- DISW_2$ensemble
ISW_1_ens  <- ISW_1$ensemble
ISW_2_ens  <- ISW_2$ensemble

DISW_1 <- DISW_1$new_data
DISW_2 <- DISW_2$new_data
ISW_1  <- ISW_1$new_data
ISW_2  <- ISW_2$new_data
DISW_1$model <- models[3]
DISW_2$model <- models[4]
ISW_1$model  <- models[5]
ISW_2$model  <- models[6]

naive_ensemble <- naive_ensemble[!(naive_ensemble$type == "mean"), ]

ensemble_data <- rbind(naive_ensemble, DISW_1, DISW_2, ISW_1, ISW_2)

r <- range(DISW_1$forecast_date)
ensemble_data <- ensemble_data[(ensemble_data$forecast_date >= r[1]) & (ensemble_data$forecast_date <= r[2]), ]
ensemble_data$model <- factor(x = ensemble_data$model, levels = models)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# COMPUTE SCORE
# Compute WIS for all post-processed models, given truth final data
##################################################

skip_first_days <- 30 # Extra gap
compute_wis_file <- paste("RESULTS/FITTED_OBJECTS/WIS/", group, "_wis_size_", training_size, "_", state_age_file, "quant_", as.character(quant), "_horiz_", as.character(horiz), "_extra_gap_", skip_first_days,".RDS", sep = "")

if (file.exists(compute_wis_file)) {
  wis_truth <- readRDS(file = compute_wis_file)
} else {
  wis_truth <- list()
  for (i in 1:nrow(state_age)) {
    print(state_age[i, ])
    tmp_ensemble_data <- ensemble_data %>% filter(location == state_age[i, "state"], age_group == state_age[i, "age"])
    tmp_truth_data    <- truth_data    %>% filter(location == state_age[i, "state"], age_group == state_age[i, "age"])
    
    # Make all models comparable: `skip_first_days = 0`, since I already filtered the `ensemble_data` before
    wis_truth[[i]] <- compute_wis_truth(data = tmp_ensemble_data, truth_data = tmp_truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)
  }
  if (length(wis_truth) == 1) {
    wis_truth <- wis_truth[[1]]
  } else {
    wis_truth <- summarize_stratified_wis_truth(wis_truth)
  }
  saveRDS(object = wis_truth, file = compute_wis_file)
}

df_wis <- wis_truth$df_wis
wis_summ <- wis_truth$wis_summ

if (FALSE) { # Temporary
  df_wis[df_wis$model %in% c("ISW (St.1.)", "ISW (St.2.)"), "wis"] <- 0
  for (i in 1:length(wis_summ)) { wis_summ[[i]][5:6] <- 0 } 
  # df_wis[df_wis$model %in% c("DISW (St.1.)", "ISW (St.1.)", "ISW (St.2.)"), "wis"] <- 0
  # for (i in 1:length(wis_summ)) { wis_summ[[i]][c(3, 5:6)] <- 0 } 
}

# Bar plot
p <- plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 100)
if (nrow(state_age) != 1) {
p + ggtitle(paste("Stratified by ", ifelse(nrow(state_age) == 16, "STATE", "AGE"), sep = ""))
} else { p }

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)

###############
### WIS per day
###############

cmb_wis <- list()

for (i in 1:nrow(state_age)) {
  print(state_age[i, ])
  count <- 1
  b <- txtProgressBar(min = 0, max = length(horizon), initial = 0)
  
  tmp_ensemble_data <- ensemble_data %>% filter(location == state_age[i, "state"], age_group == state_age[i, "age"])
  tmp_truth_data    <- truth_data    %>% filter(location == state_age[i, "state"], age_group == state_age[i, "age"])
  
  cmb_wis[[i]] <- list()
  for (h in horizon) {
    cmb_wis[[i]][[as.character(h)]] <- compute_wis_data(data = tmp_ensemble_data, truth_data = tmp_truth_data, start_date = r[1], end_date = r[2], horizon = h, models = models, probs = probs)
    
    count <- count + 1
    setTxtProgressBar(b, count)
  }
  close(b)
}

tmp_cmb_wis <- cmb_wis[[1]]
for (i in 1:length(tmp_cmb_wis)) { tmp_cmb_wis[[i]] <- matrix(data = 0, nrow = nrow(tmp_cmb_wis[[i]]), ncol = ncol(tmp_cmb_wis[[i]]))} 
for (i in 1:nrow(state_age)) {
  for (j in 1:length(cmb_wis[[i]])) {
    tmp_cmb_wis[[j]] <- tmp_cmb_wis[[j]] + cmb_wis[[i]][[j]]
  }
}
for (i in 1:length(tmp_cmb_wis)) { tmp_cmb_wis[[i]] <- tmp_cmb_wis[[i]] / nrow(state_age) }
cmb_wis <- tmp_cmb_wis

wis_days <- compute_wis_days(wis = cmb_wis, models = models, start_date = r[1], end_date = r[2], total_days = training_size, average = TRUE)
wis_days$model <- factor(x = wis_days$model, levels = models)

# Plotting
ggplot(data = wis_days, aes(x = forecast_date, y = value, color = model)) +
  geom_line(linewidth = 1, alpha = rep(ifelse(unique(wis_days$forecast_date) < (r[1] + skip_first_days), 0.25, 1), 6)) +
  geom_vline(xintercept = r[1] + 2, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(r[1] + skip_first_days), linetype = "dashed") +
  scale_color_manual(NULL, values = colors) +
  # expand_limits(y = 0) +
  expand_limits(x = r[1] - uncertain_size) +
  labs(x = "Forecast date", y = "WIS (Averaged over horizons and M.W. up to 90 days)") +
  theme_bw() +
  theme(legend.position = "right", 
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
        axis.title.y = element_text(size = 11),
        axis.text = element_text(size = 11),
        text = element_text(size = 11, family = "LM Roman 10"))

##################################################
# PLOT WEIGHTS
# The range `rr` has to be set according to the original data forecast dates
##################################################

# TO BE IMPLEMENTED
