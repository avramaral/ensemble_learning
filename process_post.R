source("header.R")
source("utils.R")
source("aux.R")

ens_method <- "pinball" 
skip_recent_days <- FALSE 
method <- "Mean" # c("Mean", "Median", "all_quant") 

training_size <- 90
uncertain_size <- 40

quant <- TRUE
horiz <- TRUE 

state_idx <- 17
age_idx <- 7

reparameterize <- TRUE 

reparameterize_file <- ifelse(reparameterize, "new_", "")
method_files <- ifelse(method == "all_quant", "all_quant_", "")

##################################################
# LOAD AND PRE-PROCESS DATA (ALSO POST-PROCESSED DATA)
# `state_idx` and `age_idx` must be selected (if such data exist)
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

models <- c(models, "Mean", "Median")
colors <- c(colors, "#009E73", "#60D1B3")

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
naive_ensemble <- readRDS(file = naive_ensemble_file)
data <- rbind(data, naive_ensemble)

##################################################

count <- 1
for (m in models) {
  tmp_postprocessed_file <- paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/", reparameterize_file, method_files, "post-processing_model_", m, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
  tmp_postprocessed_data <- readRDS(file = tmp_postprocessed_file)
  tmp_postprocessed_data <- tmp_postprocessed_data$new_data 
  tmp_postprocessed_data$model <- m
  
  if (count == 1) {
    postprocessed_data <- tmp_postprocessed_data
  } else {
    postprocessed_data <- rbind(postprocessed_data, tmp_postprocessed_data)
  }
  
  count <- count + 1
}
cp_postprocessed_data <- postprocessed_data

r <- range(postprocessed_data$forecast_date)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

##################################################
# UNTRAINED ENSEMBLE
# Compute the naive ensemble using `mean` and `median` for the post-processed individual models
##################################################

postprocessed_naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/POST_PROCESSED/", reparameterize_file, method_files, "postprocessed_naive_ensemble_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

if (file.exists(postprocessed_naive_ensemble_file)) {
  postprocessed_naive_ensemble <- readRDS(file = postprocessed_naive_ensemble_file)
} else {
  tmp_postprocessed_data <- postprocessed_data[!(postprocessed_data$model %in% c("Mean", "Median")), ]
  
  postprocessed_naive_ensemble <- compute_naive_ensemble(data = tmp_postprocessed_data, loc = state, age_gr = age)
  saveRDS(object = postprocessed_naive_ensemble, file = postprocessed_naive_ensemble_file)
}

postprocessed_data <- postprocessed_data[!(postprocessed_data$model %in% c("Mean", "Median")), ]
postprocessed_data <- rbind(postprocessed_data, postprocessed_naive_ensemble)

##################################################
# COMPUTE SCORE
# Compute WIS for all post-processed models, given truth final data
##################################################

# Make all models comparable: `skip_first_days = 40` (`+ 1` excluded, since the range for the post-processes data is different)
# + 30 to exclude the noisy results when skipping the recent past
skip_first_days <- uncertain_size + 30
wis_truth <- compute_wis_truth(data = postprocessed_data, truth_data = truth_data, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)

df_wis <- wis_truth$df_wis
wis_summ <- wis_truth$wis_summ

# Bar plot
reference_pts <- c(182.23, 175.92, 125.06, 135.40, 93.67, 137.85, 143.98, 209.49, 81.95, 80.76)
wis_bar <- plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = models, colors = colors, ylim_manual = 220, skip_space = TRUE, reference_pts = reference_pts)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = models, horizon = horizon, wis_summ = wis_summ)
wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = models, colors = colors)

if (skip_recent_days) { tmp_mtd <- "omit recent data" } else if (!skip_recent_days & method == "all_quant") { tmp_mtd <- "full set of quantiles" } else { tmp_mtd <- "plug-in point nowcast" }
tmp_ttl <- "" # paste("WIS (post-processed) ", ifelse(horiz, "varying weights horizon", "shared weights horizon"), " and ", tmp_mtd, sep = "")
p_total <- wis_bar + wis_line_horizon + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
ggsave(filename = paste("PLOTS/POSTPROCESS/WIS_all_post_skip_", skip_recent_days, "_horiz_", horiz, "_method_", method, ".jpeg", sep = ""), plot = p_total, width = 3500, height = 1400, units = c("px"), dpi = 300, bg = "white") 

##################################################
# PLOT OTHER POST-PROCESSED MODELS
# Must set `hh` to the horizon for the left-most plot
##################################################

hh <- 0 # Horizon for the left-most plot
extra_skip <- 30

postprocessed_plots_file <- paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/PLOTS/postprocessed_plots_", method, "_hh_", hh, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

if (file.exists(postprocessed_plots_file)) {
  postprocessed_plots <- readRDS(file = postprocessed_plots_file)
} else {
  postprocessed_plots <- list()
  for (i in 1:length(models)) {
    model <- models[i]
    color <- colors[i]
    
    print(paste("Model: ", model, " (", sprintf("%02d", i), "/", sprintf("%02d", length(models)), ").", sep = ""))
    postprocessed_plots[[i]] <- plot_postprocessed_models(data = data, nowcasts = cp_postprocessed_data, truth_data = truth_data, model = model, r = r, training_size = training_size, uncertain_size = uncertain_size, hh = hh, ens_method = ens_method, extra_skip = extra_skip, skip_recent_days = skip_recent_days)
  }

  saveRDS(object = postprocessed_plots, file = postprocessed_plots_file)
}

if (skip_recent_days) { tmp_mtd <- "omit recent data" } else if (!skip_recent_days & method == "all_quant") { tmp_mtd <- "full set of quantiles" } else { tmp_mtd <- "plug-in point nowcast" }
tmp_ttl <- paste("Post-processed unweighted ensemble (", ifelse(horiz, "varying weights horizon", "shared weights horizon"), " and ", tmp_mtd, ")", sep = "")
# p_total <- postprocessed_plots[[9]]$p_total / postprocessed_plots[[10]]$p_total + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
for (m in 1:length(models)) {
  ggsave(filename = paste("PLOTS/POSTPROCESS/post_", models[m], "_skip_", skip_recent_days, "_horiz_", horiz, "_method_", method, ".jpeg", sep = ""), plot = postprocessed_plots[[m]]$p_total, width = 3500, height = 1400, units = c("px"), dpi = 300, bg = "white") 
}
