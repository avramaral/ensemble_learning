source("header.R")
source("utils.R")
source("aux.R")

ens_method <- "pinball" 
skip_recent_days <- FALSE 
method <- "Mean" # c("Mean", "Median", "all_quant") 

training_size <- 90 # 90
uncertain_size <- 40

quant <- TRUE
horiz <- TRUE

state_idx <- 17
age_idx <- 7

reparameterize <- TRUE 

reparameterize_file <- ifelse(reparameterize, "new_", "")
method_files <- ifelse(method == "all_quant", "all_quant_", "")

strata <- "all" # c("states", "ages", "all")

include_unw_ensemble <- F

##################################################
# LOAD AND PRE-PROCESS DATA (ALSO POST-PROCESSED DATA)
# `state_idx` and `age_idx` must be selected (if such data exist)
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

if (strata == "all") { KIT_frozen_baseline <- data %>% filter(model == "KIT-frozen_baseline") }

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

if (strata == "all") {
  r <- range(data$forecast_date)
  KIT_frozen_baseline <- KIT_frozen_baseline %>% filter(forecast_date >= r[1], forecast_date <= r[2], age_group %in% age, location %in% state)
  baseline <- KIT_frozen_baseline %>% filter(location == "DE", age_group == "00+", quantile == 0.5) %>% select(target_end_date, target, value)
  baseline_00 <- baseline %>% filter(target ==   "0 day ahead inc hosp") %>% select(target_end_date, value)
  baseline_14 <- baseline %>% filter(target == "-14 day ahead inc hosp") %>% select(target_end_date, value)
}

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
tmp_name <- ""
if (strata == "all") { 
  tmp_models <- models[1:8]; tmp_colors <- colors[1:8] 
  if (include_unw_ensemble) {
  tmp_models <- models;      tmp_colors <- colors
  tmp_name <- "full_"
  }
} else { 
  tmp_models <- models[1:7]; tmp_colors <- colors[1:7] 
  if (include_unw_ensemble) {
  tmp_models <- models;      tmp_colors <- colors
  tmp_name <- "full_"
  }
}

# Make all models comparable: `skip_first_days = 40` (`+ 1` excluded, since the range for the post-processes data is different)
# + 30 to exclude the noisy results when skipping the recent past
if (skip_recent_days) { skip_first_days <- 30 } else { skip_first_days <- uncertain_size + 30 }
wis_truth <- compute_wis_truth(data = postprocessed_data, truth_data = truth_data, models = tmp_models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)

df_wis <- wis_truth$df_wis
wis_summ <- wis_truth$wis_summ

coverage_file <- paste("RESULTS/FITTED_OBJECTS/COVERAGE/coverage_method_", tmp_name, method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_strata_", strata, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
if (file.exists(coverage_file)) {
  coverage_models <- readRDS(file = coverage_file)
} else {
  coverage_models <- compute_coverage(data = postprocessed_data, truth_data = truth_data, models = tmp_models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, strata = strata)
  saveRDS(object = coverage_models, file = coverage_file)
}

# Bar plot
reference_pts <- c(182.23, 175.92, 125.06, 135.40, 93.67, 137.85, 143.98, 209.49) #, 81.95, 80.76)
reference_pts_50 <- c(0.17, 0.19, 0.71, 0.09, 0.20, 0.24, 0.27, 0.18) #, 0.42, 0.37)
reference_pts_95 <- c(0.49, 0.66, 1.00, 0.27, 0.53, 0.55, 0.65, 0.41) #, 0.90, 0.75)

if (include_unw_ensemble) {
  reference_pts    <- c(reference_pts,  81.95, 80.76)
  reference_pts_50 <- c(reference_pts_50, 0.42, 0.37)
  reference_pts_95 <- c(reference_pts_95, 0.90, 0.75)
}


############################################
# Compute WIS for the baseline model (NEW) #
############################################

# Filtering is based on the `ensemble_data` object
tmp_baseline_data <- KIT_frozen_baseline %>% filter(location %in% unique(postprocessed_data$location), age_group == unique(postprocessed_data$age_group), forecast_date >= range(postprocessed_data$forecast_date)[1], forecast_date <= range(postprocessed_data$forecast_date)[2], !is.na(quantile))

wis_baseline <- compute_wis_truth(data = tmp_baseline_data, truth_data = truth_data, models = "KIT-frozen_baseline", horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)
df_wis_baseline <- wis_baseline$df_wis

############################################

wis_bar <- plot_wis_bar(df_wis = df_wis, wis_summ = wis_summ, models = tmp_models, colors = tmp_colors, ylim_manual = 220, skip_space = FALSE, reference_pts = reference_pts, change_name_select = FALSE, df_wis_baseline = df_wis_baseline)
coverage_bar <- plot_coverage(coverage_models = coverage_models, models = tmp_models, colors = tmp_colors, reference_pts_50 = reference_pts_50, reference_pts_95 = reference_pts_95, change_name_select = FALSE)

# Line plot over the horizons
df_wis_horizon_truth <- compute_wis_horizon_truth(models = tmp_models, horizon = horizon, wis_summ = wis_summ)
wis_line_horizon <- plot_wis_line_horizon(df_wis_horizon = df_wis_horizon_truth, models = tmp_models, colors = tmp_colors)

if (skip_recent_days) { tmp_mtd <- "omit recent data" } else if (!skip_recent_days & method == "all_quant") { tmp_mtd <- "full set of quantiles" } else { tmp_mtd <- "plug-in point nowcast" }
tmp_ttl <- "" # paste("WIS (post-processed) ", ifelse(horiz, "varying weights horizon", "shared weights horizon"), " and ", tmp_mtd, sep = "")
p_total <- wis_bar + wis_line_horizon + coverage_bar + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
saveRDS(object = p_total, file = paste("PLOTS/POSTPROCESS/WIS_all_post_skip_", tmp_name, skip_recent_days, "_horiz_", horiz, "_method_", method, ".RDS", sep = ""))
ggsave(filename = paste("PLOTS/POSTPROCESS/WIS_all_post_skip_", tmp_name, skip_recent_days, "_horiz_", horiz, "_method_", method, ".jpeg", sep = ""), plot = p_total, width = 4600, height = 1500, units = c("px"), dpi = 300, bg = "white")

# Alternative way to combine it, so we can include the title
t_all    <- grid::textGrob("National level", gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
c_all    <- (wrap_elements(panel = t_all)    / p_total)    + plot_layout(heights = c(1, 10))
ggsave(filename = paste("PLOTS/POSTPROCESS/WIS_all_post_skip_", tmp_name, skip_recent_days, "_horiz_", horiz, "_method_", method, ".jpeg", sep = ""), plot = c_all, width = 4600, height = 1500, units = c("px"), dpi = 300, bg = "white")


if (FALSE) { # Plot all training windows

  p_60  <- readRDS("PLOTS/POSTPROCESS/varying_weights_horizon_and_plug-in_point_nowcast/60/WIS_all_post_skip_FALSE_horiz_TRUE_method_Mean.RDS")  &  plot_annotation(title = "60")  & theme(plot.title = element_text(hjust = 0.5, size = 18))
  p_90  <- readRDS("PLOTS/POSTPROCESS/varying_weights_horizon_and_plug-in_point_nowcast/90/WIS_all_post_skip_FALSE_horiz_TRUE_method_Mean.RDS")  &  plot_annotation(title = "90")  & theme(plot.title = element_text(hjust = 0.5, size = 18))
  p_ALL <- readRDS("PLOTS/POSTPROCESS/varying_weights_horizon_and_plug-in_point_nowcast/200/WIS_all_post_skip_FALSE_horiz_TRUE_method_Mean.RDS") &  plot_annotation(title = "ALL") & theme(plot.title = element_text(hjust = 0.5, size = 18))

  t_60  <- grid::textGrob("60",  gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  t_90  <- grid::textGrob("90",  gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))
  t_ALL <- grid::textGrob("ALL", gp = gpar(fontfamily = "LM Roman 10", cex = 1.5))

  c_60  <- (wrap_elements(panel = t_60)  / p_60)  + plot_layout(heights = c(1, 10))
  c_90  <- (wrap_elements(panel = t_90)  / p_90)  + plot_layout(heights = c(1, 10))
  c_ALL <- (wrap_elements(panel = t_ALL) / p_ALL) + plot_layout(heights = c(1, 10))

  p_total3 <-   wrap_elements(c_60  + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) /
                wrap_elements(c_90  + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0)))) /
                wrap_elements(c_ALL + plot_annotation(theme = theme(plot.margin = margin(-15, 0, -5, 0))))
  ggsave(filename = "PLOTS/POSTPROCESS/varying_weights_horizon_and_plug-in_point_nowcast/WIS_POST_SIZES_3.jpeg", plot = p_total3, width = 4700, height = 4600, units = c("px"), dpi = 300, bg = "white")
}

##################################################
# PLOT OTHER POST-PROCESSED MODELS
# Must set `hh` to the horizon for the left-most plot
##################################################

hh <- -14 # 0 # Horizon for the left-most plot
extra_skip <- 30
if (hh == 0) { baseline_tmp <- baseline_00 } else { baseline_tmp <- baseline_14 }

average <- FALSE # Should daily WIS be aggregated in a MV?

if (!average) { tmp_char <- "raw_commonY_" } else { tmp_char <- "" }

postprocessed_plots_file <- paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/PLOTS/postprocessed_", tmp_char, "plots_", method, "_hh_", hh, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")

if (file.exists(postprocessed_plots_file)) {
  postprocessed_plots <- readRDS(file = postprocessed_plots_file)
} else {
  postprocessed_plots <- list()
  for (i in 1:length(tmp_models)) {
    model <- models[i]
    color <- colors[i]
    
    print(paste("Model: ", model, " (", sprintf("%02d", i), "/", sprintf("%02d", length(models)), ").", sep = ""))
    postprocessed_plots[[i]] <- plot_postprocessed_models(data = data, nowcasts = cp_postprocessed_data, truth_data = truth_data, model = model, r = r, training_size = training_size, uncertain_size = uncertain_size, baseline_tmp = baseline_tmp, hh = hh, ens_method = ens_method, extra_skip = extra_skip, skip_recent_days = skip_recent_days,
                                                          average = average)
  }

  saveRDS(object = postprocessed_plots, file = postprocessed_plots_file)
}

if (skip_recent_days) { tmp_mtd <- "omit recent data" } else if (!skip_recent_days & method == "all_quant") { tmp_mtd <- "full set of quantiles" } else { tmp_mtd <- "plug-in point nowcast" }
tmp_ttl <- paste("Post-processed unweighted ensemble (", ifelse(horiz, "varying weights horizon", "shared weights horizon"), " and ", tmp_mtd, ")", sep = "")
# p_total <- postprocessed_plots[[9]]$p_total / postprocessed_plots[[10]]$p_total + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
for (m in 1:length(tmp_models)) {
  ggsave(filename = paste("PLOTS/POSTPROCESS/post_", tmp_char, models[m], "_skip_", skip_recent_days, "_horiz_", horiz, "_method_", method, ".jpeg", sep = ""), plot = postprocessed_plots[[m]]$p_total, width = 3500, height = 1400, units = c("px"), dpi = 300, bg = "white")
}

# xxx <- postprocessed_plots

# I have to manually load the "-14 days" horizon
# p_total_tmp <- xxx[[4]]$p_total / postprocessed_plots[[8]]$p_total ## 0 and (-14) days, respectively (LMU and SZ)
# ggsave(filename = paste("PLOTS/POSTPROCESS/post_", tmp_char, "skip_", skip_recent_days, "_horiz_", horiz, "_method_", method, "_0_-14.jpeg", sep = ""), plot = p_total_tmp, width = 3500, height = 2800, units = c("px"), dpi = 300, bg = "white")
