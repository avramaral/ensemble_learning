source("header.R")
source("utils.R")
source("aux.R")

# Setting: PP4, as per Table 1 
skip_recent_days <- FALSE 
method <- "Mean" # c("Mean", "Median", "all_quant") 

strata <- "all"
state_idx <- 17
age_idx <- 7

quant <- TRUE
horiz <- TRUE

training_size <- 90 
uncertain_size <- 40

reparameterize <- TRUE 
reparameterize_file <- ifelse(reparameterize, "new_", "")
method_files <- ifelse(method == "all_quant", "all_quant_", "")

#############################
# LOAD AND PRE-PROCESS DATA #
#############################

### RAW DATA ###

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

data <- data %>% filter(!is.na(quantile), !(model %in% c("Mean", "Median")))

### POST-PROCESSED DATA ###

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

postprocessed_data <- postprocessed_data %>% filter(location %in% unique(data$location), age_group %in% unique(data$age_group), forecast_date >= range(data$forecast_date)[1], forecast_date <= range(data$forecast_date)[2], model %in% unique(data$model))

##################################################
##################################################

# Make both data sets comparable 

simple_models <- models[!(models %in% c("Mean", "Median"))]

data <- data %>% filter(forecast_date >= range(postprocessed_data$forecast_date)[1], forecast_date <= range(postprocessed_data$forecast_date)[2]) %>% arrange(location, age_group, model, forecast_date, target_end_date)
postprocessed_data <- postprocessed_data %>% arrange(location, age_group, model, forecast_date, target_end_date)

pairwise_models <- combn(simple_models, 2)

filename_dist <- paste("PLOTS/POSTPROCESS/pairwise_comparison_skip_", skip_recent_days, "_horiz_", horiz, "_method_", method, ".RDS", sep = "")

if (!file.exists(filename_dist)) {
  dist_results <- list()
  dist_results_horizon <- list()
  dist_results_horizon_all <- list()
  for (h in horizon) { dist_results_horizon[[as.character(h)]] <- list(dist_raw = c(), dist_postprocessed = c()) }
  for (i in 1:ncol(pairwise_models)) {
    print(paste("Pair: ", i, " (out of ", ncol(pairwise_models), ")", sep = ""))
    
    pair <- c(pairwise_models[, i])
    
    # Raw data
    
    data_1 <- data %>% filter(model == pair[1])
    data_2 <- data %>% filter(model == pair[2])
    
    postprocessed_data_1 <- postprocessed_data %>% filter(model == pair[1])
    postprocessed_data_2 <- postprocessed_data %>% filter(model == pair[2])
    
    days <- seq(range(data$forecast_date)[1], range(data$forecast_date)[2], by = "1 day")
    
    
    dist_raw <- c(); dist_postprocessed <- c()
    count_days <- 0
    pb <- txtProgressBar(min = 1, max = length(days), initial = 1)
    for (d in days) {
      count_days <- count_days + 1
      
      data_1_tmp <- data_1 %>% filter(forecast_date == d)
      data_2_tmp <- data_2 %>% filter(forecast_date == d)
      
      postprocessed_data_1_tmp <- postprocessed_data_1 %>% filter(forecast_date == d)
      postprocessed_data_2_tmp <- postprocessed_data_2 %>% filter(forecast_date == d)
      
      for (h in horizon) {
        
        data_1_tmp_tmp <- data_1_tmp %>% filter(target == paste(h, " day ahead inc hosp", sep = ""))
        data_2_tmp_tmp <- data_2_tmp %>% filter(target == paste(h, " day ahead inc hosp", sep = ""))
        
        postprocessed_data_1_tmp_tmp <- postprocessed_data_1_tmp %>% filter(target == paste(h, " day ahead inc hosp", sep = ""))
        postprocessed_data_2_tmp_tmp <- postprocessed_data_2_tmp %>% filter(target == paste(h, " day ahead inc hosp", sep = ""))
        
        tmp_dist_raw           <- wis_distance(q_vect = data_1_tmp_tmp$value,               obs_vect = data_2_tmp_tmp$value              )
        tmp_dist_postprocessed <- wis_distance(q_vect = postprocessed_data_1_tmp_tmp$value, obs_vect = postprocessed_data_2_tmp_tmp$value)
        
        dist_raw           <- c(dist_raw,           tmp_dist_raw)
        dist_postprocessed <- c(dist_postprocessed, tmp_dist_postprocessed)
        
        dist_results_horizon[[as.character(h)]]$dist_raw           <- c(dist_results_horizon[[as.character(h)]]$dist_raw,           tmp_dist_raw)
        dist_results_horizon[[as.character(h)]]$dist_postprocessed <- c(dist_results_horizon[[as.character(h)]]$dist_postprocessed, tmp_dist_postprocessed)
        
      }
      setTxtProgressBar(pb, count_days)
    }
    close(pb)
    
    dist_results[[i]] <- list(dist_raw = dist_raw, dist_postprocessed = dist_postprocessed)
    dist_results_horizon_all[[i]] <- dist_results_horizon
  }
  
  saveRDS(object = dist_results,             file = filename_dist)
  saveRDS(object = dist_results_horizon_all, file = sub("Mean", paste0(method, "_horiz"), filename_dist))
  
} else {
  dist_results             <- readRDS(file = filename_dist)
  dist_results_horizon_all <- readRDS(file = sub("Mean", paste0(method, "_horiz"), filename_dist))
}


dist_raw <- c(); dist_postprocessed <- c()
for (i in 1:ncol(pairwise_models)) {
  
  dist_raw           <- c(dist_raw,           dist_results[[i]]$dist_raw)
  dist_postprocessed <- c(dist_postprocessed, dist_results[[i]]$dist_postprocessed)
}

all_dist <- as_tibble(data.frame(dist_raw = dist_raw, dist_postprocessed = dist_postprocessed))


all_dist_horizon <- list()
for (h in horizon) {
  dist_raw <- c(); dist_postprocessed <- c()
  for (i in 1:ncol(pairwise_models)) {
    dist_raw           <- c(dist_raw,           dist_results_horizon_all[[i]][[as.character(h)]]$dist_raw)
    dist_postprocessed <- c(dist_postprocessed, dist_results_horizon_all[[i]][[as.character(h)]]$dist_postprocessed)
  }
  all_dist_horizon[[as.character(h)]] <- as_tibble(data.frame(dist_raw = dist_raw, dist_postprocessed = dist_postprocessed))
}

# summary_stats <- all_dist %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
#   group_by(variable) %>%
#   summarise(lower  = quantile(value, 0.025),
#             upper  = quantile(value, 0.975),
#             median = median(value),
#             mean   = mean(value)) %>% mutate(variable = factor(variable, levels = c("dist_raw", "dist_postprocessed")))
# 
# pp_error_bar <- ggplot(summary_stats, aes(x = variable)) +
#   geom_errorbar(aes(ymin = lower,  ymax = upper ), width = 0.250, colour = "black") +
#   geom_errorbar(aes(ymin = median, ymax = median), width = 0.125, colour = "blue") +
#   geom_point(aes(y = mean),   shape = 4,  size = 5, colour = "red") +
#   scale_x_discrete(labels = c("dist_raw" = "Original models", "dist_postprocessed" = "Post-processed models")) +
#   labs(title = "", x = "", y = "Integrated Quadratic Distance (IQD)") +
#   theme_bw() +
#   theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"))
#
# ggsave(filename = paste("PLOTS/POSTPROCESS/errorbar_pairwise_comparison_skip_", skip_recent_days, "_horiz_", horiz, "_method_", method, ".jpeg", sep = ""), plot = pp_error_bar, width = 1200, height = 1050, units = c("px"), dpi = 300, bg = "white")

filtered_data <- all_dist %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
                              group_by(variable) %>%
                              filter(value >= quantile(value, 0.025) & value <= quantile(value, 0.975)) %>%  mutate(variable = factor(variable, levels = c("dist_raw", "dist_postprocessed")))

pp_violin <- ggplot(filtered_data, aes(x = variable, y = value, fill = variable)) +
  geom_violin(colour = "black") +
  stat_summary(fun = "median", geom = "point", aes(shape = "Median"), size = 2.5, colour = "black") +
  stat_summary(fun = "mean",   geom = "point", aes(shape = "Mean"),   size = 2.5, colour = "black") +
  scale_x_discrete(labels = c("dist_raw" = "Original models", "dist_postprocessed" = "Post-processed models")) +
  scale_fill_manual(values = c("dist_raw" = "#0000FF44", "dist_postprocessed" = "#FF000044")) +
  scale_shape_manual(values = c("Median" = 1, "Mean" = 4)) +
  labs(title = "All horizons", x = "", y = "Integrated Quadratic Distance (IQD)", shape = "") +
  guides(fill = "none") +
  ylim(c(0, 1300)) + 
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"))

############################
# PLOTS FOR OTHER HORIZONS #
############################

# HORIZON = 0

tmp_hh <- 0
filtered_data_opt_1 <- all_dist_horizon[[as.character(tmp_hh)]] %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
                                                                   group_by(variable) %>%
                                                                   filter(value >= quantile(value, 0.025) & value <= quantile(value, 0.975)) %>%  mutate(variable = factor(variable, levels = c("dist_raw", "dist_postprocessed")))

pp_violin_opt_1 <- ggplot(filtered_data_opt_1, aes(x = variable, y = value, fill = variable)) +
  geom_violin(colour = "black") +
  stat_summary(fun = "median", geom = "point", aes(shape = "Median"), size = 2.5, colour = "black") +
  stat_summary(fun = "mean",   geom = "point", aes(shape = "Mean"),   size = 2.5, colour = "black") +
  scale_x_discrete(labels = c("dist_raw" = "Original models", "dist_postprocessed" = "Post-processed models")) +
  scale_fill_manual(values = c("dist_raw" = "#0000FF44", "dist_postprocessed" = "#FF000044")) +
  scale_shape_manual(values = c("Median" = 1, "Mean" = 4)) +
  labs(title = "Horizon = 0 days", x = "", y = "Integrated Quadratic Distance (IQD)", shape = "") +
  guides(fill = "none") +
  ylim(c(0, 1300)) + 
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"))

# HORIZON = 0

tmp_hh <- -7
filtered_data_opt_2 <- all_dist_horizon[[as.character(tmp_hh)]] %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
                                                                   group_by(variable) %>%
                                                                   filter(value >= quantile(value, 0.025) & value <= quantile(value, 0.975)) %>%  mutate(variable = factor(variable, levels = c("dist_raw", "dist_postprocessed")))

pp_violin_opt_2 <- ggplot(filtered_data_opt_2, aes(x = variable, y = value, fill = variable)) +
  geom_violin(colour = "black") +
  stat_summary(fun = "median", geom = "point", aes(shape = "Median"), size = 2.5, colour = "black") +
  stat_summary(fun = "mean",   geom = "point", aes(shape = "Mean"),   size = 2.5, colour = "black") +
  scale_x_discrete(labels = c("dist_raw" = "Original models", "dist_postprocessed" = "Post-processed models")) +
  scale_fill_manual(values = c("dist_raw" = "#0000FF44", "dist_postprocessed" = "#FF000044")) +
  scale_shape_manual(values = c("Median" = 1, "Mean" = 4)) +
  labs(title = "Horizon = -7 days", x = "", y = "Integrated Quadratic Distance (IQD)", shape = "") +
  guides(fill = "none") +
  ylim(c(0, 1300)) + 
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"))


##################
# COMBINED PLOTS #
##################

pp_violin_opt_1_no_ylabel <- pp_violin_opt_1 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
pp_violin_opt_2_no_ylabel <- pp_violin_opt_2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

pp_wis_age_comb <- pp_violin + pp_violin_opt_1_no_ylabel + pp_violin_opt_2_no_ylabel + plot_layout(guides = "collect") & theme(legend.position = "bottom") 

ggsave(filename = paste("PLOTS/POSTPROCESS/violin_pairwise_comparison_skip_",   skip_recent_days, "_horiz_", horiz, "_method_", method, ".jpeg", sep = ""), plot = pp_wis_age_comb, width = 3000, height = 1250, units = c("px"), dpi = 300, bg = "white")




