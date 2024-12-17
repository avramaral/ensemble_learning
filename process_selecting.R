source("header.R")
source("utils.R")
source("aux.R")

ens_method <- "ranked_unweighted"

n_ensemble_models <- 1:8
unweighted_method <- c("Mean", "Median")
comb_models <- apply(X = expand.grid(unweighted_method, n_ensemble_models), MARGIN = 1, FUN = function (x, ...) { paste(x[1], "_", x[2], sep = "") } ) %>% sort()

skip_recent_days <- FALSE

training_size <- 90
uncertain_size <- 40

quant <- TRUE 
horiz <- TRUE

state_idx <- 17
age_idx <- 7

reparameterize <- TRUE 
method <- "Mean"

reparameterize_file <- ifelse(reparameterize, "new_", "")
method_files <- ifelse(method == "all_quant", "all_quant_", "")

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

models <- c(models, "Mean", "Median")
colors <- c(colors, "#009E73", "#60D1B3")

naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
naive_ensemble <- readRDS(file = naive_ensemble_file)
data <- rbind(data, naive_ensemble)

r <- range(data$forecast_date)
horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

extra_gap <- 30

##################################################
# LOAD FITTED MODELS AND COMPUTE THE WIS TRUTH
##################################################

groups <- 1:8

results_comb_files <- paste("OTHER_ANALYSES/RESULTS/naive_ensemble_group_", groups, ".RDS", sep = "")

lls <- rep(0, length(groups))
results_comb <- list()
sum_mean   <- NULL
sum_median <- NULL
for (i in 1:length(groups)) {
  print(paste("Group: ", i, sep = ""))
  
  results_comb[[i]] <- readRDS(file = results_comb_files[i])
  lls[i] <- length(results_comb[[i]]$wis_truth)
  
  if (i != groups[length(groups)]) { pb <- txtProgressBar(min = 1, max = lls[i], initial = 1) }
  for (l in 1:lls[i]) {
    ##################################################
    # Compute `wis_truth` for all combinations (SLOW)
    ##################################################
    tmp_wis_comp_file <-  paste("RESULTS/FITTED_OBJECTS/WIS/COMB/wis_", i, "_", l, "_extra_gap_", extra_gap, ".RDS", sep = "")
    if (!file.exists(tmp_wis_comp_file)) {
      tmp_data <- results_comb[[i]]$naive_ensemble[[l]]
  
      skip_first_days <- uncertain_size + extra_gap + 1
      tmp_wis <- compute_wis_truth(data = tmp_data, truth_data = truth_data, models = c("Mean", "Median"), horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days, verbose = FALSE)
    
      saveRDS(object = tmp_wis, file = tmp_wis_comp_file)
    } else {
      tmp_wis <- readRDS(file = tmp_wis_comp_file)
    }
    ##################################################
   
    tmp <- tmp_wis$df_wis
    tmp_sum_mean   <- sum(tmp[tmp$model == "Mean", "wis"])
    tmp_sum_median <- sum(tmp[tmp$model == "Median", "wis"])
    
    if (length(sum_mean) == 0) {
      sum_mean   <- c(i, tmp_sum_mean)
      sum_median <- c(i, tmp_sum_median)
    } else {
      sum_mean   <- rbind(sum_mean,   c(i, tmp_sum_mean))
      sum_median <- rbind(sum_median, c(i, tmp_sum_median))
    }
    
    if (i != groups[length(groups)]) { setTxtProgressBar(pb, l) }
  }
  if (i != groups[length(groups)]) { close(pb) }
}

sum_mean <- as.data.frame(sum_mean)
rownames(sum_mean) <- 1:nrow(sum_mean)
colnames(sum_mean) <- c("group", "wis")

sum_median <- as.data.frame(sum_median)
rownames(sum_median) <- 1:nrow(sum_median)
colnames(sum_median) <- c("group", "wis")

plot_comb <- function (data, name = "Mean", ylim = c(75, 200), col = "red", ...) {
  set.seed(1)
  data$group <- data$group + rnorm(length(data$group), 0, 0.1)
  p <- ggplot(data) + 
    geom_point(aes(x = group, y = wis), shape = 21, size = 2, color = col, fill = NA, stroke = 1) +
    scale_y_continuous(breaks = seq(ylim[1], ylim[2], length.out = 6)) +
    scale_x_continuous(breaks = 1:8) + # seq(min(data$group), max(data$group))) +
    labs(x = "Number of models", y = "WIS (Avg. over time points and horizons)", title = name) +
    expand_limits(y = ylim) +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12),
          text = element_text(family = "LM Roman 10"))
  
  p
}

p1 <- plot_comb(data = sum_mean, name = "Mean", col = "black", ylim = c(0, 200))
p2 <- plot_comb(data = sum_median, name = "Median", col = "black", ylim = c(0, 200))

p_total <- p1 + p2 + plot_annotation(theme = theme(plot.margin = margin()))
p_total

##################################################
# REAL-TIME MODEL SELECTION SCORE
##################################################

new_data <- list()
ensemble <- list()

# Load data
count <- 1
for (m in comb_models) {
  tmp_file <- paste("RESULTS/FITTED_OBJECTS/", reparameterize_file, method_files, "method_", ens_method, "_", m ,"_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
  tmp_data <- readRDS(file = tmp_file)
  tmp_new_data <- tmp_data$new_data
  tmp_new_data$model <- m
  
  new_data[[count]] <- tmp_new_data
  ensemble[[count]] <- tmp_data$ensemble
  
  count <- count + 1
}

r <- range(new_data[[1]]$forecast_date)

# Compute score
# Make all models comparable: `skip_first_days = uncertain_size + extra_gap` (to prevent unstable results)
skip_first_days <- uncertain_size + extra_gap # 30

tmp_wis_file <- paste("RESULTS/FITTED_OBJECTS/WIS/wis_selecting_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), "_extra_gap_", extra_gap, ".RDS", sep = "")

if (!file.exists(tmp_wis_file)) { 
  
  wis_truth <- list()
  for (i in 1:length(comb_models)) {
    print(comb_models[i])
    wis_truth[[as.character(comb_models[i])]] <- compute_wis_truth(data = new_data[[i]], truth_data = truth_data, models = comb_models[[i]], horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = skip_first_days)
  }
  
  saveRDS(object = wis_truth, file = tmp_wis_file)
} else {
  wis_truth <- readRDS(file = tmp_wis_file)
}

##################################################
# PLOT RESULTS
##################################################

wis_mean   <- data.frame(n_models = 1:8, value = rep(0, 8))
wis_median <- data.frame(n_models = 1:8, value = rep(0, 8))

for (i in 1:8) {
  wis_truth_mean <- wis_truth[[paste("Mean_", i, sep = "")]]
  wis_mean[i, "value"] <- sum(wis_truth_mean$df_wis[, "wis"])
  
  wis_truth_median <- wis_truth[[paste("Median_", i, sep = "")]]
  wis_median[i, "value"] <- sum(wis_truth_median$df_wis[, "wis"])
}

common_col <- "red"
common_fil <- "red" # NA

wis_mean[8, 2] <- sum_mean[nrow(sum_mean), 2]
wis_median[8, 2] <- sum_median[nrow(sum_median), 2]

shape_plot <- 21 # 23
p1_updated <- p1 + geom_point(data = wis_mean  , aes(x = n_models, y = value), color = common_col, fill = common_fil, size = 2, shape = shape_plot, stroke = 1) + geom_hline(yintercept = sum_mean[  nrow(sum_mean  ), 2], linetype = "dashed")
p2_updated <- p2 + geom_point(data = wis_median, aes(x = n_models, y = value), color = common_col, fill = common_fil, size = 2, shape = shape_plot, stroke = 1) + geom_hline(yintercept = sum_median[nrow(sum_median), 2], linetype = "dashed")

tmp_ttl <- "" # paste("WIS (model selection) ", ifelse(horiz, "varying weights horizon", "shared weights horizon"), sep = "")
p_total_updated <- p1_updated + p2_updated # + plot_annotation(title = tmp_ttl, theme = theme(plot.margin = margin(), text = element_text(size = 14, family = "LM Roman 10")))
p_total_updated

saveRDS(object = p_total_updated, file = paste("PLOTS/SELECTION/WIS_selection_horiz_", horiz, ".RDS", sep = ""))
ggsave(filename = paste("PLOTS/SELECTION/WIS_selection_horiz_", horiz, ".jpeg", sep = ""), plot = p_total_updated, width = 3500, height = 1400, units = c("px"), dpi = 300, bg = "white") 

