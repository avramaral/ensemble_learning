source("header.R")
source("utils.R")
source("aux.R")

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

naive_ensemble <- readRDS(file = "TMP/naive_ensemble.RDS")

filtered_data <- filter_data(data = data, truth_data = truth_data, loc = "DE", age_gr = "00+", extra_delay = 7)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

models <- c("KIT", "LMU") # c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") #, "MeanEnsemble", "MedianEnsemble")
colors <- c("#56B4E9", "#F0E442") # c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") #, "#009E73", "#60D1B3")
r <- range(data$forecast_date)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

###########################################################################

# Slice the original data files and return a "training set" (both for the nowcasts and real values) based on the past X days.

if (FALSE) {
  retrieved_data <- retrieve_data(data = data, truth_data = truth_data, naive_ensemble = naive_ensemble, models = models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = 1, total_days = 90)
  
  y         <- retrieved_data$y
  y_current <- retrieved_data$y_current 
  values    <- retrieved_data$values
  current   <- retrieved_data$current
} else {
  y         <- readRDS(file = "TMP/y.RDS")
  y_current <- readRDS(file = "TMP/y_current.RDS") 
  values    <- readRDS(file = "TMP/values.RDS")
  current   <- readRDS(file = "TMP/current.RDS")
}

###########################################################################

# Compute WIS and (untrained) weights (as the inverse of WIS).

quant <- TRUE

wis <- list()
wis_cm <- list()

for (h in horizon) {
  print(paste("Horizon: ", h, sep = ""))
  
  wis[[as.character(h)]] <- compute_wis_data(data = values, truth_data = y, start_date = r[1], end_date = r[2], horizon = h, models = models, method = "median", quant = quant)
  
  if (!quant) {
    wis_cm[[as.character(h)]] <- wis[[as.character(h)]] |> colMeans(na.rm = TRUE)
  } else {
    wis_cm[[as.character(h)]] <- apply(X = wis[[as.character(h)]], MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)
  }
}

plotting_WIS(horizon = horizon, wis_cm = wis_cm, colors = colors, models = models, quant = quant) 

weights <- lapply(X = wis_cm, FUN = compute_weights)

###########################################################################

# Compute the ensemble nowcasts based on different implemented methods ("mean", "median", "wis", "pinball"). The weights can be common to all quantiles or separately estimated ("quant = TRUE").

quant <- TRUE
hh    <- TRUE

new_data <- data.frame(location = character(), age_group = character(), forecast_date = as.Date(character()), target_end_date = as.Date(character()), target = character(), type = character(), quantile = double(), value = double(), pathogen = character(), model = character(), retrospective = logical(), stringsAsFactors = FALSE)
new_data <- as_tibble(new_data)
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
method <- "median" # c("mean", "median", "wis", "pinball")

skip_first_days <- 1
days <- seq(r[1] + skip_first_days, r[2], by = "1 day") 

ensemble <- list()
count <- 1
for (k in 1:length(days)) { 
  
  dt <- days[k]
  
  print(paste(dt, " (", sprintf("%03d", count), "/", sprintf("%03d", length(days)), ")", sep = ""))
  
  if (hh) {
    ensemble[[as.character(dt)]] <- list()
    
    b <- txtProgressBar(min = 1, max = length(horizon), initial = 1) 
    
    for (i in 1:length(horizon)) {
      h <- horizon[i]
      
      ensemble[[as.character(dt)]][[as.character(h)]] <- compute_ensemble(values = values[[as.character(dt)]][[as.character(h)]],
                                                                          current = current[[as.character(dt)]][[as.character(h)]],
                                                                          weights = weights[[as.character(h)]],
                                                                          y = y[[as.character(dt)]][[as.character(h)]],
                                                                          y_current = y_current[[as.character(dt)]][[as.character(h)]],
                                                                          method = method,
                                                                          lower = 0, upper = 1, quant = quant)
      
      for (q in 1:7) {
        value <- ensemble[[as.character(dt)]][[as.character(h)]]$nowcast
        if (class(value)[1] == "numeric") {
          value <- value[q]
        } else  {
          value <- value[1, q]
        }
        new_data <- new_data |> add_row(location = "DE", age_group = "00+", forecast_date = dt, target_end_date = (dt + h), target = paste(h, " day ahead inc hosp", sep = ""), type = "quantile", quantile =  probs[q], value = value, pathogen = "COVID-19", model = method, retrospective = FALSE)
      }
      
      setTxtProgressBar(b, i)
    }
    close(b)
  } else { # Aggregated over quantiles (only implemented for the "pinball" method)
    
    ensemble[[as.character(dt)]] <- compute_ensemble(values = values[[as.character(dt)]],
                                                     current = current[[as.character(dt)]],
                                                     weights = weights,
                                                     y = y[[as.character(dt)]],
                                                     y_current = y_current[[as.character(dt)]],
                                                     method = method,
                                                     lower = 0, upper = 1, quant = quant, hh = hh)
    
    
    for (i in 1:length(horizon)) {
      for (q in 1:7) {
        value <- ensemble[[as.character(dt)]]$nowcast[as.character(horizon[i]), q]
        new_data <- new_data |> add_row(location = "DE", age_group = "00+", forecast_date = dt, target_end_date = (dt + horizon[i]), target = paste(horizon[i], " day ahead inc hosp", sep = ""), type = "quantile", quantile =  probs[q], value = value, pathogen = "COVID-19", model = method, retrospective = FALSE)
      }
    }
    
  }
  
  count <- count + 1
}

saveRDS(object = list(ensemble = ensemble, new_data = new_data), file = paste("TMP/FITTED_OBJECTS/ensemble_", method, "_quant_", quant, "_hh_", hh, ".RDS", sep = ""))

if (TRUE) {
  w_hat <- weights_tibble(ensemble = ensemble, r = r, models = models, horizon = horizon)
  saveRDS(object = w_hat, file = paste("TMP/FITTED_OBJECTS/W_HAT_ensemble_", method, "_quant_", quant, "_hh_", hh, ".RDS", sep = ""))
  plotting_weights(w_hat = w_hat, h = 0, q = probs[1])
}

###########################################################################

# Compute WIS for the ensemble approach based on the newly generated nowcasts.

cmb_data <- rbind(data, new_data)
# cmb_data <- rbind(cmb_data, new_data)

cmb_models <- c(models, "MeanEnsemble", "MedianEnsemble", method)
cmb_colors <- c(colors, "#009E73", "#60D1B3", "#FF0000")
# cmb_models <- c(models, "mean", "median", "pinball")
# cmb_colors <- c(colors, "#009E73", "#60D1B3", "#FF0000")

cmb_wis <- list()
cmb_wis_cm <- list()

quant <- FALSE
skip_first_days <- 1

for (h in horizon) {
  print(paste("Horizon: ", h, sep = ""))
  cmb_wis[[as.character(h)]] <- compute_wis_data(data = cmb_data, truth_data = truth_data, start_date = r[1], end_date = r[2], horizon = h, models = cmb_models, skip_first_days = skip_first_days, quant = quant, training = FALSE) 
  if (!quant) {
    cmb_wis_cm[[as.character(h)]] <- cmb_wis[[as.character(h)]] |> colMeans(na.rm = TRUE)
  } else {
    cmb_wis_cm[[as.character(h)]] <- apply(X = cmb_wis[[as.character(h)]], MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)
  }
}

plotting_WIS(horizon = horizon, wis_cm = cmb_wis_cm, colors = cmb_colors, models = cmb_models, quant = quant) 

if (quant) {
  wis_days <- list()
  for (q in 1:length(probs)) {
    wis_days[[q]] <- compute_wis_days(wis = cmb_wis, models = cmb_models, q = q)
    tmp_plot <- plotting_wis_days(wis_days = wis_days[[q]], q = probs[q])
    print(tmp_plot)
  }
} else {
  wis_days <- compute_wis_days(wis = cmb_wis, models = cmb_models)
  tmp_plot <- plotting_wis_days(wis_days = wis_days, q = "aggregated")
  print(tmp_plot)
}

###########################################################################