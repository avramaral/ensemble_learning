source("header.R")
source("utils.R")
source("aux.R")

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

naive_ensemble <- readRDS(file = "TMP/naive_ensemble.RDS")

filtered_data <- filter_data(data = data, truth_data = truth_data, loc = "DE", age_gr = "00+", extra_delay = 7)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") #, "MeanEnsemble", "MedianEnsemble")
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") #, "#009E73", "#60D1B3")
r <- range(data$forecast_date)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

###########################################################################

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

quant <- FALSE

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

new_data <- data.frame(location = character(), age_group = character(), forecast_date = as.Date(character()), target_end_date = as.Date(character()), target = character(), type = character(), quantile = double(), value = double(), pathogen = character(), model = character(), retrospective = logical(), stringsAsFactors = FALSE)
new_data <- as_tibble(new_data)
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
method <- "wis" # c("mean", "median", "wis", "pinball")

skip_first_days <- 1
days <- seq(r[1] + skip_first_days, r[2], by = "1 day") 

ensemble <- list()
count <- 1
for (k in 1:length(days)) { 
  
  dt <- days[k]
  
  print(paste(dt, " (", sprintf("%03d", count), "/", sprintf("%03d", length(days)), ")", sep = ""))
  
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
                                                                        lower = 0, upper = 1)
    
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
  
  count <- count + 1
}

###########################################################################

cmb_data <- rbind(data, new_data)

cmb_models <- c(models, "MeanEnsemble", "MedianEnsemble", method)
cmb_colors <- c(colors, "#009E73", "#60D1B3", "#FF0000")

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

###########################################################################
