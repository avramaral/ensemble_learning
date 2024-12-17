source("header.R")

# Hint: the possible values for each parameter are written as an in-line comment

ens_method <- "pinball" # c("wis", "pinball")
skip_recent_days <- TRUE # c(TRUE, FALSE) # Skip the recent past, or replace it by our "best guess"

training_size <- 90  # c(90)
uncertain_size <- 40 # c(40)

quant <- TRUE  # Weights depend (or not) on the quantiles (now, only `TRUE` has fitted objects)
horiz <- TRUE  # Weights depend (or not) on the horizons  (now, only `TRUE` has fitted objects)

post_processing <- FALSE # c(TRUE, FALSE)
post_select_mod <- "Epiforecasts" # Only makes sense if `post_processing == TRUE`

state <- "DE"  # c("DE")
age   <- "00+" # c("00+")

fitted_model_file <- ifelse(post_processing,
                            paste("RESULTS/FITTED_OBJECTS/POST_PROCESSED/post-processing_model_", ens_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""),
                            paste("RESULTS/FITTED_OBJECTS/method_", ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))

f <- readRDS(file = fitted_model_file)

##### Newly generated data set based on the chosen approach (in the same format as the original data)
new_data <- f$new_data

new_data_file <- ifelse(post_processing,
                        paste("FORMATTED_RESULTS/POST_PROCESSED/NEW_DATA_post-processing_model_", ens_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""),
                        paste("FORMATTED_RESULTS/NEW_DATA_method_", ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))

saveRDS(object = new_data, file = new_data_file)

new_data

##### Estimated parameters
p <- f$ensemble

forecast_days <- as.Date(names(p))
horizons      <- as.numeric(names(p[[1]]))
quantiles     <- as.numeric(names(p[[1]][[1]]$theta))
models        <- colnames(p[[1]][[1]]$weights)

# Format the estimates

create_new_tibble <- function (...) {
  as_tibble(data.frame(location         = character(), 
                       age_group        = character(), 
                       forecast_date    = as.Date(character()), 
                       target_end_date  = as.Date(character()), 
                       target           = character(),
                       quantile         = double(),
                       weight           = double(), 
                       theta            = double(), 
                       phi              = double(), 
                       model            = character(), 
                       stringsAsFactors = FALSE))
}

estimates_file <- ifelse(post_processing,
                         paste("FORMATTED_RESULTS/POST_PROCESSED/ESTIMATES_post-processing_model_", ens_models, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""),
                         paste("FORMATTED_RESULTS/ESTIMATES_method_", ens_method, "_size_", training_size, "_skip_", as.character(skip_recent_days), "_state_", state, "_age_", age, "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = ""))

if (!file.exists(estimates_file)) {
  estimates <- create_new_tibble()
  
  pb <- txtProgressBar(min = 1, max = length(forecast_days), initial = 1)
  for (i in 1:length(forecast_days)) {
    d <- as.Date(forecast_days[i])
    for (h in horizons) {
      for (q in quantiles) {
        for (m in models) {
          estimates <- estimates %>% add_row(location        = state,
                                             age_group       = age,
                                             forecast_date   = d,
                                             target_end_date = d - h,
                                             target          = paste(h, "day ahead inc hosp"),
                                             quantile        = q,
                                             weight          = p[[as.character(d)]][[as.character(h)]]$weights[as.character(q), m],
                                             theta           = p[[as.character(d)]][[as.character(h)]]$theta[as.character(q)],
                                             phi             = p[[as.character(d)]][[as.character(h)]]$phi[which(quantiles == q)],
                                             model           = m)       
        }
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  saveRDS(object = estimates, file = estimates_file)
} else {
  estimates <- readRDS(estimates_file)
}

estimates


