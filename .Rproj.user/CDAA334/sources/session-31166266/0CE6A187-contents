select_real_data <- function (naive_ensemble, truth_data, dt, horizon, n_forecast = 28, stable = 40, total_days = 90, method = "median", ...) {
  
  r <- range(data$forecast_date)
  number_days <- as.numeric((dt - r[1]))
  number_days <- min(number_days, total_days)
  
  real_data <- rep(0, number_days)
  
  for (n in 1:number_days) {
    
    target_end_date_value = dt - n + horizon
    
    if ((- n + horizon) < (-stable)) {
      real_data[n] <- truth_data[truth_data$date == target_end_date_value, ]$truth
    } else if ((- n + horizon) < (- n_forecast)) {
      real_data[n] <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon), target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
    } else {
      real_data[n] <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
    }
  }
  real_data
}

# MAKE IT FASTER
select_nowcasts <- function (data, dt, horizon, models, n_quantiles = 7, total_days = 90, current = FALSE, ...) {
  
  values_list <- list()
  
  if (current) {
    
    values <- matrix(data = 0, nrow = length(models), ncol = n_quantiles)
    
    for (m in 1:length(models)) {
      
      tmp <- data |> filter(forecast_date == dt, target_end_date == dt + horizon, type == "quantile", model == models[m]) |> select(value) |> unlist()  
      
      if (length(tmp) != 0) {
        values[m, ] <- tmp
      } else {
        values[m, ] <- rep(NA, n_quantiles)
      }
    }
    
    values_list[[1]] <- values
    
  } else {
    
    r <- range(data$forecast_date)
    number_days <- as.numeric((dt - r[1]))
    number_days <- min(number_days, total_days)
    
    for (n in 1:number_days) {
      
      values <- matrix(data = 0, nrow = length(models), ncol = n_quantiles)
      
      for (m in 1:length(models)) {
        
        tmp <- data |> filter(forecast_date == dt - n, target_end_date == dt - n + horizon, type == "quantile", model == models[m]) |> select(value) |> unlist()  
        
        if (length(tmp) != 0) {
          values[m, ] <- tmp
        } else {
          values[m, ] <- rep(NA, n_quantiles)
        }
      }
      
      values_list[[n]] <- values
      
    }
  }

  values_list
}

##################################################

# Reparameterize the weights as a function of theta
par_weights <- function (theta, wis, ...) {
  (exp(- theta * wis)) / (sum(exp(- theta * wis)))
}

# Cost function to be minimized (internal function)
cost_function <- function (theta, probs, values, y, wis, ...) {
  N <- length(values)
  
  ens_wis <- rep(0, N)
  for (n in 1:N) {
    w <- par_weights(theta = theta, wis = wis[n, ])
    ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n])  
  }
  
  sum(ens_wis, na.rm = TRUE)
}

# Compute ensemble prediction, such that the WIS is minimized (regression approach)
# values: matrix of M x Q, where M is the number of models and Q is the number of quantiles
# y: true value
# initial_theta: initial guess for theta (= 0, i.e., non-weighed model)
# probs: vector of size Q with the quantile levels
ensemble_pinball <- function (values, y, initial_theta = 0, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), lower = -1, upper = 1, ...) {
  N <- length(values)
  M <- nrow(values[[1]])
  
  wis <- matrix(data = 0, nrow = N, ncol = M)
  for (n in 1:N) {
    wis[n, ] <- apply(X = values[[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[n])  
  }
  
  if (sum(wis, na.rm = TRUE) == 0) {
    theta <- 0
  } else {
    theta <- optimize(f = cost_function, lower = lower, upper = upper, maximum = FALSE, probs = probs, values = values, y = y, wis = wis)$minimum  
  }
  
  list(theta = theta)
}

