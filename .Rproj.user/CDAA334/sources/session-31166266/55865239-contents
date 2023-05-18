
filter_data <- function (data, truth_data, loc = "DE", age_gr = "00+", extra_delay = 0, truth_past = 50, ...) {
  
  data <- data |> filter(location == loc, age_group == age_gr)
  truth_data <- truth_data |> filter(location == loc, age_group == age_gr)
  
  models <- unique(data$model)
  
  r_tmp <- list()
  for (i in 1:length(models)) {
    data_tmp <- data |> filter(model == models[i])
    r_tmp[[i]] <- range(data_tmp$forecast_date)
    
    if (i == 1) {
      r <- r_tmp[[i]]
    } else {
      r <- c(max(r[1], r_tmp[[i]][1]), min(r[2], r_tmp[[i]][2]))
    }
  }
  
  truth_r <- range(truth_data$date)
  r <- c(max(r[1], truth_r[1]), min(r[2], truth_r[2]))
  
  data <- data |> filter((forecast_date >= (r[1] + extra_delay)) & (forecast_date <= r[2]))
  truth_data <- truth_data |> filter((date >= (r[1] + extra_delay - truth_past)) & (date <= r[2]))
  
  list(data = data, truth_data = truth_data)
}

##################################################################################

select_real_data <- function (naive_ensemble, truth_data, dt, horizon, n_forecast = 28, stable = 40, total_days = 90, method = "median", current = FALSE, ...) {
  
  if (current) {
    target_end_date_value = dt + horizon
    
    real_data <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
  } else {
    r <- range(data$forecast_date)
    number_days <- as.numeric((dt - r[1]))
    number_days <- min(number_days, total_days)
    
    real_data <- rep(0, number_days)
    
    for (n in 1:number_days) {
      
      target_end_date_value = dt - n + horizon
      
      if ((- n + horizon) < (- stable)) {
        real_data[n] <- truth_data[truth_data$date == target_end_date_value, ]$truth
      } else if ((- n + horizon) < (- n_forecast)) {
        real_data[n] <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon), target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
      } else {
        real_data[n] <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
      }
    }
  }
  real_data
}

##################################################################################

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

##################################################################################

retrieve_data <- function (data, truth_data, naive_ensemble, models, horizon, start_date, end_date, skip_first_days = 1, total_days = 90, ...) {
  
  days <- seq(start_date + skip_first_days, end_date, by = "1 day") 
  
  y <- list()
  values <- list()
  current <- list()
  y_current <- list()
  
  count <- 1
  for (k in 1:length(days)) {
    dt <- days[k]
    print(paste(dt, " (", sprintf("%03d", count), "/", sprintf("%03d", length(days)), ")", sep = ""))
    
    values[[as.character(dt)]] <- list()
    y[[as.character(dt)]] <- list()
    current[[as.character(dt)]] <- list()
    y_current[[as.character(dt)]] <- list()
    
    b <- txtProgressBar(min = 1, max = length(horizon), initial = 1)
    
    for (i in 1:length(horizon)) {
      
      h <- horizon[i]
      
      y[[as.character(dt)]][[as.character(h)]] <- select_real_data(naive_ensemble = naive_ensemble, truth_data = truth_data, dt = dt, horizon = h, total_days = total_days)
      y_current[[as.character(dt)]][[as.character(h)]] <- select_real_data(naive_ensemble = naive_ensemble, truth_data = truth_data, dt = dt, horizon = h, current = TRUE)
      values[[as.character(dt)]][[as.character(h)]] <- select_nowcasts(data = data, dt = dt, horizon = h, models = models, total_days = total_days)
      current[[as.character(dt)]][[as.character(h)]] <- select_nowcasts(data = data, dt = dt, horizon = h, models = models, current = TRUE)
      
      setTxtProgressBar(b, i) 
    }
    close(b)
    count <- count + 1
  }
  
  list(y = y, y_current = y_current, values = values, current = current)
}

##################################################################################

compute_wis <- function (probs, quant, y, average = TRUE, ...) {
  if (length(probs) != length(quant)) { stop("'probs' and 'quant' must have the same size.") }
  partial <- apply(X = data.frame(probs = c(probs), quant = c(quant)), MARGIN = 1, FUN = function (x) {
    2 * (as.integer(y <= x[2]) - x[1]) * (x[2] - y)
  })
  if (average) {
    result <- mean(partial)
  } else {
    result <- partial
  }
  result
}

##################################################################################

compute_wis_data <- function (data, truth_data, start_date, end_date, horizon, models, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), method = "median", total_days = 90, skip_first_days = 1, quant = FALSE, training = TRUE, ...) {
  days <- seq(start_date + skip_first_days, end_date, by = "1 day")
  
  if (!quant) {
    wis <- matrix(data = 0, nrow = length(days), ncol = length(models))
  } else {
    wis <- array(data = 0, dim = c(length(days), length(models), length(probs)))
  }
  
  if (training) {
    
    b <- txtProgressBar(min = 0, max = length(days), initial = 0)
    for (k in 1:length(days)) {
      dt <- days[k]
      
      number_days <- as.numeric((dt - start_date))
      number_days <- min(number_days, total_days)
      
      if (!quant) {
        wis_tmp <- matrix(data = 0, nrow = number_days, ncol = length(models))
      } else {
        wis_tmp <- array(data = 0, dim = c(number_days, length(models), length(probs)))
      }
      
      for (n in 1:number_days) {
        tmp_data <- data[[as.character(dt)]][[as.character(horizon)]][[n]]
        tmp_y <- truth_data[[as.character(dt)]][[as.character(horizon)]][n]
        
        for (m in 1:length(models)) {
          if (!quant) {
            wis_tmp[n, m  ] <- compute_wis(probs = probs, quant = tmp_data[m, ], y = tmp_y, average = (!quant))  
          } else {
            wis_tmp[n, m, ] <- compute_wis(probs = probs, quant = tmp_data[m, ], y = tmp_y, average = (!quant))  
          }
          
        }
      }
      if (!quant) {
        wis[k,   ] <- apply(X = wis_tmp, MARGIN = 2      , FUN = mean)
      } else {
        wis[k, , ] <- apply(X = wis_tmp, MARGIN = c(2, 3), FUN = mean)
      }
      
      setTxtProgressBar(b, k)
    }
    close(b)
    
  } else {
    
    b <- txtProgressBar(min = 0, max = length(days), initial = 0)
    for (k in 1:length(days)) {
      dt <- days[k]
      td <- dt + horizon
      
      sliced_data <- data |> filter(forecast_date == dt, target_end_date == td, type == "quantile")
      sliced_y <- truth_data |> filter(date == td) |> select(truth) |> as.integer()  
      
      for (m in 1:length(models)) {
        if (!quant) {
          wis[k, m  ] <- compute_wis(probs = sliced_data[sliced_data$model == models[m], ]$quantile, quant = sliced_data[sliced_data$model == models[m], ]$value, y = sliced_y, average = (!quant))
        } else {
          wis[k, m, ] <- compute_wis(probs = sliced_data[sliced_data$model == models[m], ]$quantile, quant = sliced_data[sliced_data$model == models[m], ]$value, y = sliced_y, average = (!quant))
          
        }
      }
      setTxtProgressBar(b, k)
    }
    close(b)
  }
  wis
}

##################################################################################

compute_weights <- function (wis = NULL, ...) {
  weights <- 1 / wis
  if (is.null(dim(weights))) {
    weights <- weights / sum(weights)  
  } else {
    for (i in 1:ncol(weights)) {
      weights[, i] <- weights[, i] / sum(weights[, i])
    }
  }
  
  weights
}

##################################################################################

compute_ensemble <- function (values, current, weights = NULL, y = NULL, y_current = NULL, method = c("mean", "median", "wis", "pinball"), lower = 0, upper = 1, ...) {
  m <- method[1]
  
  if (m == "mean") {
    res <- ensemble_mean(values = current[[1]])
  } else if (m == "median") {
    res <- ensemble_median(values = current[[1]])
  } else if (m == "wis") {
    res <- ensemble_wis(values = current[[1]], weights = weights)
  } else if (m == "pinball") {
    res <- ensemble_pinball(values = values, current = current[[1]], y = y, y_current = y_current, lower = lower, upper = upper)
  } else {
    stop("Choose a valid method.")
  }
  
  res
}

##################################################################################

ensemble_mean <- function (values, ...) { 
  list(nowcast = apply(X = values, MARGIN = 2, FUN = mean), weights = NULL)
}

##################################################################################

ensemble_median <- function (values, ...) { 
  list(nowcast = apply(X = values, MARGIN = 2, FUN = median), weights = NULL)
}

##################################################################################

ensemble_wis <- function (values, weights, mean = TRUE, ...) {
  if (mean) {
    if (is.null(nrow(weights))) {
      res <- weights %*% values
    } else {
      res <- rep(0, 7)
      for (q in 1:7) {
        res[q] <- weights[, q] %*% values[, q]
      }
    }
    
  } else {
    print("Yet to be implemented")
  }
  list(nowcast = res, weights = weights)
}

##################################################################################

par_weights <- function (theta, wis, ...) {
  (exp(- theta * wis)) / (sum(exp(- theta * wis)))
}

##################################################################################

cost_function <- function (theta, probs, values, y, wis, ...) {
  N <- length(values)
  
  ens_wis <- rep(0, N)
  for (n in 1:N) {
    w <- par_weights(theta = theta, wis = wis[n, ])
    ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n])  
  }
  
  sum(ens_wis, na.rm = TRUE)
}

##################################################################################

ensemble_pinball <- function (values, current, y, y_current, initial_theta = 0, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), lower = 0, upper = 1, ...) {
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
  
  w <- par_weights(theta = theta, wis = apply(X = wis, MARGIN = 2, FUN = mean))
  
  list(nowcast = w %*% current, weights = w, theta = theta)
}

##################################################################################
