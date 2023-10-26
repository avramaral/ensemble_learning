
##################################################
##################################################
##################################################

unregister_dopar <- function () {
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}

##################################################
##################################################
##################################################

filter_data <- function (data, truth_data, models, loc = "DE", age_gr = "00+", extra_delay = 0, truth_past = 90, missing_range = FALSE, ...) {
  
  data <- data |> filter(location %in% loc, age_group %in% age_gr, model %in% models)
  truth_data <- truth_data |> filter(location %in% loc, age_group %in% age_gr)
  
  models <- unique(data$model)
  
  if (!missing_range) {
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
  } else {
    r <- range(data$forecast_date)
  }
  
  truth_r <- range(truth_data$date)
  r <- c(max(r[1], truth_r[1]), min(r[2], truth_r[2])) 
  
  data <- data |> filter((forecast_date >= (r[1] + extra_delay)) & (forecast_date <= r[2]))
  truth_data <- truth_data |> filter((date >= (r[1] + extra_delay - truth_past)) & (date <= r[2]))
  
  list(data = data, truth_data = truth_data)
}

##################################################
##################################################
##################################################

compute_naive_ensemble <- function (data, loc = "DE", age_gr = "00+", ...) {
  
  data_mean <- data[0, ]
  data_median <- data[0, ]
  
  days <- unique(data$forecast_date)
  horizons <- unique(data$target)
  quantiles <- unique(data$quantile)
  
  count <- 1
  b <- txtProgressBar(min = 1, max = length(days), initial = 1)
  for (d in days) {
    for (h in horizons) {
      for (q in quantiles) {
        
        if (is.na(q)) {
          tmp_value <- data |> filter(forecast_date == d, target == h, type == "mean", is.na(quantile)) |> select(value) |> c() |> unlist()
        } else {
          tmp_value <- data |> filter(forecast_date == d, target == h, quantile == q) |> select(value) |> c() |> unlist()
        }
        
        tmp_mean   <- mean(tmp_value, na.rm = TRUE)
        tmp_median <- median(tmp_value, na.rm = TRUE)
        
        hh <- as.numeric(strsplit(h, " ")[[1]][1])
        
        if (is.na(q)) { tt <- "mean" } else { tt <- "quantile" }
        
        data_mean   <- data_mean   |> add_row(location = loc, age_group = age_gr, forecast_date = days[count], target_end_date = (days[count] + hh), target = h, type = tt, quantile = q, value = tmp_mean,   pathogen = "COVID-19", model = "Mean",   retrospective = FALSE)
        data_median <- data_median |> add_row(location = loc, age_group = age_gr, forecast_date = days[count], target_end_date = (days[count] + hh), target = h, type = tt, quantile = q, value = tmp_median, pathogen = "COVID-19", model = "Median", retrospective = FALSE)
      }
    }
    count <- count + 1
    setTxtProgressBar(b, count) 
  }
  close(b)
  
  rbind(data_mean, data_median)
}

##################################################
##################################################
##################################################

retrieve_data <- function (data, truth_data, naive_ensemble, models, horizon, start_date, end_date, skip_first_days = 1, training_size = 90, method = "Median", ...) {
  
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
      
      y[[as.character(dt)]][[as.character(h)]] <- select_real_data(naive_ensemble = naive_ensemble, truth_data = truth_data, dt = dt, horizon = h, training_size = training_size, method = method)
      y_current[[as.character(dt)]][[as.character(h)]] <- select_real_data(naive_ensemble = naive_ensemble, truth_data = truth_data, dt = dt, horizon = h, method = method, current = TRUE)
      values[[as.character(dt)]][[as.character(h)]] <- select_nowcasts(data = data, dt = dt, horizon = h, models = models, training_size = training_size)
      current[[as.character(dt)]][[as.character(h)]] <- select_nowcasts(data = data, dt = dt, horizon = h, models = models, current = TRUE)
      
      setTxtProgressBar(b, i) 
    }
    close(b)
    count <- count + 1
  }
  
  list(y = y, y_current = y_current, values = values, current = current)
}

##################################################
##################################################
##################################################

reparameterize_model <- function (y, y_current, values, current, baseline, state = "DE", age = "00+", method = NULL, ...) {

  new_y <- list()
  new_y_current <- list()
  new_values  <- list()
  new_current <- list()
  
  for_dates <- names(values)
  n_days    <- length(for_dates)
  
  pb <- txtProgressBar(min = 1, max = n_days, initial = 1) 
  for (n in 1:n_days) {
    
    d <- for_dates[n] 
    horizon <- as.numeric(names(values[[as.character(d)]]))
    
    new_y[[as.character(d)]]         <- list()
    new_y_current[[as.character(d)]] <- list()
    new_values[[as.character(d)]]    <- list()
    new_current[[as.character(d)]]   <- list()
    
    for (h in horizon) {
    
      new_values[[as.character(d)]][[as.character(h)]]  <- list()
      new_current[[as.character(d)]][[as.character(h)]] <- list()
      
      avlb_training_size <- length(values[[as.character(d)]][[as.character(h)]])
      if (!is.null(method)) { new_y[[as.character(d)]][[as.character(h)]] <- matrix(data = 0, nrow = avlb_training_size, ncol = length(probs)) }
      
      for (i in 1:avlb_training_size) {
        b <- baseline %>% filter(location == state, age_group == age, forecast_date == (as.Date(d) - i), target == paste(h, " day ahead inc hosp", sep = ""), type == "quantile") %>% select(value) %>% c() %>% unlist() %>% unname()
        new_values[[as.character(d)]][[as.character(h)]][[i]] <- values[[as.character(d)]][[as.character(h)]][[i]] - b
      
        if (is.null(method)) { 
          new_y[[as.character(d)]][[as.character(h)]][i] <- y[[as.character(d)]][[as.character(h)]][i] - b[4]
        } else { # For fancy score
          new_y[[as.character(d)]][[as.character(h)]][i, ] <- y[[as.character(d)]][[as.character(h)]][i, ] - b[4]
        }
      }
      
      b <- baseline %>% filter(location == state, age_group == age, forecast_date == d, target == paste(h, " day ahead inc hosp", sep = ""), type == "quantile") %>% select(value) %>% c() %>% unlist() %>% unname()
      new_current[[as.character(d)]][[as.character(h)]][[1]] <- current[[as.character(d)]][[as.character(h)]][[1]] - b
      
      new_y_current[[as.character(d)]][[as.character(h)]] <- y_current[[as.character(d)]][[as.character(h)]] - b[4]
    }
    setTxtProgressBar(pb, n)
  }
  close(pb)
  
  list(y = new_y, y_current = new_y_current, values = new_values, current = new_current)
}

##################################################
##################################################
##################################################

select_real_data <- function (naive_ensemble, truth_data, dt, horizon, n_forecast = 28, stable = 40, training_size = 90, method = "Median", current = FALSE, ...) {
  
  if (current) {
    target_end_date_value = dt + horizon
    
    if (method == "Median") {
      real_data <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
    } else if (method == "Mean") {
      real_data <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, is.na(quantile), model == method) |> select(value) |> unlist()
    } else if (method == "all_quant") {
      real_data <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, !is.na(quantile), model == "Median") |> select(value) |> unlist()
    } else { stop("Invalid method.") }
    
  } else {
    r <- range(data$forecast_date)
    number_days <- as.numeric((dt - r[1]))
    number_days <- min(number_days, training_size)
    
    if (method == "all_quant") {
      real_data <- matrix(0, number_days, length(probs))
    } else {
      real_data <- rep(0, number_days)
    }
    
    for (n in 1:number_days) {
      
      target_end_date_value = dt - n + horizon
      
      if ((- n + horizon) < (- stable)) {
        if (method != "all_quant") {
          real_data[n] <- truth_data[truth_data$date == target_end_date_value, ]$truth
        } else {
          real_data[n, ] <- rep(truth_data[truth_data$date == target_end_date_value, ]$truth, length(probs))
        }
        
      } else if ((- n + horizon) < (- n_forecast)) {
        if (method == "Median") {
          tmp_real_data <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon), target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
        } else if (method == "Mean") {
          tmp_real_data <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon), target_end_date == target_end_date_value,  is.na(quantile), model == method) |> select(value) |> unlist()
        } else if (method == "all_quant") {
          tmp_real_data <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon), target_end_date == target_end_date_value, !is.na(quantile), model == "Median") |> select(value) |> unlist()
        }
        inner_count <- 0
        while (((length(tmp_real_data) != 1) & (length(tmp_real_data) != 7)) & (inner_count < 10)) {
          inner_count <- inner_count + 1
          if (method == "Median") {
            tmp_real_data <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon) + inner_count, target_end_date == target_end_date_value + inner_count, quantile == 0.5, model == method) |> select(value) |> unlist()
          } else if (method == "Mean") {
            tmp_real_data <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon) + inner_count, target_end_date == target_end_date_value + inner_count,  is.na(quantile), model == method) |> select(value) |> unlist()
          } else if (method == "all_quant") {
            tmp_real_data <- naive_ensemble |> filter(forecast_date == dt - n + (n_forecast + horizon) + inner_count, target_end_date == target_end_date_value + inner_count, !is.na(quantile), model == "Median") |> select(value) |> unlist()
          }
        } 
        if (inner_count >= 10) { stop("Too many errors in a row.") }
        if (method != "all_quant") {
          real_data[n] <- tmp_real_data
        } else {
          real_data[n, ] <- tmp_real_data
        }
        
      } else {
        if (method == "Median") {
          tmp_real_data <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, quantile == 0.5, model == method) |> select(value) |> unlist()
        } else if (method == "Mean") {
          tmp_real_data <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value,  is.na(quantile), model == method) |> select(value) |> unlist()
        } else if (method == "all_quant") {
          tmp_real_data <- naive_ensemble |> filter(forecast_date == dt, target_end_date == target_end_date_value, !is.na(quantile), model == "Median") |> select(value) |> unlist()
        }
        inner_count <- 0
        while (((length(tmp_real_data) != 1) & (length(tmp_real_data) != 7)) & (inner_count < 10)) { 
          inner_count <- inner_count + 1
          if (method == "Median") {
            tmp_real_data <- naive_ensemble |> filter(forecast_date == dt + inner_count, target_end_date == target_end_date_value + inner_count, quantile == 0.5, model == method) |> select(value) |> unlist()
          } else if (method == "Mean") {
            tmp_real_data <- naive_ensemble |> filter(forecast_date == dt + inner_count, target_end_date == target_end_date_value + inner_count,  is.na(quantile), model == method) |> select(value) |> unlist()
          } else if (method == "all_quant") {
            tmp_real_data <- naive_ensemble |> filter(forecast_date == dt + inner_count, target_end_date == target_end_date_value + inner_count, !is.na(quantile), model == "Median") |> select(value) |> unlist()
          }
        } 
        if (inner_count >= 10) { stop("Too many errors in a row.") }
        if (method != "all_quant") {
          real_data[n] <- tmp_real_data
        } else {
          real_data[n, ] <- tmp_real_data
        }
      }
    }
  }
  real_data
}

##################################################
##################################################
##################################################

select_nowcasts <- function (data, dt, horizon, models, n_quantiles = 7, training_size = 90, current = FALSE, ...) {
  
  values_list <- list()
  
  if (current) {
    
    values <- matrix(data = 0, nrow = length(models), ncol = n_quantiles)
    
    for (m in 1:length(models)) {
      
      tmp <- data |> filter(forecast_date == dt, target_end_date == dt + horizon, type == "quantile", model == models[m]) |> select(value) |> unlist()  
      inner_count <- 0
      while ((length(tmp) == 0) & (inner_count < 10)) {
        inner_count <- inner_count + 1
        tmp <- data |> filter(forecast_date == dt + inner_count, target_end_date == dt + horizon + inner_count, type == "quantile", model == models[m]) |> select(value) |> unlist() 
      }
      
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
    number_days <- min(number_days, training_size)
    
    for (n in 1:number_days) {
      
      values <- matrix(data = 0, nrow = length(models), ncol = n_quantiles)
      
      tmp_models <- ungroup(data) |> filter(forecast_date == dt - n, target_end_date == dt - n + horizon, type == "quantile")  
      
      for (m in 1:length(models)) {

        tmp <- c(tmp_models[tmp_models$model == models[m], "value"])$value
        
        inner_count <- 0
        while ((length(tmp) == 0) & (inner_count < 10)) { # Backup recovery
          inner_count <- inner_count + 1
          tmp <- data |> filter(forecast_date == dt - n + inner_count, target_end_date == dt - n + horizon + inner_count, type == "quantile", model == models[m]) |> select(value) |> unlist()  
        }
        
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
##################################################
##################################################

compute_wis_three_components <- function (data, truth_data, start_date, end_date, horizon, models, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), skip_first_days = 1, verbose = TRUE, ...) {
  
  days <- seq(start_date + skip_first_days, end_date, by = "1 day")
  
  wis <- matrix(data = 0, nrow = length(days), ncol = length(models))
  
  wis_sprd <- matrix(data = 0, nrow = length(days), ncol = length(models))
  wis_over <- matrix(data = 0, nrow = length(days), ncol = length(models))
  wis_undr <- matrix(data = 0, nrow = length(days), ncol = length(models))
  
  if (verbose) { b <- txtProgressBar(min = 0, max = length(days), initial = 0) }
  for (k in 1:length(days)) {
    dt <- days[k]
    td <- dt + horizon
    
    sliced_data <- data |> filter(forecast_date == dt, target_end_date == td, type == "quantile")
    sliced_y <- truth_data |> filter(date == td) |> select(truth) |> as.integer()  
    
    for (m in 1:length(models)) {
      probs_tmp <- sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$quantile
      quant_tmp <- sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$value
      wis[k, m  ]      <- compute_wis(probs = probs_tmp, quant = quant_tmp, y = sliced_y, average = TRUE)
      wis_sprd[k, m  ] <- compute_wis(probs = probs_tmp, quant = quant_tmp, y = median(quant_tmp), average = TRUE)
      diff_wis <- wis[k, m  ] - wis_sprd[k, m  ]
      if (length(quant_tmp) != 0) {
        if (median(quant_tmp) > sliced_y) {
          wis_over[k, m  ] <- diff_wis
          wis_undr[k, m  ] <- 0
        } else {
          wis_undr[k, m  ] <- diff_wis
          wis_over[k, m  ] <- 0
        }  
      } else {
        wis_over[k, m  ] <- NaN
        wis_undr[k, m  ] <- NaN
      }
      
    }
    if (verbose) { setTxtProgressBar(b, k) }
  }
  if (verbose) { close(b) }
  
  list(wis = wis, wis_sprd = wis_sprd, wis_over = wis_over, wis_undr = wis_undr)
}

##################################################
##################################################
##################################################

compute_wis <- function (probs, quant, y, average = TRUE, ...) {
  if (length(probs) != length(quant)) { stop("`probs` and `quant` must have the same size.") }
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

quantile_distance <- function (q, obs_vect, q_level, obs_levels = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), ...) {
  k <- length(obs_levels)
  weights <- (c(obs_levels[-1], 2 - obs_levels[k]) - c(-obs_levels[1], obs_levels[-k])) / 2
  result <- 2 * sum((sign(q_level - obs_levels) != sign(q - obs_vect)) * ifelse(q_level == obs_levels, 0.5, 1) * weights * abs(q - obs_vect))
  result
}

wis_distance <- function (q_vect, obs_vect, q_levels = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), obs_levels = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), average = TRUE, ...) {
  k <- length(q_vect)
  # Compute quantile distances for each level
  qd_i <- function (i) { quantile_distance(q_vect[i], obs_vect, q_level = q_levels[i], obs_levels = obs_levels) }
  qds <- sapply(1:k, qd_i)

  if (average) {
    r <- mean(qds)
  } else {
    r <- qds
  }
  
  r
}

##################################################
##################################################
##################################################

compute_wis_truth <- function (data, truth_data, models, horizon, start_date, end_date, probs =  c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), skip_first_days = 41, verbose = TRUE, ...) {
  wis <- list()
  wis_summ <- list()
  wis_sprd <- list()
  wis_over <- list()
  wis_undr <- list()
  
  wis_summ_avg <- rep(0, length(models))
  wis_sprd_avg <- rep(0, length(models))
  wis_over_avg <- rep(0, length(models))
  wis_undr_avg <- rep(0, length(models))
  
  if (!verbose) { b <- txtProgressBar(min = 1, max = length(horizon), initial = 1) }
  
  count <- 1
  for (h in horizon) {
    if (verbose) { print(paste("Horizon: ", h, sep = "")) }
    
    # Make all models comparable. Commonly, `skip_first_days = 40 + 1`
    wis[[as.character(h)]] <- compute_wis_three_components(data = data, truth_data = truth_data, skip_first_days = skip_first_days, start_date = start_date, end_date = end_date, horizon = h, models = models, probs = probs, verbose = verbose)
    wis_summ[[as.character(h)]] <- wis[[as.character(h)]]$wis      |> colMeans(na.rm = TRUE)
    wis_sprd[[as.character(h)]] <- wis[[as.character(h)]]$wis_sprd |> colMeans(na.rm = TRUE)
    wis_over[[as.character(h)]] <- wis[[as.character(h)]]$wis_over |> colMeans(na.rm = TRUE)
    wis_undr[[as.character(h)]] <- wis[[as.character(h)]]$wis_undr |> colMeans(na.rm = TRUE)
    
    wis_summ_avg <- wis_summ_avg + wis_summ[[as.character(h)]] 
    wis_sprd_avg <- wis_sprd_avg + wis_sprd[[as.character(h)]]
    wis_over_avg <- wis_over_avg + wis_over[[as.character(h)]]
    wis_undr_avg <- wis_undr_avg + wis_undr[[as.character(h)]]
    
    count <- count + 1
    if (!verbose) { setTxtProgressBar(b, count) }
  }
  
  if (!verbose) { close(b) }
  
  wis_summ_avg <- wis_summ_avg / length(horizon)
  wis_sprd_avg <- wis_sprd_avg / length(horizon)
  wis_over_avg <- wis_over_avg / length(horizon)
  wis_undr_avg <- wis_undr_avg / length(horizon)
  
  df_wis <- data.frame(model = rep(models, 3), wis = c(wis_sprd_avg, wis_over_avg, wis_undr_avg), component = rep(c("sprd", "over", "undr"), each = length(models)))
  df_wis$model <- factor(x = df_wis$model, levels = models)
  
  list(df_wis = as_tibble(df_wis), wis_summ = wis_summ)
}

##################################################
##################################################
##################################################

summarize_stratified_wis_truth <- function (wis_truth, ...) {
  
  S <- length(wis_truth)
  df_wis <- wis_truth[[1]]$df_wis
  df_wis$wis <- 0
  wis_summ <- wis_truth[[1]]$wis_summ
  H <- length(wis_summ)
  M <- length(wis_summ[[1]])
  for (i in 1:H) { wis_summ[[i]] <- rep(0, M) }
  
  for (s in 1:S) {
    df_wis$wis <- df_wis$wis + wis_truth[[s]]$df_wis$wis
    for (i in 1:H) { wis_summ[[i]] <- wis_summ[[i]] + wis_truth[[s]]$wis_summ[[i]] }
  }
  df_wis$wis <- df_wis$wis / S
  for (i in 1:H) { wis_summ[[i]] <- wis_summ[[i]] / S }
  
  res <- list(df_wis = df_wis, wis_summ = wis_summ)
  res
}

##################################################
##################################################
##################################################

# This function is a probably a duplicate (double check it)
compute_wis_data <- function (data, truth_data, start_date, end_date, horizon, models, skip_first_days = 0, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), ...) {
  days <- seq(start_date + skip_first_days, end_date, by = "1 day")
  
  wis <- matrix(data = 0, nrow = length(days), ncol = length(models))
  
  for (k in 1:length(days)) {
    dt <- days[k]
    td <- dt + horizon
    
    sliced_data <- data |> filter(forecast_date == dt, target_end_date == td, type == "quantile")
    sliced_y <- truth_data |> filter(date == td) |> select(truth) |> as.integer()  
    
    for (m in 1:length(models)) {
      wis[k, m ] <- compute_wis(probs = sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$quantile, quant = sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$value, y = sliced_y)
    }
  }
  
  wis
}

##################################################
##################################################
##################################################

# Compute WIS per days based on the result from `compute_wis_data()`
compute_wis_days <- function (wis, models, start_date, end_date, q = 0, total_days = 90, skip_first_days = 1, average = TRUE, ...) {
  
  days <- seq(start_date + skip_first_days, end_date, by = "1 day") 
  
  wis_days <- data.frame(forecast_date = as.Date(character()), model = character(), value = double(), stringsAsFactors = FALSE)
  wis_days <- as_tibble(wis_days)
  
  H <- length(wis)
  
  for (k in 2:length(days)) { 
    
    dt <- days[k]
    
    if (average) {
      
      number_days <- min((k - 1), total_days)
      partial_wis <- matrix(data = 0, nrow = (H * number_days), ncol = ncol(wis[[1]]))
      
      for (i in 1:H) {
        if (!q) {
          partial_wis[(((i - 1) * number_days) + 1):(i * number_days), ] <- wis[[i]][((k - number_days + 1):(k)), ]
        } else {
          tmp_wis <- wis[[i]][, , q]
          partial_wis[(((i - 1) * number_days) + 1):(i * number_days), ] <- tmp_wis[(max(1, (k - total_days))):(k - 1), ] # wis[[i]][((k - number_days + 1):(k)), , q]
        }
      }
    } else {
      
      partial_wis <- matrix(data = 0, nrow = H, ncol = ncol(wis[[1]]))
      for (i in 1:H) {
        if (!q) {
          partial_wis[i, ] <- wis[[i]][k, ]
        } else {
          tmp_wis <- wis[[i]][, , q]
          partial_wis[i, ] <- tmp_wis[k, ]
        }
      }
    }
    partial_wis <- apply(X = partial_wis, MARGIN = 2, FUN = mean)
    
    for (p in 1:length(partial_wis)) {
      wis_days <- wis_days |> add_row(forecast_date = dt, model = models[p], value = partial_wis[p])
    }
  }
  wis_days
}


##################################################
##################################################
##################################################

compute_wis_horizon_truth <- function (models, horizon, wis_summ, ...) {
  df_wis_horizon <- data.frame(model = rep(models, length(horizon)), horizon = rep(horizon, each = length(models)), wis = 0)
  count <- 1
  for (i in 1:length(wis_summ)) {
    for (j in 1:length(models)) {
      df_wis_horizon$wis[count] <- wis_summ[[i]][j]
      count <- count + 1
    }
  }
  
  df_wis_horizon$model <- factor(x = df_wis_horizon$model, levels = models)
  df_wis_horizon
}

##################################################
##################################################
##################################################

process_data_skip_days <- function (y, values, uncertain_size = 40, method = "other", ...) {
  
  values_idx_1 <- names(values)
  values_idx_2 <- names(values[[1]])
    
  b <- txtProgressBar(min = 1, max = length(values_idx_1), initial = 1)
  for (i in 1:length(values_idx_1)) {
    for (j in 1:length(values_idx_2)) {
      values[[values_idx_1[i]]][[as.character(values_idx_2[j])]][(1:uncertain_size)] <- NULL
      if (method != "all_quant") {
        y[[values_idx_1[i]]][[as.character(values_idx_2[j])]] <- y[[values_idx_1[i]]][[as.character(values_idx_2[j])]][-(1:uncertain_size)]
      } else {
        y[[values_idx_1[i]]][[as.character(values_idx_2[j])]] <- y[[values_idx_1[i]]][[as.character(values_idx_2[j])]][-(1:uncertain_size), ]
      }
      
    }
    setTxtProgressBar(b, i)
  }
  close(b)

  list(y = y, values = values)
}

##################################################
##################################################
##################################################

process_data_ignore_models <- function (current, values, idx_models = 1:8, ...) {
  
  N <- length(values)
  H <- length(values[[1]])
  
  b <- txtProgressBar(min = 1, max = N, initial = 1)
  for (n in 1:N) {
    for (h in 1:H) {
      tmp_count <- length(values[[n]][[h]])
      for (count in 1:tmp_count) {
        values[[n]][[h]][[count]] <- values[[n]][[h]][[count]][idx_models, ]
      }
      current[[n]][[h]][[1]] <- current[[n]][[h]][[1]][idx_models, ]
    }
    setTxtProgressBar(b, n)
  }
  close(b)
  
  list(current = current, values = values)
}

##################################################
##################################################
##################################################

compute_wis_training_horizon <- function (data, truth_data, start_date, end_date, horizon, models, training_size, skip_first_days = 1, quant = TRUE, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), method = "other", ...) {
  days <- seq(start_date + skip_first_days, end_date, by = "1 day")
  
  if (!quant) {
    wis <- matrix(data = 0, nrow = length(days), ncol = length(models))
  } else {
    wis <- array(data = 0, dim = c(length(days), length(models), length(probs)))
  }

  b <- txtProgressBar(min = 0, max = length(days), initial = 0)
  for (k in 1:length(days)) {
    dt <- days[k]
    
    number_days <- as.numeric((dt - start_date))
    number_days <- min(number_days, training_size)
    number_days <- number_days - (skip_first_days - 1)
    
    if (!quant) {
      wis_tmp <- matrix(data = 0, nrow = number_days, ncol = length(models))
    } else {
      wis_tmp <- array(data = 0, dim = c(number_days, length(models), length(probs)))
    }
    
    for (n in 1:number_days) {
      tmp_data <- data[[as.character(dt)]][[as.character(horizon)]][[n]]
      if (method != "all_quant") {
        tmp_y <- truth_data[[as.character(dt)]][[as.character(horizon)]][n]
      } else {
        tmp_y <- truth_data[[as.character(dt)]][[as.character(horizon)]][n, ]
      }
  
      for (m in 1:length(models)) {
    
        if (method != "all_quant") {
          if (!quant) {
            wis_tmp[n, m  ] <- compute_wis(probs = probs, quant = tmp_data[m, ], y = tmp_y, average = (!quant))  
          } else {
            wis_tmp[n, m, ] <- compute_wis(probs = probs, quant = tmp_data[m, ], y = tmp_y, average = (!quant))  
          }
        } else {
          if (!quant) {
            wis_tmp[n, m  ] <- wis_distance(q_vect = tmp_data[m, ], obs_vect = tmp_y)
          } else {
            for (i in 1:length(probs)) {
              wis_tmp[n, m, i] <- quantile_distance(q = tmp_data[m, i], obs_vect = tmp_y, q_level = probs[i])
            }
          }
        }
      }
    }
    if (!quant) {
      wis[k,   ] <- apply(X = wis_tmp, MARGIN = 2      , FUN = mean)
    } else {
      wis[k, , ] <- apply(X = wis_tmp, MARGIN = c(2, 3), FUN = mean)
    }
    # if (!quant) {
    #   stop("To be implemented.")
    # } else {
    #   wis[k, , ] <- compute_relative_wis(wis_tmp = wis_tmp, models = models, quant = quant)
    # }
    
    setTxtProgressBar(b, k)
  }
  close(b)
    
  wis
}

##################################################
##################################################
##################################################


compute_wis_training <- function (data, truth_data, start_date, end_date, horizon, models, training_size, skip_recent_days = FALSE, quant = TRUE, horiz = TRUE, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), method = "other", ...) {
  
  skip_first_days <- ifelse(skip_recent_days, 1 + uncertain_size, 1)
  
  wis <- list()
  wis_avg <- list()
  
  for (h in horizon) {
    print(paste("Horizon: ", h, sep = ""))
    
    wis[[as.character(h)]] <- compute_wis_training_horizon(data = data, truth_data = truth_data, start_date = start_date, end_date = end_date, horizon = h, models = models, training_size = training_size, skip_first_days = skip_first_days, quant = quant, method = method)
    
    if (!quant) {
      wis_avg[[as.character(h)]] <- wis[[as.character(h)]] |> colMeans(na.rm = TRUE)
    } else {
      wis_avg[[as.character(h)]] <- apply(X = wis[[as.character(h)]], MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)
    }
  }
    
  if (!horiz) {
    
    wis <- apply(X = simplify2array(wis), MARGIN = c(1, 2, 3), FUN = mean)
    
    if (!quant) {
      stop("To be implemented.")
      # wis_avg <- apply(X = simplify2array(wis_avg), MARGIN = c(1   ), FUN = mean) # Include it if needed
    } else {
      wis_avg <- apply(X = simplify2array(wis_avg), MARGIN = c(1, 2), FUN = mean)
    }
    
  }
  
  list(wis = wis, wis_avg = wis_avg)
}

##################################################
##################################################
##################################################

compute_relative_wis <- function (wis_tmp, models, quant, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), ...) {
  
  res <- matrix(data = NA, nrow = length(models), ncol = length(probs))
  
  if (!quant) {
    # To be implemented.
  } else {
    for (j in 1:length(probs)) {
      res[, j] <- aux_compute_relative_wis(x = wis_tmp[, , j])
    }
  }
  res
}

aux_compute_relative_wis <- function (x, ...) {
  
  if (is.null(dim(x))) {
    x <- matrix(data = x, nrow = 1, ncol = length(x))
  }
  
  n_mods <- ncol(x)
  n_item <- nrow(x) 
  
  rwis <- rep(0, n_mods)
  for (n in 1:n_mods) {
    
    part_prod <- 1
    
    for (m in 1:n_mods) {
      I_set <- rep(TRUE, n_item)
      I_set <- I_set & (!is.na(x[, n])) & (!is.na(x[, m]))
      
      nume <- sum(x[I_set, n])
      deno <- sum(x[I_set, m])
      
      part_prod <- part_prod * (nume / deno)
    }
    
    rwis[n] <- part_prod ** (1 / n_mods)
  }
  
  rwis
}

##################################################
##################################################
##################################################

compute_weights <- function (wis = NULL, ...) {

  wis <- wis + 1e-6
  weights <- 1 / wis

  if (is.null(dim(weights))) { # Weights do not depend on time
    weights <- weights / sum(weights, na.rm = TRUE)
  } else { # Weights depend on time
    if (length(dim(weights)) == 3) { # Weights depend on the quantiles
      for (d in 1:(dim(weights)[1])) {
        for (i in 1:(dim(weights)[3])) {
          weights[d, , i] <- weights[d, , i] / sum(weights[d, , i], na.rm = TRUE)
        }
      }
    } else { # Weights do not depend on the quantiles
      for (i in 1:ncol(weights)) {
        weights[, i] <- weights[, i] / sum(weights[, i], na.rm = TRUE)
      }
    }
  }

  weights
}

##################################################
##################################################
##################################################

compute_wis_horizon_training <- function (wis_avg, models, quant = TRUE, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), ...) {
  
  horizon <- as.numeric(names(wis_avg))
  
  if (!quant) {
    df_wis_horizon <- data.frame(model = rep(models, length(horizon)), horizon = rep(horizon, each = length(models)), wis = 0)  
  } else {
    df_wis_horizon <- data.frame(model = rep(models, (length(horizon) * length(probs))), horizon = rep(rep(horizon, each = length(models)), length(probs)), probs = rep(probs, each = (length(models) * length(horizon))), wis = 0)  
    df_wis_horizon$probs <- factor(x = df_wis_horizon$probs, levels = probs) 
  }
  df_wis_horizon$model <- factor(x = df_wis_horizon$model, levels = models) 
  
  for (h in horizon) {
    df_wis_horizon[df_wis_horizon$horizon == h, "wis"] <- c(wis_avg[[as.character(h)]])
  } 
  
  df_wis_horizon
}

##################################################
##################################################
##################################################

create_new_tibble <- function (...) {
  as_tibble(data.frame(location         = character(), 
                       age_group        = character(), 
                       forecast_date    = as.Date(character()), 
                       target_end_date  = as.Date(character()), 
                       target           = character(), 
                       type             = character(), 
                       quantile         = double(), 
                       value            = double(), 
                       pathogen         = character(), 
                       model            = character(), 
                       retrospective    = logical(), 
                       stringsAsFactors = FALSE))
}

##################################################
##################################################
##################################################

compute_ensemble <- function (ens_method, y, y_current, values, current, ens_models = NULL, weights = NULL, k = NULL, lower = -10, upper = 10, quant = TRUE, horiz = TRUE, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), short_grid_search = TRUE, by = 0.01, n_ensemble_models = NULL, unweighted_method = NULL, method = NULL, ...) {
  m <- ens_method[1]
  
  if (m == "wis") {
    res <- ensemble_wis(current = current, weights = weights, k = k)
  } else if (m == "pinball") {
    # if (!horiz) { current <- list(current) } 
    res <- ensemble_pinball(y = y, y_current = y_current, values = values, current = current, ens_models = ens_models, lower = lower, upper = upper, quant = quant, horiz = horiz, probs = probs, short_grid_search = short_grid_search, by = by, method = method)
  } else if (m == "ranked_unweighted") {
    res <- ensemble_ranked_unweighted(y = y, y_current = y_current, values = values, current = current, n_ensemble_models = n_ensemble_models, unweighted_method = unweighted_method, quant = quant, horiz = horiz)
  } else {
    stop("Choose a valid ensemble method.")
  }
  
  res
}

##################################################
##################################################
##################################################

ensemble_wis <- function (current, weights, k = NULL,...) {
  
  if (length(dim(weights)) == 3) { # Weights depend on the quantiles
    
    if (!is.list(current)) { # National level
      
      res <- rep(0, length(probs))
      for (q in 1:length(probs)) {
        idx_missing <- which(is.na(current[, q]))
        if (length(idx_missing) == 0) { idx_missing <- -(1:length(current[, q])) }
        tmp_current <- current[, q][-idx_missing]
        tmp_weights <- weights[k, , q][-idx_missing]
        tmp_weights <- tmp_weights / sum(tmp_weights)
        res[q] <- tmp_weights %*% tmp_current
        
        # Format weights to export
        weights[k, , q][idx_missing] <- 0
        weights[k, , q] <- weights[k, , q] / sum(weights[k, , q])
      }
      
    } else { # Stratified analysis
      
      n_strata <- length(current)
      res <- matrix(data = 0, nrow = n_strata, ncol = length(probs))
      
      for (i in 1:n_strata) {
        for (q in 1:length(probs)) {
          idx_missing <- which(is.na(current[[i]][, q]))
          if (length(idx_missing) == 0) { idx_missing <- -(1:length(current[[i]][, q])) }
          tmp_current <- current[[i]][, q][-idx_missing]
          tmp_weights <- weights[k, , q][-idx_missing]
          tmp_weights <- tmp_weights / sum(tmp_weights)
          res[i, q] <- tmp_weights %*% tmp_current
        }
      }
      for (q in 1:length(probs)) {
        # Format weights to export
        weights[k, , q][idx_missing] <- 0
        weights[k, , q] <- weights[k, , q] / sum(weights[k, , q])
      }
      
    }
    
    weights <- weights[k, , ]
    
  } else { # Weights do not depend on the quantiles # UPDATE IT TO ACCOUNT FOR POSSIBLY MISSING NOWCASTS
    if (is.null(nrow(weights))) {
      res <- weights %*% current
    } else {
      res <- rep(0, length(probs))
      for (q in 1:length(probs)) {
        res[q] <- weights[, q] %*% current[, q]
      }
    }
  }
  
  list(nowcast = res, weights = weights)
}

##################################################
##################################################
##################################################

input_new_data <- function (new_data, ensemble, ens_method, h, state, age, day, horizon = -28:0, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), horiz = TRUE, ...) {
  
  name_method <- ifelse(ens_method == "wis", "DISW", "ISW")
  
  if (horiz) {
    for (q in 1:length(probs)) {
      value <- ensemble[[as.character(dt)]][[as.character(h)]]$nowcast
      if (class(value)[1] == "numeric") {
        value <- value[q]
      } else  {
        if (nrow(value) == 1) {
          value <- value[1, q]
        } else {
          value <- value[ , q]
        }
      }
      if (length(value) == 1) {
        new_data <- new_data |> add_row(location = state, age_group = age, forecast_date = dt, target_end_date = (dt + h), target = paste(h, " day ahead inc hosp", sep = ""), type = "quantile", quantile =  probs[q], value = value, pathogen = "COVID-19", model = name_method, retrospective = FALSE)
      } else {
        for (i in 1:length(value)) {
          if (length(value) == 16) {
            new_data <- new_data |> add_row(location = state[i], age_group = age, forecast_date = dt, target_end_date = (dt + h), target = paste(h, " day ahead inc hosp", sep = ""), type = "quantile", quantile =  probs[q], value = value[i], pathogen = "COVID-19", model = name_method, retrospective = FALSE)
          } else if (length(value == 6)) {
            new_data <- new_data |> add_row(location = state, age_group = age[i], forecast_date = dt, target_end_date = (dt + h), target = paste(h, " day ahead inc hosp", sep = ""), type = "quantile", quantile =  probs[q], value = value[i], pathogen = "COVID-19", model = name_method, retrospective = FALSE)
          } else {
            stop("Error when adding a new row.")
          }
        } 
      }
    }
  } else {
    for (i in 1:length(horizon)) {
      for (q in 1:length(probs)) {
        value <- ensemble[[as.character(dt)]]$nowcast[as.character(horizon[i]), q]
        new_data <- new_data |> add_row(location = state, age_group = age, forecast_date = dt, target_end_date = (dt + horizon[i]), target = paste(horizon[i], " day ahead inc hosp", sep = ""), type = "quantile", quantile =  probs[q], value = value, pathogen = "COVID-19", model = name_method, retrospective = FALSE)
      }
    }
  }
  
  new_data
}

##################################################
##################################################
##################################################

ensemble_pinball <- function (y, y_current, values, current, ens_models = NULL, lower = -10, upper = 10, quant = TRUE, horiz = TRUE, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), short_grid_search = TRUE, by = 0.01, method = NULL, ...) {

  if (!horiz & !quant) { stop("This combination of `quant` and `horiz` is not implemented.") }
  
  # H: number of horizons
  # N: number of day points
  # M: number of models
  
  if (!is.list(y_current) | length(y_current) == 29) { # National level
    if (horiz) { # Weights depend on the horizons
      N <- length(values)
      M <- nrow(values[[1]])
      if (is.null(M)) {
        M <- 1
      }
    } else { # Weights do not depend on the horizons
      H <- length(values)
      N <- length(values[[1]])
      M <- nrow(values[[1]][[1]])
      if (is.null(M)) {
        M <- 1
      }
    }
    
    if (!quant) { # Weights do not depend on the quantiles
      wis <- matrix(data = 0, nrow = N, ncol = M)
    } else { # Weights depend on the quantiles
      if (horiz) {
        wis <- array(data = 0, dim = c(N, M, length(probs)))
      } else {
        if (M == 1) {
          wis <- array(data = 0, dim = c((N * H), length(probs)))
        } else {
          wis <- array(data = 0, dim = c((N * H), M, length(probs)))
        }
      }
    }
    
    # Compute the score (WIS)
    if (horiz) {
      for (n in 1:N) {
        if (!quant) { # Missing implementation for more than one model
          wis[n, ] <- apply(X = values[[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[n], average = (!quant))  
        } else {
          if (M == 1) {
            wis[n, , ] <- compute_wis(probs = probs, quant = values[[n]], y = y[n], average = (!quant))
          } else {
            wis[n, , ] <- apply(X = values[[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[n], average = (!quant)) |> t() 
          }
        }
      }
    } else { 
      horizon <- as.numeric(names(y))
      
      count <- 1
      for (n in 1:N) {
        for (h in 1:H) {
          if (M == 1) { # Post-processing
            if (method == "all_quant") {
              tmp_fancy_score <- rep(0, length(probs))
              for (q in 1:length(probs)) {
                tmp_fancy_score[q] <- quantile_distance(q = values[[as.character(horizon[h])]][[n]][q], obs_vect = y[[as.character(horizon[h])]][n, ], q_level = probs[q]) ##### DOUBLE CHECK THE ARGUMENTS
              }
              wis[count,   ] <-  tmp_fancy_score
            } else {
              wis[count,   ] <-  compute_wis(quant = values[[as.character(horizon[h])]][[n]][q], probs = probs, y = y[[as.character(horizon[h])]][n], average = (!quant))
            }
          } else {
            wis[count, , ] <- apply(X = values[[as.character(horizon[h])]][[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[[as.character(horizon[h])]][n], average = (!quant)) |> t()
          }
          count <- count + 1
        }
      }
    }
    
    ##################################################
    
    # Optimization routine
    if (sum(wis, na.rm = TRUE) == 0) {
      theta <- 0
    } else {
      if (!quant) {
        # To be implemented
        stop("To be implemented.")
      } else {
        
        theta <- rep(x = 0, times = length(probs))
        phi <- rep(x = 0, times = length(probs))
        
        if ((M == 1) & (!horiz)) { # Post-processing
          for (q in 1:length(probs)) {
            tmp_wis <- wis[, q]
            if (is.null(dim(tmp_wis))) {
              tmp_wis <- t(as.matrix(tmp_wis)) 
            }
            #########################
            est_pars <- grid_optim(probs = probs, values = values, y = y, q = q, quant = quant, M = M, wis = tmp_wis, by = by, theta_lim = c(lower, upper), horiz = horiz, short_grid_search = short_grid_search, method = method)
            phi[q] <- est_pars[1]
          }
        } else {
          for (q in 1:length(probs)) {
            tmp_wis <- wis[, , q]
            if (is.null(dim(tmp_wis))) {
              tmp_wis <- t(as.matrix(tmp_wis)) 
            }
            est_pars <- grid_optim(probs = probs, values = values, y = y, q = q, quant = quant, M = M, wis = tmp_wis, by = by, theta_lim = c(lower, upper), horiz = horiz, short_grid_search = short_grid_search, method = method)
            if (M == 1) {
              phi[q] <- est_pars[1]
            } else {
              theta[q] <- est_pars[1]
              phi[q]   <- est_pars[2] 
            }
          }
        }
      }
    }

    ##################################################
    
    # Compute the new weights and nowcasts
    if (!quant) {
      w <- par_weights(theta = theta, wis = apply(X = wis, MARGIN = 2, FUN = mean))
      result <- list(nowcast = w %*% current, weights = w, theta = theta)
    } else {
      w <- matrix(data = 0, nrow = length(probs), ncol = M)
      if (horiz) {
        nowcast <- rep(x = 0, times = length(probs))
      } else {
        nowcast <- matrix(data = 0, nrow = H, ncol = length(probs))
        rownames(nowcast) <- horizon
      }
      tmp_wis_list <- list()
      for (q in 1:length(probs)) {
        if ((M == 1) & (!horiz)) {
          tmp_wis <- wis[,   q]
        } else {
          tmp_wis <- wis[, , q] 
        }
        if (is.null(dim(tmp_wis))) {
          tmp_wis <- t(as.matrix(tmp_wis)) 
        }
        tmp_wis_list[[q]] <- apply(X = tmp_wis, MARGIN = 2, FUN = mean)
        if (M == 1) {
          w[q, ] <- phi[q]
        } else {
          w[q, ] <- par_weights_scale(theta = theta[q], phi = phi[q], wis = apply(X = tmp_wis, MARGIN = 2, FUN = mean))
        }
        if (horiz) {
          if (M == 1) {
            nowcast[q] <- w[q, ] %*% current[  q]
          } else {
            nowcast[q] <- w[q, ] %*% current[, q]
          }
        } else {
          for (h in 1:H) {
            if (M == 1) {
              nowcast[as.character(horizon[h]), q] <- w[q, ] %*% current[[as.character(horizon[h])]][[1]][  q]
            } else {
              nowcast[as.character(horizon[h]), q] <- w[q, ] %*% current[[as.character(horizon[h])]][[1]][, q] 
            }
          }
        }
      }
      
      if (TRUE) {
        if (is.matrix(w)) { rownames(w) <- probs; colnames(w) <- ens_models }
        if (is.vector(nowcast)) { names(nowcast) <- probs }
        if (is.vector(theta)) { names(theta) <- probs }
      }
      
      result <- list(nowcast = nowcast, weights = w, theta = theta, phi = phi) 
    }
    
    ##################################################
    
  } else { # Stratified analysis (not implemented if the weights do not depend on the horizons)
    if (horiz) {
      S <- length(values) # Number of strata
      N <- length(values[[1]])
      M <- nrow(values[[1]][[1]])
      if (is.null(M)) {
        M <- 1
      }
    } else {
      # TBD
      stop("Not implemented.")
    }
    
    wis <- list()
    for (s in 1:S) {
      wis[[s]] <- array(data = 0, dim = c(N, M, length(probs)))
    }
    
    # Compute the score (WIS)
    for (s in 1:S) {
      for (n in 1:N) {
        if (M == 1) { # The post-processing implementation for stratified analysis is incomplete 
          wis[[s]][n, , ] <- compute_wis(probs = probs, quant = values[[s]][[n]], y = y[[s]][n], average = (!quant))
        } else {
          wis[[s]][n, , ] <- apply(X = values[[s]][[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[[s]][n], average = (!quant)) |> t() 
        }
      }
    }
    
    # Optimization routine
    if (sum(unlist(wis), na.rm = TRUE) == 0) {
      theta <- 0
    } else {
  
      theta <- rep(x = 0, times = length(probs))
      phi <- rep(x = 0, times = length(probs))
      
      for (q in 1:length(probs)) {
        
        tmp_wis <- list()
        for (s in 1:S) {
          tmp_wis[[s]] <- wis[[s]][, , q]
          
          if (is.null(dim(tmp_wis[[s]]))) {
            tmp_wis[[s]] <- t(as.matrix(tmp_wis[[s]])) 
          }
        }

        est_pars <- grid_optim(probs = probs, values = values, y = y, q = q, quant = quant, M = M, wis = tmp_wis, by = by, theta_lim = c(lower, upper), horiz = horiz, short_grid_search = short_grid_search, method = method)
        if (M == 1) {
          phi[q] <- est_pars[1]
        } else {
          theta[q] <- est_pars[1]
          phi[q]   <- est_pars[2] 
        }
      }
    }


    # Compute the new weights and nowcasts
    w <- matrix(data = 0, nrow = length(probs), ncol = M)
    nowcast <- matrix(data = 0, nrow = S, ncol = length(probs))
  
    wis <- elementwise_avg_3d(wis) # Averaging over the strata
    
    tmp_wis_list <- list()
    for (q in 1:length(probs)) {
      tmp_wis <- wis[, , q]
      if (is.null(dim(tmp_wis))) { tmp_wis <- t(as.matrix(tmp_wis)) }
      tmp_wis_list[[q]] <- apply(X = tmp_wis, MARGIN = 2, FUN = mean)
      if (M == 1) {
        w[q, ] <- phi[q]
      } else {
        w[q, ] <- par_weights_scale(theta = theta[q], phi = phi[q], wis = apply(X = tmp_wis, MARGIN = 2, FUN = mean))
      }
    }
    
    for (s in 1:S) {
      for (q in 1:length(probs)) {
        if (M == 1) {
          nowcast[s, q] <- w[q, ] %*% current[[s]][  q]
        } else {
          nowcast[s, q] <- w[q, ] %*% current[[s]][, q]
        }
      }
    }
    
    if (TRUE) { 
      if (is.matrix(w)) { rownames(w) <- probs; colnames(w) <- ens_models }
      if (is.vector(nowcast)) { names(nowcast) <- probs } else if (is.matrix(nowcast)) { colnames(nowcast) <- probs }
      if (is.vector(theta)) { names(theta) <- probs }
    }
    
    result <- list(nowcast = nowcast, weights = w, theta = theta, phi = phi) 
  }
  
  result
}

##################################################
##################################################
##################################################

ensemble_ranked_unweighted <- function (y, y_current, values, current, n_ensemble_models, unweighted_method = "mean", ...) {
  
  if (horiz) { 
    N <- length(values)
    M <- nrow(values[[1]])
    if (is.null(M)) {
      M <- 1
    }
  } else { 
    H <- length(values)
    N <- length(values[[1]])
    M <- nrow(values[[1]][[1]])
  }
  
  if (!quant) { 
    wis <- matrix(data = 0, nrow = N, ncol = M)
  } else { 
    if (horiz) {
      wis <- array(data = 0, dim = c(N, M, length(probs)))
    } else { 
      wis <- array(data = 0, dim = c((N * H), M, length(probs)))
    }
  }
  
  # Compute WIS
  if (horiz) {
    for (n in 1:N) {
      if (!quant) { # Missing implementation for more than one model
        wis[n, ] <- apply(X = values[[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[n], average = (!quant))  
      } else {
        if (M == 1) { 
          wis[n, , ] <- compute_wis(probs = probs, quant = values[[n]], y = y[n], average = (!quant))
        } else {
          wis[n, , ] <- apply(X = values[[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[n], average = (!quant)) |> t() 
        }
      }
    }
  } else { 
    horizon <- as.numeric(names(y))
    
    count <- 1
    for (n in 1:N) {
      for (h in 1:H) {
        wis[count, , ] <- apply(X = values[[as.character(horizon[h])]][[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[[as.character(horizon[h])]][n], average = (!quant)) |> t()
        count <- count + 1
      }
    }
  } 
  
  # Compute weights
  if (!quant) {
    # TBD
    stop("To be implemented.")
  } else {
    wis <- apply(X = wis, MARGIN = c(2, 3), FUN = mean)
    w <- matrix(data = 0, nrow = length(probs), ncol = M)
    w <- t(w)
    
    for (q in 1:length(probs)) {
      x <- wis[, q]
      min_pos <- which(x %in% sort(x)[1:n_ensemble_models])
      w[min_pos, q] <- 1
      w[min_pos, q] <- w[min_pos, q] / sum(w[min_pos, q])
    }
    
    w <- t(w)
  }
  
  # Compute nowcasts
  if (horiz) {
    nowcast <- rep(x = 0, times = length(probs))
  } else {
    nowcast <- matrix(data = 0, nrow = H, ncol = length(probs))
    rownames(nowcast) <- horizon
  }
  
  for (q in 1:length(probs)) {
    if (horiz) {
      if (unweighted_method == "mean") {
        nowcast[q] <- mean(current[, q][which(w[q, ] > 0)], na.rm = TRUE)
      } else {
        nowcast[q] <- median(current[, q][which(w[q, ] > 0)], na.rm = TRUE)
      }
    } else {
      for (h in 1:H) {
        if (unweighted_method == "mean") {
          tmp_nowcast <- mean(current[[as.character(horizon[h])]][[1]][, q][which(w[q, ] > 0)], na.rm = TRUE)
        } else {
          tmp_nowcast <- median(current[[as.character(horizon[h])]][[1]][, q][which(w[q, ] > 0)], na.rm = TRUE)
        }
        nowcast[as.character(horizon[h]), q] <- tmp_nowcast
      }
    }
  }
  
  if (TRUE) {
    if (is.matrix(w)) { rownames(w) <- probs; colnames(w) <- ens_models }
    if (is.vector(nowcast)) { names(nowcast) <- probs }
  }
  
  result <- list(nowcast = nowcast, weights = w) 
  result
}

##################################################
##################################################
##################################################

grid_optim <- function (probs, values, y, q, quant, M, wis = NULL, by = 0.01, theta_lim = c(-10, 10), phi_lim = c(0.05, 5), horiz = TRUE, short_grid_search = TRUE, method = NULL, ...) {
    
    if (M == 1) { 
      
      wis <- t(wis) 
      pts <- seq(phi_lim[1], phi_lim[2], by = by)
      rsp <- foreach(i = 1:length(pts)) %dopar% { cost_function(pars = pts[i], probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, horiz = horiz, method = method) } 
      pts <- cbind(pts, unlist(rsp))
      colnames(pts) <- c("phi", "cost")
      pos <- which.min(pts[, 2])
      result <- pts[pos, ]
      
    } else {
      
      theta <- seq(theta_lim[1], theta_lim[2], length.out = ((4 * 1 / by) + 1)) # length.out = ((2 * 1 / by) + 1)
      phi <- seq(phi_lim[1], phi_lim[2], by = by)
      
      pts <- expand.grid(theta, phi)
      
      ##################################################
      
      if (short_grid_search) {
        phi_1 <- which(phi == 1)
        
        count  <- 1
        before <- FALSE
        after  <- FALSE
        for (i in 1:((length(phi) - 1) / 2)) {
          
          if (count == 1) {
            # phi = 1
            tmp_pts_1 <- pts[pts[, 2] == phi[phi_1], ]
            tmp_rsp_1 <- foreach(i = 1:nrow(tmp_pts_1)) %dopar% { cost_function(pars = unlist(c(tmp_pts_1[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, horiz = horiz, method = method) }
            tmp_min_1 <- min(unlist(tmp_rsp_1))
            tmp_par_1 <- c(unlist(c(tmp_pts_1[which.min(unlist(tmp_rsp_1)), 1:2])), tmp_min_1)
            names(tmp_par_1) <- c("theta", "phi", "cost")
            
            # before phi = 1
            tmp_pts_b <- pts[pts[, 2] == phi[phi_1 - i], ]
            tmp_rsp_b <- foreach(i = 1:nrow(tmp_pts_b)) %dopar% { cost_function(pars = unlist(c(tmp_pts_b[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, horiz = horiz, method = method) }
            tmp_min_b <- min(unlist(tmp_rsp_b))
            tmp_par_b <- c(unlist(c(tmp_pts_b[which.min(unlist(tmp_rsp_b)), 1:2])), tmp_min_b)
            names(tmp_par_b) <- c("theta", "phi", "cost")
            
            # after  phi = 1
            tmp_pts_a <- pts[pts[, 2] == phi[phi_1 + i], ]
            tmp_rsp_a <- foreach(i = 1:nrow(tmp_pts_a)) %dopar% { cost_function(pars = unlist(c(tmp_pts_a[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, horiz = horiz, method = method) }
            tmp_min_a <- min(unlist(tmp_rsp_a))
            tmp_par_a <- c(unlist(c(tmp_pts_a[which.min(unlist(tmp_rsp_a)), 1:2])), tmp_min_a)
            names(tmp_par_a) <- c("theta", "phi", "cost")
            
            if (tmp_min_1 <= min(tmp_min_b, tmp_min_a)) { before <- FALSE; after <- FALSE; tmp_par <- tmp_par_1 
            } else if (tmp_min_b <= min(tmp_min_1, tmp_min_a)) { before <- TRUE;  after <- FALSE; tmp_par <- tmp_par_b 
            } else if (tmp_min_a <= min(tmp_min_1, tmp_min_b)) { before <- FALSE; after <- TRUE;  tmp_par <- tmp_par_a } else { stop("Error 'min'") }
            
          } else if ((before == TRUE ) & (after == FALSE)) {
            
            tmp_pts_b <- pts[pts[, 2] == phi[phi_1 - i], ]
            tmp_rsp_b <- foreach(i = 1:nrow(tmp_pts_b)) %dopar% { cost_function(pars = unlist(c(tmp_pts_b[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, horiz = horiz, method = method) }
            tmp_min_b <- min(unlist(tmp_rsp_b))
            tmp_par_b <- c(unlist(c(tmp_pts_b[which.min(unlist(tmp_rsp_b)), 1:2])), tmp_min_b)
            names(tmp_par_b) <- c("theta", "phi", "cost")
            
            if (tmp_min_b < tmp_par[3]) {
              tmp_par <- tmp_par_b
              before  <- TRUE
              after   <- FALSE
            } else {
              before  <- FALSE
              after   <- FALSE
            }
            
          } else if ((before == FALSE) & (after == TRUE )) {
            
            tmp_pts_a <- pts[pts[, 2] == phi[phi_1 + i], ]
            tmp_rsp_a <- foreach(i = 1:nrow(tmp_pts_a)) %dopar% { cost_function(pars = unlist(c(tmp_pts_a[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, horiz = horiz, method = method) }
            tmp_min_a <- min(unlist(tmp_rsp_a))
            tmp_par_a <- c(unlist(c(tmp_pts_a[which.min(unlist(tmp_rsp_a)), 1:2])), tmp_min_a)
            names(tmp_par_a) <- c("theta", "phi", "cost")
            
            if (tmp_min_a < tmp_par[3]) {
              tmp_par <- tmp_par_a
              before  <- FALSE
              after   <- TRUE
            } else {
              before  <- FALSE
              after   <- FALSE
            }
            
          } else if ((before == FALSE) & (after == FALSE)) {
            # DO NOTHING
          } else { stop("Error") }
          
          count <- count + 1
        }
        
        result <- tmp_par
      } else {
        
        rsp <- foreach(i = 1:nrow(pts)) %dopar% { cost_function(pars = unlist(c(pts[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, horiz = horiz, method = method) } 
        
        pts <- cbind(pts, unlist(rsp))
        colnames(pts) <- c("theta", "phi", "cost")
        pos <- which.min(pts$cost)
        
        result <- unlist(c(pts[pos, 1:3]))
      }
    }
  
  result
}

##################################################
##################################################
##################################################

elementwise_avg <- function (my_list, ...) {
  max_length <- max(sapply(my_list, length))
  result <- numeric(length = max_length)
  
  for (i in 1:max_length) {
    result[i] <- mean(sapply(my_list, function(x) ifelse(i <= length(x), x[i], NA)))
  }
  
  result
}

elementwise_avg_3d <- function (my_list, ...) {
  max_dim1 <- max(sapply(my_list, function(x) dim(x)[1]))
  max_dim2 <- max(sapply(my_list, function(x) dim(x)[2]))
  max_dim3 <- max(sapply(my_list, function(x) dim(x)[3]))
  
  result <- array(NA, dim = c(max_dim1, max_dim2, max_dim3))
  
  for (i in 1:max_dim1) {
    for (j in 1:max_dim2) {
      for (k in 1:max_dim3) {
        result[i, j, k] <- mean(sapply(my_list, function(x) {
          if (i <= dim(x)[1] && j <= dim(x)[2] && k <= dim(x)[3]) {
            x[i, j, k]
          } else {
            NA
          }
        }))
      }
    }
  }
  
  result
}

cost_function <- function (pars, probs, values, y, wis, q = 1, quant = FALSE, horiz = TRUE, method = NULL, ...) {
  
  if (!is.list(wis)) { # National level
    
    if (length(pars) == 1) { # If just one model (post-processing)
      
      phi <- pars
      
      if (horiz) {
        N <- length(values)
        ens_wis <- rep(0, N)
      } else {
        H <- length(values)
        N <- length(values[[1]])
        ens_wis <- rep(0, (N * H))
      }
      
      w <- phi
      
      count_H <- 1
      count_N <- 1
      for (n in 1:length(ens_wis)) {
        if (horiz) {
          ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n], average = (!quant))[q]  
        } else {
          if (method == "all_quant") {
            ens_wis[n] <- quantile_distance(q = (w %*% values[[count_H]][[count_N]])[q], obs_vect = y[[count_H]][count_N, ], q_level = probs[q]) ##### DOUBLE CHECK THE ARGUMENTS
          } else {
            ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[count_H]][[count_N]], y = y[[count_H]][count_N], average = (!quant))[q]
          }
        }
        
        count_N <- count_N + 1
        if ((n %% N) == 0) {
          count_H <- count_H + 1  
          count_N <- 1
        }
      }
      
    } else { # Ensemble
      theta <- pars[1]
      phi   <- pars[2]
      
      if (horiz) {
        N <- length(values)
        ens_wis <- rep(0, N)
      } else { 
        H <- length(values)
        N <- length(values[[1]])
        
        ens_wis <- rep(0, (N * H))
      }
      
      w <- par_weights_scale(theta = theta, phi = phi, wis = apply(X = wis, MARGIN = 2, FUN = mean))
      
      count_H <- 1
      count_N <- 1
      for (n in 1:length(ens_wis)) {
        
        if (!quant) {
          ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n], average = (!quant))  
        } else {
          if (horiz) {
            ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n], average = (!quant))[q]  
          } else { 
            ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[count_H]][[count_N]], y = y[[count_H]][count_N], average = (!quant))[q]  
          }
        }
        
        count_N <- count_N + 1
        if ((n %% N) == 0) {
          count_H <- count_H + 1  
          count_N <- 1
        }
      }
    }
  } else { # Stratified analysis
    
    if (length(pars) == 1) { 
      # TBD
      stop("Cost function for post-processing for stratified analysis has not been implemented yet.")
    } else {
      theta <- pars[1]
      phi   <- pars[2]
      
      S <- length(values)
      N <- length(values[[1]])
      ens_wis <- matrix(0, S, N)
      
      w <- list()
      for (s in 1:S) {
        w[[s]] <- par_weights_scale(theta = theta, phi = phi, wis = apply(X = wis[[s]], MARGIN = 2, FUN = mean))
      }
      w <- elementwise_avg(w)
      
      for (s in 1:S) {
        for (n in 1:ncol(ens_wis)) {
          ens_wis[s, n] <- compute_wis(probs = probs, quant = w %*% values[[s]][[n]], y = y[[s]][n], average = (!quant))[q]  
        }
      }
    }
  }
  
  mean(ens_wis, na.rm = TRUE)
}

##################################################
##################################################
##################################################

par_weights <- function (theta, wis, std_wis = TRUE, ...) {
  wis <- mpfr(wis, 512)
  if (std_wis) { wis <- wis / sum(wis) } 
  result <- as.numeric((exp(- theta * wis)) / (sum(exp(- theta * wis))))
  
  result
}

# par_weights_scale <- function (theta, phi, wis, std_wis = TRUE, ...) {
#   wis <- mpfr(wis, 512)
#   if (std_wis) { wis <- wis / sum(wis) } 
#   result <- as.numeric(phi * (exp(- theta * wis)) / (sum(exp(- theta * wis))))
#   
#   result
# }

par_weights_scale <- function (theta, phi, wis, std_wis = TRUE, ...) {
  wis <- mpfr(wis, 512)
  wis <- wis + 1e-6
  if (std_wis) { wis <- wis / sum(wis) } 
  result <- as.numeric(phi * (1 / ((wis) ** theta)) / sum((1 / ((wis) ** theta))))
  
  result
}

##################################################
##################################################
##################################################

weights_tibble <- function (ensemble, r, models, horizon, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), skip_first_days = 1, ens_model = NULL, horiz = TRUE, ...) {
  days <- seq(r[1] + skip_first_days, r[2], by = "1 day")
  w_hat <- data.frame(forecast_date = as.Date(character()), horizon = character(), model = character(), quant = double(), value = double(), stringsAsFactors = FALSE)
  w_hat <- as_tibble(w_hat)
  
  if (horiz) {
    
    b <- txtProgressBar(min = 1, max = length(days), initial = 1) 
    for (k in 1:length(days)) { 
      dt <- days[k]
      for (i in 1:length(horizon)) {
        for (m in 1:length(models)) {
          for (q in 1:length(probs)) {
            if (ens_model == "DISW") {
              value <- ensemble[[as.character(dt)]][[as.character(horizon[i])]]$weights[, q][m]
            } else {
              value <- ensemble[[as.character(dt)]][[as.character(horizon[i])]]$weights[q, ][m]
            }
            w_hat <- w_hat |> add_row(forecast_date = dt, horizon = as.character(horizon[i]), model = models[m], quant = probs[q], value = value)
          }
        }
      }
      setTxtProgressBar(b, k)
    }
    close(b)
    
  } else {
    
    b <- txtProgressBar(min = 1, max = length(days), initial = 1) 
    for (k in 1:length(days)) { 
      dt <- days[k]
      for (m in 1:length(models)) {
        for (q in 1:length(probs)) {
          if (ens_model == "DISW") {
            value <- ensemble[[as.character(dt)]]$weights[, q][m]
          } else {
            value <- ensemble[[as.character(dt)]]$weights[q, ][m]
          }
          for (h in horizon) {
            w_hat <- w_hat |> add_row(forecast_date = dt, horizon = as.character(h), model = models[m], quant = probs[q], value = value)           
          }
        }
      }
      setTxtProgressBar(b, k)
    }
    close(b)
    
  }
  
  w_hat
}

##################################################
##################################################
##################################################

summarize_w_hat <- function (w_hat, hh = NULL, qq = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), between_0_1 = FALSE, ...) {
  days <- unique(w_hat$forecast_date)
  horizon <- unique(w_hat$horizon)
  models <- unique(w_hat$model)
  
  new_w_hat <- data.frame(forecast_date = as.Date(character()), model = character(), value = double(), stringsAsFactors = FALSE)
  new_w_hat <- as_tibble(new_w_hat)
  
  b <- txtProgressBar(min = 1, max = length(days), initial = 1) 
  
  for (k in 1:length(days)) {
    for (m in 1:length(models)) {
      if (is.null(hh)) {
        tmp_value <- w_hat |> filter(forecast_date == days[k], model == models[m], quant %in% qq) |> select(value) |> unlist() |> mean(na.rm = TRUE)
      } else {
        tmp_value <- w_hat |> filter(forecast_date == days[k], model == models[m], horizon == hh) |> select(value) |> unlist() |> mean(na.rm = TRUE)
      }
      new_w_hat <- new_w_hat |> add_row(forecast_date = days[k], model = models[m], value = tmp_value)
    }
    
    tmp_un_values <- new_w_hat |> filter(forecast_date == days[k]) |> select(value) |> unlist()
    tmp_un_values <- tmp_un_values / sum(tmp_un_values)
    if (between_0_1) {
      new_w_hat[new_w_hat$forecast_date == days[k], ]$value <- tmp_un_values  
    }
    
    setTxtProgressBar(b, k)
  }
  
  close(b)
  
  new_w_hat
}

##################################################
##################################################
##################################################

format_w_hat <- function (w_hat, aggregate_total = FALSE, indep_quant = FALSE, indep_horiz = TRUE, qqs = c(0.025, 0.5, 0.975), probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), ...) {
  new_w_hat <- list()
  
  if (aggregate_total | indep_quant) {
    count <- 1
    for (qq in qqs) {
      if (aggregate_total) { qq <- probs }
      new_w_hat[[count]] <- summarize_w_hat(w_hat = w_hat, qq = qq)
      
      if (!aggregate_total) {
        if (count == 1) {
          new_w_hat_total <- new_w_hat[[count]]
          new_w_hat_total <- new_w_hat_total |> mutate(quant = qq)
        } else {
          tmp <- new_w_hat[[count]]
          tmp <- tmp |> mutate (quant = qq)
          new_w_hat_total <- rbind(new_w_hat_total, tmp)
        }
      }
      
      if (aggregate_total) { break } 
      count <- count + 1
    }
  } else {
    hhs <- as.numeric(unique(w_hat$horizon))
    count <- 1
    
    for (hh in hhs) {
      print(paste("Horizon: ", hh, sep = ""))
      new_w_hat[[count]] <- summarize_w_hat(w_hat = w_hat, hh = hh)
      
      if (count == 1) {
        new_w_hat_total <- new_w_hat[[count]]
        new_w_hat_total <- new_w_hat_total |> mutate(horizon = hh)
      } else {
        tmp <- new_w_hat[[count]]
        tmp <- tmp |> mutate (horizon = hh)
        new_w_hat_total <- rbind(new_w_hat_total, tmp)
      }
      
      count <- count + 1
    }
  }
  
  if (aggregate_total) { 
    result <- list(new_w_hat = new_w_hat[[1]]) 
  } else {
    result <- list(new_w_hat = new_w_hat, new_w_hat_total = new_w_hat_total)
  }
  
  result
}

##################################################
##################################################
##################################################

complete_NA <- function (x, ...) {
  sub_x <- x[is.na(x$value), ]
  
  for (i in 1:nrow(sub_x)) {
    loc <- sub_x[i, ]$location
    age <- sub_x[i, ]$age_group
    fcd <- sub_x[i, ]$forecast_date
    ted <- sub_x[i, ]$target_end_date
    qtl <- sub_x[i, ]$quantile
    mod <- sub_x[i, ]$model
    
    if (as.numeric(fcd - ted) != 28) {
      x[((x$location == loc) & (x$age_group == age) & (x$forecast_date == fcd) & (x$target_end_date ==  ted     ) & (x$quantile == qtl) & (x$model == mod)), "value"] <- 
      x[((x$location == loc) & (x$age_group == age) & (x$forecast_date == fcd) & (x$target_end_date == (ted - 1)) & (x$quantile == qtl) & (x$model == mod)), "value"]
    } else {
      x[((x$location == loc) & (x$age_group == age) & (x$forecast_date == fcd) & (x$target_end_date ==  ted     ) & (x$quantile == qtl) & (x$model == mod)), "value"] <- 
      x[((x$location == loc) & (x$age_group == age) & (x$forecast_date == fcd) & (x$target_end_date == (ted + 1)) & (x$quantile == qtl) & (x$model == mod)), "value"]
    }
  }
  
  x
}

##################################################
##################################################
##################################################

add_baseline_new_data <- function (new_data, baseline = NULL, reparameterize = FALSE, ...) {
  
  if (reparameterize) {
    
    if (nrow(new_data) != nrow(baseline)) { stop("`new_data` and `baseline` must have same size.") }
    
    new_data <- new_data %>% arrange(location, age_group, forecast_date, target_end_date, quantile)
    baseline <- baseline %>% arrange(location, age_group, forecast_date, target_end_date, quantile)
    
    new_data$value <- baseline$value + new_data$value
  }
  
  new_data
}


add_baseline_ensemble <- function (ensemble, baseline = NULL, reparameterize = FALSE, horiz = FALSE, ...) {
  
  if (reparameterize) {
    
    if (horiz) { horizon <- unique(as.numeric(names(ensemble[[1]]))) }
    
    if (length(ensemble) != length(unique(baseline$forecast_date))) { stop("`ensemble` and `baseline` must have same size.") }
    
    ds <- names(ensemble)
    
    if (length(ds) > 1) { b <- txtProgressBar(min = 1, max = length(ds), initial = 1) }
    for (i in 1:length(ds)) {
      
      d <- ds[i]
      
      if (horiz) {
        tmp_ensemble <- list()
        for (h in horizon) {
          tmp_ensemble[[as.character(h)]] <- ensemble[[as.character(d)]][[as.character(h)]]$nowcast
        }
      } else {
        tmp_ensemble <- ensemble[[as.character(d)]]$nowcast
        J <- nrow(tmp_ensemble) 
      }
      
      if (horiz) {
        for (h in horizon) {
          tmp_values <- baseline %>% filter(forecast_date == as.Date(d), target == paste(h, "day ahead inc hosp")) %>% select(value) %>% c() %>% unlist() %>% unname()
          tmp_ensemble[[as.character(h)]] <- tmp_ensemble[[as.character(h)]] + tmp_values
        }
        
      } else {
        for (j in 1:J) {
          h <- as.numeric(rownames(tmp_ensemble)[j])
          
          tmp_values <- baseline %>% filter(forecast_date == as.Date(d), target == paste(h, "day ahead inc hosp")) %>% select(value) %>% c() %>% unlist() %>% unname()
          tmp_ensemble[j, ] <- tmp_ensemble[j, ] + tmp_values
        }
      }
      
      if (horiz) {
        for (h in horizon) {
          ensemble[[as.character(d)]][[as.character(h)]]$nowcast <- tmp_ensemble[[as.character(h)]]
        }
      } else {
        ensemble[[as.character(d)]]$nowcast <- tmp_ensemble
      }
      if (length(ds) > 1) { setTxtProgressBar(b, i) }
    }
  }
  if (length(ds) > 1) { close(b) }
  
  ensemble
}

##################################################
##################################################
##################################################
