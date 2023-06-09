
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

compute_wis_data <- function (data, truth_data, start_date, end_date, horizon, models, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), method = "median", total_days = 90, skip_first_days = 1, skip_last_days = 0, quant = FALSE, training = TRUE, ...) {
  days <- seq(start_date + skip_first_days, end_date - skip_last_days, by = "1 day")
  
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
            wis[k, m  ] <- compute_wis(probs = sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$quantile, quant = sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$value, y = sliced_y, average = (!quant))
        } else {
          # wis[k, m, ] <- compute_wis(probs = sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$quantile[1:7], quant = sliced_data[sliced_data$model == models[m], ]$value[1:7], y = sliced_y, average = (!quant))
            wis[k, m, ] <- compute_wis(probs = sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$quantile, quant = sliced_data[((sliced_data$model == models[m]) & (sliced_data$quantile %in% probs)), ]$value, y = sliced_y, average = (!quant))
        }
      }
      setTxtProgressBar(b, k)
    }
    close(b)
  }
  wis
}

##################################################################################

compute_wis_days <- function (wis, models, q = 0, total_days = 90, skip_first_days = 1, skip_last_days = 0, average = TRUE, ...) {
  
  days <- seq(r[1] + skip_first_days, r[2] - skip_last_days, by = "1 day") 
  
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
          stop("To be implemented.")
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

##################################################################################

grid_optim <- function (probs, values, y, q, quant, M, wis = NULL, by = 0.01, theta_lim = c(-5, 5), phi_lim = c(0.75, 1.25), hh = TRUE, boo_wis = TRUE, theta_weights = TRUE) {
  if (theta_weights) {
    
    theta <- seq(theta_lim[1], theta_lim[2], length.out = ((2 * 1 / by) + 1))
    phi <- seq(phi_lim[1], phi_lim[2], by = by)
    
    pts <- expand.grid(theta, phi)
    
    ##################################################
    ##################################################
    
    phi_1 <- which(phi == 1)

    count  <- 1
    before <- FALSE
    after  <- FALSE
    for (i in 1:((length(phi) - 1) / 2)) {

      if (count == 1) {
        # phi = 1
        tmp_pts_1 <- pts[pts[, 2] == phi[phi_1], ]
        tmp_rsp_1 <- foreach(i = 1:nrow(tmp_pts_1)) %dopar% { cost_function(pars = unlist(c(tmp_pts_1[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = theta_weights) }
        tmp_min_1 <- min(unlist(tmp_rsp_1))
        tmp_par_1 <- c(unlist(c(tmp_pts_1[which.min(unlist(tmp_rsp_1)), 1:2])), tmp_min_1)
        names(tmp_par_1) <- c("theta", "phi", "cost")

        # before phi = 1
        tmp_pts_b <- pts[pts[, 2] == phi[phi_1 - i], ]
        tmp_rsp_b <- foreach(i = 1:nrow(tmp_pts_b)) %dopar% { cost_function(pars = unlist(c(tmp_pts_b[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = theta_weights) }
        tmp_min_b <- min(unlist(tmp_rsp_b))
        tmp_par_b <- c(unlist(c(tmp_pts_b[which.min(unlist(tmp_rsp_b)), 1:2])), tmp_min_b)
        names(tmp_par_b) <- c("theta", "phi", "cost")

        # after  phi = 1
        tmp_pts_a <- pts[pts[, 2] == phi[phi_1 + i], ]
        tmp_rsp_a <- foreach(i = 1:nrow(tmp_pts_a)) %dopar% { cost_function(pars = unlist(c(tmp_pts_a[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = theta_weights) }
        tmp_min_a <- min(unlist(tmp_rsp_a))
        tmp_par_a <- c(unlist(c(tmp_pts_a[which.min(unlist(tmp_rsp_a)), 1:2])), tmp_min_a)
        names(tmp_par_a) <- c("theta", "phi", "cost")

               if (tmp_min_1 <= min(tmp_min_b, tmp_min_a)) { before <- FALSE; after <- FALSE; tmp_par <- tmp_par_1 
        } else if (tmp_min_b <= min(tmp_min_1, tmp_min_a)) { before <- TRUE;  after <- FALSE; tmp_par <- tmp_par_b 
        } else if (tmp_min_a <= min(tmp_min_1, tmp_min_b)) { before <- FALSE; after <- TRUE;  tmp_par <- tmp_par_a } else { stop("Error 'min'") }

      } else if ((before == TRUE ) & (after == FALSE)) {
      
        tmp_pts_b <- pts[pts[, 2] == phi[phi_1 - i], ]
        tmp_rsp_b <- foreach(i = 1:nrow(tmp_pts_b)) %dopar% { cost_function(pars = unlist(c(tmp_pts_b[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = theta_weights) }
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
        tmp_rsp_a <- foreach(i = 1:nrow(tmp_pts_a)) %dopar% { cost_function(pars = unlist(c(tmp_pts_a[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = theta_weights) }
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

    ##################################################
    ##################################################
    
    # rsp <- foreach(i = 1:nrow(pts)) %dopar% { cost_function(pars = unlist(c(pts[i, ])), probs = probs, values = values, y = y, wis = wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = theta_weights) } # UPDATE LAST PARAMETER
  
  } else {
    w <- list()
    for (m in 1:M) { w[[m]] <- seq(0, 1, by = by) }
    pts <- expand.grid(w)
    
    rsp <- foreach(i = 1:nrow(pts)) %dopar% { cost_function_weights_ind(w = unlist(c(pts[i, ])), probs = probs, values = values, y = y, q = q, quant = quant, exponentiate = FALSE) }
    
    # }
    # 
    # pts <- cbind(pts, unlist(rsp))
    # if (theta_weights) { colnames(pts) <- c("theta", "phi", "cost") } else { colnames(pts) <- c(paste("w_", 1:M, sep = ""), "cost") }
    # pos <- which.min(pts$cost)
    # 
    # if (theta_weights) { result <- unlist(c(pts[pos, 1:3])) } else { result <- unlist(c(pts[pos, 1:M])) }
  
    pts <- cbind(pts, unlist(rsp))
    colnames(pts) <- c(paste("w_", 1:M, sep = ""), "cost")
    pos <- which.min(pts$cost)
    result <- unlist(c(pts[pos, 1:M]))
    
  }
 result
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

compute_ensemble <- function (values, current, weights = NULL, y = NULL, y_current = NULL, method = c("mean", "median", "wis", "pinball"), lower = 0, upper = 1, by = 0.01, quant = FALSE, hh = TRUE, models = NULL, boo_wis = TRUE, two_pars = TRUE, theta_weights = TRUE, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), ...) {
  m <- method[1]
  
  if (m == "mean") {
    res <- ensemble_mean(values = current[[1]])
  } else if (m == "median") {
    res <- ensemble_median(values = current[[1]])
  } else if (m == "wis") {
    res <- ensemble_wis(values = current[[1]], weights = weights)
  } else if (m == "pinball") {
    if (hh) { current <- current[[1]] } 
    res <- ensemble_pinball(values = values, current = current, y = y, y_current = y_current, lower = lower, upper = upper, by = by, quant = quant, hh = hh, models = models, boo_wis = boo_wis, two_pars = two_pars, theta_weights = theta_weights, probs = probs)
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
      res <- rep(0, length(probs))
      for (q in 1:length(probs)) {
        res[q] <- weights[, q] %*% values[, q]
      }
    }
    
  } else {
    print("Yet to be implemented")
  }
  list(nowcast = res, weights = weights)
}

##################################################################################

par_weights <- function (theta, wis, boo_wis, std_wis = TRUE, ...) {
  wis <- mpfr(wis, 512)
  if (std_wis) { wis <- wis / sum(wis) } 
  if (boo_wis) {
    result <- as.numeric((exp(- theta * wis)) / (sum(exp(- theta * wis))))
  } else {
    result <- as.numeric(exp(c(0, theta)) / (sum(exp(c(0, theta)))))
  }
  result
}

par_weights_scale <- function (theta, phi, wis, boo_wis, std_wis = TRUE, ...) {
  
  wis <- mpfr(wis, 512)
  if (std_wis) { wis <- wis / sum(wis) } 
  if (boo_wis) {
    result <- as.numeric(phi * (exp(- theta * wis)) / (sum(exp(- theta * wis))))
  } else {
    result <- as.numeric(phi * exp(c(0, theta)) / (sum(exp(c(0, theta)))))
  }
  result
}

##################################################################################

cost_function <- function (pars, probs, values, y, wis, q = 1, quant = FALSE, hh = TRUE, boo_wis = TRUE, two_pars = TRUE, ...) {
  if (two_pars) {
    theta <- pars[1]
    phi   <- pars[2]
  } else {
    theta <- pars
  }
  
  if (hh) {
    N <- length(values)
    ens_wis <- rep(0, N)
  } else { 
    H <- length(values)
    N <- length(values[[1]])
    
    ens_wis <- rep(0, (N * H))
  }
  
  if (two_pars) {
    w <- par_weights_scale(theta = theta, phi = phi, wis = apply(X = wis, MARGIN = 2, FUN = mean), boo_wis = boo_wis)
  } else {
    w <- par_weights(theta = theta, wis = apply(X = wis, MARGIN = 2, FUN = mean), boo_wis = boo_wis)
  }
  
  count_H <- 1
  count_N <- 1
  for (n in 1:length(ens_wis)) {
    # print(paste(count_H, "-", count_N, sep = ""))
    if (!quant) {
      ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n], average = (!quant))  
    } else {
      if (hh) {
        ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n], average = (!quant))[q]  
      } else { # Aggregating over horizons
        ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[count_H]][[count_N]], y = y[[count_H]][count_N], average = (!quant))[q]  
      }
    }
    
    count_N <- count_N + 1
    if ((n %% N) == 0) {
      count_H <- count_H + 1  
      count_N <- 1
    }
  }
  
  mean(ens_wis, na.rm = TRUE) # sum(ens_wis, na.rm = TRUE)
}


cost_function_weights_ind <- function (w, probs, values, y, q = 1, quant = FALSE, exponentiate = TRUE, ...) {
  if (exponentiate) { w <- exp(w) }
  
  N <- length(values)
  ens_wis <- rep(0, N)
  
  for (n in 1:length(ens_wis)) {
    ens_wis[n] <- compute_wis(probs = probs, quant = w %*% values[[n]], y = y[n], average = (!quant))[q]
  }
  
  mean(ens_wis, na.rm = TRUE) # sum(ens_wis, na.rm = TRUE) 
}

##################################################################################

ensemble_pinball <- function (values, current, y, y_current, initial_theta = 0, lower = 0, upper = 1, by = 0.01, quant = FALSE, hh = TRUE, models = NULL, boo_wis = TRUE, name_values = TRUE, two_pars = TRUE, theta_weights = TRUE, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), ...) {
  if (!hh & !quant) { stop("This combination of 'quant' and 'hh' is not implemented yet.") }
  
  if (hh) {
    N <- length(values)
    M <- nrow(values[[1]])
  } else { # Aggregate over quantiles
    H <- length(values)
    N <- length(values[[1]])
    M <- nrow(values[[1]][[1]])
  }
  
  if (!quant) {
    wis <- matrix(data = 0, nrow = N, ncol = M)
  } else {
    if (hh) {
      wis <- array(data = 0, dim = c(N, M, length(probs)))
    } else {
      wis <- array(data = 0, dim = c((N * H), M, length(probs)))
    }
  }
  
  if (hh) {
    for (n in 1:N) {
      if (!quant) {
        wis[n,   ] <- apply(X = values[[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[n], average = (!quant))  
      } else {
        wis[n, , ] <- apply(X = values[[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[n], average = (!quant)) |> t()
      }
    }
  } else { # Aggregating over horizons
    horizon <- as.numeric(names(y))
    
    count <- 1
    for (n in 1:N) {
      for (h in 1:H) {
        wis[count, , ] <- apply(X = values[[as.character(horizon[h])]][[n]], MARGIN = 1, FUN = compute_wis, probs = probs, y = y[[as.character(horizon[h])]][n], average = (!quant)) |> t()
        count <- count + 1
      }
    }
  }
  
  if (sum(wis, na.rm = TRUE) == 0) {
    theta <- 0
  } else {
    if (!quant) {
      theta <- optimize(f = cost_function, lower = lower, upper = upper, maximum = FALSE, probs = probs, values = values, y = y, wis = wis, boo_wis = boo_wis)$minimum  
    } else {
      if (theta_weights) {
        theta <- rep(x = 0, times = length(probs))
        if (two_pars) {
          phi <- rep(x = 0, times = length(probs))
        }
        for (q in 1:length(probs)) {
          tmp_wis <- wis[, , q]
          if (is.null(dim(tmp_wis))) {
            tmp_wis <- t(as.matrix(tmp_wis)) 
          }
          if (two_pars) {
            est_pars <- grid_optim(probs = probs, values = values, y = y, q = q, quant = quant, M = M, wis = tmp_wis, by = by, theta_lim = c(lower, upper), hh = hh, boo_wis = boo_wis, theta_weights = theta_weights)
            # est_pars <- optim(par = c(0, 1), fn = cost_function, method = c("L-BFGS-B"), lower = c(lower, 0), upper = c(upper, Inf), probs = probs, values = values, y = y, wis = tmp_wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = two_pars)$par 
            theta[q] <- est_pars[1]
            phi[q]   <- est_pars[2]
          } else {
            theta[q] <- optimize(f = cost_function, lower = lower, upper = upper, maximum = FALSE, probs = probs, values = values, y = y, wis = tmp_wis, q = q, quant = quant, hh = hh, boo_wis = boo_wis, two_pars = two_pars)$minimum  
          } 
        }
      } else { # Estimate the weights independently
        if (!quant) {
          # To be implemented
        } else {
          w <- matrix(data = 0, nrow = length(probs), ncol = M)
          nowcast <- rep(x = 0, times = length(probs))
          for (q in 1:length(probs)) {
            # w[q, ] <- optim(par = rep(log(1 / M), M), fn = cost_function_weights_ind, method = c("BFGS"), probs = probs, values = values, y = y, q = q, quant = quant)$par 
            # w[q, ] <- exp(w[q, ])
            
            # w[q, ] <- optim(par = rep((1 / M), M), fn = cost_function_weights_ind, method = c("L-BFGS-B"), lower = rep(0, M), upper = rep(1, M), probs = probs, values = values, y = y, q = q, quant = quant, exponentiate = FALSE)$par 
            
            w[q, ] <- grid_optim(probs = probs, values = values, y = y, q = q, quant = quant, M = M, theta_weights = theta_weights)
            
            nowcast[q] <- w[q, ] %*% current[, q]
          }
        }
        result <- list(nowcast = nowcast, weights = w)
      }
    }
  }
  
  if (theta_weights) {
    if (!quant) {
      w <- par_weights(theta = theta, wis = apply(X = wis, MARGIN = 2, FUN = mean), boo_wis = boo_wis)
      result <- list(nowcast = w %*% current, weights = w, theta = theta)
    } else {
      w <- matrix(data = 0, nrow = length(probs), ncol = M)
      if (hh) {
        nowcast <- rep(x = 0, times = length(probs))
      } else {
        nowcast <- matrix(data = 0, nrow = H, ncol = length(probs))
        rownames(nowcast) <- horizon
      }
      tmp_wis_list <- list()
      for (q in 1:length(probs)) {
        tmp_wis <- wis[, , q]
        if (is.null(dim(tmp_wis))) {
          tmp_wis <- t(as.matrix(tmp_wis)) 
        }
        tmp_wis_list[[q]] <- apply(X = tmp_wis, MARGIN = 2, FUN = mean)
        if (two_pars) {
          w[q, ] <- par_weights_scale(theta = theta[q], phi = phi[q], wis = apply(X = tmp_wis, MARGIN = 2, FUN = mean), boo_wis = boo_wis)
        } else {
          w[q, ] <- par_weights(theta = theta[q], wis = apply(X = tmp_wis, MARGIN = 2, FUN = mean), boo_wis = boo_wis)
        }
        if (hh) {
          nowcast[q] <- w[q, ] %*% current[, q]
        } else {
          for (h in 1:H) {
            nowcast[as.character(horizon[h]), q] <- w[q, ] %*% current[[as.character(horizon[h])]][[1]][, q]
          }
        }
      }
      if (name_values) {
        if (is.matrix(w)) { rownames(w) <- probs; colnames(w) <- models }
        if (is.vector(nowcast)) { names(nowcast) <- probs }
        if (is.vector(theta)) { names(theta) <- probs }
      }
      
      if (two_pars) {
        result <- list(nowcast = nowcast, weights = w, theta = theta, phi = phi) # wis = tmp_wis_list)
      } else {
        result <- list(nowcast = nowcast, weights = w, theta = theta)
      }
    }
  }
  
  result
}

##################################################################################
