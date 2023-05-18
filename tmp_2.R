compute_wis_data <- function (data, truth_data, start_date, end_date, horizon, models, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), method = "median", total_days = 90, ...) {
  days <- seq(start_date + 1, end_date, by = "1 day")
  wis <- matrix(data = 0, nrow = length(days), ncol = length(models))
  
  b <- txtProgressBar(min = 0, max = N, initial = 0)
  for (k in 1:length(days)) {
    dt <- days[k]
    
    number_days <- as.numeric((dt - start_date))
    number_days <- min(number_days, total_days)
    
    wis_tmp <- matrix(data = 0, nrow = number_days, ncol = length(models))
    
    for (n in 1:number_days) {
      tmp_data <- data[[as.character(dt)]][[as.character(horizon)]][[n]]
      tmp_y <- truth_data[[as.character(dt)]][[as.character(horizon)]][n]
      for (m in 1:length(models)) {
        wis_tmp[n, m] <- compute_wis(probs = probs, quant = tmp_data[m, ], y = tmp_y)  
      }
    }
  
    wis[k, ] <- apply(X = wis_tmp, MARGIN = 2, FUN = mean)
    setTxtProgressBar(b, n)
  }
  close(b)
  wis
}
