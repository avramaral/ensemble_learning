
plotting_WIS <-  function (horizon, wis_cm, colors, models, quant = FALSE, ...) {
  if (!quant) {
    matplot(x = horizon, y = do.call(what = rbind, args = wis_cm), type = "l", col = colors, lty = 1, xlab = "Horizon (days)", ylab = "WIS", lwd = 3, ylim = c(0, max(unlist(wis_cm))), main = "National level")
    legend("topleft", inset = 0.01, legend = models, col = colors, pch = 15, box.lty = 0)
  } else {
    for (q in 1:7) {
      tmp <- matrix(data = 0, nrow = length(horizon), ncol = length(models))
      count <- 1
      for (h in horizon) {
        tmp[count, ] <- wis_cm[[as.character(h)]][, q]
        count <- count + 1
      }
      matplot(x = horizon, y = tmp, type = "l", col = colors, lty = 1, xlab = "Horizon (days)", ylab = "WIS", lwd = 3, ylim = c(0, max(tmp)), main = paste("National level ", "(", probs[q], ")", sep = ""))
      legend("topleft", inset = 0.01, legend = models, col = colors, pch = 15, box.lty = 0)
    }
  }
}

##################################################################################

weights_tibble <- function (ensemble, r, models, horizon, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), skip_first_days = 1, skip_last_days = 0, ...) {
  skip_first_days <- skip_first_days
  days <- seq(r[1] + skip_first_days, r[2] - skip_last_days, by = "1 day")
  w_hat <- data.frame(forecast_date = as.Date(character()), horizon = character(), model = character(), quant = double(), value = double(), stringsAsFactors = FALSE)
  w_hat <- as_tibble(w_hat)
  
  b <- txtProgressBar(min = 1, max = length(days), initial = 1) 
  for (k in 1:length(days)) { 
    dt <- days[k]
    for (i in 1:length(horizon)) {
      for (m in 1:length(models)) {
        for (q in 1:length(probs)) {
          value <- ensemble[[as.character(dt)]][[as.character(horizon[i])]]$weights[q, ][m]
          w_hat <- w_hat |> add_row(forecast_date = dt, 
                                    horizon = as.character(horizon[i]), 
                                    model = models[m], 
                                    quant = probs[q], 
                                    value = value)
        }
      }
    }
    setTxtProgressBar(b, k)
  }
  close(b)
  
  w_hat
}

##################################################################################

plotting_weights <- function (w_hat, h = "0", q = 0.5, ...) {
  data <- w_hat |> filter(horizon == as.character(h), quant == q)
  print(data)
  
  data %>% 
    ggplot(aes(fill = as.factor(model), x = forecast_date)) +
    geom_bar(aes(y = value), position = "stack", stat = "identity") + # c(fill, stack)
    scale_fill_manual("Models", values = c("Epiforecasts" = "#B30000",
                                           "ILM"          = "#E69F00", 
                                           "KIT"          = "#56B4E9", 
                                           "LMU"          = "#F0E442", 
                                           "RIVM"         = "#80471C", 
                                           "RKI"          = "#3C4AAD", 
                                           "SU"           = "#CC79A7", 
                                           "SZ"           = "#000000")) +
    labs(x = "Forecast date", y = "Weights", title = paste("Horizon: ", h, " (", q, ")", sep = "")) + 
    theme(text = element_text(size = 12, family = "LM Roman 10")) +
    scale_y_continuous()
}

##################################################################################

plotting_wis_days <- function (wis_days, q, ...) {
  p <- wis_days |> 
    ggplot(aes(x = forecast_date, y = value, color = model)) +
    geom_line(linewidth = 1) +
    scale_color_manual("Models", values = c("Epiforecasts"   = "#B30000",
                                            "ILM"            = "#E69F00", 
                                            "KIT"            = "#56B4E9", 
                                            "LMU"            = "#F0E442", 
                                            "RIVM"           = "#80471C", 
                                            "RKI"            = "#3C4AAD", 
                                            "SU"             = "#CC79A7", 
                                            "SZ"             = "#000000",
                                            "MeanEnsemble"   = "#009E73",
                                            "MedianEnsemble" = "#60D1B3",
                                            "mean"           = "#009E73",
                                            "median"         = "#60D1B3",
                                            "wis"            = "#FF0000",
                                            "pinball"        = "#FF0000")) +
    labs(x = "Forecast date", y = "WIS", title = paste("Averaged over horizons (", q, ")", sep = "")) +
    theme(text = element_text(size = 12, family = "LM Roman 10"))
  
  p
}

##################################################################################
