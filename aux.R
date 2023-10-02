
##################################################
##################################################
##################################################

plot_wis_bar <- function (df_wis, wis_summ, models, colors, ylim_manual = 200, ...) {
  
  # Compute the total WIS based on the 3-part decomposition
  df_wis_total <- data.frame(model = rep(NA, length(models)), wis = rep(NA, length(models)))
  df_wis_total$model <- models
  df_wis_total$model <- factor(x = df_wis_total$model, levels = models)
  df_wis_total$wis <- Reduce(`+`, wis_summ) / length(wis_summ)
  
  colors_ordered <- colors
  names(colors_ordered) <- models
  
  pp <- ggplot() +
    geom_bar(data = df_wis, aes(x = model, y = wis), fill = "white", stat = "identity") +
    geom_bar(data = df_wis, aes(x = model, y = wis, fill = model, alpha = component, color = model), stat = "identity") +
    geom_label(
      data = df_wis_total, aes(x = model, y = 0.5 * as.numeric(wis), label = sprintf("%0.2f", round(as.numeric(wis), digits = 2))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.15, "lines"),
      size = 10 / .pt,
      family = "LM Roman 10",
      label.padding = unit(0.2, "lines") 
    ) + 
    scale_fill_manual(values = colors_ordered, guide = "none") +
    scale_color_manual(values = colors_ordered, guide = "none") +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1.0), 
      labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    scale_x_discrete(limits = rev(models), drop = FALSE) +
    labs(x = NULL, y = "WIS (Averaged over time points and horizons)", color = "Model", alpha = "Decomposition of WIS") +
    ylim(0, ylim_manual) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16, family = "LM Roman 10"), 
          axis.ticks.y = element_blank()
    )
  
  pp
}

##################################################
##################################################
##################################################

plot_wis_line_horizon <- function (df_wis_horizon, models, colors, quant = FALSE, legend = FALSE, ...) {

  horizon <- unique(df_wis_horizon$horizon)
  
  colors_ordered <- colors
  names(colors_ordered) <- models
  
  pp <- ggplot(data = df_wis_horizon, aes(x = horizon, y = wis, color = model)) +
    { if (quant)
        facet_wrap(~probs, ncol = 3, scales = "free") 
    } +
    geom_line(linewidth = 1) +
    scale_color_manual(values = colors_ordered) +
    labs(x = "Horizon (days)", y = "WIS (Averaged over time points)", color = "Model") +
    scale_x_continuous(breaks = 0:5 * -5, minor_breaks = horizon) +
    expand_limits(y = 0) +
    theme_bw() +
    theme(legend.position = ifelse(legend, "bottom", "none"), text = element_text(size = 16, family = "LM Roman 10"))
    
  pp
}

##################################################
##################################################
##################################################

plot_postprocessed_models <- function (data, nowcasts, truth_data, model, r, training_size, uncertain_size, hh = 0, ens_method = "wis", horizon = -28:0, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), ...) {
  
  name_method <- ifelse(ens_method == "wis", "DISW", "ISW")
  
  cmb_models <- c(model, name_method)
  cmb_colors <- c(color, "royalblue4")
  names(cmb_colors) <- cmb_models
  
  mm <- model
  df_nowcast <- nowcasts   |> filter(target == paste(hh, " day ahead inc hosp", sep = ""), type == "quantile", quantile %in% probs, model == mm) |> select(target_end_date, quantile, value, model)
  df_nowcast <- df_nowcast |> rename(date = target_end_date)
  df_true_dt <- truth_data |> filter(date >= range(df_nowcast$date)[1], date <= range(df_nowcast$date)[2])
  
  for (i in 1:nrow(df_true_dt)) { 
    for (j in 1:length(unique(df_nowcast$model))) {
      df_nowcast <- df_nowcast |> add_row(date = df_true_dt$date[i], quantile = 0, value = df_true_dt$truth[i], model = unique(df_nowcast$model)[j]) 
    }
  }
  df_nowcast <- df_nowcast |> pivot_wider(names_from = quantile, values_from = value)
  
  alphas <- setNames(c(0.75, 0.4), c("50%", "95%"))
  line_colors <- setNames(c("red", "gray"), c("Final", "At time of nowcast"))
  p1 <- ggplot(df_nowcast) +
    geom_ribbon(aes(x = date, ymin = `0.025`, ymax = `0.975`, alpha = "95%"), fill = "skyblue3") +
    geom_ribbon(aes(x = date, ymin = `0.25` , ymax = `0.75`, alpha = "50%"),  fill = "skyblue3") +
    geom_line(aes(x = date, y = `0.5`), linetype = "solid", linewidth = 0.5, color = "royalblue4") + 
    geom_line(aes(x = date, y = `0`, color = "Final"),  linetype = "solid", linewidth = 0.5)  + 
    labs(x = NULL, y = "COVID-19 7-day hospitalization incidence in Germany", title = paste("Horizon: ", hh, " days (Post-processed ", model, ")", sep = "")) +
    scale_alpha_manual(
      name = "Nowcasts with \nprediction intervals", values = alphas,
      guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
    ) +
    scale_color_manual(
      name = "Truth", values = line_colors,
      guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)
    ) +
    scale_y_continuous(breaks = c(5000, 10000, 15000), limits = c(2500, 17500)) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.key.size = unit(0.65, "lines"),
      strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
      axis.title.y = element_text(size = 11),
      axis.text = element_text(size = 11),
      axis.ticks = element_line(colour = "black", linewidth = 0.25),
      panel.grid.major = element_line(linewidth = 0.15),
      panel.grid.minor = element_line(linewidth = 0.1),
      plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
      legend.margin = margin(0, 0, 0, 5),
      legend.box.spacing = unit(0, "pt"),
      legend.background = element_rect(fill = "transparent"),
      text = element_text(family = "LM Roman 10")
    )  
  
  ##################################################
  
  tmp_data_1 <- data[data$model == model, ]
  tmp_data_2 <- nowcasts[nowcasts$model == model, ]
  tmp_data_2$model <- name_method
  tmp_data_T <- rbind(tmp_data_1, tmp_data_2)
  
  wis_truth <- compute_wis_truth(data = tmp_data_T, truth_data = truth_data, models = cmb_models, horizon = horizon, start_date = r[1], end_date = r[2], skip_first_days = uncertain_size, verbose = FALSE)
  
  df_wis <- wis_truth$df_wis
  wis_summ <- wis_truth$wis_summ
  
  
  df_wis_horizon <- data.frame(model = rep(cmb_models, length(horizon)), horizon = rep(horizon, each = length(cmb_models)), wis = 0)
  count <- 1
  for (i in 1:length(wis_summ)) {
    for (j in 1:length(cmb_models)) {
      df_wis_horizon$wis[count] <- wis_summ[[i]][j]
      count <- count + 1
    }
  }
  
  df_wis_horizon[df_wis_horizon$model == name_method, ]$model <- "Post-processed"
  df_wis_horizon$model <- factor(df_wis_horizon$model, levels = c(model, "Post-processed"))
  names(cmb_colors) <- c(names(cmb_colors)[1], "Post-processed")
  
  p2 <- ggplot(data = df_wis_horizon, aes(x = horizon, y = wis, color = model)) +
    geom_line(linewidth = 1) +
    scale_color_manual(NULL, values = cmb_colors) +
    labs(x = "Horizon (days)", y = "WIS (Averaged over time points)", color = "Model") +
    scale_x_continuous(breaks = 0:5 * -5, minor_breaks = -28:0) +
    expand_limits(y = 0) +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 11),
          axis.text = element_text(size = 11),
          text = element_text(size = 11, family = "LM Roman 10"))
  
  ##################################################
  
  cmb_wis <- list()
  
  count <- 1
  b <- txtProgressBar(min = 0, max = length(horizon), initial = 0)
  for (h in horizon) {
    cmb_wis[[as.character(h)]] <- compute_wis_data(data = tmp_data_T, truth_data = truth_data, start_date = r[1], end_date = r[2], horizon = h, models = cmb_models, probs = probs)
    
    count <- count + 1
    setTxtProgressBar(b, count)
  }
  close(b)
  
  total_days <- ifelse(skip_recent_days, (training_size - uncertain_size), training_size)
  wis_days <- compute_wis_days(wis = cmb_wis, models = cmb_models, start_date = r[1], end_date = r[2], total_days = total_days)
  
  wis_days[wis_days$model == name_method, ]$model <- "Post-processed"
  p3 <- ggplot(data = wis_days, aes(x = forecast_date, y = value, color = model)) +
    geom_line(linewidth = 1, alpha = rep(ifelse(unique(wis_days$forecast_date) < (r[1] + uncertain_size), 0.25, 1), 2)) +
    geom_vline(xintercept = as.numeric(r[1] + uncertain_size), linetype = "dashed") + 
    scale_color_manual(NULL, values = cmb_colors) +
    expand_limits(y = 0) +
    labs(x = "Forecast date", y = "WIS (Averaged over horizons and M.W. up to 90 days)") +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 11),
          axis.text = element_text(size = 11),
          text = element_text(size = 11, family = "LM Roman 10"))
  
  ##################################################
  
  p_total <- p1 + p2 + p3 + plot_layout(width = c(2, 2, 2)) + plot_annotation(theme = theme(plot.margin = margin()))
  list(p1 = p1, p2 = p2, p3 = p3, p_total = p_total)
}

##################################################
##################################################
##################################################

plotting_summarized_weights <- function (w_hat, r, models, colors, uncertain_size = 40, ...) {
  names(colors) <- models
  alphas <- c(1, 2)
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + uncertain_size), alphas[1], alphas[2])))
  ifelse(length(unique(w_hat$aa)) == 1, rr <- 1, rr <- c(0.25, 1))
  pp <- w_hat %>% 
    ggplot(aes(fill = as.factor(model), x = forecast_date)) +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") +
    geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)) - 0.5, linetype = "dashed") + 
    scale_fill_manual("Models", values = colors) +
    scale_alpha_manual(values = rr, guide = "none") +
    scale_x_date(limit = c((r[1] + 1), max(w_hat$forecast_date))) + 
    labs(x = "Forecast date", y = "Weights (Averaged over horizons and quantiles)") + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10")) +
    scale_y_continuous()
  
  pp
}

##################################################
##################################################
##################################################

plotting_quantile_weights <- function (w_hat, r, models, colors, uncertain_size = 40, ...) {
  names(colors) <- models
  
  alphas <- c(1, 2)
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 40), alphas[1], alphas[2])))
  if (length(unique(w_hat$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
  w_hat$quant <- factor(w_hat$quant)
  
  pp <- ggplot(w_hat, aes(fill = as.factor(model), x = forecast_date)) +
    facet_wrap("quant", scales = "fixed", ncol = 1) +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") + # c(fill, stack)
    geom_vline(xintercept = as.numeric(r[1] + 41) - 0.5, linetype = "dashed") + 
    scale_fill_manual("Models", values = colors) +
    scale_alpha_manual(values = af, guide = "none") +
    scale_x_date(limit = c((r[1] + 1), max(w_hat$forecast_date))) + 
    labs(x = "Forecast date", y = "Weights (Averaged over horizons)") + 
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          legend.position = "right",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          strip.text = element_text(size = 16, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.ticks = element_line(colour = "black", linewidth = 0.25),
          panel.grid.major = element_line(linewidth = 0.15),
          panel.grid.minor = element_line(linewidth = 0.1),
          plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
          legend.margin = margin(0, 0, 0, 5),
          legend.box.spacing = unit(0, "pt"),
          legend.background = element_rect(fill = "transparent"),
          text = element_text(size = 16, family = "LM Roman 10")) +
    scale_y_continuous()
  
  pp
}

##################################################
##################################################
##################################################

plotting_horizon_weights <- function (w_hat, r, models, colors, uncertain_size = 40, hhs = c(-28, -23, -18, -13, -8, -3, 0), big_title = "", ...) {
  names(colors) <- models
  
  alphas <- c(1, 2)
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 40), alphas[1], alphas[2])))
  if (length(unique(w_hat$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
  
  w_hat <- w_hat |> filter(horizon %in% hhs)
  w_hat$horizon <- factor(x = w_hat$horizon, levels = hhs)
  
  pp <- ggplot(w_hat, aes(fill = as.factor(model), x = forecast_date)) +
    facet_wrap("horizon", scales = "fixed", nrow = ceiling(length(hhs) / 2)) +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") +
    geom_vline(xintercept = as.numeric(r[1] + 41) - 0.5, linetype = "dashed") + 
    scale_fill_manual("Models", values = colors) +
    scale_alpha_manual(values = af, guide = "none") +
    scale_x_date(limit = c((r[1] + 1), max(w_hat$forecast_date))) + 
    labs(x = "Forecast date", y = "Weights (Averaged over quantiles)", title = big_title) + 
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          legend.position = "right",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          strip.text = element_text(size = 16, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.ticks = element_line(colour = "black", linewidth = 0.25),
          panel.grid.major = element_line(linewidth = 0.15),
          panel.grid.minor = element_line(linewidth = 0.1),
          plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
          legend.margin = margin(0, 0, 0, 5),
          legend.box.spacing = unit(0, "pt"),
          legend.background = element_rect(fill = "transparent"),
          text = element_text(size = 16, family = "LM Roman 10")) +
    scale_y_continuous()
  
  pp
}

