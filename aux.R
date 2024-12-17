
##################################################
##################################################
##################################################

plot_wis_bar <- function (df_wis, wis_summ, models, colors, ylim_manual = 200, skip_space = FALSE, skip_first = 1:8, skip_last = 9:10, reference_pts = NULL, change_name_select = FALSE, df_wis_baseline = NULL, add_best_ind_models = FALSE, ...) {
  
  if (!is.null(df_wis_baseline)) { total_baseline <- sum(df_wis_baseline$wis) }
  
  if (!is.null(reference_pts) &  skip_space) {
    reference_pts <- data.frame(model = c(models[skip_first], "space", models[skip_last]), value = c(reference_pts[skip_first], NA, reference_pts[skip_last]))
    reference_pts$model <- factor(x = reference_pts$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
  }
  
  if (!is.null(reference_pts) & !skip_space) {
    reference_pts <- data.frame(model = models, value = reference_pts)
    reference_pts$model <- factor(x = models, levels = models, labels = models)
  }
  
  # Compute the total WIS based on the 3-part decomposition
  df_wis_total <- data.frame(model = rep(NA, length(models)), wis = rep(NA, length(models)))
  df_wis_total$model <- models
  df_wis_total$model <- factor(x = df_wis_total$model, levels = models)
  df_wis_total$wis <- Reduce(`+`, wis_summ) / length(wis_summ)
  
  if (skip_space) {
    df_wis$model <- as.character(df_wis$model)
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "sprd"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "over"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "undr"))
    df_wis$model <- factor(x = df_wis$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_wis$wis <- as.numeric(df_wis$wis)
  }

  if (change_name_select) { 
    models <- c("Mean", "Median", "Post-Mean", "Post-Median", "Mean-Post", "Median-Post", "DISW 1", "DISW 2", "DISW 3", "DISW 4", "AISW 1", "AISW 2", "AISW 3", "AISW 4", "Select-4 Mean 2", "Select-4 Median 2")
  }
  
  if (skip_space) {
    colors_ordered <- c(colors[skip_first], "cyan", colors[skip_last])
    names(colors_ordered) <- c(models[skip_first], "space", models[skip_last])
  } else {
    colors_ordered <- colors
    names(colors_ordered) <- models
  }
  
  if (change_name_select) {
    df_wis <- df_wis %>% mutate(model = as.character(model)) %>% mutate(model = ifelse(model == "Select-4 Mean",   "Select-4 Mean 2",   model)) %>% mutate(model = as.factor(model))
    df_wis <- df_wis %>% mutate(model = as.character(model)) %>% mutate(model = ifelse(model == "Select-4 Median", "Select-4 Median 2", model)) %>% mutate(model = as.factor(model))
  
    df_wis_total <- df_wis_total %>% mutate(model = as.character(model)) %>% mutate(model = ifelse(model == "Select-4 Mean",   "Select-4 Mean 2",   model)) %>% mutate(model = as.factor(model))
    df_wis_total <- df_wis_total %>% mutate(model = as.character(model)) %>% mutate(model = ifelse(model == "Select-4 Median", "Select-4 Median 2", model)) %>% mutate(model = as.factor(model))
    
  }
  
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
    { if (!is.null(reference_pts)) geom_point(data = reference_pts, aes(x = model, y = value), pch = 1, size = 3) } +
    scale_fill_manual(values = colors_ordered, guide = "none") +
    scale_color_manual(values = colors_ordered, guide = "none") +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1.0), 
      labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    { if ( skip_space) scale_x_discrete(limits = rev(c(models[skip_first], "space", models[skip_last])), labels = rev(c(models[skip_first], " ", models[skip_last])), drop = FALSE) } +
    { if (!skip_space) scale_x_discrete(limits = rev(models), drop = FALSE) } +
    #####################
    # Add second x-axis #
    #####################
    { if (!is.null(df_wis_baseline)) scale_y_continuous(name = "WIS (Averaged all)", limits = c(0, ylim_manual), sec.axis = sec_axis(~ . / total_baseline, name = "Relative WIS") ) } +
    { if ( is.null(df_wis_baseline)) ylim(0, ylim_manual) } +
    #####################
    # + best ind models #
    #####################
    # { if (add_best_ind_models) geom_hline(yintercept = c(93.67), linetype = "solid",  color = "#444444FF") } +
    # { if (add_best_ind_models) geom_hline(yintercept = c(89.26), linetype = "dotted", color = "#444444FF") } +
    { if (add_best_ind_models) geom_hline(aes(yintercept = 93.67, linetype = "without post-processing"), color = "#444444FF") } +
    { if (add_best_ind_models) geom_hline(aes(yintercept = 89.26, linetype = "with post-processing"),    color = "#444444FF") } +
    scale_linetype_manual(name = "Best individual model",
      values = c("without post-processing" = "solid", "with post-processing" = "dotted")
    ) +
    #####################
    # Arrange legends #
    #####################
    guides(
      alpha =    guide_legend(order = 1, title.position = "top"),
      linetype = guide_legend(order = 2, title.position = "top")
    ) +
    #####################
    labs(x = NULL, y = "WIS (Averaged all)", color = "Model", alpha = "Decomposition of WIS") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.box = "vertical", # Stack legends vertically
          legend.title.align = 0.5,  # Centre-align legend titles
          legend.text.align = 0.5,   # Centre-align legend text
          legend.box.just = "center", # Centre the entire legend box
          text = element_text(size = 16, family = "LM Roman 10"), 
          axis.ticks.y = element_blank())
  
  pp
}

create_line_legend <- function (...) {
  legend_data <- data.frame(
    x = c(1, 2),
    y = c(1, 1),
    linetype = c("Without post-processing", "With post-processing")
  )
  
  line_legend <- ggplot(legend_data, aes(x = x, y = y, linetype = linetype)) +
    geom_line(size = 0.65, colour = "#444444FF") +
    scale_linetype_manual(name = "Best individual model", values = c("Without post-processing" = "solid", "With post-processing" = "dotted")) +
    theme_minimal() +
    theme(legend.position = "bottom", text = element_text(size = 16, family = "LM Roman 10"),
          legend.key.width = unit(2.25, "lines"),
          legend.key.height = unit(0.1, "lines")) + 
    guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5))
  
  # Extract the legend as a grob
  legend_best_ind_models <- get_legend(line_legend)
  
  empty_plot <- ggplot() + theme_void()
  
  combined_legend <- plot_grid(empty_plot, legend_best_ind_models, nrow = 1, rel_widths = c(0.16, 0.84))
  
  combined_legend
}

##################################################

plot_wis_bar_stratified <- function (df_wis, wis_summ, models, colors, ylim_manual = 200, skip_space = FALSE, reference_pts = NULL, ...) {
  
  if (idx_missing_model == 2) { models <- c(models[1], "ILM",     models[2:9]) } else if (idx_missing_model == 6) { models <- c(models[1:5], "RKI",     models[6:9])  }
  if (idx_missing_model == 2) { colors <- c(colors[1], "#E69F00", colors[2:9]) } else if (idx_missing_model == 6) { colors <- c(colors[1:5], "#3C4AAD", colors[6:9])  }
  
  if (!is.null(reference_pts) & skip_space) {
    reference_pts <- data.frame(model = c(models[1:8], "space", models[9:10]), value = c(reference_pts[1:(idx_missing_model - 1)], NA, reference_pts[(idx_missing_model):7], NA, reference_pts[8:9]))
    reference_pts$model <- factor(x = reference_pts$model, levels = c(models[1:8], "space", models[9:10]), labels = c(models[1:8], "space", models[9:10]))
  }
  
  # Compute the total WIS based on the 3-part decomposition
  df_wis_total <- data.frame(model = rep(NA, length(models)), wis = rep(NA, length(models)))
  df_wis_total$model <- models
  df_wis_total$model <- factor(x = df_wis_total$model, levels = models)
  tmp_wis <- Reduce(`+`, wis_summ) / length(wis_summ)
  tmp_wis <- c(tmp_wis[1:(idx_missing_model - 1)], NA, tmp_wis[idx_missing_model:9])
  df_wis_total$wis <- tmp_wis
  
  if (skip_space) {
    df_wis$model <- as.character(df_wis$model)
    for (i in 1:2) {
      df_wis <- rbind(df_wis, c(as.factor("space"), 0, "sprd"))
      df_wis <- rbind(df_wis, c(as.factor("space"), 0, "over"))
      df_wis <- rbind(df_wis, c(as.factor("space"), 0, "undr"))
    }
    tmp_models <- models[-idx_missing_model]
    df_wis$model <- factor(x = df_wis$model, levels = c(tmp_models[1:7], "space", tmp_models[8:9]), labels = c(tmp_models[1:7], "space", tmp_models[8:9]))
    df_wis$wis <- as.numeric(df_wis$wis)
  }
  
  if (skip_space) {
    colors_ordered <- c(colors[1:8], "cyan", colors[9:10])
    names(colors_ordered) <- c(models[1:8], "space", models[9:10])
  } else {
    colors_ordered <- colors
    names(colors_ordered) <- models
  }
  names(colors_ordered)[idx_missing_model] <- "space"
  colors_ordered[idx_missing_model] <- "cyan"
  
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
    { if (!is.null(reference_pts) & skip_space) geom_point(data = reference_pts, aes(x = model, y = value), pch = 1, size = 3) } +
    scale_fill_manual(values = colors_ordered, guide = "none") +
    scale_color_manual(values = colors_ordered, guide = "none") +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1.0), 
      labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    { if ( skip_space) scale_x_discrete(limits = rev(c(models[1:8], "space", models[9:10])), labels = rev(c(models[1:8], " ", models[9:10])), drop = FALSE) } +
    { if (!skip_space) scale_x_discrete(limits = rev(models), drop = FALSE) } +
    labs(x = NULL, y = "WIS (Averaged all)", color = "Model", alpha = "Decomposition of WIS") +
    ylim(0, ylim_manual) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16, family = "LM Roman 10"), 
          axis.ticks.y = element_blank()
    )
  
  pp
}

##################################################

plot_wis_bar_stratified_simplified <- function (df_wis, wis_summ, models, colors, ylim_manual = 200, reference_pts = NULL, df_wis_baseline = NULL, ...) {
  
  if (!is.null(df_wis_baseline)) { total_baseline <- sum(df_wis_baseline$wis) }
  
  nm <- length(models)
  
  if (idx_missing_model == 2) { models <- c(models[1], "ILM",     models[2:nm]) } else if (idx_missing_model == 6) { models <- c(models[1:5], "RKI",     models[6:nm])  }
  if (idx_missing_model == 2) { colors <- c(colors[1], "#E69F00", colors[2:nm]) } else if (idx_missing_model == 6) { colors <- c(colors[1:5], "#3C4AAD", colors[6:nm])  }
  
  if (!is.null(reference_pts)) {
    reference_pts <- data.frame(model = models, value = c(reference_pts[1:(idx_missing_model - 1)], NA, reference_pts[(idx_missing_model):nm]))
    reference_pts$model <- factor(x = reference_pts$model, levels = models, labels = models)
  }
  
  # Compute the total WIS based on the 3-part decomposition
  df_wis_total <- data.frame(model = rep(NA, length(models)), wis = rep(NA, length(models)))
  df_wis_total$model <- models
  df_wis_total$model <- factor(x = df_wis_total$model, levels = models)
  tmp_wis <- Reduce(`+`, wis_summ) / length(wis_summ)
  tmp_wis <- c(tmp_wis[1:(idx_missing_model - 1)], NA, tmp_wis[idx_missing_model:nm])
  df_wis_total$wis <- tmp_wis
  
  colors_ordered <- colors
  names(colors_ordered) <- models
  
  names(colors_ordered)[idx_missing_model] <- "space"
  colors_ordered[idx_missing_model] <- "cyan"
  
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
    { if (!is.null(reference_pts)) geom_point(data = reference_pts, aes(x = model, y = value), pch = 1, size = 3) } +
    scale_fill_manual(values = colors_ordered, guide = "none") +
    scale_color_manual(values = colors_ordered, guide = "none") +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1.0), 
      labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    { scale_x_discrete(limits = rev(models), drop = FALSE) } +
    #####################
    # Add second x-axis #
    #####################
    { if (!is.null(df_wis_baseline)) scale_y_continuous(name = "WIS (Averaged all)", limits = c(0, ylim_manual), sec.axis = sec_axis(~ . / total_baseline, name = "Relative WIS") ) } +
    { if ( is.null(df_wis_baseline)) ylim(0, ylim_manual) } +
    #####################
    labs(x = NULL, y = "WIS (Averaged all)", color = "Model", alpha = "Decomposition of WIS") +
    # ylim(0, ylim_manual) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16, family = "LM Roman 10"), 
          axis.ticks.y = element_blank()
    )
  
  pp
}

##################################################

plot_wis_bar_ensemble <- function (df_wis, wis_summ, models, colors, ylim_manual = 200, skip_space = FALSE, skip_first = 1:2, skip_last = 3:6, df_wis_baseline = df_wis_baseline, add_best_ind_models = FALSE, best_ind_models = c(0, 0), ...) {
  
  if (!is.null(df_wis_baseline)) { total_baseline <- sum(df_wis_baseline$wis) }
  
  # Compute the total WIS based on the 3-part decomposition
  df_wis_total <- data.frame(model = rep(NA, length(models)), wis = rep(NA, length(models)))
  df_wis_total$model <- models
  df_wis_total$model <- factor(x = df_wis_total$model, levels = models)
  tmp_wis <- Reduce(`+`, wis_summ) / length(wis_summ)
  df_wis_total$wis <- tmp_wis
  
  if (skip_space) {
    df_wis$model <- as.character(df_wis$model)
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "sprd"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "over"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "undr"))
    df_wis$model <- factor(x = df_wis$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_wis$wis <- as.numeric(df_wis$wis)
  }
  
  if (skip_space) {
    colors_ordered <- c(colors[skip_first], "cyan", colors[skip_last])
    names(colors_ordered) <- c(models[skip_first], "space", models[skip_last])
  } else {
    colors_ordered <- colors
    names(colors_ordered) <- models
  }

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
    # scale_x_discrete(limits = rev(models), drop = FALSE) +
    { if ( skip_space) scale_x_discrete(limits = rev(c(models[skip_first], "space", models[skip_last])), labels = rev(c(models[skip_first], " ", models[skip_last])), drop = FALSE) } +
    { if (!skip_space) scale_x_discrete(limits = rev(models), drop = FALSE) } +
    #####################
    # Add second x-axis #
    #####################
    { if (!is.null(df_wis_baseline)) scale_y_continuous(name = "WIS (Averaged all)", limits = c(0, ylim_manual), sec.axis = sec_axis(~ . / total_baseline, name = "Relative WIS") ) } +
    { if ( is.null(df_wis_baseline)) ylim(0, ylim_manual) } +
    #####################
    # + best ind models #
    #####################
    { if (add_best_ind_models) geom_hline(yintercept = c(best_ind_models[1]), linetype = "solid", color = "#444444FF") } +
    { if (add_best_ind_models) geom_hline(yintercept = c(best_ind_models[2]), linetype = "dotted", color = "#444444FF") } +
    #####################
    labs(x = NULL, y = "WIS (Averaged all)", color = "Model", alpha = "Decomposition of WIS") +
    # ylim(0, ylim_manual) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16, family = "LM Roman 10"), 
          axis.ticks.y = element_blank()
    )
  
  pp
}

##################################################

plot_wis_bar_ensemble_old <- function (df_wis, wis_summ, models, colors, ylim_manual = 200, skip_space = FALSE, ...) {
  
  models <- c("DISW 1", "DISW 2", "DISW 3", "DISW 4", "AISW 1", "AISW 2", "AISW 3", "AISW 4", "Mean", "Median")
  DISW_colors <- colorRampPalette(c("#8B0000", "#E6ADD8"))(4)
  AISW_colors <- colorRampPalette(c("#00008B", "#ADD8E6"))(4)
  colors <- c(DISW_colors, AISW_colors, "#009E73", "#60D1B3")
  
  # Compute the total WIS based on the 3-part decomposition
  df_wis_total <- data.frame(model = rep(NA, length(models)), wis = rep(NA, length(models)))
  df_wis_total$model <- models
  df_wis_total$model <- factor(x = df_wis_total$model, levels = models)
  tmp_wis <- Reduce(`+`, wis_summ) / length(wis_summ)
  tmp_wis <- c(NA, tmp_wis[1], NA, tmp_wis[2], NA, tmp_wis[3], NA, tmp_wis[4], tmp_wis[5:6])
  df_wis_total$wis <- tmp_wis
  
  if (skip_space) {
    df_wis$model <- as.character(df_wis$model)
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "sprd"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "over"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "undr"))
    df_wis$model <- factor(x = df_wis$model, levels = c(models[1:8], "space", models[9:10]), labels = c(models[1:8], "space", models[9:10]))
    df_wis$wis <- as.numeric(df_wis$wis)
  }
  
  if (skip_space) {
    colors_ordered <- c(colors[1:8], "cyan", colors[9:10])
    names(colors_ordered) <- c(models[1:8], "space", models[9:10])
  } else {
    colors_ordered <- colors
    names(colors_ordered) <- models
  }
  
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
    { if ( skip_space) scale_x_discrete(limits = rev(c(models[1:8], "space", models[9:10])), labels = rev(c(models[1:8], " ", models[9:10])), drop = FALSE) } +
    { if (!skip_space) scale_x_discrete(limits = rev(models), drop = FALSE) } +
    labs(x = NULL, y = "WIS (Averaged all)", color = "Model", alpha = "Decomposition of WIS") +
    ylim(0, ylim_manual) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16, family = "LM Roman 10"), 
          axis.ticks.y = element_blank()
    )
  
  pp
}

##################################################

plot_wis_bar_size <- function (df_wis, wis_summ, models, colors, ylim_manual = 100, skip_space = FALSE, skip_first = 1:2, skip_last = 3:5, df_wis_baseline = NULL, ...) {

  if (!is.null(df_wis_baseline)) { total_baseline <- sum(df_wis_baseline$wis) }
  
  # Compute the total WIS based on the 3-part decomposition
  df_wis_total <- data.frame(model = rep(NA, length(models)), wis = rep(NA, length(models)))
  df_wis_total$model <- models
  df_wis_total$model <- factor(x = df_wis_total$model, levels = models)
  df_wis_total$wis <- Reduce(`+`, wis_summ) / length(wis_summ)
  
  if (skip_space) {
    df_wis$model <- as.character(df_wis$model)
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "sprd"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "over"))
    df_wis <- rbind(df_wis, c(as.factor("space"), 0, "undr"))
    df_wis$model <- factor(x = df_wis$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_wis$wis <- as.numeric(df_wis$wis)
  }
  
  
  if (skip_space) {
    colors_ordered <- c(colors[skip_first], "cyan", colors[skip_last])
    names(colors_ordered) <- c(models[skip_first], "space", models[skip_last])
  } else {
    colors_ordered <- colors
    names(colors_ordered) <- models
  }
  
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
    { if ( skip_space) scale_x_discrete(limits = rev(c(models[skip_first], "space", models[skip_last])), labels = rev(c(models[skip_first], " ", models[skip_last])), drop = FALSE) } +
    { if (!skip_space) scale_x_discrete(limits = rev(models), drop = FALSE) } +
    #####################
    # Add second x-axis #
    #####################
    { if (!is.null(df_wis_baseline)) scale_y_continuous(name = "WIS (Averaged all)", limits = c(0, ylim_manual), sec.axis = sec_axis(~ . / total_baseline, name = "Relative WIS") ) } +
    { if ( is.null(df_wis_baseline)) ylim(0, ylim_manual) } +
    #####################
    labs(x = NULL, y = "WIS (Averaged all)", color = "Model", alpha = "Decomposition of WIS") +
    # ylim(0, ylim_manual) +
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

plot_postprocessed_models <- function (data, nowcasts, truth_data, model, r, training_size, uncertain_size, baseline_tmp, hh = 0, ens_method = "wis", horizon = -28:0, probs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975), extra_skip = 0, skip_recent_days = FALSE, average = FALSE, ...) {
  
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
  
  x_dates <- as.Date(c("2021-11-15", "2022-04-29"))
  
  alphas <- setNames(c(0.75, 0.4), c("50%", "95%"))
  line_colors <- setNames(c("red", "lightgray"), c("Final", "At time\nof nowcast"))
  p1 <- ggplot(df_nowcast) +
    geom_ribbon(aes(x = date, ymin = `0.025`, ymax = `0.975`, alpha = "95%"), fill = "skyblue3") +
    geom_ribbon(aes(x = date, ymin = `0.25` , ymax = `0.75`, alpha = "50%"),  fill = "skyblue3") +
    geom_line(aes(x = date, y = `0.5`), linetype = "solid", linewidth = 0.5, color = "royalblue4") + 
    geom_line(aes(x = date, y = `0`, color = "Final"),  linetype = "solid", linewidth = 0.5)  + 
    geom_line(data = baseline_tmp, aes(x = target_end_date, y = value, color = "At time\nof nowcast"),  linetype = "solid", linewidth = 0.5)  + 
    labs(x = NULL, y = "COVID-19 7-day hospitalization incidence in Germany", title = paste("Horizon: ", hh, " days (Post-processed ", model, ")", sep = "")) +
    scale_alpha_manual(
      name = "Nowcasts with \nprediction intervals", values = alphas,
      guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
    ) +
    scale_color_manual(
      name = "Truth", values = line_colors,
      guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)
    ) +
    scale_y_continuous(breaks = c(0, 5000, 10000, 15000), limits = c(0, 17500)) +
    xlim(x_dates) + 
    theme_bw() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
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
  
  # P2
  
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
  
  ##################################################
  
  # P3
  
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
  wis_days <- compute_wis_days(wis = cmb_wis, models = cmb_models, start_date = r[1], end_date = r[2], total_days = total_days, average = average)
  
  if (skip_recent_days) { r_1 <- (r[1] - 40 + 1); r_e <- 0 } else { r_1 <- r[1] + 1; r_e <- 0 }
  r_2 <- r[2]
  
  if (average) {
    y_lab_temp <- "WIS (Averaged over horizons and M.W. up to 90 days)"
  } else {
    y_lab_temp <- "WIS (Averaged over horizons)"
  }
  
  wis_days[wis_days$model == name_method, ]$model <- "Post-processed"
  
  ##################################################
  
  max_y <- max(df_wis_horizon$wis, wis_days$value)
  
  p2 <- ggplot(data = df_wis_horizon, aes(x = horizon, y = wis, color = model)) +
    geom_line(linewidth = 1) +
    scale_color_manual(NULL, values = cmb_colors) +
    labs(x = "Horizon (days)", y = "WIS (Averaged over time points)", color = "Model") +
    scale_x_continuous(breaks = 0:5 * -5, minor_breaks = -28:0) +
    expand_limits(y = c(0, max_y)) +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          strip.text = element_text(size = 11, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 11),
          axis.text = element_text(size = 11),
          text = element_text(size = 11, family = "LM Roman 10"))
  
  
  p3 <- ggplot(data = wis_days, aes(x = forecast_date, y = value, color = model)) +
    # geom_line(linewidth = 1, alpha = rep(ifelse(unique(wis_days$forecast_date) < (r_1 + uncertain_size + 1), 0.25, 1), 2)) +
    geom_line(linewidth = 1, alpha = rep(ifelse(unique(wis_days$forecast_date) < (r_1 - 1 + 30 + uncertain_size), 0.25, 1), 2)) +
    # geom_vline(xintercept = as.numeric(r_1 + uncertain_size + r_e + 1), linetype = "dashed") + 
    # geom_vline(xintercept = as.numeric(r_1 + uncertain_size + r_e + 1 + extra_skip), linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r_1 - 1 + 30 + uncertain_size), linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r_1 - 1 + 30 + uncertain_size - 40), linetype = "dashed") + 
    scale_color_manual(NULL, values = cmb_colors) +
    expand_limits(y = c(0, max_y)) +
    xlim(c(r_1, r_2)) +
    labs(x = "", y = y_lab_temp) +
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

plotting_summarized_weights <- function (w_hat, r, models, colors, uncertain_size = 40, extra_skip = 0, y_max = NULL, ...) {
  names(colors) <- models
  alphas <- c(1, 2)
  
  # w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + uncertain_size), alphas[1], alphas[2])))
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 30 + uncertain_size), alphas[1], alphas[2])))
  ifelse(length(unique(w_hat$aa)) == 1, rr <- 1, rr <- c(0.25, 1))
  pp <- w_hat %>% 
    ggplot(aes(fill = as.factor(model), x = forecast_date)) +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") +
    # geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)) - 0.5, linetype = "dashed") + 
    # geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)  + extra_skip) - 0.5, linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r[1] + 1 + 30 + uncertain_size - 0.5), linetype = "dashed") + 
    { if (!is.null(y_max)) geom_hline(yintercept = 1, linetype = "dashed")  } +
    scale_fill_manual("Models", values = colors) +
    scale_alpha_manual(values = rr, guide = "none") +
    scale_x_date(limit = c((r[1] + 1), max(w_hat$forecast_date))) + 
    labs(x = "Forecast date", y = "Weights (Averaged over horizons and quantiles)") + 
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 14, family = "LM Roman 10")) +
    { if (!is.null(y_max)) scale_y_continuous(breaks = seq(0, y_max, length.out = 6), limits = c(0, y_max)) } +
    { if ( is.null(y_max)) scale_y_continuous() } 
  
  pp
}

##################################################
##################################################
##################################################

plotting_quantile_weights <- function (w_hat, r, models, colors, uncertain_size = 40, extra_skip = 0, y_max = NULL, ...) {
  names(colors) <- models
  
  alphas <- c(1, 2)
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 40), alphas[1], alphas[2])))
  if (length(unique(w_hat$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
  w_hat$quant <- factor(w_hat$quant)
  
  pp <- ggplot(w_hat, aes(fill = as.factor(model), x = forecast_date)) +
    facet_wrap("quant", scales = "fixed", ncol = 1) +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") + # c(fill, stack)
    geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)) - 0.5, linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)  + extra_skip) - 0.5, linetype = "dashed") + 
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
    { if (!is.null(y_max)) scale_y_continuous(breaks = seq(0, y_max, length.out = 6), limits = c(0, y_max)) } +
    { if ( is.null(y_max)) scale_y_continuous() } 
  
  pp
}


plotting_quantile_weights_v2 <- function (w_hat, r, models, colors, uncertain_size = 40, extra_skip = 0, y_max = NULL, sub_tt = "", ...) {
  names(colors) <- models
  
  alphas <- c(1, 2)
  # w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 40), alphas[1], alphas[2])))
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 30 + uncertain_size), alphas[1], alphas[2])))
  if (length(unique(w_hat$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
  w_hat$quant <- factor(w_hat$quant)
  
  pp <- ggplot(w_hat, aes(fill = as.factor(model), x = forecast_date)) +
    facet_wrap("quant", scales = "fixed", ncol = 3) +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") + # c(fill, stack)
    # geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)) - 0.5, linetype = "dashed") + 
    # geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)  + extra_skip) - 0.5, linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r[1] + 1 + 30 + uncertain_size - 0.5), linetype = "dashed") + 
    { if (!is.null(y_max)) geom_hline(yintercept = 1, linetype = "dashed")  } +
    scale_fill_manual("Models ", values = colors) +
    scale_alpha_manual(values = af, guide = "none") +
    scale_x_date(limit = c((r[1] + 1), max(w_hat$forecast_date) + 1)) + 
    labs(x = "", y = paste("Weights", sub_tt, sep = "")) + 
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          strip.text = element_text(size = 16, margin = margin(b = 2, t = 2)),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.ticks = element_line(colour = "black", linewidth = 0.25),
          panel.grid.major = element_line(linewidth = 0.15),
          panel.grid.minor = element_line(linewidth = 0.1),
          plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
          legend.margin = margin(0, 0.5, 0, 5),
          legend.box.spacing = unit(0, "pt"),
          legend.background = element_rect(fill = "transparent"),
          text = element_text(size = 16, family = "LM Roman 10")) +
    { if (!is.null(y_max)) scale_y_continuous(breaks = seq(0, y_max, length.out = 6), limits = c(0, y_max)) } +
    { if ( is.null(y_max)) scale_y_continuous() } +
    guides(fill = guide_legend(nrow = 2))
  
  pp
}

##################################################
##################################################
##################################################

plotting_horizon_weights <- function (w_hat, r, models, colors, uncertain_size = 40, hhs = c(-28, -23, -18, -13, -8, -3, 0), big_title = "", extra_skip = 0, y_max = NULL, ...) {
  names(colors) <- models
  
  alphas <- c(1, 2)
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 40), alphas[1], alphas[2])))
  if (length(unique(w_hat$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
  
  w_hat <- w_hat |> filter(horizon %in% hhs)
  w_hat$horizon <- factor(x = w_hat$horizon, levels = hhs)
  
  pp <- ggplot(w_hat, aes(fill = as.factor(model), x = forecast_date)) +
    facet_wrap("horizon", scales = "fixed", nrow = ceiling(length(hhs) / 2)) +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") +
    geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)) - 0.5, linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)  + extra_skip) - 0.5, linetype = "dashed") + 
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
    { if (!is.null(y_max)) scale_y_continuous(breaks = seq(0, y_max, length.out = 6), limits = c(0, y_max)) } +
    { if ( is.null(y_max)) scale_y_continuous() } 
  
  pp
}

plotting_horizon_weights_v2 <- function (w_hat, r, models, colors, uncertain_size = 40, hhs = c(-28, -24, -20, -16, -12, -8, -4, 0), big_title = "", extra_skip = 0, y_max = NULL, all_same = FALSE, ...) {
  names(colors) <- models
  
  alphas <- c(1, 2)
  w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 30 + uncertain_size), alphas[1], alphas[2])))
  if (length(unique(w_hat$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
  
  w_hat <- w_hat |> filter(horizon %in% hhs)
  w_hat$horizon <- factor(x = w_hat$horizon, levels = hhs)
  
  if (all_same) {
    w_hat <- w_hat[w_hat$horizon == hhs[1],]
    leg_pos <- "right"
  } else {
    leg_pos <- "bottom"
  }

  pp <- ggplot(w_hat, aes(fill = as.factor(model), x = forecast_date)) +
    { if (!all_same) facet_wrap("horizon", scales = "fixed", ncol = 3) } +
    geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") +
    # geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)) - 0.5, linetype = "dashed") + 
    # geom_vline(xintercept = as.numeric(r[1] + (uncertain_size + 1)  + extra_skip) - 0.5, linetype = "dashed") + 
    geom_vline(xintercept = as.numeric(r[1] + 1 + 30 + uncertain_size - 0.5), linetype = "dashed") + 
    { if (!is.null(y_max)) geom_hline(yintercept = 1, linetype = "dashed") } +
    scale_fill_manual("Models", values = colors) +
    scale_alpha_manual(values = af, guide = "none") +
    scale_x_date(limit = c((r[1] + 1), max(w_hat$forecast_date) + 1)) + 
    labs(x = "", y = "Weights (Averaged over quantiles)", title = big_title) + 
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          legend.position = leg_pos,
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
    { if (!is.null(y_max)) scale_y_continuous(breaks = seq(0, y_max, length.out = 6), limits = c(0, y_max)) } +
    { if ( is.null(y_max)) scale_y_continuous() }
  
  pp
}

##################################################
##################################################
##################################################

plot_coverage <- function (coverage_models, models, colors, reference_pts_50 = NULL, reference_pts_95 = NULL, change_name_select = FALSE, skip_space = FALSE, skip_first = 1:8, skip_last = 9:10, ...) {
  
  if (!is.null(reference_pts_50) & !is.null(reference_pts_95)) {
    reference_pts <- data.frame(model = models, c50 = reference_pts_50, c95 = reference_pts_95)
    reference_pts$model <- factor(x = models, levels = models, labels = models)
  }
 
  alphas <- setNames(c(0.75, 0.5), c("50%", "95%"))
  
  df_coverage <- as.data.frame(matrix(0, nrow = length(models) * 2, ncol = 4))
  colnames(df_coverage) <- c("model", "interval", "value", "alpha_v")
  i = 1
  for (m in 1:length(models)) {
    df_coverage[i, ] <- c(models[m], 0.50, coverage_models$coverage_50[[as.character(models[m])]], "50%")
    i <- i + 1
  }
  for (m in 1:length(models)) {
    df_coverage[i, ] <- c(models[m], 0.95, coverage_models$coverage_95[[as.character(models[m])]], "95%")
    i <- i + 1
  }
  df_coverage$model    <- factor(df_coverage$model, levels = models)
  df_coverage$interval <- as.numeric(df_coverage$interval)
  df_coverage$value    <- as.numeric(df_coverage$value)
  df_coverage$alpha_v  <- factor(df_coverage$alpha_v, levels = names(alphas))
  
  if (skip_space) {
    df_coverage$model <- as.character(df_coverage$model)
    df_coverage_1 <- df_coverage[1:(nrow(df_coverage) / 2), ]
    df_coverage_2 <- df_coverage[((nrow(df_coverage) / 2) + 1):nrow(df_coverage), ]
    df_coverage_1 <- rbind(df_coverage_1, c("space", "0.50", 0, "50%"))
    df_coverage_2 <- rbind(df_coverage_2, c("space", "0.95", 0, "95%"))
    df_coverage_1$model <- factor(x = df_coverage_1$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_coverage_2$model <- factor(x = df_coverage_2$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_coverage <- rbind(df_coverage_1, df_coverage_2)
    
    df_coverage$model <- factor(x = df_coverage$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_coverage$value <- as.numeric(df_coverage$value)
  }
  
  if (change_name_select) { 
    models <- c("Mean", "Median", "Post-Mean", "Post-Median", "Mean-Post", "Median-Post", "DISW 1", "DISW 2", "DISW 3", "DISW 4", "AISW 1", "AISW 2", "AISW 3", "AISW 4", "Select-4 Mean 2", "Select-4 Median 2")
  }
  
  if (skip_space) {
    colors_ordered <- c(colors[skip_first], "cyan", colors[skip_last])
    names(colors_ordered) <- c(models[skip_first], "space", models[skip_last])
  } else {
    colors_ordered <- colors
    names(colors_ordered) <- models
  }
  
  colors_ordered <- colors
  names(colors_ordered) <- models
  
  if (change_name_select) {
    df_coverage <- df_coverage %>% mutate(model = as.character(model)) %>% mutate(model = ifelse(model == "Select-4 Mean",   "Select-4 Mean 2",   model)) %>% mutate(model = as.factor(model))
    df_coverage <- df_coverage %>% mutate(model = as.character(model)) %>% mutate(model = ifelse(model == "Select-4 Median", "Select-4 Median 2", model)) %>% mutate(model = as.factor(model))
  }
  
  pp <- ggplot() + 
    geom_col(data = df_coverage[df_coverage$interval == 0.95, ], aes(x = model, y = value, fill = model, alpha = alpha_v)) +
    geom_col(data = df_coverage[df_coverage$interval == 0.50, ], aes(x = model, y = value, fill = model, alpha = alpha_v)) +
    { if (!is.null(reference_pts_50)) geom_point(data = reference_pts, aes(x = model, y = c50), pch = 16, size = 3, color = "black", alpha = alphas["50%"]) } +
    { if (!is.null(reference_pts_95)) geom_point(data = reference_pts, aes(x = model, y = c95), pch = 16, size = 3, color = "black", alpha = alphas["95%"]) } +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    scale_fill_manual(values = colors_ordered, guide = "none") +
    scale_color_manual(values = colors_ordered, guide = "none") +
    scale_alpha_manual(values = alphas, labels = names(alphas), guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)) +
    { if ( skip_space) scale_x_discrete(limits = rev(c(models[skip_first], "space", models[skip_last])), labels = rev(c(models[skip_first], " ", models[skip_last])), drop = FALSE) } +
    { if (!skip_space) scale_x_discrete(limits = rev(models), drop = FALSE) } +
    labs(x = NULL, y = "Empirical coverage (Averaged all)", color = "Model", alpha = "Prediction interval") +
    ylim(c(0, 1)) +
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

plot_coverage_stratified <- function (coverage_models, models, colors, reference_pts_50 = NULL, reference_pts_95 = NULL, ...) {
  
  nm <- length(models)
  
  if (idx_missing_model == 2) { models <- c(models[1], "ILM",     models[2:nm]) } else if (idx_missing_model == 6) { models <- c(models[1:5], "RKI",     models[6:nm])  }
  if (idx_missing_model == 2) { colors <- c(colors[1], "#E69F00", colors[2:nm]) } else if (idx_missing_model == 6) { colors <- c(colors[1:5], "#3C4AAD", colors[6:nm])  }

  
  if (!is.null(reference_pts_50) & !is.null(reference_pts_95)) {
    reference_pts_50 <- c(reference_pts_50[1:(idx_missing_model - 1)], NA, reference_pts_50[idx_missing_model:nm])
    reference_pts_95 <- c(reference_pts_95[1:(idx_missing_model - 1)], NA, reference_pts_95[idx_missing_model:nm])
    
    reference_pts <- data.frame(model = models, c50 = reference_pts_50, c95 = reference_pts_95)
    reference_pts$model <- factor(x = models, levels = models, labels = models)
  }
  
  colors_ordered <- colors
  names(colors_ordered) <- models
  
  names(colors_ordered)[idx_missing_model] <- "space"
  colors_ordered[idx_missing_model] <- "cyan"
  
  alphas <- setNames(c(0.75, 0.5), c("50%", "95%"))
  
  df_coverage <- as.data.frame(matrix(0, nrow = length(models) * 2, ncol = 4))
  colnames(df_coverage) <- c("model", "interval", "value", "alpha_v")
  i = 1
  for (m in 1:length(models)) {
    tmp_obj <- coverage_models$coverage_50[[as.character(models[m])]]
    if (is.null(tmp_obj)) { tmp_obj <- 0 }
    
    df_coverage[i, ] <- c(models[m], 0.50, tmp_obj, "50%")
    i <- i + 1
  }
  for (m in 1:length(models)) {
    tmp_obj <- coverage_models$coverage_95[[as.character(models[m])]]
    if (is.null(tmp_obj)) { tmp_obj <- 0 }
    
    df_coverage[i, ] <- c(models[m], 0.95, tmp_obj, "95%")
    i <- i + 1
  }

  df_coverage$model    <- factor(df_coverage$model, levels = models)
  df_coverage$interval <- as.numeric(df_coverage$interval)
  df_coverage$value    <- as.numeric(df_coverage$value)
  df_coverage$alpha_v  <- factor(df_coverage$alpha_v, levels = names(alphas))
  
  
  pp <- ggplot() + 
    geom_col(data = df_coverage[df_coverage$interval == 0.95, ], aes(x = model, y = value, fill = model, alpha = alpha_v)) +
    geom_col(data = df_coverage[df_coverage$interval == 0.50, ], aes(x = model, y = value, fill = model, alpha = alpha_v)) +
    { if (!is.null(reference_pts_50)) geom_point(data = reference_pts, aes(x = model, y = c50), pch = 16, size = 3, color = "black", alpha = alphas["50%"]) } +
    { if (!is.null(reference_pts_95)) geom_point(data = reference_pts, aes(x = model, y = c95), pch = 16, size = 3, color = "black", alpha = alphas["95%"]) } +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    scale_fill_manual(values = colors_ordered, guide = "none") +
    scale_color_manual(values = colors_ordered, guide = "none") +
    scale_alpha_manual(values = alphas, labels = names(alphas), guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)) +
    scale_x_discrete(limits = rev(models), drop = FALSE) + 
    labs(x = NULL, y = "Empirical coverage (Averaged all)", color = "Model", alpha = "Prediction interval") +
    ylim(c(0, 1)) +
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

plot_coverage_ensemble <- function (coverage_models, models, colors, reference_pts_50 = NULL, reference_pts_95 = NULL, skip_space = FALSE, skip_first = 1:8, skip_last = 9:10, ...) {

  # colors_ordered <- colors
  # names(colors_ordered) <- models
  
  alphas <- setNames(c(0.75, 0.5), c("50%", "95%"))
  
  df_coverage <- as.data.frame(matrix(0, nrow = length(models) * 2, ncol = 4))
  colnames(df_coverage) <- c("model", "interval", "value", "alpha_v")
  i = 1
  for (m in 1:length(models)) {
    tmp_obj <- coverage_models$coverage_50[[as.character(models[m])]]
    if (is.null(tmp_obj)) { tmp_obj <- 0 }
    
    df_coverage[i, ] <- c(models[m], 0.50, tmp_obj, "50%")
    i <- i + 1
  }
  for (m in 1:length(models)) {
    tmp_obj <- coverage_models$coverage_95[[as.character(models[m])]]
    if (is.null(tmp_obj)) { tmp_obj <- 0 }
    
    df_coverage[i, ] <- c(models[m], 0.95, tmp_obj, "95%")
    i <- i + 1
  }
  
  df_coverage$model    <- factor(df_coverage$model, levels = models)
  df_coverage$interval <- as.numeric(df_coverage$interval)
  df_coverage$value    <- as.numeric(df_coverage$value)
  df_coverage$alpha_v  <- factor(df_coverage$alpha_v, levels = names(alphas))
  
  if (skip_space) {
    df_coverage$model <- as.character(df_coverage$model)
    df_coverage_1 <- df_coverage[1:(nrow(df_coverage) / 2), ]
    df_coverage_2 <- df_coverage[((nrow(df_coverage) / 2) + 1):nrow(df_coverage), ]
    df_coverage_1 <- rbind(df_coverage_1, c("space", "0.50", 0, "50%"))
    df_coverage_2 <- rbind(df_coverage_2, c("space", "0.95", 0, "95%"))
    df_coverage_1$model <- factor(x = df_coverage_1$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_coverage_2$model <- factor(x = df_coverage_2$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_coverage <- rbind(df_coverage_1, df_coverage_2)
    
    df_coverage$model <- factor(x = df_coverage$model, levels = c(models[skip_first], "space", models[skip_last]), labels = c(models[skip_first], "space", models[skip_last]))
    df_coverage$value <- as.numeric(df_coverage$value)
  }
  
  if (skip_space) {
    colors_ordered <- c(colors[skip_first], "cyan", colors[skip_last])
    names(colors_ordered) <- c(models[skip_first], "space", models[skip_last])
  } else {
    colors_ordered <- colors
    names(colors_ordered) <- models
  }
  
  
  pp <- ggplot() + 
    geom_col(data = df_coverage[df_coverage$interval == 0.95, ], aes(x = model, y = value, fill = model, alpha = alpha_v)) +
    geom_col(data = df_coverage[df_coverage$interval == 0.50, ], aes(x = model, y = value, fill = model, alpha = alpha_v)) +
    { if (!is.null(reference_pts_50)) geom_point(data = reference_pts, aes(x = model, y = c50), pch = 16, size = 3, color = "black", alpha = alphas["50%"]) } +
    { if (!is.null(reference_pts_95)) geom_point(data = reference_pts, aes(x = model, y = c95), pch = 16, size = 3, color = "black", alpha = alphas["95%"]) } +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    scale_fill_manual(values = colors_ordered, guide = "none") +
    scale_color_manual(values = colors_ordered, guide = "none") +
    scale_alpha_manual(values = alphas, labels = names(alphas), guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)) +
    #scale_x_discrete(limits = rev(models), drop = FALSE) + 
    { if ( skip_space) scale_x_discrete(limits = rev(c(models[skip_first], "space", models[skip_last])), labels = rev(c(models[skip_first], " ", models[skip_last])), drop = FALSE) } +
    { if (!skip_space) scale_x_discrete(limits = rev(models), drop = FALSE) } +
    labs(x = NULL, y = "Empirical coverage (Averaged all)", color = "Model", alpha = "Prediction interval") +
    ylim(c(0, 1)) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16, family = "LM Roman 10"), 
          axis.ticks.y = element_blank()
    )
  
  pp
}








