source("header.R")
source("utils.R")
source("aux.R")
library("ggh4x") 

training_size <- 90 
uncertain_size <- 40

quant <- TRUE 
horiz <- TRUE

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

KIT_frozen_baseline <- data %>% filter(model == "KIT-frozen_baseline")

state <- "DE"
age   <- "00+"

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 

filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

r <- range(data$forecast_date)

horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

KIT_frozen_baseline <- KIT_frozen_baseline %>% filter(forecast_date >= r[1], forecast_date <= r[2], age_group %in% age, location %in% state)
baseline <- KIT_frozen_baseline %>% filter(location == "DE", age_group == "00+", quantile == 0.5) %>% select(target_end_date, target, value)
baseline_00 <- baseline %>% filter(target ==   "0 day ahead inc hosp") %>% select(target_end_date, value)
baseline_14 <- baseline %>% filter(target == "-14 day ahead inc hosp") %>% select(target_end_date, value)

if (FALSE) {
  tmp_name <- "_also_ensemble"
  he <- 2800
  
  naive_ensemble_file <- paste("DATA/UNTRAINED_ENSEMBLE/naive_ensemble_state_", state, "_age_", age, ".RDS", sep = "")
  naive_ensemble <- readRDS(file = naive_ensemble_file)

  data <- rbind(data, naive_ensemble)
  models <- c(models, "Mean", "Median")
  colors <- c(colors, "#009E73", "#60D1B3")
} else { tmp_name <- ""; he <- 1850 }

data <- data |> filter(!is.na(quantile), target %in% c("0 day ahead inc hosp", "-14 day ahead inc hosp"))
data <- data |> pivot_wider(names_from = quantile, values_from = value)
data$model  <- factor(data$model,  levels = models)
data$target <- factor(data$target, levels = c("0 day ahead inc hosp", "-14 day ahead inc hosp"))

plot_nowcast <- function (data, truth_data, hh, baseline_tmp, ...) {
  
  alphas <- setNames(c(0.75, 0.4), c("50%", "95%"))
  line_colors <- setNames(c("red", "lightgray"), c("Final", "At time of nowcast"))
  
  x_dates <- as.Date(c("2021-11-15", "2022-04-29"))
  
  ggplot(data) +
    facet_wrap(~model, scales = "fixed", ncol = 4) +
    geom_ribbon(aes(x = target_end_date, ymin = `0.025`, ymax = `0.975`, alpha = "95%"), fill = "skyblue3") +
    geom_ribbon(aes(x = target_end_date, ymin = `0.25` , ymax = `0.75`, alpha = "50%"), fill = "skyblue3") +
    geom_line(aes(x = target_end_date, y = `0.5`), linetype = "solid", linewidth = 0.5, color = "royalblue4") + 
    geom_line(data = truth_data, aes(x = date, y = truth, color = "Final"),  linetype = "solid", linewidth = 0.5)  + 
    geom_line(data = baseline_tmp, aes(x = target_end_date, y = value, color = "At time of nowcast"),  linetype = "solid", linewidth = 0.5)  + 
    labs(x = NULL, y = "", title = paste("\nHorizon: ", hh, " days", sep = "")) + # y = "COVID-19 7-day hospitalization incidence in Germany") + 
    scale_alpha_manual(
      name = "Nowcasts with \nprediction intervals", values = alphas,
      guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
    ) +
    scale_color_manual(
      name = "Truth", values = line_colors,
      guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)
    ) +
    scale_y_continuous(breaks = seq(0, 15000, 5000), limits = c(0, 17500)) +
    xlim(x_dates) + 
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.65, "lines"),
      strip.text = element_text(size = 12, margin = margin(b = 2, t = 2)),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.ticks = element_line(colour = "black", linewidth = 0.25),
      panel.grid.major = element_line(linewidth = 0.15),
      panel.grid.minor = element_line(linewidth = 0.1),
      plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
      legend.margin = margin(0, 0, 0, 5),
      legend.box.spacing = unit(0, "pt"),
      legend.background = element_rect(fill = "transparent"),
      text = element_text(family = "LM Roman 10"),
    ) 
}


truth_data_1 <- truth_data |> filter(date >= "2021-11-29", date <= "2022-04-29")
truth_data_2 <- truth_data |> filter(date >= "2021-11-15", date <= "2022-04-15")

ll <- ggplot() + geom_text(aes(1, 1, label = "COVID-19 7-day hospitalization incidence in Germany"), angle = 90, family = "LM Roman 10", size = 5.5) + theme_void()
p_total <- ll + (plot_nowcast(data[data$target ==   "0 day ahead inc hosp", ], truth_data_1,   "0", baseline_00) / 
                 plot_nowcast(data[data$target == "-14 day ahead inc hosp", ], truth_data_2, "-14", baseline_14)) +  
           plot_annotation(theme = theme(plot.margin = margin())) + 
           plot_layout(widths = c(1, 20), guides = "collect") & theme(legend.position = "right")
ggsave(filename = paste("PLOTS/nowcast_per_horizon", tmp_name, ".jpeg", sep = ""), plot = p_total, width = 3500, height = he, units = c("px"), dpi = 300, bg = "white") 
