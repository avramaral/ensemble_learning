suppressMessages(library("tidyverse"))
suppressMessages(library("patchwork"))
DATES <- c("2022-02-01", "2022-03-01", "2022-04-01")

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ", "Mean ensemble", "Median ensemble") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#D7CB3B", "#80471C", "#3C4AAD", "#CC79A7", "#000000", "#009E73", "#60D1B3")

data_demo <- readRDS(file = "DATA/data_demo.RDS")

df_nowcast   <- data_demo$df_nowcast
df_truth     <- data_demo$df_truth
df_unrevised <- df_truth |> filter(as_of == "Unrevised, \nInitial reports")
df_final     <- df_truth |> filter(as_of == "2022-06-09") # last day + 40
df_truth     <- df_truth |> filter(as_of != "Unrevised, \nInitial reports", as_of != "2022-12-31")

r <- range(df_unrevised$date)

truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")
truth_data <- truth_data |> filter(location == "DE", age_group == "00+", date >= r[1], date <= r[2]) 

plot_data <- function (i, m, cc = c("#000000", "red", "#999999"), ...) {
  pos_model <- which(m == models)
  
  ALPHAS <- setNames(c(0.75, 0.4), c("50%", "95%"))
  COLORS <- setNames(cc, c(DATES[i], "2022-06-09", "Unrevised"))
  lw <- 0.5
  
  pp <- ggplot() +
    geom_line(data = df_final, aes(x = date, y = truth, color = "2022-06-09") , linewidth = lw) +
    geom_line(data = df_truth[df_truth$as_of == as.Date(DATES[i]), ], aes(x = date, y = truth, color = DATES[i]), linewidth = lw) +
    geom_line(data = df_unrevised, aes(x = date, y = truth, color = "Unrevised"), linewidth = lw) +
    geom_line(data = df_nowcast[[pos_model]][df_nowcast[[pos_model]]$as_of == as.Date(DATES[i]), ], aes(x = target_end_date, y = quantile_0.5, group = as_of), color = colors[pos_model], linewidth = lw) +
    geom_ribbon(
      data = df_nowcast[[pos_model]][df_nowcast[[pos_model]]$as_of == as.Date(DATES[i]), ],
      aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, group = as_of, alpha = "50%"),
      fill = colors[pos_model]
    ) +
    geom_ribbon(
      data = df_nowcast[[pos_model]][df_nowcast[[pos_model]]$as_of == as.Date(DATES[i]), ],
      aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, group = as_of, alpha = "95%"),
      fill = colors[pos_model]
    ) +
    geom_vline(xintercept = as.Date(DATES[i]), linetype = "dashed") +
    scale_alpha_manual(name = paste("Nowcasts with prediction \nintervals (", m, ")", sep = ""), values = ALPHAS, guide = guide_legend(title.position = "top", order = 1)) +
    scale_color_manual(name = paste("Data \nversion", sep = ""), values = COLORS, guide = guide_legend(title.position = "top")) + 
    xlim(r) +
    # ylim(c(2500, 13000)) +
    scale_y_continuous(breaks = seq(0, 12500, 2500), limits = c(0, 13000)) + 
    labs(x = "", y = "") + # y = "COVID-19 7-day hospitalization incidence in Germany") +
    theme_bw() +
    theme(legend.position = "bottom", legend.direction = "vertical", legend.box = "horizontal",
          text = element_text(size = 14, family = "LM Roman 10"),
          axis.title.y = element_text(hjust = 1))
  
  pp
}

ll <- ggplot() + geom_text(aes(1, 1, label = "7-day hospitalization incidence"), angle = 90, family = "LM Roman 10", size = 6) + theme_void()



p_1 <- plot_data(i = 1, m = "KIT")
p_2 <- plot_data(i = 2, m = "LMU")
p_3 <- plot_data(i = 3, m = "Mean ensemble")

p_total <- ll + p_1 + p_2 + p_3 + plot_layout(widths = c(1, 20, 20, 20))
ggsave(filename = paste("PLOTS/initial_plot.jpeg", sep = ""), plot = p_total, width = 4600, height = 1620, units = c("px"), dpi = 300, bg = "white") 

  