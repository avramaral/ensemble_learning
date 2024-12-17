
hh <- -1
j <- 7
qq <- probs[j]

models <- models_orig
colors <- colors_orig

w_hat <- w_hat_ISW_2 %>% filter(horizon == hh, quant == qq)
names(colors) <- models_orig

alphas <- c(1, 2)
w_hat <- w_hat |> add_column(aa = factor(ifelse(w_hat$forecast_date <= (r[1] + 40), alphas[1], alphas[2])))
if (length(unique(w_hat$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
w_hat$quant <- factor(w_hat$quant)

ggplot(w_hat, aes(fill = as.factor(model), x = forecast_date)) +
  geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") +
  geom_vline(xintercept = as.numeric(r[1] + 41) - 0.5, linetype = "dashed") + 
  scale_fill_manual("Models", values = colors) +
  scale_alpha_manual(values = af, guide = "none") +
  scale_x_date(limit = c((r[1] + 1), max(w_hat$forecast_date))) + 
  labs(x = "Forecast date", y = "Weights", title = qq) + 
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.05)) +
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
        text = element_text(size = 16, family = "LM Roman 10"))

tt <- c()
n_pts <- length(ISW_2$ensemble)
for(i in 1:n_pts) {
  tt <- c(tt, ISW_2$ensemble[[i]][[as.character(hh)]]$theta[which(probs == qq)])
}
plot(tt, type = "l", ylim = c(-5, 5), main = probs[j])
abline(v = 40, col = 2)

