w_hat_generic <- w_hat_ISW_2

hh <- -27
qq <- c(0.025, 0.5, 0.975)

partial_w_hat <- list()
partial_w_hat$new_w_hat <- list()

count <- 1
for (q in qq) {
  partial_w_hat$new_w_hat[[count]] <- w_hat_ISW_1 %>% filter(horizon == as.character(hh), quant == q)
  
  count <- count + 1
}

partial_w_hat$new_w_hat_total <- rbind(partial_w_hat$new_w_hat[[1]], partial_w_hat$new_w_hat[[2]], partial_w_hat$new_w_hat[[3]]) %>% select(c("forecast_date", "model", "value", "quant"))

names(colors_orig) <- models_orig

alphas <- c(1, 2)
w_hat_plot <- partial_w_hat$new_w_hat_total
w_hat_plot <- w_hat_plot |> add_column(aa = factor(ifelse(w_hat_plot$forecast_date <= (r[1] + 40 - 1), alphas[1], alphas[2])))
if (length(unique(w_hat_plot$aa)) == 1) { af <- 1 } else { af <- c(0.25, 1) }
w_hat_plot$quant <- factor(w_hat_plot$quant)

ggplot(w_hat_plot, aes(fill = as.factor(model), x = forecast_date)) +
  facet_wrap("quant", scales = "fixed", ncol = 1) +
  geom_bar(aes(y = value, alpha = aa), position = "stack", stat = "identity") + # c(fill, stack)
  geom_vline(xintercept = as.numeric(r[1] + 41 - 1) - 0.5, linetype = "dashed") + 
  scale_fill_manual("Models", values = colors_orig) +
  scale_alpha_manual(values = af, guide = "none") +
  scale_x_date(limit = c((r[1] + 1), max(w_hat_plot$forecast_date))) + 
  labs(x = "Forecast date", y = "Weights (Common for all horizons)") + 
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


ens <- ISW_2_ens

probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

dd <- names(ens)
thetas <- matrix(data = 0, nrow = length(dd), ncol = length(probs))
count <- 1
for (d in dd) {
  thetas[count, ] <- ens[[as.character(d)]]$theta
  count <- count + 1
}
thetas

matplot(thetas, type = "l", lty = 1, col = 2:8, xlab = "Days", ylab = "Theta", lwd = 2, ylim = c(-10, 10))
abline(v = 41, lwd = 2, lty = 2)
legend("bottomright", col = 2:8, legend = probs, lty = 1, lwd = 2)

##########

dd <- names(ens)
phis <- matrix(data = 0, nrow = length(dd), ncol = length(probs))
count <- 1
for (d in dd) {
  phis[count, ] <- ens[[as.character(d)]]$phi
  count <- count + 1
}
phis

matplot(phis, type = "l", lty = 1, col = 2:8, xlab = "Days", ylab = "Phis", lwd = 2, ylim = c(0.5, 1.5))
abline(v = 41, lwd = 2, lty = 2)
abline(h = 0.75, lwd = 2, lty = 2)
abline(h = 1.25, lwd = 2, lty = 2)
legend("bottomright", col = 2:8, legend = probs, lty = 1, lwd = 2)
