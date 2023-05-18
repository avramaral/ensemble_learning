
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