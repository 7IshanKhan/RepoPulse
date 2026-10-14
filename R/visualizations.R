# ggplot2 visualisations. Each function returns a plot object; print it to display
# in RStudio, or pass it to ggplot2::ggsave() to save it.

.require_ggplot2 <- function() check_repopulse_dependencies("plots")

plot_arrivals <- function(counts, fit = NULL) {
  .require_ggplot2()
  p <- ggplot2::ggplot(counts, ggplot2::aes(x = bin_start, y = count)) +
    ggplot2::geom_col(fill = "#4169A1", width = 0.85) +
    ggplot2::labs(title = "Issue arrivals over time", x = "Time bin", y = "New issues") +
    ggplot2::theme_minimal(base_size = 12)
  if (!is.null(fit)) {
    chosen <- if (!is.null(fit$selected)) fit$selected else fit
    label <- sprintf("Selected baseline: %s, mean %.2f per bin", chosen$type, chosen$mu)
    p <- p + ggplot2::labs(subtitle = label)
  }
  p
}

plot_backlog_history <- function(history) {
  .require_ggplot2()
  ggplot2::ggplot(history, ggplot2::aes(x = date, y = estimated_backlog)) +
    ggplot2::geom_line(linewidth = 0.8, colour = "#A34A28") +
    ggplot2::labs(
      title = "Reconstructed backlog from available issue records",
      subtitle = "This is a record-based reconstruction, not guaranteed complete historical truth.",
      x = "Date", y = "Estimated open issues"
    ) + ggplot2::theme_minimal(base_size = 12)
}

plot_survival_curves <- function(survival_fit, max_days = NULL) {
  .require_ggplot2()
  km <- survival_fit$km
  max_time <- if (is.null(max_days)) {
    max(c(km$time, survival_fit$observations$duration_days), na.rm = TRUE)
  } else as.numeric(max_days)
  if (!is.finite(max_time) || max_time <= 0) max_time <- 1
  if (nrow(km)) {
    km_data <- rbind(
      data.frame(time = 0, survival = 1, lower = 1, upper = 1, model = "Kaplan-Meier"),
      data.frame(time = km$time, survival = km$survival, lower = km$lower, upper = km$upper, model = "Kaplan-Meier")
    )
  } else {
    km_data <- data.frame(time = 0, survival = 1, lower = 1, upper = 1, model = "Kaplan-Meier")
  }
  plot <- ggplot2::ggplot(km_data, ggplot2::aes(x = time, y = survival)) +
    ggplot2::geom_step(linewidth = 0.9, colour = "#222222") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.12, inherit.aes = TRUE) +
    ggplot2::labs(title = "Issue resolution survival", x = "Days since issue creation", y = "Probability not yet resolved") +
    ggplot2::coord_cartesian(xlim = c(0, max_time), ylim = c(0, 1)) + ggplot2::theme_minimal(base_size = 12)

  grid <- seq(0, max_time, length.out = 150L)
  models <- survival_fit$models
  for (name in c("exponential", "weibull")) {
    model <- models[[name]]
    if (identical(model$status, "ok")) {
      line_data <- data.frame(time = grid, survival = predict_survival(model, grid), model = name)
      plot <- plot + ggplot2::geom_line(
        data = line_data,
        ggplot2::aes(x = time, y = survival, linetype = model),
        linewidth = 0.8, inherit.aes = FALSE
      )
    }
  }
  plot + ggplot2::labs(linetype = "Curve")
}

plot_forecast <- function(forecast) {
  .require_ggplot2()
  d <- forecast$daily_summary
  ggplot2::ggplot(d, ggplot2::aes(x = day, y = mean_backlog)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#6A9C78", alpha = 0.22) +
    ggplot2::geom_line(linewidth = 1, colour = "#28613B") +
    ggplot2::geom_line(ggplot2::aes(y = median_backlog), linewidth = 0.65, linetype = "dashed", colour = "#6C7D41") +
    ggplot2::labs(
      title = "Monte Carlo backlog forecast",
      subtitle = sprintf("Expected backlog with %.0f%% simulation interval", forecast$summary$confidence_level * 100),
      x = "Days ahead", y = "Open issue backlog",
      caption = "Band shows the central forecast interval across simulated paths, not parameter uncertainty."
    ) + ggplot2::theme_minimal(base_size = 12)
}
