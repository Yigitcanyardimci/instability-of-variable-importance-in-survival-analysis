# =============================================================================
# VIMP + INSTABILITY 
# =============================================================================

plot_vimp_with_fr <- function(summary_df) {
  
  plot_df <- summary_df %>%
    dplyr::mutate(
      relative_importance = normalized_vimp_area / sum(normalized_vimp_area, na.rm = TRUE),
      percentage = relative_importance * 100
    ) %>%
    dplyr::arrange(relative_importance) %>%
    dplyr::mutate(
      variable = factor(variable, levels = variable),
      y_pos = 1
    )
  
  p1 <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = relative_importance, y = variable, fill = variable)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(percentage, 1), "%")),
      hjust = -0.1, size = 3.5
    ) +
    ggplot2::labs(x = "Relative Importance", y = "Variable", title = "Variable Importance and Instability") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::expand_limits(x = max(plot_df$relative_importance, na.rm = TRUE) * 1.15)
  
  p2 <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = vimp_instability, y = y_pos, color = variable)
  ) +
    ggplot2::geom_hline(yintercept = 1, linewidth = 0.8, color = "gray70") +
    ggplot2::geom_point(size = 4) +
    ggplot2::labs(x = "Instability", y = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position      = "none",
      axis.text.y          = element_blank(),
      axis.ticks.y         = element_blank(),
      panel.grid.minor     = element_blank(),
      panel.grid.major.y   = element_blank()
    )
  
  p1 / p2 + patchwork::plot_layout(heights = c(3, 1.2))
}

# =============================================================================
# 3D PLOT - C-INDEX
# =============================================================================

plot_3d_cindex <- function(results_df) {
  
  plotly::plot_ly(
    data = results_df,
    x = ~mean_instability,
    y = ~c_index,
    z = ~censoring_rate,
    type = "scatter3d",
    mode = "markers+text",
    text = ~model,
    textposition = "top center",
    marker = list(
      size = 10,
      color = "#534AB7",
      showscale = FALSE
    ),
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "Instability: %{x:.3f}<br>",
      "C-index: %{y:.3f}<br>",
      "Censoring rate: %{z:.1%}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Instability", range = c(0, 1)),
        yaxis = list(title = "C-index", range = c(0, 1)),
        zaxis = list(
          title = "Censoring rate",
          range = c(0, 1),
          tickformat = ".0%"
        ),
        camera = list(eye = list(x = 1.4, y = 1.4, z = 1.2))
      ),
      title = list(
        text = "Model comparison: instability vs C-index vs censoring rate",
        font = list(size = 13)
      )
    )
}

# =============================================================================
# 3D PLOT - BRIER
# =============================================================================

plot_3d_brier <- function(results_df) {
  
  plotly::plot_ly(
    data = results_df,
    x = ~mean_instability,
    y = ~brier,
    z = ~censoring_rate,
    type = "scatter3d",
    mode = "markers+text",
    text = ~model,
    textposition = "top center",
    marker = list(
      size = 10,
      color = "#D85A30",
      showscale = FALSE
    ),
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "Instability: %{x:.3f}<br>",
      "Brier score: %{y:.3f}<br>",
      "Censoring rate: %{z:.1%}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Instability", range = c(0, 1)),
        yaxis = list(title = "Brier score", range = c(0, 0.5)),
        zaxis = list(
          title = "Censoring rate",
          range = c(0, 1),
          tickformat = ".0%"
        ),
        camera = list(eye = list(x = 1.4, y = 1.4, z = 1.2))
      ),
      title = list(
        text = "Model comparison: instability vs Brier score vs censoring rate",
        font = list(size = 13)
      )
    )
}

# =============================================================================
# QUADRANT PLOT
# =============================================================================

plot_quadrant <- function(results_df,
                          perf_col    = "c_index",
                          perf_label  = "C-index",
                          metric_type = c("higher_better", "lower_better"),
                          x_mid       = 0.5,
                          y_mid       = 0.5) {
  
  metric_type <- match.arg(metric_type)
  
  df <- results_df %>%
    dplyr::rename(perf = dplyr::all_of(perf_col))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = mean_instability, y = perf))
  
  if (metric_type == "higher_better") {
    p <- p +
      ggplot2::annotate("rect", xmin = 0,    xmax = x_mid, ymin = y_mid, ymax = 1,    fill = "#1D9E75", alpha = 0.07) +
      ggplot2::annotate("rect", xmin = x_mid, xmax = 1,    ymin = y_mid, ymax = 1,    fill = "#F4A261", alpha = 0.07) +
      ggplot2::annotate("rect", xmin = 0,    xmax = x_mid, ymin = 0,    ymax = y_mid, fill = "#5DADE2", alpha = 0.07) +
      ggplot2::annotate("rect", xmin = x_mid, xmax = 1,    ymin = 0,    ymax = y_mid, fill = "#E74C3C", alpha = 0.07)
  }
  
  if (metric_type == "lower_better") {
    p <- p +
      ggplot2::annotate("rect", xmin = 0,    xmax = x_mid, ymin = 0,    ymax = y_mid, fill = "#1D9E75", alpha = 0.07) +
      ggplot2::annotate("rect", xmin = x_mid, xmax = 1,    ymin = 0,    ymax = y_mid, fill = "#F4A261", alpha = 0.07) +
      ggplot2::annotate("rect", xmin = 0,    xmax = x_mid, ymin = y_mid, ymax = 1,    fill = "#5DADE2", alpha = 0.07) +
      ggplot2::annotate("rect", xmin = x_mid, xmax = 1,    ymin = y_mid, ymax = 1,    fill = "#E74C3C", alpha = 0.07)
  }
  
  p +
    ggplot2::geom_vline(xintercept = x_mid, linetype = "dashed", color = "gray50", linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = y_mid, linetype = "dashed", color = "gray50", linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(fill = perf), shape = 21, size = 5, color = "white", stroke = 1.5) +
    ggplot2::scale_fill_gradient(low = "#D85A30", high = "#1D9E75") +
    ggrepel::geom_text_repel(ggplot2::aes(label = model), size = 3.5, fontface = "bold", color = "gray20", box.padding = 0.4, max.overlaps = 20) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(x = "Instability", y = perf_label, title = paste0("Model comparison: ", perf_label, " vs instability")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 13),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "none"
    )
}

