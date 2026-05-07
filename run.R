source("install_packages.R")
source("import_datasets.R")
source("prepare_data.R")
source("task.R")
source("data_split.R")
source("modeling.R")
source("performance.R")
source("correlation_metrics.R")
source("global_explain.R")
source("vimp_analysis.R")
source("plots.R")
source("analyses.R")



# ALL DATASETS — COMBINE PLOT DATA

all_plot_data <- rbind(
  grace_plot_data, heart_plot_data, melanoma_plot_data, ova_plot_data,
  aids2_plot_data, veteran_plot_data, breast_plot_data, e1684_plot_data,
  epileptic_plot_data, dialysis_plot_data, pbc_plot_data,
  prostateSurvival_plot_data, acath_plot_data, prostate_plot_data,
  dataDIVAT1_plot_data, dataDIVAT3_plot_data, TRACE_plot_data,
  FRTCS_plot_data, zinc_plot_data, stagec_plot_data, LeukSurv_plot_data,
  nwtco_plot_data, retinopathy_plot_data, Framingham_plot_data,
  csl_plot_data, cancer_plot_data, whas500_plot_data, cgd_plot_data,
  Pbc3_plot_data, GBSG2_plot_data, colon_plot_data, mgus_plot_data,
  flchain_plot_data, Rossi_plot_data, burn_plot_data, rott2_plot_data,
  d_oropha_rec_plot_data, rdata_plot_data
)


# EPSILON SENSITIVITY ANALYSIS

eps_values <- c(0, 0.001, 0.003, 0.005, 0.01)

datasets <- list(
  grace            = list(vimp = grace_global$vimp_list,            perf = list(rsf = grace_perf_rsf,            cforest = grace_perf_cforest,            blackboost = grace_perf_blackboost,            cox = grace_perf_cox,            xgboost = grace_perf_xgboost),            data = grace_data),
  melanoma         = list(vimp = melanoma_global$vimp_list,         perf = list(rsf = melanoma_perf_rsf,         cforest = melanoma_perf_cforest,         blackboost = melanoma_perf_blackboost,         cox = melanoma_perf_cox,         xgboost = melanoma_perf_xgboost),         data = melanoma_data),
  ova              = list(vimp = ova_global$vimp_list,              perf = list(rsf = ova_perf_rsf,              cforest = ova_perf_cforest,              blackboost = ova_perf_blackboost,              cox = ova_perf_cox,              xgboost = ova_perf_xgboost),              data = ova_data),
  aids2            = list(vimp = aids2_global$vimp_list,            perf = list(rsf = aids2_perf_rsf,            cforest = aids2_perf_cforest,            blackboost = aids2_perf_blackboost,            cox = aids2_perf_cox,            xgboost = aids2_perf_xgboost),            data = aids2_data),
  veteran          = list(vimp = veteran_global$vimp_list,          perf = list(rsf = veteran_perf_rsf,          cforest = veteran_perf_cforest,          blackboost = veteran_perf_blackboost,          cox = veteran_perf_cox,          xgboost = veteran_perf_xgboost),          data = veteran_data),
  breast           = list(vimp = breast_global$vimp_list,           perf = list(rsf = breast_perf_rsf,           cforest = breast_perf_cforest,           blackboost = breast_perf_blackboost,           cox = breast_perf_cox,           xgboost = breast_perf_xgboost),           data = breast_data),
  e1684            = list(vimp = e1684_global$vimp_list,            perf = list(rsf = e1684_perf_rsf,            cforest = e1684_perf_cforest,            blackboost = e1684_perf_blackboost,            cox = e1684_perf_cox,            xgboost = e1684_perf_xgboost),            data = e1684_data),
  epileptic        = list(vimp = epileptic_global$vimp_list,        perf = list(rsf = epileptic_perf_rsf,        cforest = epileptic_perf_cforest,        blackboost = epileptic_perf_blackboost,        cox = epileptic_perf_cox,        xgboost = epileptic_perf_xgboost),        data = epileptic_data),
  dialysis         = list(vimp = dialysis_global$vimp_list,         perf = list(rsf = dialysis_perf_rsf,         cforest = dialysis_perf_cforest,         blackboost = dialysis_perf_blackboost,         cox = dialysis_perf_cox,         xgboost = dialysis_perf_xgboost),         data = dialysis_data),
  pbc              = list(vimp = pbc_global$vimp_list,              perf = list(rsf = pbc_perf_rsf,              cforest = pbc_perf_cforest,              blackboost = pbc_perf_blackboost,              cox = pbc_perf_cox,              xgboost = pbc_perf_xgboost),              data = pbc_data),
  prostateSurvival = list(vimp = prostateSurvival_global$vimp_list, perf = list(rsf = prostateSurvival_perf_rsf, cforest = prostateSurvival_perf_cforest, blackboost = prostateSurvival_perf_blackboost, cox = prostateSurvival_perf_cox, xgboost = prostateSurvival_perf_xgboost), data = prostateSurvival_data),
  acath            = list(vimp = acath_global$vimp_list,            perf = list(rsf = acath_perf_rsf,            cforest = acath_perf_cforest,            blackboost = acath_perf_blackboost,            cox = acath_perf_cox,            xgboost = acath_perf_xgboost),            data = acath_data),
  prostate         = list(vimp = prostate_global$vimp_list,         perf = list(rsf = prostate_perf_rsf,         cforest = prostate_perf_cforest,         blackboost = prostate_perf_blackboost,         cox = prostate_perf_cox,         xgboost = prostate_perf_xgboost),         data = prostate_data),
  dataDIVAT1       = list(vimp = dataDIVAT1_global$vimp_list,       perf = list(rsf = dataDIVAT1_perf_rsf,       cforest = dataDIVAT1_perf_cforest,       blackboost = dataDIVAT1_perf_blackboost,       cox = dataDIVAT1_perf_cox,       xgboost = dataDIVAT1_perf_xgboost),       data = dataDIVAT1_data),
  dataDIVAT3       = list(vimp = dataDIVAT3_global$vimp_list,       perf = list(rsf = dataDIVAT3_perf_rsf,       cforest = dataDIVAT3_perf_cforest,       blackboost = dataDIVAT3_perf_blackboost,       cox = dataDIVAT3_perf_cox,       xgboost = dataDIVAT3_perf_xgboost),       data = dataDIVAT3_data),
  TRACE            = list(vimp = TRACE_global$vimp_list,            perf = list(rsf = TRACE_perf_rsf,            cforest = TRACE_perf_cforest,            blackboost = TRACE_perf_blackboost,            cox = TRACE_perf_cox,            xgboost = TRACE_perf_xgboost),            data = TRACE_data),
  FRTCS            = list(vimp = FRTCS_global$vimp_list,            perf = list(rsf = FRTCS_perf_rsf,            cforest = FRTCS_perf_cforest,            blackboost = FRTCS_perf_blackboost,            cox = FRTCS_perf_cox,            xgboost = FRTCS_perf_xgboost),            data = FRTCS_data),
  zinc             = list(vimp = zinc_global$vimp_list,             perf = list(rsf = zinc_perf_rsf,             cforest = zinc_perf_cforest,             blackboost = zinc_perf_blackboost,             cox = zinc_perf_cox,             xgboost = zinc_perf_xgboost),             data = zinc_data),
  stagec           = list(vimp = stagec_global$vimp_list,           perf = list(rsf = stagec_perf_rsf,           cforest = stagec_perf_cforest,           blackboost = stagec_perf_blackboost,           cox = stagec_perf_cox,           xgboost = stagec_perf_xgboost),           data = stagec_data),
  LeukSurv         = list(vimp = LeukSurv_global$vimp_list,         perf = list(rsf = LeukSurv_perf_rsf,         cforest = LeukSurv_perf_cforest,         blackboost = LeukSurv_perf_blackboost,         cox = LeukSurv_perf_cox,         xgboost = LeukSurv_perf_xgboost),         data = LeukSurv_data),
  nwtco            = list(vimp = nwtco_global$vimp_list,            perf = list(rsf = nwtco_perf_rsf,            cforest = nwtco_perf_cforest,            blackboost = nwtco_perf_blackboost,            cox = nwtco_perf_cox,            xgboost = nwtco_perf_xgboost),            data = nwtco_data),
  retinopathy      = list(vimp = retinopathy_global$vimp_list,      perf = list(rsf = retinopathy_perf_rsf,      cforest = retinopathy_perf_cforest,      blackboost = retinopathy_perf_blackboost,      cox = retinopathy_perf_cox,      xgboost = retinopathy_perf_xgboost),      data = retinopathy_data),
  Framingham       = list(vimp = Framingham_global$vimp_list,       perf = list(rsf = Framingham_perf_rsf,       cforest = Framingham_perf_cforest,       blackboost = Framingham_perf_blackboost,       cox = Framingham_perf_cox,       xgboost = Framingham_perf_xgboost),       data = Framingham_data),
  csl              = list(vimp = csl_global$vimp_list,              perf = list(rsf = csl_perf_rsf,              cforest = csl_perf_cforest,              blackboost = csl_perf_blackboost,              cox = csl_perf_cox,              xgboost = csl_perf_xgboost),              data = csl_data),
  cancer           = list(vimp = cancer_global$vimp_list,           perf = list(rsf = cancer_perf_rsf,           cforest = cancer_perf_cforest,           blackboost = cancer_perf_blackboost,           cox = cancer_perf_cox,           xgboost = cancer_perf_xgboost),           data = cancer_data),
  whas500          = list(vimp = whas500_global$vimp_list,          perf = list(rsf = whas500_perf_rsf,          cforest = whas500_perf_cforest,          blackboost = whas500_perf_blackboost,          cox = whas500_perf_cox,          xgboost = whas500_perf_xgboost),          data = whas500_data),
  cgd              = list(vimp = cgd_global$vimp_list,              perf = list(rsf = cgd_perf_rsf,              cforest = cgd_perf_cforest,              blackboost = cgd_perf_blackboost,              cox = cgd_perf_cox,              xgboost = cgd_perf_xgboost),              data = cgd_data),
  Pbc3             = list(vimp = Pbc3_global$vimp_list,             perf = list(rsf = Pbc3_perf_rsf,             cforest = Pbc3_perf_cforest,             blackboost = Pbc3_perf_blackboost,             cox = Pbc3_perf_cox,             xgboost = Pbc3_perf_xgboost),             data = Pbc3_data),
  GBSG2            = list(vimp = GBSG2_global$vimp_list,            perf = list(rsf = GBSG2_perf_rsf,            cforest = GBSG2_perf_cforest,            blackboost = GBSG2_perf_blackboost,            cox = GBSG2_perf_cox,            xgboost = GBSG2_perf_xgboost),            data = GBSG2_data),
  colon            = list(vimp = colon_global$vimp_list,            perf = list(rsf = colon_perf_rsf,            cforest = colon_perf_cforest,            blackboost = colon_perf_blackboost,            cox = colon_perf_cox,            xgboost = colon_perf_xgboost),            data = colon_data),
  flchain          = list(vimp = flchain_global$vimp_list,          perf = list(rsf = flchain_perf_rsf,          cforest = flchain_perf_cforest,          blackboost = flchain_perf_blackboost,          cox = flchain_perf_cox,          xgboost = flchain_perf_xgboost),          data = flchain_data),
  Rossi            = list(vimp = Rossi_global$vimp_list,            perf = list(rsf = Rossi_perf_rsf,            cforest = Rossi_perf_cforest,            blackboost = Rossi_perf_blackboost,            cox = Rossi_perf_cox,            xgboost = Rossi_perf_xgboost),            data = Rossi_data),
  burn             = list(vimp = burn_global$vimp_list,             perf = list(rsf = burn_perf_rsf,             cforest = burn_perf_cforest,             blackboost = burn_perf_blackboost,             cox = burn_perf_cox,             xgboost = burn_perf_xgboost),             data = burn_data),
  rott2            = list(vimp = rott2_global$vimp_list,            perf = list(rsf = rott2_perf_rsf,            cforest = rott2_perf_cforest,            blackboost = rott2_perf_blackboost,            cox = rott2_perf_cox,            xgboost = rott2_perf_xgboost),            data = rott2_data),
  d_oropha_rec     = list(vimp = d_oropha_rec_global$vimp_list,     perf = list(rsf = d_oropha_rec_perf_rsf,     cforest = d_oropha_rec_perf_cforest,     blackboost = d_oropha_rec_perf_blackboost,     cox = d_oropha_rec_perf_cox,     xgboost = d_oropha_rec_perf_xgboost),     data = d_oropha_rec_data),
  rdata            = list(vimp = rdata_global$vimp_list,            perf = list(rsf = rdata_perf_rsf,            cforest = rdata_perf_cforest,            blackboost = rdata_perf_blackboost,            cox = rdata_perf_cox,            xgboost = rdata_perf_xgboost),            data = rdata_data)
)

model_map <- c(
  rsf_model        = "RSF",
  cforest_model    = "CForest",
  blackboost_model = "BlackBoost",
  cox_model        = "Cox",
  xgboost_model    = "XGBoost"
)

perf_map <- c(
  rsf_model        = "rsf",
  cforest_model    = "cforest",
  blackboost_model = "blackboost",
  cox_model        = "cox",
  xgboost_model    = "xgboost"
)

all_eps_plot_data <- lapply(eps_values, function(eps) {
  dataset_results <- lapply(names(datasets), function(ds_name) {
    ds        <- datasets[[ds_name]]
    vimp_list <- ds$vimp
    perf_list <- ds$perf
    data_df   <- ds$data
    model_results <- lapply(names(model_map), function(model_key) {
      vimp_obj <- vimp_list[[model_key]]
      if (is.null(vimp_obj)) return(NULL)
      summary_df <- tryCatch(calculate_vimp_summary(vimp_obj, eps = eps), error = function(e) NULL)
      if (is.null(summary_df)) return(NULL)
      perf_key <- perf_map[[model_key]]
      perf_obj <- perf_list[[perf_key]]
      data.frame(
        dataset          = ds_name,
        model            = model_map[[model_key]],
        eps              = eps,
        mean_instability = mean(summary_df$vimp_instability, na.rm = TRUE),
        c_index          = perf_obj$c_index,
        brier            = perf_obj$brier,
        censoring_rate   = mean(data_df$event == 0, na.rm = TRUE)
      )
    })
    dplyr::bind_rows(model_results)
  })
  dplyr::bind_rows(dataset_results)
})
all_eps_plot_data <- dplyr::bind_rows(all_eps_plot_data)


  
# EPSILON SENSITIVITY — ENCIRCLE PLOTS (all epsilon values)

p_encircle_cindex_quad <- plot_encircle_by_eps_quadrant(
  all_eps_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
) +
  ggplot2::labs(
    x     = "Variable Importance Instability",
    title = "Model Discrimination and Variable Importance Instability Across Epsilon Thresholds"
  ) +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    legend.position = "top",
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5),
    strip.text.x    = ggplot2::element_text(size = 20)
  )

p_encircle_cindex_quad

p_encircle_brier_quad <- plot_encircle_by_eps_quadrant(
  all_eps_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier Score",
  metric_type = "lower_better"
) +
  ggplot2::labs(
    x     = "Variable Importance Instability",
    title = "Model Calibration and Variable Importance Instability Across Epsilon Thresholds"
  ) +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    legend.position = "top",
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5),
    strip.text.x    = ggplot2::element_text(size = 20)
  )

p_encircle_brier_quad


  
# CORRELATION METRICS — SCATTER PLOTS (ggstatsplot)

library(ggstatsplot)

p1 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = avg_abs_cor, y = mean_instability, xlab = "", ylab = "Mean Variable Importance Instability", ggtheme = ggplot2::theme_bw(base_size = 10))
p2 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = avg_abs_cor, y = c_index,          xlab = "", ylab = "C-index (Harrell)",                  ggtheme = ggplot2::theme_bw(base_size = 10))
p3 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = avg_abs_cor, y = brier,            xlab = "Average Absolute Pairwise Correlation", ylab = "Integrated Brier Score", ggtheme = ggplot2::theme_bw(base_size = 10))
p4 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = gcd,         y = mean_instability, xlab = "", ylab = "", ggtheme = ggplot2::theme_bw(base_size = 10))
p5 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = gcd,         y = c_index,          xlab = "", ylab = "", ggtheme = ggplot2::theme_bw(base_size = 10))
p6 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = gcd,         y = brier,            xlab = "Generalised Collinearity Diagnostic (GCD)", ylab = "", ggtheme = ggplot2::theme_bw(base_size = 10))
p7 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = det_R,       y = mean_instability, xlab = "", ylab = "", ggtheme = ggplot2::theme_bw(base_size = 10))
p8 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = det_R,       y = c_index,          xlab = "", ylab = "", ggtheme = ggplot2::theme_bw(base_size = 10))
p9 <- ggstatsplot::ggscatterstats(data = all_plot_data_cor, x = det_R,       y = brier,            xlab = "Determinant of Correlation Matrix det(R)", ylab = "", ggtheme = ggplot2::theme_bw(base_size = 10))

add_xlab <- function(p, label) { p$labels$x <- label; p }

p_row1 <- patchwork::wrap_plots(
  add_xlab(p1, "Average Absolute Pairwise Correlation"),
  add_xlab(p4, "Generalised Collinearity Diagnostic (GCD)"),
  add_xlab(p7, "Determinant of Correlation Matrix det(R)"),
  ncol = 3
)

p_row2 <- patchwork::wrap_plots(
  add_xlab(p2, "Average Absolute Pairwise Correlation"),
  add_xlab(p5, "Generalised Collinearity Diagnostic (GCD)"),
  add_xlab(p8, "Determinant of Correlation Matrix det(R)"),
  ncol = 3
)

p_row3 <- patchwork::wrap_plots(
  add_xlab(p3, "Average Absolute Pairwise Correlation"),
  add_xlab(p6, "Generalised Collinearity Diagnostic (GCD)"),
  add_xlab(p9, "Determinant of Correlation Matrix det(R)"),
  ncol = 3
)

p_row1
p_row2
p_row3

  

# VARIABLE IMPORTANCE AND INSTABILITY — AIDS2 DATASET (RSF)

  
aids2_rsf_summary <- calculate_vimp_summary(aids2_rsf_vimp, eps = 0.005)

vimp_colors <- c(
  "num_age"     = "#E07B54",
  "fac_state"   = "#6DBF6D",
  "fac_T_categ" = "#2E75B6",
  "fac_sex"     = "#E05C5C"
)

aids2_rsf_p_vimp <- plot(aids2_rsf_vimp) +
  ggplot2::labs(title = NULL, subtitle = NULL, tag = NULL) +
  ggplot2::theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    axis.text       = ggplot2::element_text(size = 17),
    axis.title      = ggplot2::element_text(size = 19),
    legend.text     = ggplot2::element_text(size = 17)
  )

aids2_rsf_p_fr <- plot_vimp_with_fr(aids2_rsf_summary, var_colors = vimp_colors, base_size = 20)

aids2_rsf_combined <- cowplot::plot_grid(
  aids2_rsf_p_vimp,
  aids2_rsf_p_fr,
  ncol       = 2,
  align      = "h",
  axis       = "tb",
  rel_widths = c(1, 1)
)

aids2_rsf_combined



# AVERAGE MODEL PERFORMANCE ACROSS ALL DATASETS

all_eps_plot_data |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    mean_instability = mean(mean_instability, na.rm = TRUE),
    mean_c_index     = mean(c_index, na.rm = TRUE),
    mean_brier       = mean(brier, na.rm = TRUE)
  ) |>
  dplyr::arrange(desc(mean_instability))
