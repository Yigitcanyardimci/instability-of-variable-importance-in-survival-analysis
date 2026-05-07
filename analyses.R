# =============================================================================
# GRACE ANALİZİ
# =============================================================================

grace_data   <- prepare_grace(grace_raw)
grace_task   <- make_task_grace(grace_data)
grace_split  <- data_split(grace_task)
grace_models <- train_models(grace_task, grace_split)


# PERFORMANS

grace_perf_rsf <- measure_performance(grace_models$rsf_model, grace_task, grace_split)
grace_perf_rsf$c_index
grace_perf_rsf$brier
grace_perf_rsf$auc

grace_perf_cforest <- measure_performance(grace_models$cforest_model, grace_task, grace_split)
grace_perf_cforest$c_index
grace_perf_cforest$brier
grace_perf_cforest$auc

grace_perf_blackboost <- measure_performance(grace_models$blackboost_model, grace_task, grace_split)
grace_perf_blackboost$c_index
grace_perf_blackboost$brier
grace_perf_blackboost$auc

grace_perf_cox <- measure_performance(grace_models$cox_model, grace_task, grace_split)
grace_perf_cox$c_index
grace_perf_cox$brier
grace_perf_cox$auc

grace_perf_xgboost <- measure_performance(grace_models$xgboost_model, grace_task, grace_split)
grace_perf_xgboost$c_index
grace_perf_xgboost$brier
grace_perf_xgboost$auc


# GLOBAL AÇIKLAMA

grace_global <- global_explain(grace_task, grace_models)


# RSF

grace_rsf_vimp <- grace_global$vimp_list$rsf_model
grace_rsf_p_vimp <- plot(grace_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
grace_rsf_p_vimp

grace_rsf_summary <- calculate_vimp_summary(grace_rsf_vimp, eps = 0)
grace_rsf_summary

grace_rsf_p_fr <- plot_vimp_with_fr(grace_rsf_summary)
grace_rsf_p_fr

grace_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(grace_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = grace_perf_rsf$c_index,
  brier            = grace_perf_rsf$brier,
  censoring_rate   = mean(grace_data$event == 0, na.rm = TRUE)
)


# CFOREST

grace_cforest_vimp <- grace_global$vimp_list$cforest_model
grace_cforest_p_vimp <- plot(grace_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
grace_cforest_p_vimp

grace_cforest_summary <- calculate_vimp_summary(grace_cforest_vimp, eps = 0)
grace_cforest_summary

grace_cforest_p_fr <- plot_vimp_with_fr(grace_cforest_summary)
grace_cforest_p_fr

grace_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(grace_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = grace_perf_cforest$c_index,
  brier            = grace_perf_cforest$brier,
  censoring_rate   = mean(grace_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

grace_blackboost_vimp <- grace_global$vimp_list$blackboost_model
grace_blackboost_p_vimp <- plot(grace_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
grace_blackboost_p_vimp

grace_blackboost_summary <- calculate_vimp_summary(grace_blackboost_vimp, eps = 0)
grace_blackboost_summary

grace_blackboost_p_fr <- plot_vimp_with_fr(grace_blackboost_summary)
grace_blackboost_p_fr

grace_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(grace_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = grace_perf_blackboost$c_index,
  brier            = grace_perf_blackboost$brier,
  censoring_rate   = mean(grace_data$event == 0, na.rm = TRUE)
)


# COX

grace_cox_vimp <- grace_global$vimp_list$cox_model
grace_cox_p_vimp <- plot(grace_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
grace_cox_p_vimp

grace_cox_summary <- calculate_vimp_summary(grace_cox_vimp, eps = 0)
grace_cox_summary

grace_cox_p_fr <- plot_vimp_with_fr(grace_cox_summary)
grace_cox_p_fr

grace_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(grace_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = grace_perf_cox$c_index,
  brier            = grace_perf_cox$brier,
  censoring_rate   = mean(grace_data$event == 0, na.rm = TRUE)
)


# XGBOOST

grace_xgboost_vimp <- grace_global$vimp_list$xgboost_model
grace_xgboost_p_vimp <- plot(grace_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
grace_xgboost_p_vimp

grace_xgboost_summary <- calculate_vimp_summary(grace_xgboost_vimp, eps = 0)
grace_xgboost_summary

grace_xgboost_p_fr <- plot_vimp_with_fr(grace_xgboost_summary)
grace_xgboost_p_fr

grace_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(grace_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = grace_perf_xgboost$c_index,
  brier            = grace_perf_xgboost$brier,
  censoring_rate   = mean(grace_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

grace_plot_data <- rbind(
  grace_rsf_plot_data,
  grace_cforest_plot_data,
  grace_blackboost_plot_data,
  grace_cox_plot_data,
  grace_xgboost_plot_data
)


# 3D GRAFİKLER

grace_p_3d_cindex <- plot_3d_cindex(grace_plot_data)
grace_p_3d_cindex

grace_p_3d_brier <- plot_3d_brier(grace_plot_data)
grace_p_3d_brier


# QUADRANT GRAFİKLER

grace_p_quad_cindex <- plot_quadrant(
  grace_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
grace_p_quad_cindex

grace_p_quad_brier <- plot_quadrant(
  grace_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
grace_p_quad_brier



# =============================================================================
# HEART ANALİZİ
# =============================================================================

heart_data   <- prepare_heart(heart_raw)
heart_task   <- make_task_heart(heart_data)
heart_split  <- data_split(heart_task)
heart_models <- train_models(heart_task, heart_split)


# PERFORMANS

heart_perf_rsf <- measure_performance(heart_models$rsf_model, heart_task, heart_split)
heart_perf_rsf$c_index
heart_perf_rsf$brier
heart_perf_rsf$auc

heart_perf_cforest <- measure_performance(heart_models$cforest_model, heart_task, heart_split)
heart_perf_cforest$c_index
heart_perf_cforest$brier
heart_perf_cforest$auc

heart_perf_blackboost <- measure_performance(heart_models$blackboost_model, heart_task, heart_split)
heart_perf_blackboost$c_index
heart_perf_blackboost$brier
heart_perf_blackboost$auc

heart_perf_cox <- measure_performance(heart_models$cox_model, heart_task, heart_split)
heart_perf_cox$c_index
heart_perf_cox$brier
heart_perf_cox$auc

heart_perf_xgboost <- measure_performance(heart_models$xgboost_model, heart_task, heart_split)
heart_perf_xgboost$c_index
heart_perf_xgboost$brier
heart_perf_xgboost$auc


# GLOBAL AÇIKLAMA

heart_global <- global_explain(heart_task, heart_models)


# RSF

heart_rsf_vimp <- heart_global$vimp_list$rsf_model
heart_rsf_p_vimp <- plot(heart_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_rsf_p_vimp

heart_rsf_summary <- calculate_vimp_summary(heart_rsf_vimp, eps = 0)
heart_rsf_summary

heart_rsf_p_fr <- plot_vimp_with_fr(heart_rsf_summary)
heart_rsf_p_fr

heart_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(heart_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_rsf$c_index,
  brier            = heart_perf_rsf$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# CFOREST

heart_cforest_vimp <- heart_global$vimp_list$cforest_model
heart_cforest_p_vimp <- plot(heart_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_cforest_p_vimp

heart_cforest_summary <- calculate_vimp_summary(heart_cforest_vimp, eps = 0)
heart_cforest_summary

heart_cforest_p_fr <- plot_vimp_with_fr(heart_cforest_summary)
heart_cforest_p_fr

heart_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(heart_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_cforest$c_index,
  brier            = heart_perf_cforest$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

heart_blackboost_vimp <- heart_global$vimp_list$blackboost_model
heart_blackboost_p_vimp <- plot(heart_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_blackboost_p_vimp

heart_blackboost_summary <- calculate_vimp_summary(heart_blackboost_vimp, eps = 0)
heart_blackboost_summary

heart_blackboost_p_fr <- plot_vimp_with_fr(heart_blackboost_summary)
heart_blackboost_p_fr

heart_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(heart_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_blackboost$c_index,
  brier            = heart_perf_blackboost$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# COX

heart_cox_vimp <- heart_global$vimp_list$cox_model
heart_cox_p_vimp <- plot(heart_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_cox_p_vimp

heart_cox_summary <- calculate_vimp_summary(heart_cox_vimp, eps = 0)
heart_cox_summary

heart_cox_p_fr <- plot_vimp_with_fr(heart_cox_summary)
heart_cox_p_fr

heart_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(heart_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_cox$c_index,
  brier            = heart_perf_cox$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# XGBOOST

heart_xgboost_vimp <- heart_global$vimp_list$xgboost_model
heart_xgboost_p_vimp <- plot(heart_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_xgboost_p_vimp

heart_xgboost_summary <- calculate_vimp_summary(heart_xgboost_vimp, eps = 0)
heart_xgboost_summary

heart_xgboost_p_fr <- plot_vimp_with_fr(heart_xgboost_summary)
heart_xgboost_p_fr

heart_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(heart_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_xgboost$c_index,
  brier            = heart_perf_xgboost$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

heart_plot_data <- rbind(
  heart_rsf_plot_data,
  heart_cforest_plot_data,
  heart_blackboost_plot_data,
  heart_cox_plot_data,
  heart_xgboost_plot_data
)


# 3D GRAFİKLER

heart_p_3d_cindex <- plot_3d_cindex(heart_plot_data)
heart_p_3d_cindex

heart_p_3d_brier <- plot_3d_brier(heart_plot_data)
heart_p_3d_brier


# QUADRANT GRAFİKLER

heart_p_quad_cindex <- plot_quadrant(
  heart_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
heart_p_quad_cindex

heart_p_quad_brier <- plot_quadrant(
  heart_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
heart_p_quad_brier




# =============================================================================
# MELANOMA ANALİZİ
# =============================================================================

melanoma_data   <- prepare_melanoma(melanoma_raw)
melanoma_task   <- make_task_melanoma(melanoma_data)
melanoma_split  <- data_split(melanoma_task)
melanoma_models <- train_models(melanoma_task, melanoma_split)


# PERFORMANS

melanoma_perf_rsf <- measure_performance(melanoma_models$rsf_model, melanoma_task, melanoma_split)
melanoma_perf_rsf$c_index
melanoma_perf_rsf$brier
melanoma_perf_rsf$auc

melanoma_perf_cforest <- measure_performance(melanoma_models$cforest_model, melanoma_task, melanoma_split)
melanoma_perf_cforest$c_index
melanoma_perf_cforest$brier
melanoma_perf_cforest$auc

melanoma_perf_blackboost <- measure_performance(melanoma_models$blackboost_model, melanoma_task, melanoma_split)
melanoma_perf_blackboost$c_index
melanoma_perf_blackboost$brier
melanoma_perf_blackboost$auc

melanoma_perf_cox <- measure_performance(melanoma_models$cox_model, melanoma_task, melanoma_split)
melanoma_perf_cox$c_index
melanoma_perf_cox$brier
melanoma_perf_cox$auc

melanoma_perf_xgboost <- measure_performance(melanoma_models$xgboost_model, melanoma_task, melanoma_split)
melanoma_perf_xgboost$c_index
melanoma_perf_xgboost$brier
melanoma_perf_xgboost$auc


# GLOBAL AÇIKLAMA

melanoma_global <- global_explain(melanoma_task, melanoma_models)


# RSF

melanoma_rsf_vimp <- melanoma_global$vimp_list$rsf_model
melanoma_rsf_p_vimp <- plot(melanoma_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
melanoma_rsf_p_vimp

melanoma_rsf_summary <- calculate_vimp_summary(melanoma_rsf_vimp, eps = 0)
melanoma_rsf_summary

melanoma_rsf_p_fr <- plot_vimp_with_fr(melanoma_rsf_summary)
melanoma_rsf_p_fr

melanoma_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(melanoma_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = melanoma_perf_rsf$c_index,
  brier            = melanoma_perf_rsf$brier,
  censoring_rate   = mean(melanoma_data$event == 0, na.rm = TRUE)
)


# CFOREST

melanoma_cforest_vimp <- melanoma_global$vimp_list$cforest_model
melanoma_cforest_p_vimp <- plot(melanoma_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
melanoma_cforest_p_vimp

melanoma_cforest_summary <- calculate_vimp_summary(melanoma_cforest_vimp, eps = 0)
melanoma_cforest_summary

melanoma_cforest_p_fr <- plot_vimp_with_fr(melanoma_cforest_summary)
melanoma_cforest_p_fr

melanoma_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(melanoma_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = melanoma_perf_cforest$c_index,
  brier            = melanoma_perf_cforest$brier,
  censoring_rate   = mean(melanoma_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

melanoma_blackboost_vimp <- melanoma_global$vimp_list$blackboost_model
melanoma_blackboost_p_vimp <- plot(melanoma_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
melanoma_blackboost_p_vimp

melanoma_blackboost_summary <- calculate_vimp_summary(melanoma_blackboost_vimp, eps = 0)
melanoma_blackboost_summary

melanoma_blackboost_p_fr <- plot_vimp_with_fr(melanoma_blackboost_summary)
melanoma_blackboost_p_fr

melanoma_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(melanoma_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = melanoma_perf_blackboost$c_index,
  brier            = melanoma_perf_blackboost$brier,
  censoring_rate   = mean(melanoma_data$event == 0, na.rm = TRUE)
)


# COX

melanoma_cox_vimp <- melanoma_global$vimp_list$cox_model
melanoma_cox_p_vimp <- plot(melanoma_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
melanoma_cox_p_vimp

melanoma_cox_summary <- calculate_vimp_summary(melanoma_cox_vimp, eps = 0)
melanoma_cox_summary

melanoma_cox_p_fr <- plot_vimp_with_fr(melanoma_cox_summary)
melanoma_cox_p_fr

melanoma_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(melanoma_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = melanoma_perf_cox$c_index,
  brier            = melanoma_perf_cox$brier,
  censoring_rate   = mean(melanoma_data$event == 0, na.rm = TRUE)
)


# XGBOOST

melanoma_xgboost_vimp <- melanoma_global$vimp_list$xgboost_model
melanoma_xgboost_p_vimp <- plot(melanoma_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
melanoma_xgboost_p_vimp

melanoma_xgboost_summary <- calculate_vimp_summary(melanoma_xgboost_vimp, eps = 0)
melanoma_xgboost_summary

melanoma_xgboost_p_fr <- plot_vimp_with_fr(melanoma_xgboost_summary)
melanoma_xgboost_p_fr

melanoma_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(melanoma_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = melanoma_perf_xgboost$c_index,
  brier            = melanoma_perf_xgboost$brier,
  censoring_rate   = mean(melanoma_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

melanoma_plot_data <- rbind(
  melanoma_rsf_plot_data,
  melanoma_cforest_plot_data,
  melanoma_blackboost_plot_data,
  melanoma_cox_plot_data,
  melanoma_xgboost_plot_data
)


# 3D GRAFİKLER

melanoma_p_3d_cindex <- plot_3d_cindex(melanoma_plot_data)
melanoma_p_3d_cindex

melanoma_p_3d_brier <- plot_3d_brier(melanoma_plot_data)
melanoma_p_3d_brier


# QUADRANT GRAFİKLER

melanoma_p_quad_cindex <- plot_quadrant(
  melanoma_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
melanoma_p_quad_cindex

melanoma_p_quad_brier <- plot_quadrant(
  melanoma_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
melanoma_p_quad_brier





# =============================================================================
# OVA ANALİZİ
# =============================================================================

ova_data   <- prepare_ova(ova_raw)
ova_task   <- make_task_ova(ova_data)
ova_split  <- data_split(ova_task)
ova_models <- train_models(ova_task, ova_split)


# PERFORMANS

ova_perf_rsf <- measure_performance(ova_models$rsf_model, ova_task, ova_split)
ova_perf_rsf$c_index
ova_perf_rsf$brier
ova_perf_rsf$auc

ova_perf_cforest <- measure_performance(ova_models$cforest_model, ova_task, ova_split)
ova_perf_cforest$c_index
ova_perf_cforest$brier
ova_perf_cforest$auc

ova_perf_blackboost <- measure_performance(ova_models$blackboost_model, ova_task, ova_split)
ova_perf_blackboost$c_index
ova_perf_blackboost$brier
ova_perf_blackboost$auc

ova_perf_cox <- measure_performance(ova_models$cox_model, ova_task, ova_split)
ova_perf_cox$c_index
ova_perf_cox$brier
ova_perf_cox$auc

ova_perf_xgboost <- measure_performance(ova_models$xgboost_model, ova_task, ova_split)
ova_perf_xgboost$c_index
ova_perf_xgboost$brier
ova_perf_xgboost$auc


# GLOBAL AÇIKLAMA

ova_global <- global_explain(ova_task, ova_models)


# RSF

ova_rsf_vimp <- ova_global$vimp_list$rsf_model
ova_rsf_p_vimp <- plot(ova_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
ova_rsf_p_vimp

ova_rsf_summary <- calculate_vimp_summary(ova_rsf_vimp, eps = 0)
ova_rsf_summary

ova_rsf_p_fr <- plot_vimp_with_fr(ova_rsf_summary)
ova_rsf_p_fr

ova_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(ova_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = ova_perf_rsf$c_index,
  brier            = ova_perf_rsf$brier,
  censoring_rate   = mean(ova_data$event == 0, na.rm = TRUE)
)


# CFOREST

ova_cforest_vimp <- ova_global$vimp_list$cforest_model
ova_cforest_p_vimp <- plot(ova_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
ova_cforest_p_vimp

ova_cforest_summary <- calculate_vimp_summary(ova_cforest_vimp, eps = 0)
ova_cforest_summary

ova_cforest_p_fr <- plot_vimp_with_fr(ova_cforest_summary)
ova_cforest_p_fr

ova_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(ova_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = ova_perf_cforest$c_index,
  brier            = ova_perf_cforest$brier,
  censoring_rate   = mean(ova_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

ova_blackboost_vimp <- ova_global$vimp_list$blackboost_model
ova_blackboost_p_vimp <- plot(ova_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
ova_blackboost_p_vimp

ova_blackboost_summary <- calculate_vimp_summary(ova_blackboost_vimp, eps = 0)
ova_blackboost_summary

ova_blackboost_p_fr <- plot_vimp_with_fr(ova_blackboost_summary)
ova_blackboost_p_fr

ova_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(ova_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = ova_perf_blackboost$c_index,
  brier            = ova_perf_blackboost$brier,
  censoring_rate   = mean(ova_data$event == 0, na.rm = TRUE)
)


# COX

ova_cox_vimp <- ova_global$vimp_list$cox_model
ova_cox_p_vimp <- plot(ova_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
ova_cox_p_vimp

ova_cox_summary <- calculate_vimp_summary(ova_cox_vimp, eps = 0)
ova_cox_summary

ova_cox_p_fr <- plot_vimp_with_fr(ova_cox_summary)
ova_cox_p_fr

ova_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(ova_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = ova_perf_cox$c_index,
  brier            = ova_perf_cox$brier,
  censoring_rate   = mean(ova_data$event == 0, na.rm = TRUE)
)


# XGBOOST

ova_xgboost_vimp <- ova_global$vimp_list$xgboost_model
ova_xgboost_p_vimp <- plot(ova_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
ova_xgboost_p_vimp

ova_xgboost_summary <- calculate_vimp_summary(ova_xgboost_vimp, eps = 0)
ova_xgboost_summary

ova_xgboost_p_fr <- plot_vimp_with_fr(ova_xgboost_summary)
ova_xgboost_p_fr

ova_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(ova_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = ova_perf_xgboost$c_index,
  brier            = ova_perf_xgboost$brier,
  censoring_rate   = mean(ova_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

ova_plot_data <- rbind(
  ova_rsf_plot_data,
  ova_cforest_plot_data,
  ova_blackboost_plot_data,
  ova_cox_plot_data,
  ova_xgboost_plot_data
)


# 3D GRAFİKLER

ova_p_3d_cindex <- plot_3d_cindex(ova_plot_data)
ova_p_3d_cindex

ova_p_3d_brier <- plot_3d_brier(ova_plot_data)
ova_p_3d_brier


# QUADRANT GRAFİKLER

ova_p_quad_cindex <- plot_quadrant(
  ova_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
ova_p_quad_cindex

ova_p_quad_brier <- plot_quadrant(
  ova_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
ova_p_quad_brier


# =============================================================================
# AIDS2 ANALİZİ
# =============================================================================

aids2_data   <- prepare_aids2(aids2_raw)
aids2_task   <- make_task_aids2(aids2_data)
aids2_split  <- data_split(aids2_task)
aids2_models <- train_models(aids2_task, aids2_split)


# PERFORMANS

aids2_perf_rsf <- measure_performance(aids2_models$rsf_model, aids2_task, aids2_split)
aids2_perf_rsf$c_index
aids2_perf_rsf$brier
aids2_perf_rsf$auc

aids2_perf_cforest <- measure_performance(aids2_models$cforest_model, aids2_task, aids2_split)
aids2_perf_cforest$c_index
aids2_perf_cforest$brier
aids2_perf_cforest$auc

aids2_perf_blackboost <- measure_performance(aids2_models$blackboost_model, aids2_task, aids2_split)
aids2_perf_blackboost$c_index
aids2_perf_blackboost$brier
aids2_perf_blackboost$auc

aids2_perf_cox <- measure_performance(aids2_models$cox_model, aids2_task, aids2_split)
aids2_perf_cox$c_index
aids2_perf_cox$brier
aids2_perf_cox$auc

aids2_perf_xgboost <- measure_performance(aids2_models$xgboost_model, aids2_task, aids2_split)
aids2_perf_xgboost$c_index
aids2_perf_xgboost$brier
aids2_perf_xgboost$auc


# GLOBAL AÇIKLAMA

aids2_global <- global_explain(aids2_task, aids2_models)


# RSF

aids2_rsf_vimp <- aids2_global$vimp_list$rsf_model
aids2_rsf_p_vimp <- plot(aids2_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
aids2_rsf_p_vimp

aids2_rsf_summary <- calculate_vimp_summary(aids2_rsf_vimp, eps = 0)
aids2_rsf_summary

aids2_rsf_p_fr <- plot_vimp_with_fr(aids2_rsf_summary)
aids2_rsf_p_fr

aids2_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(aids2_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = aids2_perf_rsf$c_index,
  brier            = aids2_perf_rsf$brier,
  censoring_rate   = mean(aids2_data$event == 0, na.rm = TRUE)
)


# CFOREST

aids2_cforest_vimp <- aids2_global$vimp_list$cforest_model
aids2_cforest_p_vimp <- plot(aids2_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
aids2_cforest_p_vimp

aids2_cforest_summary <- calculate_vimp_summary(aids2_cforest_vimp, eps = 0)
aids2_cforest_summary

aids2_cforest_p_fr <- plot_vimp_with_fr(aids2_cforest_summary)
aids2_cforest_p_fr

aids2_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(aids2_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = aids2_perf_cforest$c_index,
  brier            = aids2_perf_cforest$brier,
  censoring_rate   = mean(aids2_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

aids2_blackboost_vimp <- aids2_global$vimp_list$blackboost_model
aids2_blackboost_p_vimp <- plot(aids2_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
aids2_blackboost_p_vimp

aids2_blackboost_summary <- calculate_vimp_summary(aids2_blackboost_vimp, eps = 0)
aids2_blackboost_summary

aids2_blackboost_p_fr <- plot_vimp_with_fr(aids2_blackboost_summary)
aids2_blackboost_p_fr

aids2_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(aids2_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = aids2_perf_blackboost$c_index,
  brier            = aids2_perf_blackboost$brier,
  censoring_rate   = mean(aids2_data$event == 0, na.rm = TRUE)
)


# COX

aids2_cox_vimp <- aids2_global$vimp_list$cox_model
aids2_cox_p_vimp <- plot(aids2_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
aids2_cox_p_vimp

aids2_cox_summary <- calculate_vimp_summary(aids2_cox_vimp, eps = 0)
aids2_cox_summary

aids2_cox_p_fr <- plot_vimp_with_fr(aids2_cox_summary)
aids2_cox_p_fr

aids2_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(aids2_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = aids2_perf_cox$c_index,
  brier            = aids2_perf_cox$brier,
  censoring_rate   = mean(aids2_data$event == 0, na.rm = TRUE)
)


# XGBOOST

aids2_xgboost_vimp <- aids2_global$vimp_list$xgboost_model
aids2_xgboost_p_vimp <- plot(aids2_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
aids2_xgboost_p_vimp

aids2_xgboost_summary <- calculate_vimp_summary(aids2_xgboost_vimp, eps = 0)
aids2_xgboost_summary

aids2_xgboost_p_fr <- plot_vimp_with_fr(aids2_xgboost_summary)
aids2_xgboost_p_fr

aids2_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(aids2_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = aids2_perf_xgboost$c_index,
  brier            = aids2_perf_xgboost$brier,
  censoring_rate   = mean(aids2_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

aids2_plot_data <- rbind(
  aids2_rsf_plot_data,
  aids2_cforest_plot_data,
  aids2_blackboost_plot_data,
  aids2_cox_plot_data,
  aids2_xgboost_plot_data
)


# 3D GRAFİKLER

aids2_p_3d_cindex <- plot_3d_cindex(aids2_plot_data)
aids2_p_3d_cindex

aids2_p_3d_brier <- plot_3d_brier(aids2_plot_data)
aids2_p_3d_brier


# QUADRANT GRAFİKLER

aids2_p_quad_cindex <- plot_quadrant(
  aids2_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
aids2_p_quad_cindex

aids2_p_quad_brier <- plot_quadrant(
  aids2_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
aids2_p_quad_brier


# =============================================================================
# VETERAN ANALİZİ
# =============================================================================

veteran_data   <- prepare_veteran(veteran_raw)
veteran_task   <- make_task_veteran(veteran_data)
veteran_split  <- data_split(veteran_task)
veteran_models <- train_models(veteran_task, veteran_split)


# PERFORMANS

veteran_perf_rsf <- measure_performance(veteran_models$rsf_model, veteran_task, veteran_split)
veteran_perf_rsf$c_index
veteran_perf_rsf$brier
veteran_perf_rsf$auc

veteran_perf_cforest <- measure_performance(veteran_models$cforest_model, veteran_task, veteran_split)
veteran_perf_cforest$c_index
veteran_perf_cforest$brier
veteran_perf_cforest$auc

veteran_perf_blackboost <- measure_performance(veteran_models$blackboost_model, veteran_task, veteran_split)
veteran_perf_blackboost$c_index
veteran_perf_blackboost$brier
veteran_perf_blackboost$auc

veteran_perf_cox <- measure_performance(veteran_models$cox_model, veteran_task, veteran_split)
veteran_perf_cox$c_index
veteran_perf_cox$brier
veteran_perf_cox$auc

veteran_perf_xgboost <- measure_performance(veteran_models$xgboost_model, veteran_task, veteran_split)
veteran_perf_xgboost$c_index
veteran_perf_xgboost$brier
veteran_perf_xgboost$auc


# GLOBAL AÇIKLAMA

veteran_global <- global_explain(veteran_task, veteran_models)


# RSF

veteran_rsf_vimp <- veteran_global$vimp_list$rsf_model
veteran_rsf_p_vimp <- plot(veteran_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
veteran_rsf_p_vimp

veteran_rsf_summary <- calculate_vimp_summary(veteran_rsf_vimp, eps = 0)
veteran_rsf_summary

veteran_rsf_p_fr <- plot_vimp_with_fr(veteran_rsf_summary)
veteran_rsf_p_fr

veteran_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(veteran_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = veteran_perf_rsf$c_index,
  brier            = veteran_perf_rsf$brier,
  censoring_rate   = mean(veteran_data$event == 0, na.rm = TRUE)
)


# CFOREST

veteran_cforest_vimp <- veteran_global$vimp_list$cforest_model
veteran_cforest_p_vimp <- plot(veteran_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
veteran_cforest_p_vimp

veteran_cforest_summary <- calculate_vimp_summary(veteran_cforest_vimp, eps = 0)
veteran_cforest_summary

veteran_cforest_p_fr <- plot_vimp_with_fr(veteran_cforest_summary)
veteran_cforest_p_fr

veteran_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(veteran_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = veteran_perf_cforest$c_index,
  brier            = veteran_perf_cforest$brier,
  censoring_rate   = mean(veteran_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

veteran_blackboost_vimp <- veteran_global$vimp_list$blackboost_model
veteran_blackboost_p_vimp <- plot(veteran_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
veteran_blackboost_p_vimp

veteran_blackboost_summary <- calculate_vimp_summary(veteran_blackboost_vimp, eps = 0)
veteran_blackboost_summary

veteran_blackboost_p_fr <- plot_vimp_with_fr(veteran_blackboost_summary)
veteran_blackboost_p_fr

veteran_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(veteran_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = veteran_perf_blackboost$c_index,
  brier            = veteran_perf_blackboost$brier,
  censoring_rate   = mean(veteran_data$event == 0, na.rm = TRUE)
)


# COX

veteran_cox_vimp <- veteran_global$vimp_list$cox_model
veteran_cox_p_vimp <- plot(veteran_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
veteran_cox_p_vimp

veteran_cox_summary <- calculate_vimp_summary(veteran_cox_vimp, eps = 0)
veteran_cox_summary

veteran_cox_p_fr <- plot_vimp_with_fr(veteran_cox_summary)
veteran_cox_p_fr

veteran_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(veteran_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = veteran_perf_cox$c_index,
  brier            = veteran_perf_cox$brier,
  censoring_rate   = mean(veteran_data$event == 0, na.rm = TRUE)
)


# XGBOOST

veteran_xgboost_vimp <- veteran_global$vimp_list$xgboost_model
veteran_xgboost_p_vimp <- plot(veteran_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
veteran_xgboost_p_vimp

veteran_xgboost_summary <- calculate_vimp_summary(veteran_xgboost_vimp, eps = 0)
veteran_xgboost_summary

veteran_xgboost_p_fr <- plot_vimp_with_fr(veteran_xgboost_summary)
veteran_xgboost_p_fr

veteran_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(veteran_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = veteran_perf_xgboost$c_index,
  brier            = veteran_perf_xgboost$brier,
  censoring_rate   = mean(veteran_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

veteran_plot_data <- rbind(
  veteran_rsf_plot_data,
  veteran_cforest_plot_data,
  veteran_blackboost_plot_data,
  veteran_cox_plot_data,
  veteran_xgboost_plot_data
)


# 3D GRAFİKLER

veteran_p_3d_cindex <- plot_3d_cindex(veteran_plot_data)
veteran_p_3d_cindex

veteran_p_3d_brier <- plot_3d_brier(veteran_plot_data)
veteran_p_3d_brier


# QUADRANT GRAFİKLER

veteran_p_quad_cindex <- plot_quadrant(
  veteran_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
veteran_p_quad_cindex

veteran_p_quad_brier <- plot_quadrant(
  veteran_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
veteran_p_quad_brier


# =============================================================================
# BREAST ANALİZİ
# =============================================================================

breast_data   <- prepare_breast(breast_raw)
breast_task   <- make_task_breast(breast_data)
breast_split  <- data_split(breast_task)
breast_models <- train_models(breast_task, breast_split)


# PERFORMANS

breast_perf_rsf <- measure_performance(breast_models$rsf_model, breast_task, breast_split)
breast_perf_rsf$c_index
breast_perf_rsf$brier
breast_perf_rsf$auc

breast_perf_cforest <- measure_performance(breast_models$cforest_model, breast_task, breast_split)
breast_perf_cforest$c_index
breast_perf_cforest$brier
breast_perf_cforest$auc

breast_perf_blackboost <- measure_performance(breast_models$blackboost_model, breast_task, breast_split)
breast_perf_blackboost$c_index
breast_perf_blackboost$brier
breast_perf_blackboost$auc

breast_perf_cox <- measure_performance(breast_models$cox_model, breast_task, breast_split)
breast_perf_cox$c_index
breast_perf_cox$brier
breast_perf_cox$auc

breast_perf_xgboost <- measure_performance(breast_models$xgboost_model, breast_task, breast_split)
breast_perf_xgboost$c_index
breast_perf_xgboost$brier
breast_perf_xgboost$auc


# GLOBAL AÇIKLAMA

breast_global <- global_explain(breast_task, breast_models)


# RSF

breast_rsf_vimp <- breast_global$vimp_list$rsf_model
breast_rsf_p_vimp <- plot(breast_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
breast_rsf_p_vimp

breast_rsf_summary <- calculate_vimp_summary(breast_rsf_vimp, eps = 0)
breast_rsf_summary

breast_rsf_p_fr <- plot_vimp_with_fr(breast_rsf_summary)
breast_rsf_p_fr

breast_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(breast_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = breast_perf_rsf$c_index,
  brier            = breast_perf_rsf$brier,
  censoring_rate   = mean(breast_data$event == 0, na.rm = TRUE)
)


# CFOREST

breast_cforest_vimp <- breast_global$vimp_list$cforest_model
breast_cforest_p_vimp <- plot(breast_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
breast_cforest_p_vimp

breast_cforest_summary <- calculate_vimp_summary(breast_cforest_vimp, eps = 0)
breast_cforest_summary

breast_cforest_p_fr <- plot_vimp_with_fr(breast_cforest_summary)
breast_cforest_p_fr

breast_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(breast_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = breast_perf_cforest$c_index,
  brier            = breast_perf_cforest$brier,
  censoring_rate   = mean(breast_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

breast_blackboost_vimp <- breast_global$vimp_list$blackboost_model
breast_blackboost_p_vimp <- plot(breast_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
breast_blackboost_p_vimp

breast_blackboost_summary <- calculate_vimp_summary(breast_blackboost_vimp, eps = 0)
breast_blackboost_summary

breast_blackboost_p_fr <- plot_vimp_with_fr(breast_blackboost_summary)
breast_blackboost_p_fr

breast_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(breast_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = breast_perf_blackboost$c_index,
  brier            = breast_perf_blackboost$brier,
  censoring_rate   = mean(breast_data$event == 0, na.rm = TRUE)
)


# COX

breast_cox_vimp <- breast_global$vimp_list$cox_model
breast_cox_p_vimp <- plot(breast_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
breast_cox_p_vimp

breast_cox_summary <- calculate_vimp_summary(breast_cox_vimp, eps = 0)
breast_cox_summary

breast_cox_p_fr <- plot_vimp_with_fr(breast_cox_summary)
breast_cox_p_fr

breast_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(breast_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = breast_perf_cox$c_index,
  brier            = breast_perf_cox$brier,
  censoring_rate   = mean(breast_data$event == 0, na.rm = TRUE)
)


# XGBOOST

breast_xgboost_vimp <- breast_global$vimp_list$xgboost_model
breast_xgboost_p_vimp <- plot(breast_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
breast_xgboost_p_vimp

breast_xgboost_summary <- calculate_vimp_summary(breast_xgboost_vimp, eps = 0)
breast_xgboost_summary

breast_xgboost_p_fr <- plot_vimp_with_fr(breast_xgboost_summary)
breast_xgboost_p_fr

breast_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(breast_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = breast_perf_xgboost$c_index,
  brier            = breast_perf_xgboost$brier,
  censoring_rate   = mean(breast_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

breast_plot_data <- rbind(
  breast_rsf_plot_data,
  breast_cforest_plot_data,
  breast_blackboost_plot_data,
  breast_cox_plot_data,
  breast_xgboost_plot_data
)


# 3D GRAFİKLER

breast_p_3d_cindex <- plot_3d_cindex(breast_plot_data)
breast_p_3d_cindex

breast_p_3d_brier <- plot_3d_brier(breast_plot_data)
breast_p_3d_brier


# QUADRANT GRAFİKLER

breast_p_quad_cindex <- plot_quadrant(
  breast_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
breast_p_quad_cindex

breast_p_quad_brier <- plot_quadrant(
  breast_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
breast_p_quad_brier


# =============================================================================
# E1684 ANALİZİ
# =============================================================================

e1684_data   <- prepare_e1684(e1684_raw)
e1684_task   <- make_task_e1684(e1684_data)
e1684_split  <- data_split(e1684_task)
e1684_models <- train_models(e1684_task, e1684_split)


# PERFORMANS

e1684_perf_rsf <- measure_performance(e1684_models$rsf_model, e1684_task, e1684_split)
e1684_perf_rsf$c_index
e1684_perf_rsf$brier
e1684_perf_rsf$auc

e1684_perf_cforest <- measure_performance(e1684_models$cforest_model, e1684_task, e1684_split)
e1684_perf_cforest$c_index
e1684_perf_cforest$brier
e1684_perf_cforest$auc

e1684_perf_blackboost <- measure_performance(e1684_models$blackboost_model, e1684_task, e1684_split)
e1684_perf_blackboost$c_index
e1684_perf_blackboost$brier
e1684_perf_blackboost$auc

e1684_perf_cox <- measure_performance(e1684_models$cox_model, e1684_task, e1684_split)
e1684_perf_cox$c_index
e1684_perf_cox$brier
e1684_perf_cox$auc

e1684_perf_xgboost <- measure_performance(e1684_models$xgboost_model, e1684_task, e1684_split)
e1684_perf_xgboost$c_index
e1684_perf_xgboost$brier
e1684_perf_xgboost$auc


# GLOBAL AÇIKLAMA

e1684_global <- global_explain(e1684_task, e1684_models)


# RSF

e1684_rsf_vimp <- e1684_global$vimp_list$rsf_model
e1684_rsf_p_vimp <- plot(e1684_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
e1684_rsf_p_vimp

e1684_rsf_summary <- calculate_vimp_summary(e1684_rsf_vimp, eps = 0)
e1684_rsf_summary

e1684_rsf_p_fr <- plot_vimp_with_fr(e1684_rsf_summary)
e1684_rsf_p_fr

e1684_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(e1684_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = e1684_perf_rsf$c_index,
  brier            = e1684_perf_rsf$brier,
  censoring_rate   = mean(e1684_data$event == 0, na.rm = TRUE)
)


# CFOREST

e1684_cforest_vimp <- e1684_global$vimp_list$cforest_model
e1684_cforest_p_vimp <- plot(e1684_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
e1684_cforest_p_vimp

e1684_cforest_summary <- calculate_vimp_summary(e1684_cforest_vimp, eps = 0)
e1684_cforest_summary

e1684_cforest_p_fr <- plot_vimp_with_fr(e1684_cforest_summary)
e1684_cforest_p_fr

e1684_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(e1684_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = e1684_perf_cforest$c_index,
  brier            = e1684_perf_cforest$brier,
  censoring_rate   = mean(e1684_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

e1684_blackboost_vimp <- e1684_global$vimp_list$blackboost_model
e1684_blackboost_p_vimp <- plot(e1684_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
e1684_blackboost_p_vimp

e1684_blackboost_summary <- calculate_vimp_summary(e1684_blackboost_vimp, eps = 0)
e1684_blackboost_summary

e1684_blackboost_p_fr <- plot_vimp_with_fr(e1684_blackboost_summary)
e1684_blackboost_p_fr

e1684_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(e1684_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = e1684_perf_blackboost$c_index,
  brier            = e1684_perf_blackboost$brier,
  censoring_rate   = mean(e1684_data$event == 0, na.rm = TRUE)
)


# COX

e1684_cox_vimp <- e1684_global$vimp_list$cox_model
e1684_cox_p_vimp <- plot(e1684_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
e1684_cox_p_vimp

e1684_cox_summary <- calculate_vimp_summary(e1684_cox_vimp, eps = 0)
e1684_cox_summary

e1684_cox_p_fr <- plot_vimp_with_fr(e1684_cox_summary)
e1684_cox_p_fr

e1684_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(e1684_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = e1684_perf_cox$c_index,
  brier            = e1684_perf_cox$brier,
  censoring_rate   = mean(e1684_data$event == 0, na.rm = TRUE)
)


# XGBOOST

e1684_xgboost_vimp <- e1684_global$vimp_list$xgboost_model
e1684_xgboost_p_vimp <- plot(e1684_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
e1684_xgboost_p_vimp

e1684_xgboost_summary <- calculate_vimp_summary(e1684_xgboost_vimp, eps = 0)
e1684_xgboost_summary

e1684_xgboost_p_fr <- plot_vimp_with_fr(e1684_xgboost_summary)
e1684_xgboost_p_fr

e1684_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(e1684_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = e1684_perf_xgboost$c_index,
  brier            = e1684_perf_xgboost$brier,
  censoring_rate   = mean(e1684_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

e1684_plot_data <- rbind(
  e1684_rsf_plot_data,
  e1684_cforest_plot_data,
  e1684_blackboost_plot_data,
  e1684_cox_plot_data,
  e1684_xgboost_plot_data
)


# 3D GRAFİKLER

e1684_p_3d_cindex <- plot_3d_cindex(e1684_plot_data)
e1684_p_3d_cindex

e1684_p_3d_brier <- plot_3d_brier(e1684_plot_data)
e1684_p_3d_brier


# QUADRANT GRAFİKLER

e1684_p_quad_cindex <- plot_quadrant(
  e1684_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
e1684_p_quad_cindex

e1684_p_quad_brier <- plot_quadrant(
  e1684_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
e1684_p_quad_brier



# =============================================================================
# HEART ANALİZİ
# =============================================================================

heart_data   <- prepare_heart(heart_raw)
heart_task   <- make_task_heart(heart_data)
heart_split  <- data_split(heart_task)
heart_models <- train_models(heart_task, heart_split)


# PERFORMANS

heart_perf_rsf <- measure_performance(heart_models$rsf_model, heart_task, heart_split)
heart_perf_rsf$c_index
heart_perf_rsf$brier
heart_perf_rsf$auc

heart_perf_cforest <- measure_performance(heart_models$cforest_model, heart_task, heart_split)
heart_perf_cforest$c_index
heart_perf_cforest$brier
heart_perf_cforest$auc

heart_perf_blackboost <- measure_performance(heart_models$blackboost_model, heart_task, heart_split)
heart_perf_blackboost$c_index
heart_perf_blackboost$brier
heart_perf_blackboost$auc

heart_perf_cox <- measure_performance(heart_models$cox_model, heart_task, heart_split)
heart_perf_cox$c_index
heart_perf_cox$brier
heart_perf_cox$auc

heart_perf_xgboost <- measure_performance(heart_models$xgboost_model, heart_task, heart_split)
heart_perf_xgboost$c_index
heart_perf_xgboost$brier
heart_perf_xgboost$auc


# GLOBAL AÇIKLAMA

heart_global <- global_explain(heart_task, heart_models)


# RSF

heart_rsf_vimp <- heart_global$vimp_list$rsf_model
heart_rsf_p_vimp <- plot(heart_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_rsf_p_vimp

heart_rsf_summary <- calculate_vimp_summary(heart_rsf_vimp, eps = 0)
heart_rsf_summary

heart_rsf_p_fr <- plot_vimp_with_fr(heart_rsf_summary)
heart_rsf_p_fr

heart_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(heart_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_rsf$c_index,
  brier            = heart_perf_rsf$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# CFOREST

heart_cforest_vimp <- heart_global$vimp_list$cforest_model
heart_cforest_p_vimp <- plot(heart_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_cforest_p_vimp

heart_cforest_summary <- calculate_vimp_summary(heart_cforest_vimp, eps = 0)
heart_cforest_summary

heart_cforest_p_fr <- plot_vimp_with_fr(heart_cforest_summary)
heart_cforest_p_fr

heart_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(heart_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_cforest$c_index,
  brier            = heart_perf_cforest$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

heart_blackboost_vimp <- heart_global$vimp_list$blackboost_model
heart_blackboost_p_vimp <- plot(heart_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_blackboost_p_vimp

heart_blackboost_summary <- calculate_vimp_summary(heart_blackboost_vimp, eps = 0)
heart_blackboost_summary

heart_blackboost_p_fr <- plot_vimp_with_fr(heart_blackboost_summary)
heart_blackboost_p_fr

heart_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(heart_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_blackboost$c_index,
  brier            = heart_perf_blackboost$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# COX

heart_cox_vimp <- heart_global$vimp_list$cox_model
heart_cox_p_vimp <- plot(heart_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_cox_p_vimp

heart_cox_summary <- calculate_vimp_summary(heart_cox_vimp, eps = 0)
heart_cox_summary

heart_cox_p_fr <- plot_vimp_with_fr(heart_cox_summary)
heart_cox_p_fr

heart_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(heart_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_cox$c_index,
  brier            = heart_perf_cox$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# XGBOOST

heart_xgboost_vimp <- heart_global$vimp_list$xgboost_model
heart_xgboost_p_vimp <- plot(heart_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
heart_xgboost_p_vimp

heart_xgboost_summary <- calculate_vimp_summary(heart_xgboost_vimp, eps = 0)
heart_xgboost_summary

heart_xgboost_p_fr <- plot_vimp_with_fr(heart_xgboost_summary)
heart_xgboost_p_fr

heart_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(heart_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = heart_perf_xgboost$c_index,
  brier            = heart_perf_xgboost$brier,
  censoring_rate   = mean(heart_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

heart_plot_data <- rbind(
  heart_rsf_plot_data,
  heart_cforest_plot_data,
  heart_blackboost_plot_data,
  heart_cox_plot_data,
  heart_xgboost_plot_data
)


# 3D GRAFİKLER

heart_p_3d_cindex <- plot_3d_cindex(heart_plot_data)
heart_p_3d_cindex

heart_p_3d_brier <- plot_3d_brier(heart_plot_data)
heart_p_3d_brier


# QUADRANT GRAFİKLER

heart_p_quad_cindex <- plot_quadrant(
  heart_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
heart_p_quad_cindex

heart_p_quad_brier <- plot_quadrant(
  heart_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
heart_p_quad_brier


# =============================================================================
# EPİLEPTİK ANALİZİ
# =============================================================================

epileptic_data   <- prepare_epileptic(epileptic_raw)
epileptic_task   <- make_task_epileptic(epileptic_data)
epileptic_split  <- data_split(epileptic_task)
epileptic_models <- train_models(epileptic_task, epileptic_split)


# PERFORMANS

epileptic_perf_rsf <- measure_performance(epileptic_models$rsf_model, epileptic_task, epileptic_split)
epileptic_perf_rsf$c_index
epileptic_perf_rsf$brier
epileptic_perf_rsf$auc

epileptic_perf_cforest <- measure_performance(epileptic_models$cforest_model, epileptic_task, epileptic_split)
epileptic_perf_cforest$c_index
epileptic_perf_cforest$brier
epileptic_perf_cforest$auc

epileptic_perf_blackboost <- measure_performance(epileptic_models$blackboost_model, epileptic_task, epileptic_split)
epileptic_perf_blackboost$c_index
epileptic_perf_blackboost$brier
epileptic_perf_blackboost$auc

epileptic_perf_cox <- measure_performance(epileptic_models$cox_model, epileptic_task, epileptic_split)
epileptic_perf_cox$c_index
epileptic_perf_cox$brier
epileptic_perf_cox$auc

epileptic_perf_xgboost <- measure_performance(epileptic_models$xgboost_model, epileptic_task, epileptic_split)
epileptic_perf_xgboost$c_index
epileptic_perf_xgboost$brier
epileptic_perf_xgboost$auc


# GLOBAL AÇIKLAMA

epileptic_global <- global_explain(epileptic_task, epileptic_models)


# RSF

epileptic_rsf_vimp <- epileptic_global$vimp_list$rsf_model
epileptic_rsf_p_vimp <- plot(epileptic_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
epileptic_rsf_p_vimp

epileptic_rsf_summary <- calculate_vimp_summary(epileptic_rsf_vimp, eps = 0)
epileptic_rsf_summary

epileptic_rsf_p_fr <- plot_vimp_with_fr(epileptic_rsf_summary)
epileptic_rsf_p_fr

epileptic_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(epileptic_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = epileptic_perf_rsf$c_index,
  brier            = epileptic_perf_rsf$brier,
  censoring_rate   = mean(epileptic_data$event == 0, na.rm = TRUE)
)


# CFOREST

epileptic_cforest_vimp <- epileptic_global$vimp_list$cforest_model
epileptic_cforest_p_vimp <- plot(epileptic_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
epileptic_cforest_p_vimp

epileptic_cforest_summary <- calculate_vimp_summary(epileptic_cforest_vimp, eps = 0)
epileptic_cforest_summary

epileptic_cforest_p_fr <- plot_vimp_with_fr(epileptic_cforest_summary)
epileptic_cforest_p_fr

epileptic_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(epileptic_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = epileptic_perf_cforest$c_index,
  brier            = epileptic_perf_cforest$brier,
  censoring_rate   = mean(epileptic_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

epileptic_blackboost_vimp <- epileptic_global$vimp_list$blackboost_model
epileptic_blackboost_p_vimp <- plot(epileptic_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
epileptic_blackboost_p_vimp

epileptic_blackboost_summary <- calculate_vimp_summary(epileptic_blackboost_vimp, eps = 0)
epileptic_blackboost_summary

epileptic_blackboost_p_fr <- plot_vimp_with_fr(epileptic_blackboost_summary)
epileptic_blackboost_p_fr

epileptic_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(epileptic_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = epileptic_perf_blackboost$c_index,
  brier            = epileptic_perf_blackboost$brier,
  censoring_rate   = mean(epileptic_data$event == 0, na.rm = TRUE)
)


# COX

epileptic_cox_vimp <- epileptic_global$vimp_list$cox_model
epileptic_cox_p_vimp <- plot(epileptic_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
epileptic_cox_p_vimp

epileptic_cox_summary <- calculate_vimp_summary(epileptic_cox_vimp, eps = 0)
epileptic_cox_summary

epileptic_cox_p_fr <- plot_vimp_with_fr(epileptic_cox_summary)
epileptic_cox_p_fr

epileptic_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(epileptic_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = epileptic_perf_cox$c_index,
  brier            = epileptic_perf_cox$brier,
  censoring_rate   = mean(epileptic_data$event == 0, na.rm = TRUE)
)


# XGBOOST

epileptic_xgboost_vimp <- epileptic_global$vimp_list$xgboost_model
epileptic_xgboost_p_vimp <- plot(epileptic_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
epileptic_xgboost_p_vimp

epileptic_xgboost_summary <- calculate_vimp_summary(epileptic_xgboost_vimp, eps = 0)
epileptic_xgboost_summary

epileptic_xgboost_p_fr <- plot_vimp_with_fr(epileptic_xgboost_summary)
epileptic_xgboost_p_fr

epileptic_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(epileptic_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = epileptic_perf_xgboost$c_index,
  brier            = epileptic_perf_xgboost$brier,
  censoring_rate   = mean(epileptic_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

epileptic_plot_data <- rbind(
  epileptic_rsf_plot_data,
  epileptic_cforest_plot_data,
  epileptic_blackboost_plot_data,
  epileptic_cox_plot_data,
  epileptic_xgboost_plot_data
)


# 3D GRAFİKLER

epileptic_p_3d_cindex <- plot_3d_cindex(epileptic_plot_data)
epileptic_p_3d_cindex

epileptic_p_3d_brier <- plot_3d_brier(epileptic_plot_data)
epileptic_p_3d_brier


# QUADRANT GRAFİKLER

epileptic_p_quad_cindex <- plot_quadrant(
  epileptic_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
epileptic_p_quad_cindex

epileptic_p_quad_brier <- plot_quadrant(
  epileptic_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
epileptic_p_quad_brier


# =============================================================================
# DİYALİZ ANALİZİ
# =============================================================================

dialysis_data   <- prepare_dialysis(dialysis_raw)
dialysis_task   <- make_task_dialysis(dialysis_data)
dialysis_split  <- data_split(dialysis_task)
dialysis_models <- train_models(dialysis_task, dialysis_split)


# PERFORMANS

dialysis_perf_rsf <- measure_performance(dialysis_models$rsf_model, dialysis_task, dialysis_split)
dialysis_perf_rsf$c_index
dialysis_perf_rsf$brier
dialysis_perf_rsf$auc

dialysis_perf_cforest <- measure_performance(dialysis_models$cforest_model, dialysis_task, dialysis_split)
dialysis_perf_cforest$c_index
dialysis_perf_cforest$brier
dialysis_perf_cforest$auc

dialysis_perf_blackboost <- measure_performance(dialysis_models$blackboost_model, dialysis_task, dialysis_split)
dialysis_perf_blackboost$c_index
dialysis_perf_blackboost$brier
dialysis_perf_blackboost$auc

dialysis_perf_cox <- measure_performance(dialysis_models$cox_model, dialysis_task, dialysis_split)
dialysis_perf_cox$c_index
dialysis_perf_cox$brier
dialysis_perf_cox$auc

dialysis_perf_xgboost <- measure_performance(dialysis_models$xgboost_model, dialysis_task, dialysis_split)
dialysis_perf_xgboost$c_index
dialysis_perf_xgboost$brier
dialysis_perf_xgboost$auc


# GLOBAL AÇIKLAMA

dialysis_global <- global_explain(dialysis_task, dialysis_models)


# RSF

dialysis_rsf_vimp <- dialysis_global$vimp_list$rsf_model
dialysis_rsf_p_vimp <- plot(dialysis_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dialysis_rsf_p_vimp

dialysis_rsf_summary <- calculate_vimp_summary(dialysis_rsf_vimp, eps = 0)
dialysis_rsf_summary

dialysis_rsf_p_fr <- plot_vimp_with_fr(dialysis_rsf_summary)
dialysis_rsf_p_fr

dialysis_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(dialysis_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = dialysis_perf_rsf$c_index,
  brier            = dialysis_perf_rsf$brier,
  censoring_rate   = mean(dialysis_data$event == 0, na.rm = TRUE)
)


# CFOREST

dialysis_cforest_vimp <- dialysis_global$vimp_list$cforest_model
dialysis_cforest_p_vimp <- plot(dialysis_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dialysis_cforest_p_vimp

dialysis_cforest_summary <- calculate_vimp_summary(dialysis_cforest_vimp, eps = 0)
dialysis_cforest_summary

dialysis_cforest_p_fr <- plot_vimp_with_fr(dialysis_cforest_summary)
dialysis_cforest_p_fr

dialysis_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(dialysis_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = dialysis_perf_cforest$c_index,
  brier            = dialysis_perf_cforest$brier,
  censoring_rate   = mean(dialysis_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

dialysis_blackboost_vimp <- dialysis_global$vimp_list$blackboost_model
dialysis_blackboost_p_vimp <- plot(dialysis_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dialysis_blackboost_p_vimp

dialysis_blackboost_summary <- calculate_vimp_summary(dialysis_blackboost_vimp, eps = 0)
dialysis_blackboost_summary

dialysis_blackboost_p_fr <- plot_vimp_with_fr(dialysis_blackboost_summary)
dialysis_blackboost_p_fr

dialysis_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(dialysis_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = dialysis_perf_blackboost$c_index,
  brier            = dialysis_perf_blackboost$brier,
  censoring_rate   = mean(dialysis_data$event == 0, na.rm = TRUE)
)


# COX

dialysis_cox_vimp <- dialysis_global$vimp_list$cox_model
dialysis_cox_p_vimp <- plot(dialysis_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dialysis_cox_p_vimp

dialysis_cox_summary <- calculate_vimp_summary(dialysis_cox_vimp, eps = 0)
dialysis_cox_summary

dialysis_cox_p_fr <- plot_vimp_with_fr(dialysis_cox_summary)
dialysis_cox_p_fr

dialysis_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(dialysis_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = dialysis_perf_cox$c_index,
  brier            = dialysis_perf_cox$brier,
  censoring_rate   = mean(dialysis_data$event == 0, na.rm = TRUE)
)


# XGBOOST

dialysis_xgboost_vimp <- dialysis_global$vimp_list$xgboost_model
dialysis_xgboost_p_vimp <- plot(dialysis_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dialysis_xgboost_p_vimp

dialysis_xgboost_summary <- calculate_vimp_summary(dialysis_xgboost_vimp, eps = 0)
dialysis_xgboost_summary

dialysis_xgboost_p_fr <- plot_vimp_with_fr(dialysis_xgboost_summary)
dialysis_xgboost_p_fr

dialysis_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(dialysis_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = dialysis_perf_xgboost$c_index,
  brier            = dialysis_perf_xgboost$brier,
  censoring_rate   = mean(dialysis_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

dialysis_plot_data <- rbind(
  dialysis_rsf_plot_data,
  dialysis_cforest_plot_data,
  dialysis_blackboost_plot_data,
  dialysis_cox_plot_data,
  dialysis_xgboost_plot_data
)


# 3D GRAFİKLER

dialysis_p_3d_cindex <- plot_3d_cindex(dialysis_plot_data)
dialysis_p_3d_cindex

dialysis_p_3d_brier <- plot_3d_brier(dialysis_plot_data)
dialysis_p_3d_brier


# QUADRANT GRAFİKLER

dialysis_p_quad_cindex <- plot_quadrant(
  dialysis_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
dialysis_p_quad_cindex

dialysis_p_quad_brier <- plot_quadrant(
  dialysis_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
dialysis_p_quad_brier


# =============================================================================
# PBC ANALİZİ
# =============================================================================

pbc_data   <- prepare_pbc(pbc_raw)
pbc_task   <- make_task_pbc(pbc_data)
pbc_split  <- data_split(pbc_task)
pbc_models <- train_models(pbc_task, pbc_split)


# PERFORMANS

pbc_perf_rsf <- measure_performance(pbc_models$rsf_model, pbc_task, pbc_split)
pbc_perf_rsf$c_index
pbc_perf_rsf$brier
pbc_perf_rsf$auc

pbc_perf_cforest <- measure_performance(pbc_models$cforest_model, pbc_task, pbc_split)
pbc_perf_cforest$c_index
pbc_perf_cforest$brier
pbc_perf_cforest$auc

pbc_perf_blackboost <- measure_performance(pbc_models$blackboost_model, pbc_task, pbc_split)
pbc_perf_blackboost$c_index
pbc_perf_blackboost$brier
pbc_perf_blackboost$auc

pbc_perf_cox <- measure_performance(pbc_models$cox_model, pbc_task, pbc_split)
pbc_perf_cox$c_index
pbc_perf_cox$brier
pbc_perf_cox$auc

pbc_perf_xgboost <- measure_performance(pbc_models$xgboost_model, pbc_task, pbc_split)
pbc_perf_xgboost$c_index
pbc_perf_xgboost$brier
pbc_perf_xgboost$auc


# GLOBAL AÇIKLAMA

pbc_global <- global_explain(pbc_task, pbc_models)


# RSF

pbc_rsf_vimp <- pbc_global$vimp_list$rsf_model
pbc_rsf_p_vimp <- plot(pbc_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
pbc_rsf_p_vimp

pbc_rsf_summary <- calculate_vimp_summary(pbc_rsf_vimp, eps = 0)
pbc_rsf_summary

pbc_rsf_p_fr <- plot_vimp_with_fr(pbc_rsf_summary)
pbc_rsf_p_fr

pbc_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(pbc_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = pbc_perf_rsf$c_index,
  brier            = pbc_perf_rsf$brier,
  censoring_rate   = mean(pbc_data$event == 0, na.rm = TRUE)
)


# CFOREST

pbc_cforest_vimp <- pbc_global$vimp_list$cforest_model
pbc_cforest_p_vimp <- plot(pbc_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
pbc_cforest_p_vimp

pbc_cforest_summary <- calculate_vimp_summary(pbc_cforest_vimp, eps = 0)
pbc_cforest_summary

pbc_cforest_p_fr <- plot_vimp_with_fr(pbc_cforest_summary)
pbc_cforest_p_fr

pbc_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(pbc_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = pbc_perf_cforest$c_index,
  brier            = pbc_perf_cforest$brier,
  censoring_rate   = mean(pbc_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

pbc_blackboost_vimp <- pbc_global$vimp_list$blackboost_model
pbc_blackboost_p_vimp <- plot(pbc_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
pbc_blackboost_p_vimp

pbc_blackboost_summary <- calculate_vimp_summary(pbc_blackboost_vimp, eps = 0)
pbc_blackboost_summary

pbc_blackboost_p_fr <- plot_vimp_with_fr(pbc_blackboost_summary)
pbc_blackboost_p_fr

pbc_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(pbc_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = pbc_perf_blackboost$c_index,
  brier            = pbc_perf_blackboost$brier,
  censoring_rate   = mean(pbc_data$event == 0, na.rm = TRUE)
)


# COX

pbc_cox_vimp <- pbc_global$vimp_list$cox_model
pbc_cox_p_vimp <- plot(pbc_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
pbc_cox_p_vimp

pbc_cox_summary <- calculate_vimp_summary(pbc_cox_vimp, eps = 0)
pbc_cox_summary

pbc_cox_p_fr <- plot_vimp_with_fr(pbc_cox_summary)
pbc_cox_p_fr

pbc_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(pbc_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = pbc_perf_cox$c_index,
  brier            = pbc_perf_cox$brier,
  censoring_rate   = mean(pbc_data$event == 0, na.rm = TRUE)
)


# XGBOOST

pbc_xgboost_vimp <- pbc_global$vimp_list$xgboost_model
pbc_xgboost_p_vimp <- plot(pbc_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
pbc_xgboost_p_vimp

pbc_xgboost_summary <- calculate_vimp_summary(pbc_xgboost_vimp, eps = 0)
pbc_xgboost_summary

pbc_xgboost_p_fr <- plot_vimp_with_fr(pbc_xgboost_summary)
pbc_xgboost_p_fr

pbc_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(pbc_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = pbc_perf_xgboost$c_index,
  brier            = pbc_perf_xgboost$brier,
  censoring_rate   = mean(pbc_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

pbc_plot_data <- rbind(
  pbc_rsf_plot_data,
  pbc_cforest_plot_data,
  pbc_blackboost_plot_data,
  pbc_cox_plot_data,
  pbc_xgboost_plot_data
)


# 3D GRAFİKLER

pbc_p_3d_cindex <- plot_3d_cindex(pbc_plot_data)
pbc_p_3d_cindex

pbc_p_3d_brier <- plot_3d_brier(pbc_plot_data)
pbc_p_3d_brier


# QUADRANT GRAFİKLER

pbc_p_quad_cindex <- plot_quadrant(
  pbc_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
pbc_p_quad_cindex

pbc_p_quad_brier <- plot_quadrant(
  pbc_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
pbc_p_quad_brier





# 21.04


# =============================================================================
# PROSTATESURVIVAL ANALİZİ
# =============================================================================

prostateSurvival_data   <- prepare_prostateSurvival(prostateSurvival_raw)
prostateSurvival_task   <- make_task_prostateSurvival(prostateSurvival_data)
prostateSurvival_split  <- data_split(prostateSurvival_task)
prostateSurvival_models <- train_models(prostateSurvival_task, prostateSurvival_split)


# PERFORMANS

prostateSurvival_perf_rsf <- measure_performance(prostateSurvival_models$rsf_model, prostateSurvival_task, prostateSurvival_split)
prostateSurvival_perf_rsf$c_index
prostateSurvival_perf_rsf$brier
prostateSurvival_perf_rsf$auc

prostateSurvival_perf_cforest <- measure_performance(prostateSurvival_models$cforest_model, prostateSurvival_task, prostateSurvival_split)
prostateSurvival_perf_cforest$c_index
prostateSurvival_perf_cforest$brier
prostateSurvival_perf_cforest$auc

prostateSurvival_perf_blackboost <- measure_performance(prostateSurvival_models$blackboost_model, prostateSurvival_task, prostateSurvival_split)
prostateSurvival_perf_blackboost$c_index
prostateSurvival_perf_blackboost$brier
prostateSurvival_perf_blackboost$auc

prostateSurvival_perf_cox <- measure_performance(prostateSurvival_models$cox_model, prostateSurvival_task, prostateSurvival_split)
prostateSurvival_perf_cox$c_index
prostateSurvival_perf_cox$brier
prostateSurvival_perf_cox$auc

prostateSurvival_perf_xgboost <- measure_performance(prostateSurvival_models$xgboost_model, prostateSurvival_task, prostateSurvival_split)
prostateSurvival_perf_xgboost$c_index
prostateSurvival_perf_xgboost$brier
prostateSurvival_perf_xgboost$auc


# GLOBAL AÇIKLAMA

prostateSurvival_global <- global_explain(prostateSurvival_task, prostateSurvival_models)


# RSF

prostateSurvival_rsf_vimp <- prostateSurvival_global$vimp_list$rsf_model
prostateSurvival_rsf_p_vimp <- plot(prostateSurvival_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostateSurvival_rsf_p_vimp

prostateSurvival_rsf_summary <- calculate_vimp_summary(prostateSurvival_rsf_vimp, eps = 0)
prostateSurvival_rsf_summary

prostateSurvival_rsf_p_fr <- plot_vimp_with_fr(prostateSurvival_rsf_summary)
prostateSurvival_rsf_p_fr

prostateSurvival_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(prostateSurvival_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostateSurvival_perf_rsf$c_index,
  brier            = prostateSurvival_perf_rsf$brier,
  censoring_rate   = mean(prostateSurvival_data$event == 0, na.rm = TRUE)
)


# CFOREST

prostateSurvival_cforest_vimp <- prostateSurvival_global$vimp_list$cforest_model
prostateSurvival_cforest_p_vimp <- plot(prostateSurvival_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostateSurvival_cforest_p_vimp

prostateSurvival_cforest_summary <- calculate_vimp_summary(prostateSurvival_cforest_vimp, eps = 0)
prostateSurvival_cforest_summary

prostateSurvival_cforest_p_fr <- plot_vimp_with_fr(prostateSurvival_cforest_summary)
prostateSurvival_cforest_p_fr

prostateSurvival_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(prostateSurvival_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostateSurvival_perf_cforest$c_index,
  brier            = prostateSurvival_perf_cforest$brier,
  censoring_rate   = mean(prostateSurvival_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

prostateSurvival_blackboost_vimp <- prostateSurvival_global$vimp_list$blackboost_model
prostateSurvival_blackboost_p_vimp <- plot(prostateSurvival_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostateSurvival_blackboost_p_vimp

prostateSurvival_blackboost_summary <- calculate_vimp_summary(prostateSurvival_blackboost_vimp, eps = 0)
prostateSurvival_blackboost_summary

prostateSurvival_blackboost_p_fr <- plot_vimp_with_fr(prostateSurvival_blackboost_summary)
prostateSurvival_blackboost_p_fr

prostateSurvival_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(prostateSurvival_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostateSurvival_perf_blackboost$c_index,
  brier            = prostateSurvival_perf_blackboost$brier,
  censoring_rate   = mean(prostateSurvival_data$event == 0, na.rm = TRUE)
)


# COX

prostateSurvival_cox_vimp <- prostateSurvival_global$vimp_list$cox_model
prostateSurvival_cox_p_vimp <- plot(prostateSurvival_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostateSurvival_cox_p_vimp

prostateSurvival_cox_summary <- calculate_vimp_summary(prostateSurvival_cox_vimp, eps = 0)
prostateSurvival_cox_summary

prostateSurvival_cox_p_fr <- plot_vimp_with_fr(prostateSurvival_cox_summary)
prostateSurvival_cox_p_fr

prostateSurvival_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(prostateSurvival_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostateSurvival_perf_cox$c_index,
  brier            = prostateSurvival_perf_cox$brier,
  censoring_rate   = mean(prostateSurvival_data$event == 0, na.rm = TRUE)
)


# XGBOOST

prostateSurvival_xgboost_vimp <- prostateSurvival_global$vimp_list$xgboost_model
prostateSurvival_xgboost_p_vimp <- plot(prostateSurvival_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostateSurvival_xgboost_p_vimp

prostateSurvival_xgboost_summary <- calculate_vimp_summary(prostateSurvival_xgboost_vimp, eps = 0)
prostateSurvival_xgboost_summary

prostateSurvival_xgboost_p_fr <- plot_vimp_with_fr(prostateSurvival_xgboost_summary)
prostateSurvival_xgboost_p_fr

prostateSurvival_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(prostateSurvival_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostateSurvival_perf_xgboost$c_index,
  brier            = prostateSurvival_perf_xgboost$brier,
  censoring_rate   = mean(prostateSurvival_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

prostateSurvival_plot_data <- rbind(
  prostateSurvival_rsf_plot_data,
  prostateSurvival_cforest_plot_data,
  prostateSurvival_blackboost_plot_data,
  prostateSurvival_cox_plot_data,
  prostateSurvival_xgboost_plot_data
)


# 3D GRAFİKLER

prostateSurvival_p_3d_cindex <- plot_3d_cindex(prostateSurvival_plot_data)
prostateSurvival_p_3d_cindex

prostateSurvival_p_3d_brier <- plot_3d_brier(prostateSurvival_plot_data)
prostateSurvival_p_3d_brier


# QUADRANT GRAFİKLER

prostateSurvival_p_quad_cindex <- plot_quadrant(
  prostateSurvival_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
prostateSurvival_p_quad_cindex

prostateSurvival_p_quad_brier <- plot_quadrant(
  prostateSurvival_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
prostateSurvival_p_quad_brier



# =============================================================================
# ACATH ANALİZİ
# =============================================================================

acath_data   <- prepare_acath(acath_raw)
acath_task   <- make_task_acath(acath_data)
acath_split  <- data_split(acath_task)
acath_models <- train_models(acath_task, acath_split)


# PERFORMANS

acath_perf_rsf <- measure_performance(acath_models$rsf_model, acath_task, acath_split)
acath_perf_rsf$c_index
acath_perf_rsf$brier
acath_perf_rsf$auc

acath_perf_cforest <- measure_performance(acath_models$cforest_model, acath_task, acath_split)
acath_perf_cforest$c_index
acath_perf_cforest$brier
acath_perf_cforest$auc

acath_perf_blackboost <- measure_performance(acath_models$blackboost_model, acath_task, acath_split)
acath_perf_blackboost$c_index
acath_perf_blackboost$brier
acath_perf_blackboost$auc

acath_perf_cox <- measure_performance(acath_models$cox_model, acath_task, acath_split)
acath_perf_cox$c_index
acath_perf_cox$brier
acath_perf_cox$auc

acath_perf_xgboost <- measure_performance(acath_models$xgboost_model, acath_task, acath_split)
acath_perf_xgboost$c_index
acath_perf_xgboost$brier
acath_perf_xgboost$auc


# GLOBAL AÇIKLAMA

acath_global <- global_explain(acath_task, acath_models)


# RSF

acath_rsf_vimp <- acath_global$vimp_list$rsf_model
acath_rsf_p_vimp <- plot(acath_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
acath_rsf_p_vimp

acath_rsf_summary <- calculate_vimp_summary(acath_rsf_vimp, eps = 0)
acath_rsf_summary

acath_rsf_p_fr <- plot_vimp_with_fr(acath_rsf_summary)
acath_rsf_p_fr

acath_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(acath_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = acath_perf_rsf$c_index,
  brier            = acath_perf_rsf$brier,
  censoring_rate   = mean(acath_data$event == 0, na.rm = TRUE)
)


# CFOREST

acath_cforest_vimp <- acath_global$vimp_list$cforest_model
acath_cforest_p_vimp <- plot(acath_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
acath_cforest_p_vimp

acath_cforest_summary <- calculate_vimp_summary(acath_cforest_vimp, eps = 0)
acath_cforest_summary

acath_cforest_p_fr <- plot_vimp_with_fr(acath_cforest_summary)
acath_cforest_p_fr

acath_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(acath_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = acath_perf_cforest$c_index,
  brier            = acath_perf_cforest$brier,
  censoring_rate   = mean(acath_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

acath_blackboost_vimp <- acath_global$vimp_list$blackboost_model
acath_blackboost_p_vimp <- plot(acath_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
acath_blackboost_p_vimp

acath_blackboost_summary <- calculate_vimp_summary(acath_blackboost_vimp, eps = 0)
acath_blackboost_summary

acath_blackboost_p_fr <- plot_vimp_with_fr(acath_blackboost_summary)
acath_blackboost_p_fr

acath_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(acath_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = acath_perf_blackboost$c_index,
  brier            = acath_perf_blackboost$brier,
  censoring_rate   = mean(acath_data$event == 0, na.rm = TRUE)
)


# COX

acath_cox_vimp <- acath_global$vimp_list$cox_model
acath_cox_p_vimp <- plot(acath_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
acath_cox_p_vimp

acath_cox_summary <- calculate_vimp_summary(acath_cox_vimp, eps = 0)
acath_cox_summary

acath_cox_p_fr <- plot_vimp_with_fr(acath_cox_summary)
acath_cox_p_fr

acath_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(acath_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = acath_perf_cox$c_index,
  brier            = acath_perf_cox$brier,
  censoring_rate   = mean(acath_data$event == 0, na.rm = TRUE)
)


# XGBOOST

acath_xgboost_vimp <- acath_global$vimp_list$xgboost_model
acath_xgboost_p_vimp <- plot(acath_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
acath_xgboost_p_vimp

acath_xgboost_summary <- calculate_vimp_summary(acath_xgboost_vimp, eps = 0)
acath_xgboost_summary

acath_xgboost_p_fr <- plot_vimp_with_fr(acath_xgboost_summary)
acath_xgboost_p_fr

acath_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(acath_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = acath_perf_xgboost$c_index,
  brier            = acath_perf_xgboost$brier,
  censoring_rate   = mean(acath_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

acath_plot_data <- rbind(
  acath_rsf_plot_data,
  acath_cforest_plot_data,
  acath_blackboost_plot_data,
  acath_cox_plot_data,
  acath_xgboost_plot_data
)


# 3D GRAFİKLER

acath_p_3d_cindex <- plot_3d_cindex(acath_plot_data)
acath_p_3d_cindex

acath_p_3d_brier <- plot_3d_brier(acath_plot_data)
acath_p_3d_brier


# QUADRANT GRAFİKLER

acath_p_quad_cindex <- plot_quadrant(
  acath_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
acath_p_quad_cindex

acath_p_quad_brier <- plot_quadrant(
  acath_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
acath_p_quad_brier




# =============================================================================
# PROSTATE ANALİZİ
# =============================================================================

prostate_data   <- prepare_prostate(prostate_raw)
prostate_task   <- make_task_prostate(prostate_data)
prostate_split  <- data_split(prostate_task)
prostate_models <- train_models(prostate_task, prostate_split)


# PERFORMANS

prostate_perf_rsf <- measure_performance(prostate_models$rsf_model, prostate_task, prostate_split)
prostate_perf_rsf$c_index
prostate_perf_rsf$brier
prostate_perf_rsf$auc

prostate_perf_cforest <- measure_performance(prostate_models$cforest_model, prostate_task, prostate_split)
prostate_perf_cforest$c_index
prostate_perf_cforest$brier
prostate_perf_cforest$auc

prostate_perf_blackboost <- measure_performance(prostate_models$blackboost_model, prostate_task, prostate_split)
prostate_perf_blackboost$c_index
prostate_perf_blackboost$brier
prostate_perf_blackboost$auc

prostate_perf_cox <- measure_performance(prostate_models$cox_model, prostate_task, prostate_split)
prostate_perf_cox$c_index
prostate_perf_cox$brier
prostate_perf_cox$auc

prostate_perf_xgboost <- measure_performance(prostate_models$xgboost_model, prostate_task, prostate_split)
prostate_perf_xgboost$c_index
prostate_perf_xgboost$brier
prostate_perf_xgboost$auc


# GLOBAL AÇIKLAMA

prostate_global <- global_explain(prostate_task, prostate_models)


# RSF

prostate_rsf_vimp <- prostate_global$vimp_list$rsf_model
prostate_rsf_p_vimp <- plot(prostate_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostate_rsf_p_vimp

prostate_rsf_summary <- calculate_vimp_summary(prostate_rsf_vimp, eps = 0)
prostate_rsf_summary

prostate_rsf_p_fr <- plot_vimp_with_fr(prostate_rsf_summary)
prostate_rsf_p_fr

prostate_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(prostate_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostate_perf_rsf$c_index,
  brier            = prostate_perf_rsf$brier,
  censoring_rate   = mean(prostate_data$event == 0, na.rm = TRUE)
)


# CFOREST

prostate_cforest_vimp <- prostate_global$vimp_list$cforest_model
prostate_cforest_p_vimp <- plot(prostate_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostate_cforest_p_vimp

prostate_cforest_summary <- calculate_vimp_summary(prostate_cforest_vimp, eps = 0)
prostate_cforest_summary

prostate_cforest_p_fr <- plot_vimp_with_fr(prostate_cforest_summary)
prostate_cforest_p_fr

prostate_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(prostate_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostate_perf_cforest$c_index,
  brier            = prostate_perf_cforest$brier,
  censoring_rate   = mean(prostate_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

prostate_blackboost_vimp <- prostate_global$vimp_list$blackboost_model
prostate_blackboost_p_vimp <- plot(prostate_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostate_blackboost_p_vimp

prostate_blackboost_summary <- calculate_vimp_summary(prostate_blackboost_vimp, eps = 0)
prostate_blackboost_summary

prostate_blackboost_p_fr <- plot_vimp_with_fr(prostate_blackboost_summary)
prostate_blackboost_p_fr

prostate_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(prostate_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostate_perf_blackboost$c_index,
  brier            = prostate_perf_blackboost$brier,
  censoring_rate   = mean(prostate_data$event == 0, na.rm = TRUE)
)


# COX

prostate_cox_vimp <- prostate_global$vimp_list$cox_model
prostate_cox_p_vimp <- plot(prostate_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostate_cox_p_vimp

prostate_cox_summary <- calculate_vimp_summary(prostate_cox_vimp, eps = 0)
prostate_cox_summary

prostate_cox_p_fr <- plot_vimp_with_fr(prostate_cox_summary)
prostate_cox_p_fr

prostate_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(prostate_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostate_perf_cox$c_index,
  brier            = prostate_perf_cox$brier,
  censoring_rate   = mean(prostate_data$event == 0, na.rm = TRUE)
)


# XGBOOST

prostate_xgboost_vimp <- prostate_global$vimp_list$xgboost_model
prostate_xgboost_p_vimp <- plot(prostate_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
prostate_xgboost_p_vimp

prostate_xgboost_summary <- calculate_vimp_summary(prostate_xgboost_vimp, eps = 0)
prostate_xgboost_summary

prostate_xgboost_p_fr <- plot_vimp_with_fr(prostate_xgboost_summary)
prostate_xgboost_p_fr

prostate_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(prostate_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = prostate_perf_xgboost$c_index,
  brier            = prostate_perf_xgboost$brier,
  censoring_rate   = mean(prostate_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

prostate_plot_data <- rbind(
  prostate_rsf_plot_data,
  prostate_cforest_plot_data,
  prostate_blackboost_plot_data,
  prostate_cox_plot_data,
  prostate_xgboost_plot_data
)


# 3D GRAFİKLER

prostate_p_3d_cindex <- plot_3d_cindex(prostate_plot_data)
prostate_p_3d_cindex

prostate_p_3d_brier <- plot_3d_brier(prostate_plot_data)
prostate_p_3d_brier


# QUADRANT GRAFİKLER

prostate_p_quad_cindex <- plot_quadrant(
  prostate_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
prostate_p_quad_cindex

prostate_p_quad_brier <- plot_quadrant(
  prostate_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
prostate_p_quad_brier


# =============================================================================
# DATADIAVAT1 ANALİZİ
# =============================================================================

dataDIVAT1_data   <- prepare_dataDIVAT1(dataDIVAT1_raw)
dataDIVAT1_task   <- make_task_dataDIVAT1(dataDIVAT1_data)
dataDIVAT1_split  <- data_split(dataDIVAT1_task)
dataDIVAT1_models <- train_models(dataDIVAT1_task, dataDIVAT1_split)


# PERFORMANS

dataDIVAT1_perf_rsf <- measure_performance(dataDIVAT1_models$rsf_model, dataDIVAT1_task, dataDIVAT1_split)
dataDIVAT1_perf_rsf$c_index
dataDIVAT1_perf_rsf$brier
dataDIVAT1_perf_rsf$auc

dataDIVAT1_perf_cforest <- measure_performance(dataDIVAT1_models$cforest_model, dataDIVAT1_task, dataDIVAT1_split)
dataDIVAT1_perf_cforest$c_index
dataDIVAT1_perf_cforest$brier
dataDIVAT1_perf_cforest$auc

dataDIVAT1_perf_blackboost <- measure_performance(dataDIVAT1_models$blackboost_model, dataDIVAT1_task, dataDIVAT1_split)
dataDIVAT1_perf_blackboost$c_index
dataDIVAT1_perf_blackboost$brier
dataDIVAT1_perf_blackboost$auc

dataDIVAT1_perf_cox <- measure_performance(dataDIVAT1_models$cox_model, dataDIVAT1_task, dataDIVAT1_split)
dataDIVAT1_perf_cox$c_index
dataDIVAT1_perf_cox$brier
dataDIVAT1_perf_cox$auc

dataDIVAT1_perf_xgboost <- measure_performance(dataDIVAT1_models$xgboost_model, dataDIVAT1_task, dataDIVAT1_split)
dataDIVAT1_perf_xgboost$c_index
dataDIVAT1_perf_xgboost$brier
dataDIVAT1_perf_xgboost$auc


# GLOBAL AÇIKLAMA

dataDIVAT1_global <- global_explain(dataDIVAT1_task, dataDIVAT1_models)


# RSF

dataDIVAT1_rsf_vimp <- dataDIVAT1_global$vimp_list$rsf_model
dataDIVAT1_rsf_p_vimp <- plot(dataDIVAT1_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT1_rsf_p_vimp

dataDIVAT1_rsf_summary <- calculate_vimp_summary(dataDIVAT1_rsf_vimp, eps = 0)
dataDIVAT1_rsf_summary

dataDIVAT1_rsf_p_fr <- plot_vimp_with_fr(dataDIVAT1_rsf_summary)
dataDIVAT1_rsf_p_fr

dataDIVAT1_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(dataDIVAT1_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT1_perf_rsf$c_index,
  brier            = dataDIVAT1_perf_rsf$brier,
  censoring_rate   = mean(dataDIVAT1_data$event == 0, na.rm = TRUE)
)


# CFOREST

dataDIVAT1_cforest_vimp <- dataDIVAT1_global$vimp_list$cforest_model
dataDIVAT1_cforest_p_vimp <- plot(dataDIVAT1_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT1_cforest_p_vimp

dataDIVAT1_cforest_summary <- calculate_vimp_summary(dataDIVAT1_cforest_vimp, eps = 0)
dataDIVAT1_cforest_summary

dataDIVAT1_cforest_p_fr <- plot_vimp_with_fr(dataDIVAT1_cforest_summary)
dataDIVAT1_cforest_p_fr

dataDIVAT1_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(dataDIVAT1_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT1_perf_cforest$c_index,
  brier            = dataDIVAT1_perf_cforest$brier,
  censoring_rate   = mean(dataDIVAT1_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

dataDIVAT1_blackboost_vimp <- dataDIVAT1_global$vimp_list$blackboost_model
dataDIVAT1_blackboost_p_vimp <- plot(dataDIVAT1_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT1_blackboost_p_vimp

dataDIVAT1_blackboost_summary <- calculate_vimp_summary(dataDIVAT1_blackboost_vimp, eps = 0)
dataDIVAT1_blackboost_summary

dataDIVAT1_blackboost_p_fr <- plot_vimp_with_fr(dataDIVAT1_blackboost_summary)
dataDIVAT1_blackboost_p_fr

dataDIVAT1_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(dataDIVAT1_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT1_perf_blackboost$c_index,
  brier            = dataDIVAT1_perf_blackboost$brier,
  censoring_rate   = mean(dataDIVAT1_data$event == 0, na.rm = TRUE)
)


# COX

dataDIVAT1_cox_vimp <- dataDIVAT1_global$vimp_list$cox_model
dataDIVAT1_cox_p_vimp <- plot(dataDIVAT1_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT1_cox_p_vimp

dataDIVAT1_cox_summary <- calculate_vimp_summary(dataDIVAT1_cox_vimp, eps = 0)
dataDIVAT1_cox_summary

dataDIVAT1_cox_p_fr <- plot_vimp_with_fr(dataDIVAT1_cox_summary)
dataDIVAT1_cox_p_fr

dataDIVAT1_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(dataDIVAT1_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT1_perf_cox$c_index,
  brier            = dataDIVAT1_perf_cox$brier,
  censoring_rate   = mean(dataDIVAT1_data$event == 0, na.rm = TRUE)
)


# XGBOOST

dataDIVAT1_xgboost_vimp <- dataDIVAT1_global$vimp_list$xgboost_model
dataDIVAT1_xgboost_p_vimp <- plot(dataDIVAT1_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT1_xgboost_p_vimp

dataDIVAT1_xgboost_summary <- calculate_vimp_summary(dataDIVAT1_xgboost_vimp, eps = 0)
dataDIVAT1_xgboost_summary

dataDIVAT1_xgboost_p_fr <- plot_vimp_with_fr(dataDIVAT1_xgboost_summary)
dataDIVAT1_xgboost_p_fr

dataDIVAT1_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(dataDIVAT1_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT1_perf_xgboost$c_index,
  brier            = dataDIVAT1_perf_xgboost$brier,
  censoring_rate   = mean(dataDIVAT1_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

dataDIVAT1_plot_data <- rbind(
  dataDIVAT1_rsf_plot_data,
  dataDIVAT1_cforest_plot_data,
  dataDIVAT1_blackboost_plot_data,
  dataDIVAT1_cox_plot_data,
  dataDIVAT1_xgboost_plot_data
)


# 3D GRAFİKLER

dataDIVAT1_p_3d_cindex <- plot_3d_cindex(dataDIVAT1_plot_data)
dataDIVAT1_p_3d_cindex

dataDIVAT1_p_3d_brier <- plot_3d_brier(dataDIVAT1_plot_data)
dataDIVAT1_p_3d_brier


# QUADRANT GRAFİKLER

dataDIVAT1_p_quad_cindex <- plot_quadrant(
  dataDIVAT1_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
dataDIVAT1_p_quad_cindex

dataDIVAT1_p_quad_brier <- plot_quadrant(
  dataDIVAT1_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
dataDIVAT1_p_quad_brier


# =============================================================================
# DATADIAVAT3 ANALİZİ
# =============================================================================

dataDIVAT3_data   <- prepare_dataDIVAT3(dataDIVAT3_raw)
dataDIVAT3_task   <- make_task_dataDIVAT3(dataDIVAT3_data)
dataDIVAT3_split  <- data_split(dataDIVAT3_task)
dataDIVAT3_models <- train_models(dataDIVAT3_task, dataDIVAT3_split)


# PERFORMANS

dataDIVAT3_perf_rsf <- measure_performance(dataDIVAT3_models$rsf_model, dataDIVAT3_task, dataDIVAT3_split)
dataDIVAT3_perf_rsf$c_index
dataDIVAT3_perf_rsf$brier
dataDIVAT3_perf_rsf$auc

dataDIVAT3_perf_cforest <- measure_performance(dataDIVAT3_models$cforest_model, dataDIVAT3_task, dataDIVAT3_split)
dataDIVAT3_perf_cforest$c_index
dataDIVAT3_perf_cforest$brier
dataDIVAT3_perf_cforest$auc

dataDIVAT3_perf_blackboost <- measure_performance(dataDIVAT3_models$blackboost_model, dataDIVAT3_task, dataDIVAT3_split)
dataDIVAT3_perf_blackboost$c_index
dataDIVAT3_perf_blackboost$brier
dataDIVAT3_perf_blackboost$auc

dataDIVAT3_perf_cox <- measure_performance(dataDIVAT3_models$cox_model, dataDIVAT3_task, dataDIVAT3_split)
dataDIVAT3_perf_cox$c_index
dataDIVAT3_perf_cox$brier
dataDIVAT3_perf_cox$auc

dataDIVAT3_perf_xgboost <- measure_performance(dataDIVAT3_models$xgboost_model, dataDIVAT3_task, dataDIVAT3_split)
dataDIVAT3_perf_xgboost$c_index
dataDIVAT3_perf_xgboost$brier
dataDIVAT3_perf_xgboost$auc


# GLOBAL AÇIKLAMA

dataDIVAT3_global <- global_explain(dataDIVAT3_task, dataDIVAT3_models)


# RSF

dataDIVAT3_rsf_vimp <- dataDIVAT3_global$vimp_list$rsf_model
dataDIVAT3_rsf_p_vimp <- plot(dataDIVAT3_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT3_rsf_p_vimp

dataDIVAT3_rsf_summary <- calculate_vimp_summary(dataDIVAT3_rsf_vimp, eps = 0)
dataDIVAT3_rsf_summary

dataDIVAT3_rsf_p_fr <- plot_vimp_with_fr(dataDIVAT3_rsf_summary)
dataDIVAT3_rsf_p_fr

dataDIVAT3_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(dataDIVAT3_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT3_perf_rsf$c_index,
  brier            = dataDIVAT3_perf_rsf$brier,
  censoring_rate   = mean(dataDIVAT3_data$event == 0, na.rm = TRUE)
)


# CFOREST

dataDIVAT3_cforest_vimp <- dataDIVAT3_global$vimp_list$cforest_model
dataDIVAT3_cforest_p_vimp <- plot(dataDIVAT3_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT3_cforest_p_vimp

dataDIVAT3_cforest_summary <- calculate_vimp_summary(dataDIVAT3_cforest_vimp, eps = 0)
dataDIVAT3_cforest_summary

dataDIVAT3_cforest_p_fr <- plot_vimp_with_fr(dataDIVAT3_cforest_summary)
dataDIVAT3_cforest_p_fr

dataDIVAT3_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(dataDIVAT3_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT3_perf_cforest$c_index,
  brier            = dataDIVAT3_perf_cforest$brier,
  censoring_rate   = mean(dataDIVAT3_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

dataDIVAT3_blackboost_vimp <- dataDIVAT3_global$vimp_list$blackboost_model
dataDIVAT3_blackboost_p_vimp <- plot(dataDIVAT3_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT3_blackboost_p_vimp

dataDIVAT3_blackboost_summary <- calculate_vimp_summary(dataDIVAT3_blackboost_vimp, eps = 0)
dataDIVAT3_blackboost_summary

dataDIVAT3_blackboost_p_fr <- plot_vimp_with_fr(dataDIVAT3_blackboost_summary)
dataDIVAT3_blackboost_p_fr

dataDIVAT3_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(dataDIVAT3_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT3_perf_blackboost$c_index,
  brier            = dataDIVAT3_perf_blackboost$brier,
  censoring_rate   = mean(dataDIVAT3_data$event == 0, na.rm = TRUE)
)


# COX

dataDIVAT3_cox_vimp <- dataDIVAT3_global$vimp_list$cox_model
dataDIVAT3_cox_p_vimp <- plot(dataDIVAT3_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT3_cox_p_vimp

dataDIVAT3_cox_summary <- calculate_vimp_summary(dataDIVAT3_cox_vimp, eps = 0)
dataDIVAT3_cox_summary

dataDIVAT3_cox_p_fr <- plot_vimp_with_fr(dataDIVAT3_cox_summary)
dataDIVAT3_cox_p_fr

dataDIVAT3_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(dataDIVAT3_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT3_perf_cox$c_index,
  brier            = dataDIVAT3_perf_cox$brier,
  censoring_rate   = mean(dataDIVAT3_data$event == 0, na.rm = TRUE)
)


# XGBOOST

dataDIVAT3_xgboost_vimp <- dataDIVAT3_global$vimp_list$xgboost_model
dataDIVAT3_xgboost_p_vimp <- plot(dataDIVAT3_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
dataDIVAT3_xgboost_p_vimp

dataDIVAT3_xgboost_summary <- calculate_vimp_summary(dataDIVAT3_xgboost_vimp, eps = 0)
dataDIVAT3_xgboost_summary

dataDIVAT3_xgboost_p_fr <- plot_vimp_with_fr(dataDIVAT3_xgboost_summary)
dataDIVAT3_xgboost_p_fr

dataDIVAT3_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(dataDIVAT3_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = dataDIVAT3_perf_xgboost$c_index,
  brier            = dataDIVAT3_perf_xgboost$brier,
  censoring_rate   = mean(dataDIVAT3_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

dataDIVAT3_plot_data <- rbind(
  dataDIVAT3_rsf_plot_data,
  dataDIVAT3_cforest_plot_data,
  dataDIVAT3_blackboost_plot_data,
  dataDIVAT3_cox_plot_data,
  dataDIVAT3_xgboost_plot_data
)


# 3D GRAFİKLER

dataDIVAT3_p_3d_cindex <- plot_3d_cindex(dataDIVAT3_plot_data)
dataDIVAT3_p_3d_cindex

dataDIVAT3_p_3d_brier <- plot_3d_brier(dataDIVAT3_plot_data)
dataDIVAT3_p_3d_brier


# QUADRANT GRAFİKLER

dataDIVAT3_p_quad_cindex <- plot_quadrant(
  dataDIVAT3_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
dataDIVAT3_p_quad_cindex

dataDIVAT3_p_quad_brier <- plot_quadrant(
  dataDIVAT3_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
dataDIVAT3_p_quad_brier



# =============================================================================
# TRACE ANALİZİ
# =============================================================================

TRACE_data   <- prepare_TRACE(TRACE_raw)
TRACE_task   <- make_task_TRACE(TRACE_data)
TRACE_split  <- data_split(TRACE_task)
TRACE_models <- train_models(TRACE_task, TRACE_split)


# PERFORMANS

TRACE_perf_rsf <- measure_performance(TRACE_models$rsf_model, TRACE_task, TRACE_split)
TRACE_perf_rsf$c_index
TRACE_perf_rsf$brier
TRACE_perf_rsf$auc

TRACE_perf_cforest <- measure_performance(TRACE_models$cforest_model, TRACE_task, TRACE_split)
TRACE_perf_cforest$c_index
TRACE_perf_cforest$brier
TRACE_perf_cforest$auc

TRACE_perf_blackboost <- measure_performance(TRACE_models$blackboost_model, TRACE_task, TRACE_split)
TRACE_perf_blackboost$c_index
TRACE_perf_blackboost$brier
TRACE_perf_blackboost$auc

TRACE_perf_cox <- measure_performance(TRACE_models$cox_model, TRACE_task, TRACE_split)
TRACE_perf_cox$c_index
TRACE_perf_cox$brier
TRACE_perf_cox$auc

TRACE_perf_xgboost <- measure_performance(TRACE_models$xgboost_model, TRACE_task, TRACE_split)
TRACE_perf_xgboost$c_index
TRACE_perf_xgboost$brier
TRACE_perf_xgboost$auc


# GLOBAL AÇIKLAMA

TRACE_global <- global_explain(TRACE_task, TRACE_models)


# RSF

TRACE_rsf_vimp <- TRACE_global$vimp_list$rsf_model
TRACE_rsf_p_vimp <- plot(TRACE_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
TRACE_rsf_p_vimp

TRACE_rsf_summary <- calculate_vimp_summary(TRACE_rsf_vimp, eps = 0)
TRACE_rsf_summary

TRACE_rsf_p_fr <- plot_vimp_with_fr(TRACE_rsf_summary)
TRACE_rsf_p_fr

TRACE_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(TRACE_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = TRACE_perf_rsf$c_index,
  brier            = TRACE_perf_rsf$brier,
  censoring_rate   = mean(TRACE_data$event == 0, na.rm = TRUE)
)


# CFOREST

TRACE_cforest_vimp <- TRACE_global$vimp_list$cforest_model
TRACE_cforest_p_vimp <- plot(TRACE_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
TRACE_cforest_p_vimp

TRACE_cforest_summary <- calculate_vimp_summary(TRACE_cforest_vimp, eps = 0)
TRACE_cforest_summary

TRACE_cforest_p_fr <- plot_vimp_with_fr(TRACE_cforest_summary)
TRACE_cforest_p_fr

TRACE_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(TRACE_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = TRACE_perf_cforest$c_index,
  brier            = TRACE_perf_cforest$brier,
  censoring_rate   = mean(TRACE_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

TRACE_blackboost_vimp <- TRACE_global$vimp_list$blackboost_model
TRACE_blackboost_p_vimp <- plot(TRACE_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
TRACE_blackboost_p_vimp

TRACE_blackboost_summary <- calculate_vimp_summary(TRACE_blackboost_vimp, eps = 0)
TRACE_blackboost_summary

TRACE_blackboost_p_fr <- plot_vimp_with_fr(TRACE_blackboost_summary)
TRACE_blackboost_p_fr

TRACE_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(TRACE_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = TRACE_perf_blackboost$c_index,
  brier            = TRACE_perf_blackboost$brier,
  censoring_rate   = mean(TRACE_data$event == 0, na.rm = TRUE)
)


# COX

TRACE_cox_vimp <- TRACE_global$vimp_list$cox_model
TRACE_cox_p_vimp <- plot(TRACE_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
TRACE_cox_p_vimp

TRACE_cox_summary <- calculate_vimp_summary(TRACE_cox_vimp, eps = 0)
TRACE_cox_summary

TRACE_cox_p_fr <- plot_vimp_with_fr(TRACE_cox_summary)
TRACE_cox_p_fr

TRACE_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(TRACE_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = TRACE_perf_cox$c_index,
  brier            = TRACE_perf_cox$brier,
  censoring_rate   = mean(TRACE_data$event == 0, na.rm = TRUE)
)


# XGBOOST

TRACE_xgboost_vimp <- TRACE_global$vimp_list$xgboost_model
TRACE_xgboost_p_vimp <- plot(TRACE_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
TRACE_xgboost_p_vimp

TRACE_xgboost_summary <- calculate_vimp_summary(TRACE_xgboost_vimp, eps = 0)
TRACE_xgboost_summary

TRACE_xgboost_p_fr <- plot_vimp_with_fr(TRACE_xgboost_summary)
TRACE_xgboost_p_fr

TRACE_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(TRACE_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = TRACE_perf_xgboost$c_index,
  brier            = TRACE_perf_xgboost$brier,
  censoring_rate   = mean(TRACE_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

TRACE_plot_data <- rbind(
  TRACE_rsf_plot_data,
  TRACE_cforest_plot_data,
  TRACE_blackboost_plot_data,
  TRACE_cox_plot_data,
  TRACE_xgboost_plot_data
)


# 3D GRAFİKLER

TRACE_p_3d_cindex <- plot_3d_cindex(TRACE_plot_data)
TRACE_p_3d_cindex

TRACE_p_3d_brier <- plot_3d_brier(TRACE_plot_data)
TRACE_p_3d_brier


# QUADRANT GRAFİKLER

TRACE_p_quad_cindex <- plot_quadrant(
  TRACE_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
TRACE_p_quad_cindex

TRACE_p_quad_brier <- plot_quadrant(
  TRACE_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
TRACE_p_quad_brier


# =============================================================================
# FRTCS ANALİZİ
# =============================================================================

FRTCS_data   <- prepare_FRTCS(FRTCS_raw)
FRTCS_task   <- make_task_FRTCS(FRTCS_data)
FRTCS_split  <- data_split(FRTCS_task)
FRTCS_models <- train_models(FRTCS_task, FRTCS_split)


# PERFORMANS

FRTCS_perf_rsf <- measure_performance(FRTCS_models$rsf_model, FRTCS_task, FRTCS_split)
FRTCS_perf_rsf$c_index
FRTCS_perf_rsf$brier
FRTCS_perf_rsf$auc

FRTCS_perf_cforest <- measure_performance(FRTCS_models$cforest_model, FRTCS_task, FRTCS_split)
FRTCS_perf_cforest$c_index
FRTCS_perf_cforest$brier
FRTCS_perf_cforest$auc

FRTCS_perf_blackboost <- measure_performance(FRTCS_models$blackboost_model, FRTCS_task, FRTCS_split)
FRTCS_perf_blackboost$c_index
FRTCS_perf_blackboost$brier
FRTCS_perf_blackboost$auc

FRTCS_perf_cox <- measure_performance(FRTCS_models$cox_model, FRTCS_task, FRTCS_split)
FRTCS_perf_cox$c_index
FRTCS_perf_cox$brier
FRTCS_perf_cox$auc

FRTCS_perf_xgboost <- measure_performance(FRTCS_models$xgboost_model, FRTCS_task, FRTCS_split)
FRTCS_perf_xgboost$c_index
FRTCS_perf_xgboost$brier
FRTCS_perf_xgboost$auc


# GLOBAL AÇIKLAMA

FRTCS_global <- global_explain(FRTCS_task, FRTCS_models)


# RSF

FRTCS_rsf_vimp <- FRTCS_global$vimp_list$rsf_model
FRTCS_rsf_p_vimp <- plot(FRTCS_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
FRTCS_rsf_p_vimp

FRTCS_rsf_summary <- calculate_vimp_summary(FRTCS_rsf_vimp, eps = 0)
FRTCS_rsf_summary

FRTCS_rsf_p_fr <- plot_vimp_with_fr(FRTCS_rsf_summary)
FRTCS_rsf_p_fr

FRTCS_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(FRTCS_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = FRTCS_perf_rsf$c_index,
  brier            = FRTCS_perf_rsf$brier,
  censoring_rate   = mean(FRTCS_data$event == 0, na.rm = TRUE)
)


# CFOREST

FRTCS_cforest_vimp <- FRTCS_global$vimp_list$cforest_model
FRTCS_cforest_p_vimp <- plot(FRTCS_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
FRTCS_cforest_p_vimp

FRTCS_cforest_summary <- calculate_vimp_summary(FRTCS_cforest_vimp, eps = 0)
FRTCS_cforest_summary

FRTCS_cforest_p_fr <- plot_vimp_with_fr(FRTCS_cforest_summary)
FRTCS_cforest_p_fr

FRTCS_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(FRTCS_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = FRTCS_perf_cforest$c_index,
  brier            = FRTCS_perf_cforest$brier,
  censoring_rate   = mean(FRTCS_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

FRTCS_blackboost_vimp <- FRTCS_global$vimp_list$blackboost_model
FRTCS_blackboost_p_vimp <- plot(FRTCS_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
FRTCS_blackboost_p_vimp

FRTCS_blackboost_summary <- calculate_vimp_summary(FRTCS_blackboost_vimp, eps = 0)
FRTCS_blackboost_summary

FRTCS_blackboost_p_fr <- plot_vimp_with_fr(FRTCS_blackboost_summary)
FRTCS_blackboost_p_fr

FRTCS_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(FRTCS_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = FRTCS_perf_blackboost$c_index,
  brier            = FRTCS_perf_blackboost$brier,
  censoring_rate   = mean(FRTCS_data$event == 0, na.rm = TRUE)
)


# COX

FRTCS_cox_vimp <- FRTCS_global$vimp_list$cox_model
FRTCS_cox_p_vimp <- plot(FRTCS_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
FRTCS_cox_p_vimp

FRTCS_cox_summary <- calculate_vimp_summary(FRTCS_cox_vimp, eps = 0)
FRTCS_cox_summary

FRTCS_cox_p_fr <- plot_vimp_with_fr(FRTCS_cox_summary)
FRTCS_cox_p_fr

FRTCS_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(FRTCS_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = FRTCS_perf_cox$c_index,
  brier            = FRTCS_perf_cox$brier,
  censoring_rate   = mean(FRTCS_data$event == 0, na.rm = TRUE)
)


# XGBOOST

FRTCS_xgboost_vimp <- FRTCS_global$vimp_list$xgboost_model
FRTCS_xgboost_p_vimp <- plot(FRTCS_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
FRTCS_xgboost_p_vimp

FRTCS_xgboost_summary <- calculate_vimp_summary(FRTCS_xgboost_vimp, eps = 0)
FRTCS_xgboost_summary

FRTCS_xgboost_p_fr <- plot_vimp_with_fr(FRTCS_xgboost_summary)
FRTCS_xgboost_p_fr

FRTCS_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(FRTCS_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = FRTCS_perf_xgboost$c_index,
  brier            = FRTCS_perf_xgboost$brier,
  censoring_rate   = mean(FRTCS_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

FRTCS_plot_data <- rbind(
  FRTCS_rsf_plot_data,
  FRTCS_cforest_plot_data,
  FRTCS_blackboost_plot_data,
  FRTCS_cox_plot_data,
  FRTCS_xgboost_plot_data
)


# 3D GRAFİKLER

FRTCS_p_3d_cindex <- plot_3d_cindex(FRTCS_plot_data)
FRTCS_p_3d_cindex

FRTCS_p_3d_brier <- plot_3d_brier(FRTCS_plot_data)
FRTCS_p_3d_brier


# QUADRANT GRAFİKLER

FRTCS_p_quad_cindex <- plot_quadrant(
  FRTCS_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
FRTCS_p_quad_cindex

FRTCS_p_quad_brier <- plot_quadrant(
  FRTCS_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
FRTCS_p_quad_brier


# =============================================================================
# ZINC ANALİZİ
# =============================================================================

zinc_data   <- prepare_zinc(zinc_raw)
zinc_task   <- make_task_zinc(zinc_data)
zinc_split  <- data_split(zinc_task)
zinc_models <- train_models(zinc_task, zinc_split)


# PERFORMANS

zinc_perf_rsf <- measure_performance(zinc_models$rsf_model, zinc_task, zinc_split)
zinc_perf_rsf$c_index
zinc_perf_rsf$brier
zinc_perf_rsf$auc

zinc_perf_cforest <- measure_performance(zinc_models$cforest_model, zinc_task, zinc_split)
zinc_perf_cforest$c_index
zinc_perf_cforest$brier
zinc_perf_cforest$auc

zinc_perf_blackboost <- measure_performance(zinc_models$blackboost_model, zinc_task, zinc_split)
zinc_perf_blackboost$c_index
zinc_perf_blackboost$brier
zinc_perf_blackboost$auc

zinc_perf_cox <- measure_performance(zinc_models$cox_model, zinc_task, zinc_split)
zinc_perf_cox$c_index
zinc_perf_cox$brier
zinc_perf_cox$auc

zinc_perf_xgboost <- measure_performance(zinc_models$xgboost_model, zinc_task, zinc_split)
zinc_perf_xgboost$c_index
zinc_perf_xgboost$brier
zinc_perf_xgboost$auc


# GLOBAL AÇIKLAMA

zinc_global <- global_explain(zinc_task, zinc_models)


# RSF

zinc_rsf_vimp <- zinc_global$vimp_list$rsf_model
zinc_rsf_p_vimp <- plot(zinc_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
zinc_rsf_p_vimp

zinc_rsf_summary <- calculate_vimp_summary(zinc_rsf_vimp, eps = 0)
zinc_rsf_summary

zinc_rsf_p_fr <- plot_vimp_with_fr(zinc_rsf_summary)
zinc_rsf_p_fr

zinc_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(zinc_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = zinc_perf_rsf$c_index,
  brier            = zinc_perf_rsf$brier,
  censoring_rate   = mean(zinc_data$event == 0, na.rm = TRUE)
)


# CFOREST

zinc_cforest_vimp <- zinc_global$vimp_list$cforest_model
zinc_cforest_p_vimp <- plot(zinc_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
zinc_cforest_p_vimp

zinc_cforest_summary <- calculate_vimp_summary(zinc_cforest_vimp, eps = 0)
zinc_cforest_summary

zinc_cforest_p_fr <- plot_vimp_with_fr(zinc_cforest_summary)
zinc_cforest_p_fr

zinc_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(zinc_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = zinc_perf_cforest$c_index,
  brier            = zinc_perf_cforest$brier,
  censoring_rate   = mean(zinc_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

zinc_blackboost_vimp <- zinc_global$vimp_list$blackboost_model
zinc_blackboost_p_vimp <- plot(zinc_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
zinc_blackboost_p_vimp

zinc_blackboost_summary <- calculate_vimp_summary(zinc_blackboost_vimp, eps = 0)
zinc_blackboost_summary

zinc_blackboost_p_fr <- plot_vimp_with_fr(zinc_blackboost_summary)
zinc_blackboost_p_fr

zinc_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(zinc_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = zinc_perf_blackboost$c_index,
  brier            = zinc_perf_blackboost$brier,
  censoring_rate   = mean(zinc_data$event == 0, na.rm = TRUE)
)


# COX

zinc_cox_vimp <- zinc_global$vimp_list$cox_model
zinc_cox_p_vimp <- plot(zinc_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
zinc_cox_p_vimp

zinc_cox_summary <- calculate_vimp_summary(zinc_cox_vimp, eps = 0)
zinc_cox_summary

zinc_cox_p_fr <- plot_vimp_with_fr(zinc_cox_summary)
zinc_cox_p_fr

zinc_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(zinc_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = zinc_perf_cox$c_index,
  brier            = zinc_perf_cox$brier,
  censoring_rate   = mean(zinc_data$event == 0, na.rm = TRUE)
)


# XGBOOST

zinc_xgboost_vimp <- zinc_global$vimp_list$xgboost_model
zinc_xgboost_p_vimp <- plot(zinc_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
zinc_xgboost_p_vimp

zinc_xgboost_summary <- calculate_vimp_summary(zinc_xgboost_vimp, eps = 0)
zinc_xgboost_summary

zinc_xgboost_p_fr <- plot_vimp_with_fr(zinc_xgboost_summary)
zinc_xgboost_p_fr

zinc_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(zinc_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = zinc_perf_xgboost$c_index,
  brier            = zinc_perf_xgboost$brier,
  censoring_rate   = mean(zinc_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

zinc_plot_data <- rbind(
  zinc_rsf_plot_data,
  zinc_cforest_plot_data,
  zinc_blackboost_plot_data,
  zinc_cox_plot_data,
  zinc_xgboost_plot_data
)


# 3D GRAFİKLER

zinc_p_3d_cindex <- plot_3d_cindex(zinc_plot_data)
zinc_p_3d_cindex

zinc_p_3d_brier <- plot_3d_brier(zinc_plot_data)
zinc_p_3d_brier


# QUADRANT GRAFİKLER

zinc_p_quad_cindex <- plot_quadrant(
  zinc_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
zinc_p_quad_cindex

zinc_p_quad_brier <- plot_quadrant(
  zinc_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
zinc_p_quad_brier


# =============================================================================
# STAGEC ANALİZİ
# =============================================================================

stagec_data   <- prepare_stagec(stagec_raw)
stagec_task   <- make_task_stagec(stagec_data)
stagec_split  <- data_split(stagec_task)
stagec_models <- train_models(stagec_task, stagec_split)


# PERFORMANS

stagec_perf_rsf <- measure_performance(stagec_models$rsf_model, stagec_task, stagec_split)
stagec_perf_rsf$c_index
stagec_perf_rsf$brier
stagec_perf_rsf$auc

stagec_perf_cforest <- measure_performance(stagec_models$cforest_model, stagec_task, stagec_split)
stagec_perf_cforest$c_index
stagec_perf_cforest$brier
stagec_perf_cforest$auc

stagec_perf_blackboost <- measure_performance(stagec_models$blackboost_model, stagec_task, stagec_split)
stagec_perf_blackboost$c_index
stagec_perf_blackboost$brier
stagec_perf_blackboost$auc

stagec_perf_cox <- measure_performance(stagec_models$cox_model, stagec_task, stagec_split)
stagec_perf_cox$c_index
stagec_perf_cox$brier
stagec_perf_cox$auc

stagec_perf_xgboost <- measure_performance(stagec_models$xgboost_model, stagec_task, stagec_split)
stagec_perf_xgboost$c_index
stagec_perf_xgboost$brier
stagec_perf_xgboost$auc


# GLOBAL AÇIKLAMA

stagec_global <- global_explain(stagec_task, stagec_models)


# RSF

stagec_rsf_vimp <- stagec_global$vimp_list$rsf_model
stagec_rsf_p_vimp <- plot(stagec_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
stagec_rsf_p_vimp

stagec_rsf_summary <- calculate_vimp_summary(stagec_rsf_vimp, eps = 0)
stagec_rsf_summary

stagec_rsf_p_fr <- plot_vimp_with_fr(stagec_rsf_summary)
stagec_rsf_p_fr

stagec_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(stagec_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = stagec_perf_rsf$c_index,
  brier            = stagec_perf_rsf$brier,
  censoring_rate   = mean(stagec_data$event == 0, na.rm = TRUE)
)


# CFOREST

stagec_cforest_vimp <- stagec_global$vimp_list$cforest_model
stagec_cforest_p_vimp <- plot(stagec_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
stagec_cforest_p_vimp

stagec_cforest_summary <- calculate_vimp_summary(stagec_cforest_vimp, eps = 0)
stagec_cforest_summary

stagec_cforest_p_fr <- plot_vimp_with_fr(stagec_cforest_summary)
stagec_cforest_p_fr

stagec_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(stagec_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = stagec_perf_cforest$c_index,
  brier            = stagec_perf_cforest$brier,
  censoring_rate   = mean(stagec_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

stagec_blackboost_vimp <- stagec_global$vimp_list$blackboost_model
stagec_blackboost_p_vimp <- plot(stagec_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
stagec_blackboost_p_vimp

stagec_blackboost_summary <- calculate_vimp_summary(stagec_blackboost_vimp, eps = 0)
stagec_blackboost_summary

stagec_blackboost_p_fr <- plot_vimp_with_fr(stagec_blackboost_summary)
stagec_blackboost_p_fr

stagec_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(stagec_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = stagec_perf_blackboost$c_index,
  brier            = stagec_perf_blackboost$brier,
  censoring_rate   = mean(stagec_data$event == 0, na.rm = TRUE)
)


# COX

stagec_cox_vimp <- stagec_global$vimp_list$cox_model
stagec_cox_p_vimp <- plot(stagec_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
stagec_cox_p_vimp

stagec_cox_summary <- calculate_vimp_summary(stagec_cox_vimp, eps = 0)
stagec_cox_summary

stagec_cox_p_fr <- plot_vimp_with_fr(stagec_cox_summary)
stagec_cox_p_fr

stagec_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(stagec_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = stagec_perf_cox$c_index,
  brier            = stagec_perf_cox$brier,
  censoring_rate   = mean(stagec_data$event == 0, na.rm = TRUE)
)


# XGBOOST

stagec_xgboost_vimp <- stagec_global$vimp_list$xgboost_model
stagec_xgboost_p_vimp <- plot(stagec_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
stagec_xgboost_p_vimp

stagec_xgboost_summary <- calculate_vimp_summary(stagec_xgboost_vimp, eps = 0)
stagec_xgboost_summary

stagec_xgboost_p_fr <- plot_vimp_with_fr(stagec_xgboost_summary)
stagec_xgboost_p_fr

stagec_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(stagec_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = stagec_perf_xgboost$c_index,
  brier            = stagec_perf_xgboost$brier,
  censoring_rate   = mean(stagec_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

stagec_plot_data <- rbind(
  stagec_rsf_plot_data,
  stagec_cforest_plot_data,
  stagec_blackboost_plot_data,
  stagec_cox_plot_data,
  stagec_xgboost_plot_data
)


# 3D GRAFİKLER

stagec_p_3d_cindex <- plot_3d_cindex(stagec_plot_data)
stagec_p_3d_cindex

stagec_p_3d_brier <- plot_3d_brier(stagec_plot_data)
stagec_p_3d_brier


# QUADRANT GRAFİKLER

stagec_p_quad_cindex <- plot_quadrant(
  stagec_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
stagec_p_quad_cindex

stagec_p_quad_brier <- plot_quadrant(
  stagec_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
stagec_p_quad_brier


# =============================================================================
# LEUKSURV ANALİZİ
# =============================================================================

LeukSurv_data   <- prepare_LeukSurv(LeukSurv_raw)
LeukSurv_task   <- make_task_LeukSurv(LeukSurv_data)
LeukSurv_split  <- data_split(LeukSurv_task)
LeukSurv_models <- train_models(LeukSurv_task, LeukSurv_split)


# PERFORMANS

LeukSurv_perf_rsf <- measure_performance(LeukSurv_models$rsf_model, LeukSurv_task, LeukSurv_split)
LeukSurv_perf_rsf$c_index
LeukSurv_perf_rsf$brier
LeukSurv_perf_rsf$auc

LeukSurv_perf_cforest <- measure_performance(LeukSurv_models$cforest_model, LeukSurv_task, LeukSurv_split)
LeukSurv_perf_cforest$c_index
LeukSurv_perf_cforest$brier
LeukSurv_perf_cforest$auc

LeukSurv_perf_blackboost <- measure_performance(LeukSurv_models$blackboost_model, LeukSurv_task, LeukSurv_split)
LeukSurv_perf_blackboost$c_index
LeukSurv_perf_blackboost$brier
LeukSurv_perf_blackboost$auc

LeukSurv_perf_cox <- measure_performance(LeukSurv_models$cox_model, LeukSurv_task, LeukSurv_split)
LeukSurv_perf_cox$c_index
LeukSurv_perf_cox$brier
LeukSurv_perf_cox$auc

LeukSurv_perf_xgboost <- measure_performance(LeukSurv_models$xgboost_model, LeukSurv_task, LeukSurv_split)
LeukSurv_perf_xgboost$c_index
LeukSurv_perf_xgboost$brier
LeukSurv_perf_xgboost$auc


# GLOBAL AÇIKLAMA

LeukSurv_global <- global_explain(LeukSurv_task, LeukSurv_models)


# RSF

LeukSurv_rsf_vimp <- LeukSurv_global$vimp_list$rsf_model
LeukSurv_rsf_p_vimp <- plot(LeukSurv_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
LeukSurv_rsf_p_vimp

LeukSurv_rsf_summary <- calculate_vimp_summary(LeukSurv_rsf_vimp, eps = 0)
LeukSurv_rsf_summary

LeukSurv_rsf_p_fr <- plot_vimp_with_fr(LeukSurv_rsf_summary)
LeukSurv_rsf_p_fr

LeukSurv_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(LeukSurv_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = LeukSurv_perf_rsf$c_index,
  brier            = LeukSurv_perf_rsf$brier,
  censoring_rate   = mean(LeukSurv_data$event == 0, na.rm = TRUE)
)


# CFOREST

LeukSurv_cforest_vimp <- LeukSurv_global$vimp_list$cforest_model
LeukSurv_cforest_p_vimp <- plot(LeukSurv_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
LeukSurv_cforest_p_vimp

LeukSurv_cforest_summary <- calculate_vimp_summary(LeukSurv_cforest_vimp, eps = 0)
LeukSurv_cforest_summary

LeukSurv_cforest_p_fr <- plot_vimp_with_fr(LeukSurv_cforest_summary)
LeukSurv_cforest_p_fr

LeukSurv_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(LeukSurv_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = LeukSurv_perf_cforest$c_index,
  brier            = LeukSurv_perf_cforest$brier,
  censoring_rate   = mean(LeukSurv_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

LeukSurv_blackboost_vimp <- LeukSurv_global$vimp_list$blackboost_model
LeukSurv_blackboost_p_vimp <- plot(LeukSurv_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
LeukSurv_blackboost_p_vimp

LeukSurv_blackboost_summary <- calculate_vimp_summary(LeukSurv_blackboost_vimp, eps = 0)
LeukSurv_blackboost_summary

LeukSurv_blackboost_p_fr <- plot_vimp_with_fr(LeukSurv_blackboost_summary)
LeukSurv_blackboost_p_fr

LeukSurv_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(LeukSurv_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = LeukSurv_perf_blackboost$c_index,
  brier            = LeukSurv_perf_blackboost$brier,
  censoring_rate   = mean(LeukSurv_data$event == 0, na.rm = TRUE)
)


# COX

LeukSurv_cox_vimp <- LeukSurv_global$vimp_list$cox_model
LeukSurv_cox_p_vimp <- plot(LeukSurv_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
LeukSurv_cox_p_vimp

LeukSurv_cox_summary <- calculate_vimp_summary(LeukSurv_cox_vimp, eps = 0)
LeukSurv_cox_summary

LeukSurv_cox_p_fr <- plot_vimp_with_fr(LeukSurv_cox_summary)
LeukSurv_cox_p_fr

LeukSurv_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(LeukSurv_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = LeukSurv_perf_cox$c_index,
  brier            = LeukSurv_perf_cox$brier,
  censoring_rate   = mean(LeukSurv_data$event == 0, na.rm = TRUE)
)


# XGBOOST

LeukSurv_xgboost_vimp <- LeukSurv_global$vimp_list$xgboost_model
LeukSurv_xgboost_p_vimp <- plot(LeukSurv_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
LeukSurv_xgboost_p_vimp

LeukSurv_xgboost_summary <- calculate_vimp_summary(LeukSurv_xgboost_vimp, eps = 0)
LeukSurv_xgboost_summary

LeukSurv_xgboost_p_fr <- plot_vimp_with_fr(LeukSurv_xgboost_summary)
LeukSurv_xgboost_p_fr

LeukSurv_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(LeukSurv_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = LeukSurv_perf_xgboost$c_index,
  brier            = LeukSurv_perf_xgboost$brier,
  censoring_rate   = mean(LeukSurv_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

LeukSurv_plot_data <- rbind(
  LeukSurv_rsf_plot_data,
  LeukSurv_cforest_plot_data,
  LeukSurv_blackboost_plot_data,
  LeukSurv_cox_plot_data,
  LeukSurv_xgboost_plot_data
)


# 3D GRAFİKLER

LeukSurv_p_3d_cindex <- plot_3d_cindex(LeukSurv_plot_data)
LeukSurv_p_3d_cindex

LeukSurv_p_3d_brier <- plot_3d_brier(LeukSurv_plot_data)
LeukSurv_p_3d_brier


# QUADRANT GRAFİKLER

LeukSurv_p_quad_cindex <- plot_quadrant(
  LeukSurv_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
LeukSurv_p_quad_cindex

LeukSurv_p_quad_brier <- plot_quadrant(
  LeukSurv_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
LeukSurv_p_quad_brier


# =============================================================================
# NWTCO ANALİZİ
# =============================================================================

nwtco_data   <- prepare_nwtco(nwtco_raw)
nwtco_task   <- make_task_nwtco(nwtco_data)
nwtco_split  <- data_split(nwtco_task)
nwtco_models <- train_models(nwtco_task, nwtco_split)


# PERFORMANS

nwtco_perf_rsf <- measure_performance(nwtco_models$rsf_model, nwtco_task, nwtco_split)
nwtco_perf_rsf$c_index
nwtco_perf_rsf$brier
nwtco_perf_rsf$auc

nwtco_perf_cforest <- measure_performance(nwtco_models$cforest_model, nwtco_task, nwtco_split)
nwtco_perf_cforest$c_index
nwtco_perf_cforest$brier
nwtco_perf_cforest$auc

nwtco_perf_blackboost <- measure_performance(nwtco_models$blackboost_model, nwtco_task, nwtco_split)
nwtco_perf_blackboost$c_index
nwtco_perf_blackboost$brier
nwtco_perf_blackboost$auc

nwtco_perf_cox <- measure_performance(nwtco_models$cox_model, nwtco_task, nwtco_split)
nwtco_perf_cox$c_index
nwtco_perf_cox$brier
nwtco_perf_cox$auc

nwtco_perf_xgboost <- measure_performance(nwtco_models$xgboost_model, nwtco_task, nwtco_split)
nwtco_perf_xgboost$c_index
nwtco_perf_xgboost$brier
nwtco_perf_xgboost$auc


# GLOBAL AÇIKLAMA

nwtco_global <- global_explain(nwtco_task, nwtco_models)


# RSF

nwtco_rsf_vimp <- nwtco_global$vimp_list$rsf_model
nwtco_rsf_p_vimp <- plot(nwtco_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
nwtco_rsf_p_vimp

nwtco_rsf_summary <- calculate_vimp_summary(nwtco_rsf_vimp, eps = 0)
nwtco_rsf_summary

nwtco_rsf_p_fr <- plot_vimp_with_fr(nwtco_rsf_summary)
nwtco_rsf_p_fr

nwtco_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(nwtco_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = nwtco_perf_rsf$c_index,
  brier            = nwtco_perf_rsf$brier,
  censoring_rate   = mean(nwtco_data$event == 0, na.rm = TRUE)
)


# CFOREST

nwtco_cforest_vimp <- nwtco_global$vimp_list$cforest_model
nwtco_cforest_p_vimp <- plot(nwtco_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
nwtco_cforest_p_vimp

nwtco_cforest_summary <- calculate_vimp_summary(nwtco_cforest_vimp, eps = 0)
nwtco_cforest_summary

nwtco_cforest_p_fr <- plot_vimp_with_fr(nwtco_cforest_summary)
nwtco_cforest_p_fr

nwtco_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(nwtco_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = nwtco_perf_cforest$c_index,
  brier            = nwtco_perf_cforest$brier,
  censoring_rate   = mean(nwtco_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

nwtco_blackboost_vimp <- nwtco_global$vimp_list$blackboost_model
nwtco_blackboost_p_vimp <- plot(nwtco_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
nwtco_blackboost_p_vimp

nwtco_blackboost_summary <- calculate_vimp_summary(nwtco_blackboost_vimp, eps = 0)
nwtco_blackboost_summary

nwtco_blackboost_p_fr <- plot_vimp_with_fr(nwtco_blackboost_summary)
nwtco_blackboost_p_fr

nwtco_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(nwtco_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = nwtco_perf_blackboost$c_index,
  brier            = nwtco_perf_blackboost$brier,
  censoring_rate   = mean(nwtco_data$event == 0, na.rm = TRUE)
)


# COX

nwtco_cox_vimp <- nwtco_global$vimp_list$cox_model
nwtco_cox_p_vimp <- plot(nwtco_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
nwtco_cox_p_vimp

nwtco_cox_summary <- calculate_vimp_summary(nwtco_cox_vimp, eps = 0)
nwtco_cox_summary

nwtco_cox_p_fr <- plot_vimp_with_fr(nwtco_cox_summary)
nwtco_cox_p_fr

nwtco_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(nwtco_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = nwtco_perf_cox$c_index,
  brier            = nwtco_perf_cox$brier,
  censoring_rate   = mean(nwtco_data$event == 0, na.rm = TRUE)
)


# XGBOOST

nwtco_xgboost_vimp <- nwtco_global$vimp_list$xgboost_model
nwtco_xgboost_p_vimp <- plot(nwtco_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
nwtco_xgboost_p_vimp

nwtco_xgboost_summary <- calculate_vimp_summary(nwtco_xgboost_vimp, eps = 0)
nwtco_xgboost_summary

nwtco_xgboost_p_fr <- plot_vimp_with_fr(nwtco_xgboost_summary)
nwtco_xgboost_p_fr

nwtco_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(nwtco_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = nwtco_perf_xgboost$c_index,
  brier            = nwtco_perf_xgboost$brier,
  censoring_rate   = mean(nwtco_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

nwtco_plot_data <- rbind(
  nwtco_rsf_plot_data,
  nwtco_cforest_plot_data,
  nwtco_blackboost_plot_data,
  nwtco_cox_plot_data,
  nwtco_xgboost_plot_data
)


# 3D GRAFİKLER

nwtco_p_3d_cindex <- plot_3d_cindex(nwtco_plot_data)
nwtco_p_3d_cindex

nwtco_p_3d_brier <- plot_3d_brier(nwtco_plot_data)
nwtco_p_3d_brier


# QUADRANT GRAFİKLER

nwtco_p_quad_cindex <- plot_quadrant(
  nwtco_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
nwtco_p_quad_cindex

nwtco_p_quad_brier <- plot_quadrant(
  nwtco_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
nwtco_p_quad_brier


# =============================================================================
# RETINOPATHY ANALİZİ
# =============================================================================

retinopathy_data   <- prepare_retinopathy(retinopathy_raw)
retinopathy_task   <- make_task_retinopathy(retinopathy_data)
retinopathy_split  <- data_split(retinopathy_task)
retinopathy_models <- train_models(retinopathy_task, retinopathy_split)


# PERFORMANS

retinopathy_perf_rsf <- measure_performance(retinopathy_models$rsf_model, retinopathy_task, retinopathy_split)
retinopathy_perf_rsf$c_index
retinopathy_perf_rsf$brier
retinopathy_perf_rsf$auc

retinopathy_perf_cforest <- measure_performance(retinopathy_models$cforest_model, retinopathy_task, retinopathy_split)
retinopathy_perf_cforest$c_index
retinopathy_perf_cforest$brier
retinopathy_perf_cforest$auc

retinopathy_perf_blackboost <- measure_performance(retinopathy_models$blackboost_model, retinopathy_task, retinopathy_split)
retinopathy_perf_blackboost$c_index
retinopathy_perf_blackboost$brier
retinopathy_perf_blackboost$auc

retinopathy_perf_cox <- measure_performance(retinopathy_models$cox_model, retinopathy_task, retinopathy_split)
retinopathy_perf_cox$c_index
retinopathy_perf_cox$brier
retinopathy_perf_cox$auc

retinopathy_perf_xgboost <- measure_performance(retinopathy_models$xgboost_model, retinopathy_task, retinopathy_split)
retinopathy_perf_xgboost$c_index
retinopathy_perf_xgboost$brier
retinopathy_perf_xgboost$auc


# GLOBAL AÇIKLAMA

retinopathy_global <- global_explain(retinopathy_task, retinopathy_models)


# RSF

retinopathy_rsf_vimp <- retinopathy_global$vimp_list$rsf_model
retinopathy_rsf_p_vimp <- plot(retinopathy_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
retinopathy_rsf_p_vimp

retinopathy_rsf_summary <- calculate_vimp_summary(retinopathy_rsf_vimp, eps = 0)
retinopathy_rsf_summary

retinopathy_rsf_p_fr <- plot_vimp_with_fr(retinopathy_rsf_summary)
retinopathy_rsf_p_fr

retinopathy_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(retinopathy_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = retinopathy_perf_rsf$c_index,
  brier            = retinopathy_perf_rsf$brier,
  censoring_rate   = mean(retinopathy_data$event == 0, na.rm = TRUE)
)


# CFOREST

retinopathy_cforest_vimp <- retinopathy_global$vimp_list$cforest_model
retinopathy_cforest_p_vimp <- plot(retinopathy_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
retinopathy_cforest_p_vimp

retinopathy_cforest_summary <- calculate_vimp_summary(retinopathy_cforest_vimp, eps = 0)
retinopathy_cforest_summary

retinopathy_cforest_p_fr <- plot_vimp_with_fr(retinopathy_cforest_summary)
retinopathy_cforest_p_fr

retinopathy_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(retinopathy_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = retinopathy_perf_cforest$c_index,
  brier            = retinopathy_perf_cforest$brier,
  censoring_rate   = mean(retinopathy_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

retinopathy_blackboost_vimp <- retinopathy_global$vimp_list$blackboost_model
retinopathy_blackboost_p_vimp <- plot(retinopathy_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
retinopathy_blackboost_p_vimp

retinopathy_blackboost_summary <- calculate_vimp_summary(retinopathy_blackboost_vimp, eps = 0)
retinopathy_blackboost_summary

retinopathy_blackboost_p_fr <- plot_vimp_with_fr(retinopathy_blackboost_summary)
retinopathy_blackboost_p_fr

retinopathy_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(retinopathy_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = retinopathy_perf_blackboost$c_index,
  brier            = retinopathy_perf_blackboost$brier,
  censoring_rate   = mean(retinopathy_data$event == 0, na.rm = TRUE)
)


# COX

retinopathy_cox_vimp <- retinopathy_global$vimp_list$cox_model
retinopathy_cox_p_vimp <- plot(retinopathy_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
retinopathy_cox_p_vimp

retinopathy_cox_summary <- calculate_vimp_summary(retinopathy_cox_vimp, eps = 0)
retinopathy_cox_summary

retinopathy_cox_p_fr <- plot_vimp_with_fr(retinopathy_cox_summary)
retinopathy_cox_p_fr

retinopathy_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(retinopathy_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = retinopathy_perf_cox$c_index,
  brier            = retinopathy_perf_cox$brier,
  censoring_rate   = mean(retinopathy_data$event == 0, na.rm = TRUE)
)


# XGBOOST

retinopathy_xgboost_vimp <- retinopathy_global$vimp_list$xgboost_model
retinopathy_xgboost_p_vimp <- plot(retinopathy_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
retinopathy_xgboost_p_vimp

retinopathy_xgboost_summary <- calculate_vimp_summary(retinopathy_xgboost_vimp, eps = 0)
retinopathy_xgboost_summary

retinopathy_xgboost_p_fr <- plot_vimp_with_fr(retinopathy_xgboost_summary)
retinopathy_xgboost_p_fr

retinopathy_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(retinopathy_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = retinopathy_perf_xgboost$c_index,
  brier            = retinopathy_perf_xgboost$brier,
  censoring_rate   = mean(retinopathy_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

retinopathy_plot_data <- rbind(
  retinopathy_rsf_plot_data,
  retinopathy_cforest_plot_data,
  retinopathy_blackboost_plot_data,
  retinopathy_cox_plot_data,
  retinopathy_xgboost_plot_data
)


# 3D GRAFİKLER

retinopathy_p_3d_cindex <- plot_3d_cindex(retinopathy_plot_data)
retinopathy_p_3d_cindex

retinopathy_p_3d_brier <- plot_3d_brier(retinopathy_plot_data)
retinopathy_p_3d_brier


# QUADRANT GRAFİKLER

retinopathy_p_quad_cindex <- plot_quadrant(
  retinopathy_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
retinopathy_p_quad_cindex

retinopathy_p_quad_brier <- plot_quadrant(
  retinopathy_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
retinopathy_p_quad_brier


# =============================================================================
# FRAMINGHAM ANALİZİ
# =============================================================================

Framingham_data   <- prepare_Framingham(Framingham_raw)
Framingham_task   <- make_task_Framingham(Framingham_data)
Framingham_split  <- data_split(Framingham_task)
Framingham_models <- train_models(Framingham_task, Framingham_split)


# PERFORMANS

Framingham_perf_rsf <- measure_performance(Framingham_models$rsf_model, Framingham_task, Framingham_split)
Framingham_perf_rsf$c_index
Framingham_perf_rsf$brier
Framingham_perf_rsf$auc

Framingham_perf_cforest <- measure_performance(Framingham_models$cforest_model, Framingham_task, Framingham_split)
Framingham_perf_cforest$c_index
Framingham_perf_cforest$brier
Framingham_perf_cforest$auc

Framingham_perf_blackboost <- measure_performance(Framingham_models$blackboost_model, Framingham_task, Framingham_split)
Framingham_perf_blackboost$c_index
Framingham_perf_blackboost$brier
Framingham_perf_blackboost$auc

Framingham_perf_cox <- measure_performance(Framingham_models$cox_model, Framingham_task, Framingham_split)
Framingham_perf_cox$c_index
Framingham_perf_cox$brier
Framingham_perf_cox$auc

Framingham_perf_xgboost <- measure_performance(Framingham_models$xgboost_model, Framingham_task, Framingham_split)
Framingham_perf_xgboost$c_index
Framingham_perf_xgboost$brier
Framingham_perf_xgboost$auc


# GLOBAL AÇIKLAMA

Framingham_global <- global_explain(Framingham_task, Framingham_models)


# RSF

Framingham_rsf_vimp <- Framingham_global$vimp_list$rsf_model
Framingham_rsf_p_vimp <- plot(Framingham_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Framingham_rsf_p_vimp

Framingham_rsf_summary <- calculate_vimp_summary(Framingham_rsf_vimp, eps = 0)
Framingham_rsf_summary

Framingham_rsf_p_fr <- plot_vimp_with_fr(Framingham_rsf_summary)
Framingham_rsf_p_fr

Framingham_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(Framingham_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = Framingham_perf_rsf$c_index,
  brier            = Framingham_perf_rsf$brier,
  censoring_rate   = mean(Framingham_data$event == 0, na.rm = TRUE)
)


# CFOREST

Framingham_cforest_vimp <- Framingham_global$vimp_list$cforest_model
Framingham_cforest_p_vimp <- plot(Framingham_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Framingham_cforest_p_vimp

Framingham_cforest_summary <- calculate_vimp_summary(Framingham_cforest_vimp, eps = 0)
Framingham_cforest_summary

Framingham_cforest_p_fr <- plot_vimp_with_fr(Framingham_cforest_summary)
Framingham_cforest_p_fr

Framingham_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(Framingham_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = Framingham_perf_cforest$c_index,
  brier            = Framingham_perf_cforest$brier,
  censoring_rate   = mean(Framingham_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

Framingham_blackboost_vimp <- Framingham_global$vimp_list$blackboost_model
Framingham_blackboost_p_vimp <- plot(Framingham_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Framingham_blackboost_p_vimp

Framingham_blackboost_summary <- calculate_vimp_summary(Framingham_blackboost_vimp, eps = 0)
Framingham_blackboost_summary

Framingham_blackboost_p_fr <- plot_vimp_with_fr(Framingham_blackboost_summary)
Framingham_blackboost_p_fr

Framingham_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(Framingham_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = Framingham_perf_blackboost$c_index,
  brier            = Framingham_perf_blackboost$brier,
  censoring_rate   = mean(Framingham_data$event == 0, na.rm = TRUE)
)


# COX

Framingham_cox_vimp <- Framingham_global$vimp_list$cox_model
Framingham_cox_p_vimp <- plot(Framingham_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Framingham_cox_p_vimp

Framingham_cox_summary <- calculate_vimp_summary(Framingham_cox_vimp, eps = 0)
Framingham_cox_summary

Framingham_cox_p_fr <- plot_vimp_with_fr(Framingham_cox_summary)
Framingham_cox_p_fr

Framingham_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(Framingham_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = Framingham_perf_cox$c_index,
  brier            = Framingham_perf_cox$brier,
  censoring_rate   = mean(Framingham_data$event == 0, na.rm = TRUE)
)


# XGBOOST

Framingham_xgboost_vimp <- Framingham_global$vimp_list$xgboost_model
Framingham_xgboost_p_vimp <- plot(Framingham_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Framingham_xgboost_p_vimp

Framingham_xgboost_summary <- calculate_vimp_summary(Framingham_xgboost_vimp, eps = 0)
Framingham_xgboost_summary

Framingham_xgboost_p_fr <- plot_vimp_with_fr(Framingham_xgboost_summary)
Framingham_xgboost_p_fr

Framingham_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(Framingham_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = Framingham_perf_xgboost$c_index,
  brier            = Framingham_perf_xgboost$brier,
  censoring_rate   = mean(Framingham_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

Framingham_plot_data <- rbind(
  Framingham_rsf_plot_data,
  Framingham_cforest_plot_data,
  Framingham_blackboost_plot_data,
  Framingham_cox_plot_data,
  Framingham_xgboost_plot_data
)


# 3D GRAFİKLER

Framingham_p_3d_cindex <- plot_3d_cindex(Framingham_plot_data)
Framingham_p_3d_cindex

Framingham_p_3d_brier <- plot_3d_brier(Framingham_plot_data)
Framingham_p_3d_brier


# QUADRANT GRAFİKLER

Framingham_p_quad_cindex <- plot_quadrant(
  Framingham_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
Framingham_p_quad_cindex

Framingham_p_quad_brier <- plot_quadrant(
  Framingham_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
Framingham_p_quad_brier




# =============================================================================
# CSL ANALİZİ
# =============================================================================

csl_data   <- prepare_csl(csl_raw)
csl_task   <- make_task_csl(csl_data)
csl_split  <- data_split(csl_task)
csl_models <- train_models(csl_task, csl_split)


# PERFORMANS

csl_perf_rsf <- measure_performance(csl_models$rsf_model, csl_task, csl_split)
csl_perf_rsf$c_index
csl_perf_rsf$brier
csl_perf_rsf$auc

csl_perf_cforest <- measure_performance(csl_models$cforest_model, csl_task, csl_split)
csl_perf_cforest$c_index
csl_perf_cforest$brier
csl_perf_cforest$auc

csl_perf_blackboost <- measure_performance(csl_models$blackboost_model, csl_task, csl_split)
csl_perf_blackboost$c_index
csl_perf_blackboost$brier
csl_perf_blackboost$auc

csl_perf_cox <- measure_performance(csl_models$cox_model, csl_task, csl_split)
csl_perf_cox$c_index
csl_perf_cox$brier
csl_perf_cox$auc

csl_perf_xgboost <- measure_performance(csl_models$xgboost_model, csl_task, csl_split)
csl_perf_xgboost$c_index
csl_perf_xgboost$brier
csl_perf_xgboost$auc


# GLOBAL AÇIKLAMA

csl_global <- global_explain(csl_task, csl_models)


# RSF

csl_rsf_vimp <- csl_global$vimp_list$rsf_model
csl_rsf_p_vimp <- plot(csl_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
csl_rsf_p_vimp

csl_rsf_summary <- calculate_vimp_summary(csl_rsf_vimp, eps = 0)
csl_rsf_summary

csl_rsf_p_fr <- plot_vimp_with_fr(csl_rsf_summary)
csl_rsf_p_fr

csl_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(csl_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = csl_perf_rsf$c_index,
  brier            = csl_perf_rsf$brier,
  censoring_rate   = mean(csl_data$event == 0, na.rm = TRUE)
)


# CFOREST

csl_cforest_vimp <- csl_global$vimp_list$cforest_model
csl_cforest_p_vimp <- plot(csl_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
csl_cforest_p_vimp

csl_cforest_summary <- calculate_vimp_summary(csl_cforest_vimp, eps = 0)
csl_cforest_summary

csl_cforest_p_fr <- plot_vimp_with_fr(csl_cforest_summary)
csl_cforest_p_fr

csl_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(csl_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = csl_perf_cforest$c_index,
  brier            = csl_perf_cforest$brier,
  censoring_rate   = mean(csl_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

csl_blackboost_vimp <- csl_global$vimp_list$blackboost_model
csl_blackboost_p_vimp <- plot(csl_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
csl_blackboost_p_vimp

csl_blackboost_summary <- calculate_vimp_summary(csl_blackboost_vimp, eps = 0)
csl_blackboost_summary

csl_blackboost_p_fr <- plot_vimp_with_fr(csl_blackboost_summary)
csl_blackboost_p_fr

csl_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(csl_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = csl_perf_blackboost$c_index,
  brier            = csl_perf_blackboost$brier,
  censoring_rate   = mean(csl_data$event == 0, na.rm = TRUE)
)


# COX

csl_cox_vimp <- csl_global$vimp_list$cox_model
csl_cox_p_vimp <- plot(csl_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
csl_cox_p_vimp

csl_cox_summary <- calculate_vimp_summary(csl_cox_vimp, eps = 0)
csl_cox_summary

csl_cox_p_fr <- plot_vimp_with_fr(csl_cox_summary)
csl_cox_p_fr

csl_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(csl_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = csl_perf_cox$c_index,
  brier            = csl_perf_cox$brier,
  censoring_rate   = mean(csl_data$event == 0, na.rm = TRUE)
)


# XGBOOST

csl_xgboost_vimp <- csl_global$vimp_list$xgboost_model
csl_xgboost_p_vimp <- plot(csl_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
csl_xgboost_p_vimp

csl_xgboost_summary <- calculate_vimp_summary(csl_xgboost_vimp, eps = 0)
csl_xgboost_summary

csl_xgboost_p_fr <- plot_vimp_with_fr(csl_xgboost_summary)
csl_xgboost_p_fr

csl_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(csl_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = csl_perf_xgboost$c_index,
  brier            = csl_perf_xgboost$brier,
  censoring_rate   = mean(csl_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

csl_plot_data <- rbind(
  csl_rsf_plot_data,
  csl_cforest_plot_data,
  csl_blackboost_plot_data,
  csl_cox_plot_data,
  csl_xgboost_plot_data
)


# 3D GRAFİKLER

csl_p_3d_cindex <- plot_3d_cindex(csl_plot_data)
csl_p_3d_cindex

csl_p_3d_brier <- plot_3d_brier(csl_plot_data)
csl_p_3d_brier


# QUADRANT GRAFİKLER

csl_p_quad_cindex <- plot_quadrant(
  csl_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
csl_p_quad_cindex

csl_p_quad_brier <- plot_quadrant(
  csl_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
csl_p_quad_brier


# =============================================================================
# CANCER ANALİZİ
# =============================================================================

cancer_data   <- prepare_cancer(cancer_raw)
cancer_task   <- make_task_cancer(cancer_data)
cancer_split  <- data_split(cancer_task)
cancer_models <- train_models(cancer_task, cancer_split)


# PERFORMANS

cancer_perf_rsf <- measure_performance(cancer_models$rsf_model, cancer_task, cancer_split)
cancer_perf_rsf$c_index
cancer_perf_rsf$brier
cancer_perf_rsf$auc

cancer_perf_cforest <- measure_performance(cancer_models$cforest_model, cancer_task, cancer_split)
cancer_perf_cforest$c_index
cancer_perf_cforest$brier
cancer_perf_cforest$auc

cancer_perf_blackboost <- measure_performance(cancer_models$blackboost_model, cancer_task, cancer_split)
cancer_perf_blackboost$c_index
cancer_perf_blackboost$brier
cancer_perf_blackboost$auc

cancer_perf_cox <- measure_performance(cancer_models$cox_model, cancer_task, cancer_split)
cancer_perf_cox$c_index
cancer_perf_cox$brier
cancer_perf_cox$auc

cancer_perf_xgboost <- measure_performance(cancer_models$xgboost_model, cancer_task, cancer_split)
cancer_perf_xgboost$c_index
cancer_perf_xgboost$brier
cancer_perf_xgboost$auc


# GLOBAL AÇIKLAMA

cancer_global <- global_explain(cancer_task, cancer_models)


# RSF

cancer_rsf_vimp <- cancer_global$vimp_list$rsf_model
cancer_rsf_p_vimp <- plot(cancer_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cancer_rsf_p_vimp

cancer_rsf_summary <- calculate_vimp_summary(cancer_rsf_vimp, eps = 0)
cancer_rsf_summary

cancer_rsf_p_fr <- plot_vimp_with_fr(cancer_rsf_summary)
cancer_rsf_p_fr

cancer_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(cancer_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = cancer_perf_rsf$c_index,
  brier            = cancer_perf_rsf$brier,
  censoring_rate   = mean(cancer_data$event == 0, na.rm = TRUE)
)


# CFOREST

cancer_cforest_vimp <- cancer_global$vimp_list$cforest_model
cancer_cforest_p_vimp <- plot(cancer_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cancer_cforest_p_vimp

cancer_cforest_summary <- calculate_vimp_summary(cancer_cforest_vimp, eps = 0)
cancer_cforest_summary

cancer_cforest_p_fr <- plot_vimp_with_fr(cancer_cforest_summary)
cancer_cforest_p_fr

cancer_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(cancer_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = cancer_perf_cforest$c_index,
  brier            = cancer_perf_cforest$brier,
  censoring_rate   = mean(cancer_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

cancer_blackboost_vimp <- cancer_global$vimp_list$blackboost_model
cancer_blackboost_p_vimp <- plot(cancer_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cancer_blackboost_p_vimp

cancer_blackboost_summary <- calculate_vimp_summary(cancer_blackboost_vimp, eps = 0)
cancer_blackboost_summary

cancer_blackboost_p_fr <- plot_vimp_with_fr(cancer_blackboost_summary)
cancer_blackboost_p_fr

cancer_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(cancer_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = cancer_perf_blackboost$c_index,
  brier            = cancer_perf_blackboost$brier,
  censoring_rate   = mean(cancer_data$event == 0, na.rm = TRUE)
)


# COX

cancer_cox_vimp <- cancer_global$vimp_list$cox_model
cancer_cox_p_vimp <- plot(cancer_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cancer_cox_p_vimp

cancer_cox_summary <- calculate_vimp_summary(cancer_cox_vimp, eps = 0)
cancer_cox_summary

cancer_cox_p_fr <- plot_vimp_with_fr(cancer_cox_summary)
cancer_cox_p_fr

cancer_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(cancer_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = cancer_perf_cox$c_index,
  brier            = cancer_perf_cox$brier,
  censoring_rate   = mean(cancer_data$event == 0, na.rm = TRUE)
)


# XGBOOST

cancer_xgboost_vimp <- cancer_global$vimp_list$xgboost_model
cancer_xgboost_p_vimp <- plot(cancer_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cancer_xgboost_p_vimp

cancer_xgboost_summary <- calculate_vimp_summary(cancer_xgboost_vimp, eps = 0)
cancer_xgboost_summary

cancer_xgboost_p_fr <- plot_vimp_with_fr(cancer_xgboost_summary)
cancer_xgboost_p_fr

cancer_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(cancer_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = cancer_perf_xgboost$c_index,
  brier            = cancer_perf_xgboost$brier,
  censoring_rate   = mean(cancer_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

cancer_plot_data <- rbind(
  cancer_rsf_plot_data,
  cancer_cforest_plot_data,
  cancer_blackboost_plot_data,
  cancer_cox_plot_data,
  cancer_xgboost_plot_data
)


# 3D GRAFİKLER

cancer_p_3d_cindex <- plot_3d_cindex(cancer_plot_data)
cancer_p_3d_cindex

cancer_p_3d_brier <- plot_3d_brier(cancer_plot_data)
cancer_p_3d_brier


# QUADRANT GRAFİKLER

cancer_p_quad_cindex <- plot_quadrant(
  cancer_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
cancer_p_quad_cindex

cancer_p_quad_brier <- plot_quadrant(
  cancer_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
cancer_p_quad_brier


# =============================================================================
# WHAS500 ANALİZİ
# =============================================================================

whas500_data   <- prepare_whas500(whas500_raw)
whas500_task   <- make_task_whas500(whas500_data)
whas500_split  <- data_split(whas500_task)
whas500_models <- train_models(whas500_task, whas500_split)


# PERFORMANS

whas500_perf_rsf <- measure_performance(whas500_models$rsf_model, whas500_task, whas500_split)
whas500_perf_rsf$c_index
whas500_perf_rsf$brier
whas500_perf_rsf$auc

whas500_perf_cforest <- measure_performance(whas500_models$cforest_model, whas500_task, whas500_split)
whas500_perf_cforest$c_index
whas500_perf_cforest$brier
whas500_perf_cforest$auc

whas500_perf_blackboost <- measure_performance(whas500_models$blackboost_model, whas500_task, whas500_split)
whas500_perf_blackboost$c_index
whas500_perf_blackboost$brier
whas500_perf_blackboost$auc

whas500_perf_cox <- measure_performance(whas500_models$cox_model, whas500_task, whas500_split)
whas500_perf_cox$c_index
whas500_perf_cox$brier
whas500_perf_cox$auc

whas500_perf_xgboost <- measure_performance(whas500_models$xgboost_model, whas500_task, whas500_split)
whas500_perf_xgboost$c_index
whas500_perf_xgboost$brier
whas500_perf_xgboost$auc


# GLOBAL AÇIKLAMA

whas500_global <- global_explain(whas500_task, whas500_models)


# RSF

whas500_rsf_vimp <- whas500_global$vimp_list$rsf_model
whas500_rsf_p_vimp <- plot(whas500_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
whas500_rsf_p_vimp

whas500_rsf_summary <- calculate_vimp_summary(whas500_rsf_vimp, eps = 0)
whas500_rsf_summary

whas500_rsf_p_fr <- plot_vimp_with_fr(whas500_rsf_summary)
whas500_rsf_p_fr

whas500_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(whas500_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = whas500_perf_rsf$c_index,
  brier            = whas500_perf_rsf$brier,
  censoring_rate   = mean(whas500_data$event == 0, na.rm = TRUE)
)


# CFOREST

whas500_cforest_vimp <- whas500_global$vimp_list$cforest_model
whas500_cforest_p_vimp <- plot(whas500_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
whas500_cforest_p_vimp

whas500_cforest_summary <- calculate_vimp_summary(whas500_cforest_vimp, eps = 0)
whas500_cforest_summary

whas500_cforest_p_fr <- plot_vimp_with_fr(whas500_cforest_summary)
whas500_cforest_p_fr

whas500_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(whas500_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = whas500_perf_cforest$c_index,
  brier            = whas500_perf_cforest$brier,
  censoring_rate   = mean(whas500_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

whas500_blackboost_vimp <- whas500_global$vimp_list$blackboost_model
whas500_blackboost_p_vimp <- plot(whas500_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
whas500_blackboost_p_vimp

whas500_blackboost_summary <- calculate_vimp_summary(whas500_blackboost_vimp, eps = 0)
whas500_blackboost_summary

whas500_blackboost_p_fr <- plot_vimp_with_fr(whas500_blackboost_summary)
whas500_blackboost_p_fr

whas500_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(whas500_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = whas500_perf_blackboost$c_index,
  brier            = whas500_perf_blackboost$brier,
  censoring_rate   = mean(whas500_data$event == 0, na.rm = TRUE)
)


# COX

whas500_cox_vimp <- whas500_global$vimp_list$cox_model
whas500_cox_p_vimp <- plot(whas500_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
whas500_cox_p_vimp

whas500_cox_summary <- calculate_vimp_summary(whas500_cox_vimp, eps = 0)
whas500_cox_summary

whas500_cox_p_fr <- plot_vimp_with_fr(whas500_cox_summary)
whas500_cox_p_fr

whas500_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(whas500_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = whas500_perf_cox$c_index,
  brier            = whas500_perf_cox$brier,
  censoring_rate   = mean(whas500_data$event == 0, na.rm = TRUE)
)


# XGBOOST

whas500_xgboost_vimp <- whas500_global$vimp_list$xgboost_model
whas500_xgboost_p_vimp <- plot(whas500_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
whas500_xgboost_p_vimp

whas500_xgboost_summary <- calculate_vimp_summary(whas500_xgboost_vimp, eps = 0)
whas500_xgboost_summary

whas500_xgboost_p_fr <- plot_vimp_with_fr(whas500_xgboost_summary)
whas500_xgboost_p_fr

whas500_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(whas500_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = whas500_perf_xgboost$c_index,
  brier            = whas500_perf_xgboost$brier,
  censoring_rate   = mean(whas500_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

whas500_plot_data <- rbind(
  whas500_rsf_plot_data,
  whas500_cforest_plot_data,
  whas500_blackboost_plot_data,
  whas500_cox_plot_data,
  whas500_xgboost_plot_data
)


# 3D GRAFİKLER

whas500_p_3d_cindex <- plot_3d_cindex(whas500_plot_data)
whas500_p_3d_cindex

whas500_p_3d_brier <- plot_3d_brier(whas500_plot_data)
whas500_p_3d_brier


# QUADRANT GRAFİKLER

whas500_p_quad_cindex <- plot_quadrant(
  whas500_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
whas500_p_quad_cindex

whas500_p_quad_brier <- plot_quadrant(
  whas500_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
whas500_p_quad_brier


# =============================================================================
# CGD ANALİZİ
# =============================================================================

cgd_data   <- prepare_cgd(cgd_raw)
cgd_task   <- make_task_cgd(cgd_data)
cgd_split  <- data_split(cgd_task)
cgd_models <- train_models(cgd_task, cgd_split)


# PERFORMANS

cgd_perf_rsf <- measure_performance(cgd_models$rsf_model, cgd_task, cgd_split)
cgd_perf_rsf$c_index
cgd_perf_rsf$brier
cgd_perf_rsf$auc

cgd_perf_cforest <- measure_performance(cgd_models$cforest_model, cgd_task, cgd_split)
cgd_perf_cforest$c_index
cgd_perf_cforest$brier
cgd_perf_cforest$auc

cgd_perf_blackboost <- measure_performance(cgd_models$blackboost_model, cgd_task, cgd_split)
cgd_perf_blackboost$c_index
cgd_perf_blackboost$brier
cgd_perf_blackboost$auc

cgd_perf_cox <- measure_performance(cgd_models$cox_model, cgd_task, cgd_split)
cgd_perf_cox$c_index
cgd_perf_cox$brier
cgd_perf_cox$auc

cgd_perf_xgboost <- measure_performance(cgd_models$xgboost_model, cgd_task, cgd_split)
cgd_perf_xgboost$c_index
cgd_perf_xgboost$brier
cgd_perf_xgboost$auc


# GLOBAL AÇIKLAMA

cgd_global <- global_explain(cgd_task, cgd_models)


# RSF

cgd_rsf_vimp <- cgd_global$vimp_list$rsf_model
cgd_rsf_p_vimp <- plot(cgd_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cgd_rsf_p_vimp

cgd_rsf_summary <- calculate_vimp_summary(cgd_rsf_vimp, eps = 0)
cgd_rsf_summary

cgd_rsf_p_fr <- plot_vimp_with_fr(cgd_rsf_summary)
cgd_rsf_p_fr

cgd_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(cgd_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = cgd_perf_rsf$c_index,
  brier            = cgd_perf_rsf$brier,
  censoring_rate   = mean(cgd_data$event == 0, na.rm = TRUE)
)


# CFOREST

cgd_cforest_vimp <- cgd_global$vimp_list$cforest_model
cgd_cforest_p_vimp <- plot(cgd_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cgd_cforest_p_vimp

cgd_cforest_summary <- calculate_vimp_summary(cgd_cforest_vimp, eps = 0)
cgd_cforest_summary

cgd_cforest_p_fr <- plot_vimp_with_fr(cgd_cforest_summary)
cgd_cforest_p_fr

cgd_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(cgd_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = cgd_perf_cforest$c_index,
  brier            = cgd_perf_cforest$brier,
  censoring_rate   = mean(cgd_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

cgd_blackboost_vimp <- cgd_global$vimp_list$blackboost_model
cgd_blackboost_p_vimp <- plot(cgd_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cgd_blackboost_p_vimp

cgd_blackboost_summary <- calculate_vimp_summary(cgd_blackboost_vimp, eps = 0)
cgd_blackboost_summary

cgd_blackboost_p_fr <- plot_vimp_with_fr(cgd_blackboost_summary)
cgd_blackboost_p_fr

cgd_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(cgd_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = cgd_perf_blackboost$c_index,
  brier            = cgd_perf_blackboost$brier,
  censoring_rate   = mean(cgd_data$event == 0, na.rm = TRUE)
)


# COX

cgd_cox_vimp <- cgd_global$vimp_list$cox_model
cgd_cox_p_vimp <- plot(cgd_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cgd_cox_p_vimp

cgd_cox_summary <- calculate_vimp_summary(cgd_cox_vimp, eps = 0)
cgd_cox_summary

cgd_cox_p_fr <- plot_vimp_with_fr(cgd_cox_summary)
cgd_cox_p_fr

cgd_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(cgd_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = cgd_perf_cox$c_index,
  brier            = cgd_perf_cox$brier,
  censoring_rate   = mean(cgd_data$event == 0, na.rm = TRUE)
)


# XGBOOST

cgd_xgboost_vimp <- cgd_global$vimp_list$xgboost_model
cgd_xgboost_p_vimp <- plot(cgd_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
cgd_xgboost_p_vimp

cgd_xgboost_summary <- calculate_vimp_summary(cgd_xgboost_vimp, eps = 0)
cgd_xgboost_summary

cgd_xgboost_p_fr <- plot_vimp_with_fr(cgd_xgboost_summary)
cgd_xgboost_p_fr

cgd_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(cgd_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = cgd_perf_xgboost$c_index,
  brier            = cgd_perf_xgboost$brier,
  censoring_rate   = mean(cgd_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

cgd_plot_data <- rbind(
  cgd_rsf_plot_data,
  cgd_cforest_plot_data,
  cgd_blackboost_plot_data,
  cgd_cox_plot_data,
  cgd_xgboost_plot_data
)


# 3D GRAFİKLER

cgd_p_3d_cindex <- plot_3d_cindex(cgd_plot_data)
cgd_p_3d_cindex

cgd_p_3d_brier <- plot_3d_brier(cgd_plot_data)
cgd_p_3d_brier


# QUADRANT GRAFİKLER

cgd_p_quad_cindex <- plot_quadrant(
  cgd_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
cgd_p_quad_cindex

cgd_p_quad_brier <- plot_quadrant(
  cgd_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
cgd_p_quad_brier


# =============================================================================
# PBC3 ANALİZİ
# =============================================================================

Pbc3_data   <- prepare_Pbc3(Pbc3_raw)
Pbc3_task   <- make_task_Pbc3(Pbc3_data)
Pbc3_split  <- data_split(Pbc3_task)
Pbc3_models <- train_models(Pbc3_task, Pbc3_split)


# PERFORMANS

Pbc3_perf_rsf <- measure_performance(Pbc3_models$rsf_model, Pbc3_task, Pbc3_split)
Pbc3_perf_rsf$c_index
Pbc3_perf_rsf$brier
Pbc3_perf_rsf$auc

Pbc3_perf_cforest <- measure_performance(Pbc3_models$cforest_model, Pbc3_task, Pbc3_split)
Pbc3_perf_cforest$c_index
Pbc3_perf_cforest$brier
Pbc3_perf_cforest$auc

Pbc3_perf_blackboost <- measure_performance(Pbc3_models$blackboost_model, Pbc3_task, Pbc3_split)
Pbc3_perf_blackboost$c_index
Pbc3_perf_blackboost$brier
Pbc3_perf_blackboost$auc

Pbc3_perf_cox <- measure_performance(Pbc3_models$cox_model, Pbc3_task, Pbc3_split)
Pbc3_perf_cox$c_index
Pbc3_perf_cox$brier
Pbc3_perf_cox$auc

Pbc3_perf_xgboost <- measure_performance(Pbc3_models$xgboost_model, Pbc3_task, Pbc3_split)
Pbc3_perf_xgboost$c_index
Pbc3_perf_xgboost$brier
Pbc3_perf_xgboost$auc


# GLOBAL AÇIKLAMA

Pbc3_global <- global_explain(Pbc3_task, Pbc3_models)


# RSF

Pbc3_rsf_vimp <- Pbc3_global$vimp_list$rsf_model
Pbc3_rsf_p_vimp <- plot(Pbc3_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Pbc3_rsf_p_vimp

Pbc3_rsf_summary <- calculate_vimp_summary(Pbc3_rsf_vimp, eps = 0)
Pbc3_rsf_summary

Pbc3_rsf_p_fr <- plot_vimp_with_fr(Pbc3_rsf_summary)
Pbc3_rsf_p_fr

Pbc3_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(Pbc3_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = Pbc3_perf_rsf$c_index,
  brier            = Pbc3_perf_rsf$brier,
  censoring_rate   = mean(Pbc3_data$event == 0, na.rm = TRUE)
)


# CFOREST

Pbc3_cforest_vimp <- Pbc3_global$vimp_list$cforest_model
Pbc3_cforest_p_vimp <- plot(Pbc3_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Pbc3_cforest_p_vimp

Pbc3_cforest_summary <- calculate_vimp_summary(Pbc3_cforest_vimp, eps = 0)
Pbc3_cforest_summary

Pbc3_cforest_p_fr <- plot_vimp_with_fr(Pbc3_cforest_summary)
Pbc3_cforest_p_fr

Pbc3_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(Pbc3_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = Pbc3_perf_cforest$c_index,
  brier            = Pbc3_perf_cforest$brier,
  censoring_rate   = mean(Pbc3_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

Pbc3_blackboost_vimp <- Pbc3_global$vimp_list$blackboost_model
Pbc3_blackboost_p_vimp <- plot(Pbc3_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Pbc3_blackboost_p_vimp

Pbc3_blackboost_summary <- calculate_vimp_summary(Pbc3_blackboost_vimp, eps = 0)
Pbc3_blackboost_summary

Pbc3_blackboost_p_fr <- plot_vimp_with_fr(Pbc3_blackboost_summary)
Pbc3_blackboost_p_fr

Pbc3_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(Pbc3_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = Pbc3_perf_blackboost$c_index,
  brier            = Pbc3_perf_blackboost$brier,
  censoring_rate   = mean(Pbc3_data$event == 0, na.rm = TRUE)
)


# COX

Pbc3_cox_vimp <- Pbc3_global$vimp_list$cox_model
Pbc3_cox_p_vimp <- plot(Pbc3_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Pbc3_cox_p_vimp

Pbc3_cox_summary <- calculate_vimp_summary(Pbc3_cox_vimp, eps = 0)
Pbc3_cox_summary

Pbc3_cox_p_fr <- plot_vimp_with_fr(Pbc3_cox_summary)
Pbc3_cox_p_fr

Pbc3_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(Pbc3_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = Pbc3_perf_cox$c_index,
  brier            = Pbc3_perf_cox$brier,
  censoring_rate   = mean(Pbc3_data$event == 0, na.rm = TRUE)
)


# XGBOOST

Pbc3_xgboost_vimp <- Pbc3_global$vimp_list$xgboost_model
Pbc3_xgboost_p_vimp <- plot(Pbc3_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Pbc3_xgboost_p_vimp

Pbc3_xgboost_summary <- calculate_vimp_summary(Pbc3_xgboost_vimp, eps = 0)
Pbc3_xgboost_summary

Pbc3_xgboost_p_fr <- plot_vimp_with_fr(Pbc3_xgboost_summary)
Pbc3_xgboost_p_fr

Pbc3_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(Pbc3_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = Pbc3_perf_xgboost$c_index,
  brier            = Pbc3_perf_xgboost$brier,
  censoring_rate   = mean(Pbc3_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

Pbc3_plot_data <- rbind(
  Pbc3_rsf_plot_data,
  Pbc3_cforest_plot_data,
  Pbc3_blackboost_plot_data,
  Pbc3_cox_plot_data,
  Pbc3_xgboost_plot_data
)


# 3D GRAFİKLER

Pbc3_p_3d_cindex <- plot_3d_cindex(Pbc3_plot_data)
Pbc3_p_3d_cindex

Pbc3_p_3d_brier <- plot_3d_brier(Pbc3_plot_data)
Pbc3_p_3d_brier


# QUADRANT GRAFİKLER

Pbc3_p_quad_cindex <- plot_quadrant(
  Pbc3_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
Pbc3_p_quad_cindex

Pbc3_p_quad_brier <- plot_quadrant(
  Pbc3_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
Pbc3_p_quad_brier



# =============================================================================
# GBSG2 ANALİZİ
# =============================================================================

GBSG2_data   <- prepare_GBSG2(GBSG2_raw)
GBSG2_task   <- make_task_GBSG2(GBSG2_data)
GBSG2_split  <- data_split(GBSG2_task)
GBSG2_models <- train_models(GBSG2_task, GBSG2_split)


# PERFORMANS

GBSG2_perf_rsf <- measure_performance(GBSG2_models$rsf_model, GBSG2_task, GBSG2_split)
GBSG2_perf_rsf$c_index
GBSG2_perf_rsf$brier
GBSG2_perf_rsf$auc

GBSG2_perf_cforest <- measure_performance(GBSG2_models$cforest_model, GBSG2_task, GBSG2_split)
GBSG2_perf_cforest$c_index
GBSG2_perf_cforest$brier
GBSG2_perf_cforest$auc

GBSG2_perf_blackboost <- measure_performance(GBSG2_models$blackboost_model, GBSG2_task, GBSG2_split)
GBSG2_perf_blackboost$c_index
GBSG2_perf_blackboost$brier
GBSG2_perf_blackboost$auc

GBSG2_perf_cox <- measure_performance(GBSG2_models$cox_model, GBSG2_task, GBSG2_split)
GBSG2_perf_cox$c_index
GBSG2_perf_cox$brier
GBSG2_perf_cox$auc

GBSG2_perf_xgboost <- measure_performance(GBSG2_models$xgboost_model, GBSG2_task, GBSG2_split)
GBSG2_perf_xgboost$c_index
GBSG2_perf_xgboost$brier
GBSG2_perf_xgboost$auc


# GLOBAL AÇIKLAMA

GBSG2_global <- global_explain(GBSG2_task, GBSG2_models)


# RSF

GBSG2_rsf_vimp <- GBSG2_global$vimp_list$rsf_model
GBSG2_rsf_p_vimp <- plot(GBSG2_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
GBSG2_rsf_p_vimp

GBSG2_rsf_summary <- calculate_vimp_summary(GBSG2_rsf_vimp, eps = 0)
GBSG2_rsf_summary

GBSG2_rsf_p_fr <- plot_vimp_with_fr(GBSG2_rsf_summary)
GBSG2_rsf_p_fr

GBSG2_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(GBSG2_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = GBSG2_perf_rsf$c_index,
  brier            = GBSG2_perf_rsf$brier,
  censoring_rate   = mean(GBSG2_data$event == 0, na.rm = TRUE)
)


# CFOREST

GBSG2_cforest_vimp <- GBSG2_global$vimp_list$cforest_model
GBSG2_cforest_p_vimp <- plot(GBSG2_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
GBSG2_cforest_p_vimp

GBSG2_cforest_summary <- calculate_vimp_summary(GBSG2_cforest_vimp, eps = 0)
GBSG2_cforest_summary

GBSG2_cforest_p_fr <- plot_vimp_with_fr(GBSG2_cforest_summary)
GBSG2_cforest_p_fr

GBSG2_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(GBSG2_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = GBSG2_perf_cforest$c_index,
  brier            = GBSG2_perf_cforest$brier,
  censoring_rate   = mean(GBSG2_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

GBSG2_blackboost_vimp <- GBSG2_global$vimp_list$blackboost_model
GBSG2_blackboost_p_vimp <- plot(GBSG2_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
GBSG2_blackboost_p_vimp

GBSG2_blackboost_summary <- calculate_vimp_summary(GBSG2_blackboost_vimp, eps = 0)
GBSG2_blackboost_summary

GBSG2_blackboost_p_fr <- plot_vimp_with_fr(GBSG2_blackboost_summary)
GBSG2_blackboost_p_fr

GBSG2_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(GBSG2_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = GBSG2_perf_blackboost$c_index,
  brier            = GBSG2_perf_blackboost$brier,
  censoring_rate   = mean(GBSG2_data$event == 0, na.rm = TRUE)
)


# COX

GBSG2_cox_vimp <- GBSG2_global$vimp_list$cox_model
GBSG2_cox_p_vimp <- plot(GBSG2_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
GBSG2_cox_p_vimp

GBSG2_cox_summary <- calculate_vimp_summary(GBSG2_cox_vimp, eps = 0)
GBSG2_cox_summary

GBSG2_cox_p_fr <- plot_vimp_with_fr(GBSG2_cox_summary)
GBSG2_cox_p_fr

GBSG2_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(GBSG2_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = GBSG2_perf_cox$c_index,
  brier            = GBSG2_perf_cox$brier,
  censoring_rate   = mean(GBSG2_data$event == 0, na.rm = TRUE)
)


# XGBOOST

GBSG2_xgboost_vimp <- GBSG2_global$vimp_list$xgboost_model
GBSG2_xgboost_p_vimp <- plot(GBSG2_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
GBSG2_xgboost_p_vimp

GBSG2_xgboost_summary <- calculate_vimp_summary(GBSG2_xgboost_vimp, eps = 0)
GBSG2_xgboost_summary

GBSG2_xgboost_p_fr <- plot_vimp_with_fr(GBSG2_xgboost_summary)
GBSG2_xgboost_p_fr

GBSG2_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(GBSG2_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = GBSG2_perf_xgboost$c_index,
  brier            = GBSG2_perf_xgboost$brier,
  censoring_rate   = mean(GBSG2_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

GBSG2_plot_data <- rbind(
  GBSG2_rsf_plot_data,
  GBSG2_cforest_plot_data,
  GBSG2_blackboost_plot_data,
  GBSG2_cox_plot_data,
  GBSG2_xgboost_plot_data
)


# 3D GRAFİKLER

GBSG2_p_3d_cindex <- plot_3d_cindex(GBSG2_plot_data)
GBSG2_p_3d_cindex

GBSG2_p_3d_brier <- plot_3d_brier(GBSG2_plot_data)
GBSG2_p_3d_brier


# QUADRANT GRAFİKLER

GBSG2_p_quad_cindex <- plot_quadrant(
  GBSG2_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
GBSG2_p_quad_cindex

GBSG2_p_quad_brier <- plot_quadrant(
  GBSG2_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
GBSG2_p_quad_brier


# =============================================================================
# COLON ANALİZİ
# =============================================================================

colon_data   <- prepare_colon(colon_raw)
colon_task   <- make_task_colon(colon_data)
colon_split  <- data_split(colon_task)
colon_models <- train_models(colon_task, colon_split)


# PERFORMANS

colon_perf_rsf <- measure_performance(colon_models$rsf_model, colon_task, colon_split)
colon_perf_rsf$c_index
colon_perf_rsf$brier
colon_perf_rsf$auc

colon_perf_cforest <- measure_performance(colon_models$cforest_model, colon_task, colon_split)
colon_perf_cforest$c_index
colon_perf_cforest$brier
colon_perf_cforest$auc

colon_perf_blackboost <- measure_performance(colon_models$blackboost_model, colon_task, colon_split)
colon_perf_blackboost$c_index
colon_perf_blackboost$brier
colon_perf_blackboost$auc

colon_perf_cox <- measure_performance(colon_models$cox_model, colon_task, colon_split)
colon_perf_cox$c_index
colon_perf_cox$brier
colon_perf_cox$auc

colon_perf_xgboost <- measure_performance(colon_models$xgboost_model, colon_task, colon_split)
colon_perf_xgboost$c_index
colon_perf_xgboost$brier
colon_perf_xgboost$auc


# GLOBAL AÇIKLAMA

colon_global <- global_explain(colon_task, colon_models)


# RSF

colon_rsf_vimp <- colon_global$vimp_list$rsf_model
colon_rsf_p_vimp <- plot(colon_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
colon_rsf_p_vimp

colon_rsf_summary <- calculate_vimp_summary(colon_rsf_vimp, eps = 0)
colon_rsf_summary

colon_rsf_p_fr <- plot_vimp_with_fr(colon_rsf_summary)
colon_rsf_p_fr

colon_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(colon_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = colon_perf_rsf$c_index,
  brier            = colon_perf_rsf$brier,
  censoring_rate   = mean(colon_data$event == 0, na.rm = TRUE)
)


# CFOREST

colon_cforest_vimp <- colon_global$vimp_list$cforest_model
colon_cforest_p_vimp <- plot(colon_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
colon_cforest_p_vimp

colon_cforest_summary <- calculate_vimp_summary(colon_cforest_vimp, eps = 0)
colon_cforest_summary

colon_cforest_p_fr <- plot_vimp_with_fr(colon_cforest_summary)
colon_cforest_p_fr

colon_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(colon_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = colon_perf_cforest$c_index,
  brier            = colon_perf_cforest$brier,
  censoring_rate   = mean(colon_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

colon_blackboost_vimp <- colon_global$vimp_list$blackboost_model
colon_blackboost_p_vimp <- plot(colon_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
colon_blackboost_p_vimp

colon_blackboost_summary <- calculate_vimp_summary(colon_blackboost_vimp, eps = 0)
colon_blackboost_summary

colon_blackboost_p_fr <- plot_vimp_with_fr(colon_blackboost_summary)
colon_blackboost_p_fr

colon_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(colon_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = colon_perf_blackboost$c_index,
  brier            = colon_perf_blackboost$brier,
  censoring_rate   = mean(colon_data$event == 0, na.rm = TRUE)
)


# COX

colon_cox_vimp <- colon_global$vimp_list$cox_model
colon_cox_p_vimp <- plot(colon_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
colon_cox_p_vimp

colon_cox_summary <- calculate_vimp_summary(colon_cox_vimp, eps = 0)
colon_cox_summary

colon_cox_p_fr <- plot_vimp_with_fr(colon_cox_summary)
colon_cox_p_fr

colon_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(colon_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = colon_perf_cox$c_index,
  brier            = colon_perf_cox$brier,
  censoring_rate   = mean(colon_data$event == 0, na.rm = TRUE)
)


# XGBOOST

colon_xgboost_vimp <- colon_global$vimp_list$xgboost_model
colon_xgboost_p_vimp <- plot(colon_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
colon_xgboost_p_vimp

colon_xgboost_summary <- calculate_vimp_summary(colon_xgboost_vimp, eps = 0)
colon_xgboost_summary

colon_xgboost_p_fr <- plot_vimp_with_fr(colon_xgboost_summary)
colon_xgboost_p_fr

colon_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(colon_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = colon_perf_xgboost$c_index,
  brier            = colon_perf_xgboost$brier,
  censoring_rate   = mean(colon_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

colon_plot_data <- rbind(
  colon_rsf_plot_data,
  colon_cforest_plot_data,
  colon_blackboost_plot_data,
  colon_cox_plot_data,
  colon_xgboost_plot_data
)


# 3D GRAFİKLER

colon_p_3d_cindex <- plot_3d_cindex(colon_plot_data)
colon_p_3d_cindex

colon_p_3d_brier <- plot_3d_brier(colon_plot_data)
colon_p_3d_brier


# QUADRANT GRAFİKLER

colon_p_quad_cindex <- plot_quadrant(
  colon_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
colon_p_quad_cindex

colon_p_quad_brier <- plot_quadrant(
  colon_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
colon_p_quad_brier


# =============================================================================
# MGUS ANALİZİ
# =============================================================================

mgus_data   <- prepare_mgus(mgus_raw)
mgus_task   <- make_task_mgus(mgus_data)
mgus_split  <- data_split(mgus_task)
mgus_models <- train_models(mgus_task, mgus_split)


# PERFORMANS

mgus_perf_rsf <- measure_performance(mgus_models$rsf_model, mgus_task, mgus_split)
mgus_perf_rsf$c_index
mgus_perf_rsf$brier
mgus_perf_rsf$auc

mgus_perf_cforest <- measure_performance(mgus_models$cforest_model, mgus_task, mgus_split)
mgus_perf_cforest$c_index
mgus_perf_cforest$brier
mgus_perf_cforest$auc

mgus_perf_blackboost <- measure_performance(mgus_models$blackboost_model, mgus_task, mgus_split)
mgus_perf_blackboost$c_index
mgus_perf_blackboost$brier
mgus_perf_blackboost$auc

mgus_perf_cox <- measure_performance(mgus_models$cox_model, mgus_task, mgus_split)
mgus_perf_cox$c_index
mgus_perf_cox$brier
mgus_perf_cox$auc

mgus_perf_xgboost <- measure_performance(mgus_models$xgboost_model, mgus_task, mgus_split)
mgus_perf_xgboost$c_index
mgus_perf_xgboost$brier
mgus_perf_xgboost$auc


# GLOBAL AÇIKLAMA

mgus_global <- global_explain(mgus_task, mgus_models)


# RSF

mgus_rsf_vimp <- mgus_global$vimp_list$rsf_model
mgus_rsf_p_vimp <- plot(mgus_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
mgus_rsf_p_vimp

mgus_rsf_summary <- calculate_vimp_summary(mgus_rsf_vimp, eps = 0)
mgus_rsf_summary

mgus_rsf_p_fr <- plot_vimp_with_fr(mgus_rsf_summary)
mgus_rsf_p_fr

mgus_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(mgus_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = mgus_perf_rsf$c_index,
  brier            = mgus_perf_rsf$brier,
  censoring_rate   = mean(mgus_data$event == 0, na.rm = TRUE)
)


# CFOREST

mgus_cforest_vimp <- mgus_global$vimp_list$cforest_model
mgus_cforest_p_vimp <- plot(mgus_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
mgus_cforest_p_vimp

mgus_cforest_summary <- calculate_vimp_summary(mgus_cforest_vimp, eps = 0)
mgus_cforest_summary

mgus_cforest_p_fr <- plot_vimp_with_fr(mgus_cforest_summary)
mgus_cforest_p_fr

mgus_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(mgus_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = mgus_perf_cforest$c_index,
  brier            = mgus_perf_cforest$brier,
  censoring_rate   = mean(mgus_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

mgus_blackboost_vimp <- mgus_global$vimp_list$blackboost_model
mgus_blackboost_p_vimp <- plot(mgus_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
mgus_blackboost_p_vimp

mgus_blackboost_summary <- calculate_vimp_summary(mgus_blackboost_vimp, eps = 0)
mgus_blackboost_summary

mgus_blackboost_p_fr <- plot_vimp_with_fr(mgus_blackboost_summary)
mgus_blackboost_p_fr

mgus_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(mgus_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = mgus_perf_blackboost$c_index,
  brier            = mgus_perf_blackboost$brier,
  censoring_rate   = mean(mgus_data$event == 0, na.rm = TRUE)
)


# COX

mgus_cox_vimp <- mgus_global$vimp_list$cox_model
mgus_cox_p_vimp <- plot(mgus_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
mgus_cox_p_vimp

mgus_cox_summary <- calculate_vimp_summary(mgus_cox_vimp, eps = 0)
mgus_cox_summary

mgus_cox_p_fr <- plot_vimp_with_fr(mgus_cox_summary)
mgus_cox_p_fr

mgus_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(mgus_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = mgus_perf_cox$c_index,
  brier            = mgus_perf_cox$brier,
  censoring_rate   = mean(mgus_data$event == 0, na.rm = TRUE)
)


# XGBOOST

mgus_xgboost_vimp <- mgus_global$vimp_list$xgboost_model
mgus_xgboost_p_vimp <- plot(mgus_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
mgus_xgboost_p_vimp

mgus_xgboost_summary <- calculate_vimp_summary(mgus_xgboost_vimp, eps = 0)
mgus_xgboost_summary

mgus_xgboost_p_fr <- plot_vimp_with_fr(mgus_xgboost_summary)
mgus_xgboost_p_fr

mgus_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(mgus_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = mgus_perf_xgboost$c_index,
  brier            = mgus_perf_xgboost$brier,
  censoring_rate   = mean(mgus_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

mgus_plot_data <- rbind(
  mgus_rsf_plot_data,
  mgus_cforest_plot_data,
  mgus_blackboost_plot_data,
  mgus_cox_plot_data,
  mgus_xgboost_plot_data
)


# 3D GRAFİKLER

mgus_p_3d_cindex <- plot_3d_cindex(mgus_plot_data)
mgus_p_3d_cindex

mgus_p_3d_brier <- plot_3d_brier(mgus_plot_data)
mgus_p_3d_brier


# QUADRANT GRAFİKLER

mgus_p_quad_cindex <- plot_quadrant(
  mgus_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
mgus_p_quad_cindex

mgus_p_quad_brier <- plot_quadrant(
  mgus_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
mgus_p_quad_brier


# =============================================================================
# FLCHAIN ANALİZİ
# =============================================================================

flchain_data   <- prepare_flchain(flchain_raw)
flchain_task   <- make_task_flchain(flchain_data)
flchain_split  <- data_split(flchain_task)
flchain_models <- train_models(flchain_task, flchain_split)


# PERFORMANS

flchain_perf_rsf <- measure_performance(flchain_models$rsf_model, flchain_task, flchain_split)
flchain_perf_rsf$c_index
flchain_perf_rsf$brier
flchain_perf_rsf$auc

flchain_perf_cforest <- measure_performance(flchain_models$cforest_model, flchain_task, flchain_split)
flchain_perf_cforest$c_index
flchain_perf_cforest$brier
flchain_perf_cforest$auc

flchain_perf_blackboost <- measure_performance(flchain_models$blackboost_model, flchain_task, flchain_split)
flchain_perf_blackboost$c_index
flchain_perf_blackboost$brier
flchain_perf_blackboost$auc

flchain_perf_cox <- measure_performance(flchain_models$cox_model, flchain_task, flchain_split)
flchain_perf_cox$c_index
flchain_perf_cox$brier
flchain_perf_cox$auc

flchain_perf_xgboost <- measure_performance(flchain_models$xgboost_model, flchain_task, flchain_split)
flchain_perf_xgboost$c_index
flchain_perf_xgboost$brier
flchain_perf_xgboost$auc


# GLOBAL AÇIKLAMA

flchain_global <- global_explain(flchain_task, flchain_models)


# RSF

flchain_rsf_vimp <- flchain_global$vimp_list$rsf_model
flchain_rsf_p_vimp <- plot(flchain_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
flchain_rsf_p_vimp

flchain_rsf_summary <- calculate_vimp_summary(flchain_rsf_vimp, eps = 0)
flchain_rsf_summary

flchain_rsf_p_fr <- plot_vimp_with_fr(flchain_rsf_summary)
flchain_rsf_p_fr

flchain_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(flchain_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = flchain_perf_rsf$c_index,
  brier            = flchain_perf_rsf$brier,
  censoring_rate   = mean(flchain_data$event == 0, na.rm = TRUE)
)


# CFOREST

flchain_cforest_vimp <- flchain_global$vimp_list$cforest_model
flchain_cforest_p_vimp <- plot(flchain_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
flchain_cforest_p_vimp

flchain_cforest_summary <- calculate_vimp_summary(flchain_cforest_vimp, eps = 0)
flchain_cforest_summary

flchain_cforest_p_fr <- plot_vimp_with_fr(flchain_cforest_summary)
flchain_cforest_p_fr

flchain_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(flchain_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = flchain_perf_cforest$c_index,
  brier            = flchain_perf_cforest$brier,
  censoring_rate   = mean(flchain_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

flchain_blackboost_vimp <- flchain_global$vimp_list$blackboost_model
flchain_blackboost_p_vimp <- plot(flchain_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
flchain_blackboost_p_vimp

flchain_blackboost_summary <- calculate_vimp_summary(flchain_blackboost_vimp, eps = 0)
flchain_blackboost_summary

flchain_blackboost_p_fr <- plot_vimp_with_fr(flchain_blackboost_summary)
flchain_blackboost_p_fr

flchain_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(flchain_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = flchain_perf_blackboost$c_index,
  brier            = flchain_perf_blackboost$brier,
  censoring_rate   = mean(flchain_data$event == 0, na.rm = TRUE)
)


# COX

flchain_cox_vimp <- flchain_global$vimp_list$cox_model
flchain_cox_p_vimp <- plot(flchain_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
flchain_cox_p_vimp

flchain_cox_summary <- calculate_vimp_summary(flchain_cox_vimp, eps = 0)
flchain_cox_summary

flchain_cox_p_fr <- plot_vimp_with_fr(flchain_cox_summary)
flchain_cox_p_fr

flchain_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(flchain_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = flchain_perf_cox$c_index,
  brier            = flchain_perf_cox$brier,
  censoring_rate   = mean(flchain_data$event == 0, na.rm = TRUE)
)


# XGBOOST

flchain_xgboost_vimp <- flchain_global$vimp_list$xgboost_model
flchain_xgboost_p_vimp <- plot(flchain_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
flchain_xgboost_p_vimp

flchain_xgboost_summary <- calculate_vimp_summary(flchain_xgboost_vimp, eps = 0)
flchain_xgboost_summary

flchain_xgboost_p_fr <- plot_vimp_with_fr(flchain_xgboost_summary)
flchain_xgboost_p_fr

flchain_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(flchain_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = flchain_perf_xgboost$c_index,
  brier            = flchain_perf_xgboost$brier,
  censoring_rate   = mean(flchain_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

flchain_plot_data <- rbind(
  flchain_rsf_plot_data,
  flchain_cforest_plot_data,
  flchain_blackboost_plot_data,
  flchain_cox_plot_data,
  flchain_xgboost_plot_data
)


# 3D GRAFİKLER

flchain_p_3d_cindex <- plot_3d_cindex(flchain_plot_data)
flchain_p_3d_cindex

flchain_p_3d_brier <- plot_3d_brier(flchain_plot_data)
flchain_p_3d_brier


# QUADRANT GRAFİKLER

flchain_p_quad_cindex <- plot_quadrant(
  flchain_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
flchain_p_quad_cindex

flchain_p_quad_brier <- plot_quadrant(
  flchain_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
flchain_p_quad_brier


# =============================================================================
# ROSSI ANALİZİ
# =============================================================================

Rossi_data   <- prepare_Rossi(Rossi_raw)
Rossi_task   <- make_task_Rossi(Rossi_data)
Rossi_split  <- data_split(Rossi_task)
Rossi_models <- train_models(Rossi_task, Rossi_split)


# PERFORMANS

Rossi_perf_rsf <- measure_performance(Rossi_models$rsf_model, Rossi_task, Rossi_split)
Rossi_perf_rsf$c_index
Rossi_perf_rsf$brier
Rossi_perf_rsf$auc

Rossi_perf_cforest <- measure_performance(Rossi_models$cforest_model, Rossi_task, Rossi_split)
Rossi_perf_cforest$c_index
Rossi_perf_cforest$brier
Rossi_perf_cforest$auc

Rossi_perf_blackboost <- measure_performance(Rossi_models$blackboost_model, Rossi_task, Rossi_split)
Rossi_perf_blackboost$c_index
Rossi_perf_blackboost$brier
Rossi_perf_blackboost$auc

Rossi_perf_cox <- measure_performance(Rossi_models$cox_model, Rossi_task, Rossi_split)
Rossi_perf_cox$c_index
Rossi_perf_cox$brier
Rossi_perf_cox$auc

Rossi_perf_xgboost <- measure_performance(Rossi_models$xgboost_model, Rossi_task, Rossi_split)
Rossi_perf_xgboost$c_index
Rossi_perf_xgboost$brier
Rossi_perf_xgboost$auc


# GLOBAL AÇIKLAMA

Rossi_global <- global_explain(Rossi_task, Rossi_models)


# RSF

Rossi_rsf_vimp <- Rossi_global$vimp_list$rsf_model
Rossi_rsf_p_vimp <- plot(Rossi_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Rossi_rsf_p_vimp

Rossi_rsf_summary <- calculate_vimp_summary(Rossi_rsf_vimp, eps = 0)
Rossi_rsf_summary

Rossi_rsf_p_fr <- plot_vimp_with_fr(Rossi_rsf_summary)
Rossi_rsf_p_fr

Rossi_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(Rossi_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = Rossi_perf_rsf$c_index,
  brier            = Rossi_perf_rsf$brier,
  censoring_rate   = mean(Rossi_data$event == 0, na.rm = TRUE)
)


# CFOREST

Rossi_cforest_vimp <- Rossi_global$vimp_list$cforest_model
Rossi_cforest_p_vimp <- plot(Rossi_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Rossi_cforest_p_vimp

Rossi_cforest_summary <- calculate_vimp_summary(Rossi_cforest_vimp, eps = 0)
Rossi_cforest_summary

Rossi_cforest_p_fr <- plot_vimp_with_fr(Rossi_cforest_summary)
Rossi_cforest_p_fr

Rossi_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(Rossi_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = Rossi_perf_cforest$c_index,
  brier            = Rossi_perf_cforest$brier,
  censoring_rate   = mean(Rossi_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

Rossi_blackboost_vimp <- Rossi_global$vimp_list$blackboost_model
Rossi_blackboost_p_vimp <- plot(Rossi_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Rossi_blackboost_p_vimp

Rossi_blackboost_summary <- calculate_vimp_summary(Rossi_blackboost_vimp, eps = 0)
Rossi_blackboost_summary

Rossi_blackboost_p_fr <- plot_vimp_with_fr(Rossi_blackboost_summary)
Rossi_blackboost_p_fr

Rossi_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(Rossi_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = Rossi_perf_blackboost$c_index,
  brier            = Rossi_perf_blackboost$brier,
  censoring_rate   = mean(Rossi_data$event == 0, na.rm = TRUE)
)


# COX

Rossi_cox_vimp <- Rossi_global$vimp_list$cox_model
Rossi_cox_p_vimp <- plot(Rossi_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Rossi_cox_p_vimp

Rossi_cox_summary <- calculate_vimp_summary(Rossi_cox_vimp, eps = 0)
Rossi_cox_summary

Rossi_cox_p_fr <- plot_vimp_with_fr(Rossi_cox_summary)
Rossi_cox_p_fr

Rossi_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(Rossi_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = Rossi_perf_cox$c_index,
  brier            = Rossi_perf_cox$brier,
  censoring_rate   = mean(Rossi_data$event == 0, na.rm = TRUE)
)


# XGBOOST

Rossi_xgboost_vimp <- Rossi_global$vimp_list$xgboost_model
Rossi_xgboost_p_vimp <- plot(Rossi_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
Rossi_xgboost_p_vimp

Rossi_xgboost_summary <- calculate_vimp_summary(Rossi_xgboost_vimp, eps = 0)
Rossi_xgboost_summary

Rossi_xgboost_p_fr <- plot_vimp_with_fr(Rossi_xgboost_summary)
Rossi_xgboost_p_fr

Rossi_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(Rossi_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = Rossi_perf_xgboost$c_index,
  brier            = Rossi_perf_xgboost$brier,
  censoring_rate   = mean(Rossi_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

Rossi_plot_data <- rbind(
  Rossi_rsf_plot_data,
  Rossi_cforest_plot_data,
  Rossi_blackboost_plot_data,
  Rossi_cox_plot_data,
  Rossi_xgboost_plot_data
)


# 3D GRAFİKLER

Rossi_p_3d_cindex <- plot_3d_cindex(Rossi_plot_data)
Rossi_p_3d_cindex

Rossi_p_3d_brier <- plot_3d_brier(Rossi_plot_data)
Rossi_p_3d_brier


# QUADRANT GRAFİKLER

Rossi_p_quad_cindex <- plot_quadrant(
  Rossi_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
Rossi_p_quad_cindex

Rossi_p_quad_brier <- plot_quadrant(
  Rossi_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
Rossi_p_quad_brier



# =============================================================================
# BURN ANALİZİ
# =============================================================================

burn_data   <- prepare_burn(burn_raw)
burn_task   <- make_task_burn(burn_data)
burn_split  <- data_split(burn_task)
burn_models <- train_models(burn_task, burn_split)


# PERFORMANS

burn_perf_rsf <- measure_performance(burn_models$rsf_model, burn_task, burn_split)
burn_perf_rsf$c_index
burn_perf_rsf$brier
burn_perf_rsf$auc

burn_perf_cforest <- measure_performance(burn_models$cforest_model, burn_task, burn_split)
burn_perf_cforest$c_index
burn_perf_cforest$brier
burn_perf_cforest$auc

burn_perf_blackboost <- measure_performance(burn_models$blackboost_model, burn_task, burn_split)
burn_perf_blackboost$c_index
burn_perf_blackboost$brier
burn_perf_blackboost$auc

burn_perf_cox <- measure_performance(burn_models$cox_model, burn_task, burn_split)
burn_perf_cox$c_index
burn_perf_cox$brier
burn_perf_cox$auc

burn_perf_xgboost <- measure_performance(burn_models$xgboost_model, burn_task, burn_split)
burn_perf_xgboost$c_index
burn_perf_xgboost$brier
burn_perf_xgboost$auc


# GLOBAL AÇIKLAMA

burn_global <- global_explain(burn_task, burn_models)


# RSF

burn_rsf_vimp <- burn_global$vimp_list$rsf_model
burn_rsf_p_vimp <- plot(burn_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
burn_rsf_p_vimp

burn_rsf_summary <- calculate_vimp_summary(burn_rsf_vimp, eps = 0)
burn_rsf_summary

burn_rsf_p_fr <- plot_vimp_with_fr(burn_rsf_summary)
burn_rsf_p_fr

burn_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(burn_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = burn_perf_rsf$c_index,
  brier            = burn_perf_rsf$brier,
  censoring_rate   = mean(burn_data$event == 0, na.rm = TRUE)
)


# CFOREST

burn_cforest_vimp <- burn_global$vimp_list$cforest_model
burn_cforest_p_vimp <- plot(burn_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
burn_cforest_p_vimp

burn_cforest_summary <- calculate_vimp_summary(burn_cforest_vimp, eps = 0)
burn_cforest_summary

burn_cforest_p_fr <- plot_vimp_with_fr(burn_cforest_summary)
burn_cforest_p_fr

burn_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(burn_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = burn_perf_cforest$c_index,
  brier            = burn_perf_cforest$brier,
  censoring_rate   = mean(burn_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

burn_blackboost_vimp <- burn_global$vimp_list$blackboost_model
burn_blackboost_p_vimp <- plot(burn_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
burn_blackboost_p_vimp

burn_blackboost_summary <- calculate_vimp_summary(burn_blackboost_vimp, eps = 0)
burn_blackboost_summary

burn_blackboost_p_fr <- plot_vimp_with_fr(burn_blackboost_summary)
burn_blackboost_p_fr

burn_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(burn_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = burn_perf_blackboost$c_index,
  brier            = burn_perf_blackboost$brier,
  censoring_rate   = mean(burn_data$event == 0, na.rm = TRUE)
)


# COX

burn_cox_vimp <- burn_global$vimp_list$cox_model
burn_cox_p_vimp <- plot(burn_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
burn_cox_p_vimp

burn_cox_summary <- calculate_vimp_summary(burn_cox_vimp, eps = 0)
burn_cox_summary

burn_cox_p_fr <- plot_vimp_with_fr(burn_cox_summary)
burn_cox_p_fr

burn_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(burn_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = burn_perf_cox$c_index,
  brier            = burn_perf_cox$brier,
  censoring_rate   = mean(burn_data$event == 0, na.rm = TRUE)
)


# XGBOOST

burn_xgboost_vimp <- burn_global$vimp_list$xgboost_model
burn_xgboost_p_vimp <- plot(burn_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
burn_xgboost_p_vimp

burn_xgboost_summary <- calculate_vimp_summary(burn_xgboost_vimp, eps = 0)
burn_xgboost_summary

burn_xgboost_p_fr <- plot_vimp_with_fr(burn_xgboost_summary)
burn_xgboost_p_fr

burn_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(burn_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = burn_perf_xgboost$c_index,
  brier            = burn_perf_xgboost$brier,
  censoring_rate   = mean(burn_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

burn_plot_data <- rbind(
  burn_rsf_plot_data,
  burn_cforest_plot_data,
  burn_blackboost_plot_data,
  burn_cox_plot_data,
  burn_xgboost_plot_data
)


# 3D GRAFİKLER

burn_p_3d_cindex <- plot_3d_cindex(burn_plot_data)
burn_p_3d_cindex

burn_p_3d_brier <- plot_3d_brier(burn_plot_data)
burn_p_3d_brier


# QUADRANT GRAFİKLER

burn_p_quad_cindex <- plot_quadrant(
  burn_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
burn_p_quad_cindex

burn_p_quad_brier <- plot_quadrant(
  burn_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
burn_p_quad_brier




# =============================================================================
# ROTT2 ANALİZİ
# =============================================================================

rott2_data   <- prepare_rott2(rott2_raw)
rott2_task   <- make_task_rott2(rott2_data)
rott2_split  <- data_split(rott2_task)
rott2_models <- train_models(rott2_task, rott2_split)


# PERFORMANS

rott2_perf_rsf <- measure_performance(rott2_models$rsf_model, rott2_task, rott2_split)
rott2_perf_rsf$c_index
rott2_perf_rsf$brier
rott2_perf_rsf$auc

rott2_perf_cforest <- measure_performance(rott2_models$cforest_model, rott2_task, rott2_split)
rott2_perf_cforest$c_index
rott2_perf_cforest$brier
rott2_perf_cforest$auc

rott2_perf_blackboost <- measure_performance(rott2_models$blackboost_model, rott2_task, rott2_split)
rott2_perf_blackboost$c_index
rott2_perf_blackboost$brier
rott2_perf_blackboost$auc

rott2_perf_cox <- measure_performance(rott2_models$cox_model, rott2_task, rott2_split)
rott2_perf_cox$c_index
rott2_perf_cox$brier
rott2_perf_cox$auc

rott2_perf_xgboost <- measure_performance(rott2_models$xgboost_model, rott2_task, rott2_split)
rott2_perf_xgboost$c_index
rott2_perf_xgboost$brier
rott2_perf_xgboost$auc


# GLOBAL AÇIKLAMA

rott2_global <- global_explain(rott2_task, rott2_models)


# RSF

rott2_rsf_vimp <- rott2_global$vimp_list$rsf_model
rott2_rsf_p_vimp <- plot(rott2_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rott2_rsf_p_vimp

rott2_rsf_summary <- calculate_vimp_summary(rott2_rsf_vimp, eps = 0)
rott2_rsf_summary

rott2_rsf_p_fr <- plot_vimp_with_fr(rott2_rsf_summary)
rott2_rsf_p_fr

rott2_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(rott2_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = rott2_perf_rsf$c_index,
  brier            = rott2_perf_rsf$brier,
  censoring_rate   = mean(rott2_data$event == 0, na.rm = TRUE)
)


# CFOREST

rott2_cforest_vimp <- rott2_global$vimp_list$cforest_model
rott2_cforest_p_vimp <- plot(rott2_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rott2_cforest_p_vimp

rott2_cforest_summary <- calculate_vimp_summary(rott2_cforest_vimp, eps = 0)
rott2_cforest_summary

rott2_cforest_p_fr <- plot_vimp_with_fr(rott2_cforest_summary)
rott2_cforest_p_fr

rott2_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(rott2_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = rott2_perf_cforest$c_index,
  brier            = rott2_perf_cforest$brier,
  censoring_rate   = mean(rott2_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

rott2_blackboost_vimp <- rott2_global$vimp_list$blackboost_model
rott2_blackboost_p_vimp <- plot(rott2_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rott2_blackboost_p_vimp

rott2_blackboost_summary <- calculate_vimp_summary(rott2_blackboost_vimp, eps = 0)
rott2_blackboost_summary

rott2_blackboost_p_fr <- plot_vimp_with_fr(rott2_blackboost_summary)
rott2_blackboost_p_fr

rott2_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(rott2_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = rott2_perf_blackboost$c_index,
  brier            = rott2_perf_blackboost$brier,
  censoring_rate   = mean(rott2_data$event == 0, na.rm = TRUE)
)


# COX

rott2_cox_vimp <- rott2_global$vimp_list$cox_model
rott2_cox_p_vimp <- plot(rott2_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rott2_cox_p_vimp

rott2_cox_summary <- calculate_vimp_summary(rott2_cox_vimp, eps = 0)
rott2_cox_summary

rott2_cox_p_fr <- plot_vimp_with_fr(rott2_cox_summary)
rott2_cox_p_fr

rott2_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(rott2_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = rott2_perf_cox$c_index,
  brier            = rott2_perf_cox$brier,
  censoring_rate   = mean(rott2_data$event == 0, na.rm = TRUE)
)


# XGBOOST

rott2_xgboost_vimp <- rott2_global$vimp_list$xgboost_model
rott2_xgboost_p_vimp <- plot(rott2_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rott2_xgboost_p_vimp

rott2_xgboost_summary <- calculate_vimp_summary(rott2_xgboost_vimp, eps = 0)
rott2_xgboost_summary

rott2_xgboost_p_fr <- plot_vimp_with_fr(rott2_xgboost_summary)
rott2_xgboost_p_fr

rott2_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(rott2_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = rott2_perf_xgboost$c_index,
  brier            = rott2_perf_xgboost$brier,
  censoring_rate   = mean(rott2_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

rott2_plot_data <- rbind(
  rott2_rsf_plot_data,
  rott2_cforest_plot_data,
  rott2_blackboost_plot_data,
  rott2_cox_plot_data,
  rott2_xgboost_plot_data
)


# 3D GRAFİKLER

rott2_p_3d_cindex <- plot_3d_cindex(rott2_plot_data)
rott2_p_3d_cindex

rott2_p_3d_brier <- plot_3d_brier(rott2_plot_data)
rott2_p_3d_brier


# QUADRANT GRAFİKLER

rott2_p_quad_cindex <- plot_quadrant(
  rott2_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
rott2_p_quad_cindex

rott2_p_quad_brier <- plot_quadrant(
  rott2_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
rott2_p_quad_brier


# =============================================================================
# D_OROPHA_REC ANALİZİ
# =============================================================================

d_oropha_rec_data   <- prepare_d_oropha_rec(d_oropha_rec_raw)
d_oropha_rec_task   <- make_task_d_oropha_rec(d_oropha_rec_data)
d_oropha_rec_split  <- data_split(d_oropha_rec_task)
d_oropha_rec_models <- train_models(d_oropha_rec_task, d_oropha_rec_split)


# PERFORMANS

d_oropha_rec_perf_rsf <- measure_performance(d_oropha_rec_models$rsf_model, d_oropha_rec_task, d_oropha_rec_split)
d_oropha_rec_perf_rsf$c_index
d_oropha_rec_perf_rsf$brier
d_oropha_rec_perf_rsf$auc

d_oropha_rec_perf_cforest <- measure_performance(d_oropha_rec_models$cforest_model, d_oropha_rec_task, d_oropha_rec_split)
d_oropha_rec_perf_cforest$c_index
d_oropha_rec_perf_cforest$brier
d_oropha_rec_perf_cforest$auc

d_oropha_rec_perf_blackboost <- measure_performance(d_oropha_rec_models$blackboost_model, d_oropha_rec_task, d_oropha_rec_split)
d_oropha_rec_perf_blackboost$c_index
d_oropha_rec_perf_blackboost$brier
d_oropha_rec_perf_blackboost$auc

d_oropha_rec_perf_cox <- measure_performance(d_oropha_rec_models$cox_model, d_oropha_rec_task, d_oropha_rec_split)
d_oropha_rec_perf_cox$c_index
d_oropha_rec_perf_cox$brier
d_oropha_rec_perf_cox$auc

d_oropha_rec_perf_xgboost <- measure_performance(d_oropha_rec_models$xgboost_model, d_oropha_rec_task, d_oropha_rec_split)
d_oropha_rec_perf_xgboost$c_index
d_oropha_rec_perf_xgboost$brier
d_oropha_rec_perf_xgboost$auc


# GLOBAL AÇIKLAMA

d_oropha_rec_global <- global_explain(d_oropha_rec_task, d_oropha_rec_models)


# RSF

d_oropha_rec_rsf_vimp <- d_oropha_rec_global$vimp_list$rsf_model
d_oropha_rec_rsf_p_vimp <- plot(d_oropha_rec_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
d_oropha_rec_rsf_p_vimp

d_oropha_rec_rsf_summary <- calculate_vimp_summary(d_oropha_rec_rsf_vimp, eps = 0)
d_oropha_rec_rsf_summary

d_oropha_rec_rsf_p_fr <- plot_vimp_with_fr(d_oropha_rec_rsf_summary)
d_oropha_rec_rsf_p_fr

d_oropha_rec_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(d_oropha_rec_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = d_oropha_rec_perf_rsf$c_index,
  brier            = d_oropha_rec_perf_rsf$brier,
  censoring_rate   = mean(d_oropha_rec_data$event == 0, na.rm = TRUE)
)


# CFOREST

d_oropha_rec_cforest_vimp <- d_oropha_rec_global$vimp_list$cforest_model
d_oropha_rec_cforest_p_vimp <- plot(d_oropha_rec_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
d_oropha_rec_cforest_p_vimp

d_oropha_rec_cforest_summary <- calculate_vimp_summary(d_oropha_rec_cforest_vimp, eps = 0)
d_oropha_rec_cforest_summary

d_oropha_rec_cforest_p_fr <- plot_vimp_with_fr(d_oropha_rec_cforest_summary)
d_oropha_rec_cforest_p_fr

d_oropha_rec_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(d_oropha_rec_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = d_oropha_rec_perf_cforest$c_index,
  brier            = d_oropha_rec_perf_cforest$brier,
  censoring_rate   = mean(d_oropha_rec_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

d_oropha_rec_blackboost_vimp <- d_oropha_rec_global$vimp_list$blackboost_model
d_oropha_rec_blackboost_p_vimp <- plot(d_oropha_rec_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
d_oropha_rec_blackboost_p_vimp

d_oropha_rec_blackboost_summary <- calculate_vimp_summary(d_oropha_rec_blackboost_vimp, eps = 0)
d_oropha_rec_blackboost_summary

d_oropha_rec_blackboost_p_fr <- plot_vimp_with_fr(d_oropha_rec_blackboost_summary)
d_oropha_rec_blackboost_p_fr

d_oropha_rec_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(d_oropha_rec_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = d_oropha_rec_perf_blackboost$c_index,
  brier            = d_oropha_rec_perf_blackboost$brier,
  censoring_rate   = mean(d_oropha_rec_data$event == 0, na.rm = TRUE)
)


# COX

d_oropha_rec_cox_vimp <- d_oropha_rec_global$vimp_list$cox_model
d_oropha_rec_cox_p_vimp <- plot(d_oropha_rec_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
d_oropha_rec_cox_p_vimp

d_oropha_rec_cox_summary <- calculate_vimp_summary(d_oropha_rec_cox_vimp, eps = 0)
d_oropha_rec_cox_summary

d_oropha_rec_cox_p_fr <- plot_vimp_with_fr(d_oropha_rec_cox_summary)
d_oropha_rec_cox_p_fr

d_oropha_rec_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(d_oropha_rec_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = d_oropha_rec_perf_cox$c_index,
  brier            = d_oropha_rec_perf_cox$brier,
  censoring_rate   = mean(d_oropha_rec_data$event == 0, na.rm = TRUE)
)


# XGBOOST

d_oropha_rec_xgboost_vimp <- d_oropha_rec_global$vimp_list$xgboost_model
d_oropha_rec_xgboost_p_vimp <- plot(d_oropha_rec_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
d_oropha_rec_xgboost_p_vimp

d_oropha_rec_xgboost_summary <- calculate_vimp_summary(d_oropha_rec_xgboost_vimp, eps = 0)
d_oropha_rec_xgboost_summary

d_oropha_rec_xgboost_p_fr <- plot_vimp_with_fr(d_oropha_rec_xgboost_summary)
d_oropha_rec_xgboost_p_fr

d_oropha_rec_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(d_oropha_rec_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = d_oropha_rec_perf_xgboost$c_index,
  brier            = d_oropha_rec_perf_xgboost$brier,
  censoring_rate   = mean(d_oropha_rec_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

d_oropha_rec_plot_data <- rbind(
  d_oropha_rec_rsf_plot_data,
  d_oropha_rec_cforest_plot_data,
  d_oropha_rec_blackboost_plot_data,
  d_oropha_rec_cox_plot_data,
  d_oropha_rec_xgboost_plot_data
)


# 3D GRAFİKLER

d_oropha_rec_p_3d_cindex <- plot_3d_cindex(d_oropha_rec_plot_data)
d_oropha_rec_p_3d_cindex

d_oropha_rec_p_3d_brier <- plot_3d_brier(d_oropha_rec_plot_data)
d_oropha_rec_p_3d_brier


# QUADRANT GRAFİKLER

d_oropha_rec_p_quad_cindex <- plot_quadrant(
  d_oropha_rec_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
d_oropha_rec_p_quad_cindex

d_oropha_rec_p_quad_brier <- plot_quadrant(
  d_oropha_rec_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
d_oropha_rec_p_quad_brier


# =============================================================================
# RDATA ANALİZİ
# =============================================================================

rdata_data   <- prepare_rdata(rdata_raw)
rdata_task   <- make_task_rdata(rdata_data)
rdata_split  <- data_split(rdata_task)
rdata_models <- train_models(rdata_task, rdata_split)


# PERFORMANS

rdata_perf_rsf <- measure_performance(rdata_models$rsf_model, rdata_task, rdata_split)
rdata_perf_rsf$c_index
rdata_perf_rsf$brier
rdata_perf_rsf$auc

rdata_perf_cforest <- measure_performance(rdata_models$cforest_model, rdata_task, rdata_split)
rdata_perf_cforest$c_index
rdata_perf_cforest$brier
rdata_perf_cforest$auc

rdata_perf_blackboost <- measure_performance(rdata_models$blackboost_model, rdata_task, rdata_split)
rdata_perf_blackboost$c_index
rdata_perf_blackboost$brier
rdata_perf_blackboost$auc

rdata_perf_cox <- measure_performance(rdata_models$cox_model, rdata_task, rdata_split)
rdata_perf_cox$c_index
rdata_perf_cox$brier
rdata_perf_cox$auc

rdata_perf_xgboost <- measure_performance(rdata_models$xgboost_model, rdata_task, rdata_split)
rdata_perf_xgboost$c_index
rdata_perf_xgboost$brier
rdata_perf_xgboost$auc


# GLOBAL AÇIKLAMA

rdata_global <- global_explain(rdata_task, rdata_models)


# RSF

rdata_rsf_vimp <- rdata_global$vimp_list$rsf_model
rdata_rsf_p_vimp <- plot(rdata_rsf_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rdata_rsf_p_vimp

rdata_rsf_summary <- calculate_vimp_summary(rdata_rsf_vimp, eps = 0)
rdata_rsf_summary

rdata_rsf_p_fr <- plot_vimp_with_fr(rdata_rsf_summary)
rdata_rsf_p_fr

rdata_rsf_plot_data <- data.frame(
  model            = "RSF",
  mean_instability = mean(rdata_rsf_summary$vimp_instability, na.rm = TRUE),
  c_index          = rdata_perf_rsf$c_index,
  brier            = rdata_perf_rsf$brier,
  censoring_rate   = mean(rdata_data$event == 0, na.rm = TRUE)
)


# CFOREST

rdata_cforest_vimp <- rdata_global$vimp_list$cforest_model
rdata_cforest_p_vimp <- plot(rdata_cforest_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rdata_cforest_p_vimp

rdata_cforest_summary <- calculate_vimp_summary(rdata_cforest_vimp, eps = 0)
rdata_cforest_summary

rdata_cforest_p_fr <- plot_vimp_with_fr(rdata_cforest_summary)
rdata_cforest_p_fr

rdata_cforest_plot_data <- data.frame(
  model            = "CForest",
  mean_instability = mean(rdata_cforest_summary$vimp_instability, na.rm = TRUE),
  c_index          = rdata_perf_cforest$c_index,
  brier            = rdata_perf_cforest$brier,
  censoring_rate   = mean(rdata_data$event == 0, na.rm = TRUE)
)


# BLACKBOOST

rdata_blackboost_vimp <- rdata_global$vimp_list$blackboost_model
rdata_blackboost_p_vimp <- plot(rdata_blackboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rdata_blackboost_p_vimp

rdata_blackboost_summary <- calculate_vimp_summary(rdata_blackboost_vimp, eps = 0)
rdata_blackboost_summary

rdata_blackboost_p_fr <- plot_vimp_with_fr(rdata_blackboost_summary)
rdata_blackboost_p_fr

rdata_blackboost_plot_data <- data.frame(
  model            = "BlackBoost",
  mean_instability = mean(rdata_blackboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = rdata_perf_blackboost$c_index,
  brier            = rdata_perf_blackboost$brier,
  censoring_rate   = mean(rdata_data$event == 0, na.rm = TRUE)
)


# COX

rdata_cox_vimp <- rdata_global$vimp_list$cox_model
rdata_cox_p_vimp <- plot(rdata_cox_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rdata_cox_p_vimp

rdata_cox_summary <- calculate_vimp_summary(rdata_cox_vimp, eps = 0)
rdata_cox_summary

rdata_cox_p_fr <- plot_vimp_with_fr(rdata_cox_summary)
rdata_cox_p_fr

rdata_cox_plot_data <- data.frame(
  model            = "Cox",
  mean_instability = mean(rdata_cox_summary$vimp_instability, na.rm = TRUE),
  c_index          = rdata_perf_cox$c_index,
  brier            = rdata_perf_cox$brier,
  censoring_rate   = mean(rdata_data$event == 0, na.rm = TRUE)
)


# XGBOOST

rdata_xgboost_vimp <- rdata_global$vimp_list$xgboost_model
rdata_xgboost_p_vimp <- plot(rdata_xgboost_vimp) +
  theme(
    legend.title    = element_blank(),
    plot.subtitle   = element_blank(),
    plot.tag        = element_blank(),
    plot.title      = element_blank(),
    legend.position = "bottom"
  )
rdata_xgboost_p_vimp

rdata_xgboost_summary <- calculate_vimp_summary(rdata_xgboost_vimp, eps = 0)
rdata_xgboost_summary

rdata_xgboost_p_fr <- plot_vimp_with_fr(rdata_xgboost_summary)
rdata_xgboost_p_fr

rdata_xgboost_plot_data <- data.frame(
  model            = "XGBoost",
  mean_instability = mean(rdata_xgboost_summary$vimp_instability, na.rm = TRUE),
  c_index          = rdata_perf_xgboost$c_index,
  brier            = rdata_perf_xgboost$brier,
  censoring_rate   = mean(rdata_data$event == 0, na.rm = TRUE)
)


# TÜM MODELLER - PLOT DATA

rdata_plot_data <- rbind(
  rdata_rsf_plot_data,
  rdata_cforest_plot_data,
  rdata_blackboost_plot_data,
  rdata_cox_plot_data,
  rdata_xgboost_plot_data
)


# 3D GRAFİKLER

rdata_p_3d_cindex <- plot_3d_cindex(rdata_plot_data)
rdata_p_3d_cindex

rdata_p_3d_brier <- plot_3d_brier(rdata_plot_data)
rdata_p_3d_brier


# QUADRANT GRAFİKLER

rdata_p_quad_cindex <- plot_quadrant(
  rdata_plot_data,
  perf_col    = "c_index",
  perf_label  = "C-index",
  metric_type = "higher_better"
)
rdata_p_quad_cindex

rdata_p_quad_brier <- plot_quadrant(
  rdata_plot_data,
  perf_col    = "brier",
  perf_label  = "Brier score",
  metric_type = "lower_better"
)
rdata_p_quad_brier


