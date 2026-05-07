global_explain <- function(task, models) {
  
  X <- as.data.frame(task$data()) |> dplyr::select(-time, -event)
  y <- task$truth()
  
  distr_fn <- function(model, newdata) model$predict_newdata(newdata)$distr
  lp_fn    <- function(model, newdata) model$predict_newdata(newdata)$lp
  
  predict_fns <- list(
    rsf_model        = distr_fn,
    cforest_model    = distr_fn,
    blackboost_model = lp_fn,
    cox_model        = lp_fn,
    xgboost_model    = lp_fn
  )
  
  explainers <- lapply(names(models), function(nm) {
    tryCatch(
      survex::explain(models[[nm]], data = X, y = y, predict_function = predict_fns[[nm]]),
      error = function(e) { message(nm, " explainer hata: ", e$message); NULL }
    )
  })
  names(explainers) <- names(models)
  
  vimp_list <- lapply(names(explainers), function(nm) {
    if (is.null(explainers[[nm]])) return(NULL)
    tryCatch(
      survex::model_parts(explainers[[nm]]),
      error = function(e) { message(nm, " model_parts hata: ", e$message); NULL }
    )
  })
  names(vimp_list) <- names(models)
  
  list(
    explainers = explainers,
    vimp_list  = vimp_list
  )
}
