train_models <- function(task, split) {
  
  set.seed(123)
  
  rsf_model <- lrn("surv.rfsrc", id = "rfsrc")
  rsf_model$train(task, row_ids = split$train)
  
  cforest_model <- lrn("surv.cforest", id = "cforest")
  cforest_model$train(task, row_ids = split$train)
  
  blackboost_model <- lrn("surv.blackboost", id = "blackboost")
  blackboost_model$train(task, row_ids = split$train)
  
  cox_model <- lrn("surv.coxph", id = "cox")
  cox_model$train(task, row_ids = split$train)
  
  xgboost_model <- lrn("surv.xgboost.cox", id = "xgboost")
  xgboost_model$train(task, row_ids = split$train)
  
  return(list(
    rsf_model        = rsf_model,
    cforest_model    = cforest_model,
    blackboost_model = blackboost_model,
    cox_model        = cox_model,
    xgboost_model    = xgboost_model
  ))
}
