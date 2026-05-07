measure_performance <- function(model, task, split) {
  
  preds <- model$predict(task, row_ids = split$test)
  
  c_index <- as.numeric(
    preds$score(
      mlr3::msr("surv.cindex"),
      task = task
    )
  )[1]
  
  brier <- as.numeric(
    preds$score(
      mlr3::msr("surv.brier"),
      task      = task,
      train_set = split$train
    )
  )[1]
  
  auc <- as.numeric(
    preds$score(
      mlr3::msr("surv.uno_auc", integrated = TRUE),
      task      = task,
      train_set = split$train
    )
  )[1]
  
  return(list(
    c_index = c_index,
    brier   = brier,
    auc     = auc,
    preds   = preds
  ))
}
