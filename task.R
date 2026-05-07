make_task_divat2 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "divat2_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}


make_task_glioma <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "glioma_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_grace <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "grace_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_heart <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "heart_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_melanoma <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "melanoma_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_ova <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "ova_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}


make_task_aids2 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "aids2_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_veteran <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "veteran_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_breast <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "breast_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_e1684 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "e1684_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_ovarian <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "ovarian_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_heart <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "heart_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_epileptic <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "epileptic_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_dialysis <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "dialysis_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_pbc <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "pbc_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}




# 21.04

make_task_prostateSurvival <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "prostateSurvival_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}


make_task_acath <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "acath_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_glioma <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "glioma_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_prostate <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "prostate_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_dataDIVAT1 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "dataDIVAT1_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_dataDIVAT3 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "dataDIVAT3_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}


make_task_TRACE <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "TRACE_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_FRTCS <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "FRTCS_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid", "time2")),
    time    = "time",
    event   = "event"
  )
}

make_task_zinc <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "zinc_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_stagec <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "stagec_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_LeukSurv <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "LeukSurv_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_nwtco <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "nwtco_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_retinopathy <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "retinopathy_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_Framingham <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "Framingham_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}


make_task_csl <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "csl_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid", "time2")),
    time    = "time",
    event   = "event"
  )
}

make_task_cancer <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "cancer_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_whas500 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "whas500_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_cgd <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "cgd_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_Pbc3 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "Pbc3_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}


make_task_GBSG2 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "GBSG2_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_colon <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "colon_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_mgus <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "mgus_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_flchain <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "flchain_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}


make_task_Rossi <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "Rossi_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid", "time2")),
    time    = "time",
    event   = "event"
  )
}

make_task_burn <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "burn_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_actg <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "actg_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_rott2 <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "rott2_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_d_oropha_rec <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "d_oropha_rec_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}

make_task_rdata <- function(data) {
  mlr3proba::TaskSurv$new(
    id      = "rdata_task",
    backend = data |> dplyr::select(-dplyr::one_of("pid")),
    time    = "time",
    event   = "event"
  )
}









