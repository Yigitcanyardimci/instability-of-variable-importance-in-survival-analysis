prepare_divat2 <- function(data) {
  data |> mutate(
    fac_hla          = as.numeric(as.factor(fac_hla)),
    fac_retransplant = as.numeric(as.factor(fac_retransplant)),
    fac_ecd          = as.numeric(as.factor(fac_ecd))
  )
}


prepare_grace <- function(data) {
  data |> mutate(
    fac_revasc   = as.numeric(as.factor(fac_revasc)),
    fac_stchange = as.numeric(as.factor(fac_stchange))
  )
}

prepare_melanoma <- function(data) {
  data |> mutate(
    fac_sex   = as.numeric(as.factor(fac_sex)),
    fac_ulcer = as.numeric(as.factor(fac_ulcer))
  )
}

prepare_ova <- function(data) {
  data |> mutate(
    fac_karn = as.numeric(as.factor(fac_karn)),
    fac_diam = as.numeric(as.factor(fac_diam)),
    fac_figo = as.numeric(as.factor(fac_figo))
  )
}


prepare_heart <- function(data) {
  data |> mutate(
    fac_surgery    = as.numeric(as.factor(fac_surgery)),
    fac_transplant = as.numeric(as.factor(fac_transplant))
  )
}




prepare_aids2 <- function(data) {
  data |> mutate(
    fac_state   = as.numeric(as.factor(fac_state)),
    fac_sex     = as.numeric(as.factor(fac_sex)),
    fac_T_categ = as.numeric(as.factor(fac_T_categ))
  )
}

prepare_veteran <- function(data) {
  data |> mutate(
    fac_trt      = as.numeric(as.factor(fac_trt)),
    fac_celltype = as.numeric(as.factor(fac_celltype)),
    fac_prior    = as.numeric(as.factor(fac_prior))
  )
}

prepare_breast <- function(data) {
  data |> mutate(
    fac_TumorStage = as.numeric(as.factor(fac_TumorStage)),
    fac_NodalStatus = as.numeric(as.factor(fac_NodalStatus)),
    fac_Histology   = as.numeric(as.factor(fac_Histology)),
    fac_CathepsinD  = as.numeric(as.factor(fac_CathepsinD))
  )
}

prepare_e1684 <- function(data) {
  data |> mutate(
    fac_sex = as.numeric(as.factor(fac_sex)),
    fac_trt = as.numeric(as.factor(fac_trt))
  )
}

prepare_ovarian <- function(data) {
  data |> mutate(
    fac_resid_ds = as.numeric(as.factor(fac_resid_ds)),
    fac_rx       = as.numeric(as.factor(fac_rx)),
    fac_ecog_ps  = as.numeric(as.factor(fac_ecog_ps))
  )
}

prepare_heart <- function(data) {
  data |> mutate(
    fac_surgery    = as.numeric(as.factor(fac_surgery)),
    fac_transplant = as.numeric(as.factor(fac_transplant))
  )
}

prepare_epileptic <- function(data) {
  data |> mutate(
    fac_treat     = as.numeric(as.factor(fac_treat)),
    fac_gender    = as.numeric(as.factor(fac_gender)),
    fac_learn_dis = as.numeric(as.factor(fac_learn_dis))
  )
}

prepare_dialysis <- function(data) {
  data |> mutate(
    fac_center  = as.numeric(as.factor(fac_center)),
    fac_disease = as.numeric(as.factor(fac_disease))
  )
}

prepare_pbc <- function(data) {
  data |> mutate(
    fac_sex     = as.numeric(as.factor(fac_sex)),
    fac_trt     = as.numeric(as.factor(fac_trt)),
    fac_ascites = as.numeric(as.factor(fac_ascites)),
    fac_hepato  = as.numeric(as.factor(fac_hepato)),
    fac_spiders = as.numeric(as.factor(fac_spiders))
  )
}



# 21.04

  prepare_prostateSurvival <- function(data) {
    data |>
      mutate(
        fac_grade    = as.numeric(as.factor(fac_grade)),
        fac_stage    = as.numeric(as.factor(fac_stage)),
        fac_ageGroup = as.numeric(as.factor(fac_ageGroup))
      ) |>
      na.omit()
  }
  
  prepare_acath <- function(data) {
    data |>
      mutate(
        fac_sex = as.numeric(as.factor(fac_sex))
      ) |>
      na.omit()
  }
  
  prepare_glioma <- function(data) {
    data |>
      mutate(
        fac_sex       = as.numeric(as.factor(fac_sex)),
        fac_histology = as.numeric(as.factor(fac_histology)),
        fac_group     = as.numeric(as.factor(fac_group))
      ) |>
      na.omit()
  }
  
  prepare_prostate <- function(data) {
    data |>
      mutate(
        fac_stage = as.numeric(as.factor(fac_stage)),
        fac_rx    = as.numeric(as.factor(fac_rx)),
        fac_pf    = as.numeric(as.factor(fac_pf)),
        fac_hx    = as.numeric(as.factor(fac_hx)),
        fac_ekg   = as.numeric(as.factor(fac_ekg)),
        fac_bm    = as.numeric(as.factor(fac_bm))
      ) |>
      na.omit()
  }
  
  prepare_dataDIVAT1 <- function(data) {
    data |>
      mutate(
        fac_sexR    = as.numeric(as.factor(fac_sexR)),
        fac_graft   = as.numeric(as.factor(fac_graft)),
        fac_year_tx = as.numeric(as.factor(fac_year_tx))
      ) |>
      na.omit()
  }
  
  prepare_dataDIVAT3 <- function(data) {
    data |>
      mutate(
        fac_sexeR     = as.numeric(as.factor(fac_sexeR)),
        fac_year_tx   = as.numeric(as.factor(fac_year_tx)),
        fac_ante_diab = as.numeric(as.factor(fac_ante_diab)),
        fac_pra       = as.numeric(as.factor(fac_pra))
      ) |>
      na.omit()
  }
  
  
  prepare_TRACE <- function(data) {
    data |>
      mutate(
        fac_sex      = as.numeric(as.factor(fac_sex)),
        fac_chf      = as.numeric(as.factor(fac_chf)),
        fac_diabetes = as.numeric(as.factor(fac_diabetes)),
        fac_vf       = as.numeric(as.factor(fac_vf))
      ) |>
      na.omit()
  }
  
  prepare_FRTCS <- function(data) {
    data |>
      mutate(
        fac_sex     = as.numeric(as.factor(fac_sex)),
        fac_antihyp = as.numeric(as.factor(fac_antihyp))
      ) |>
      na.omit()
  }
  
  prepare_zinc <- function(data) {
    data |>
      mutate(
        fac_sex      = as.numeric(as.factor(fac_sex)),
        fac_agestr   = as.numeric(as.factor(fac_agestr)),
        fac_dysp1    = as.numeric(as.factor(fac_dysp1)),
        fac_dysp2    = as.numeric(as.factor(fac_dysp2)),
        fac_smoke    = as.numeric(as.factor(fac_smoke)),
        fac_drink    = as.numeric(as.factor(fac_drink)),
        fac_basehist = as.numeric(as.factor(fac_basehist)),
        fac_sevdysp  = as.numeric(as.factor(fac_sevdysp)),
        fac_moddysp  = as.numeric(as.factor(fac_moddysp)),
        fac_mildysp  = as.numeric(as.factor(fac_mildysp)),
        fac_zincset  = as.numeric(as.factor(fac_zincset))
      ) |>
      na.omit()
  }
  
  prepare_stagec <- function(data) {
    data |>
      mutate(
        fac_ploidy  = as.numeric(as.factor(fac_ploidy)),
        fac_eet     = as.numeric(as.factor(fac_eet)),
        fac_grade   = as.numeric(as.factor(fac_grade)),
        fac_gleason = as.numeric(as.factor(fac_gleason))
      ) |>
      na.omit()
  }
  
  prepare_LeukSurv <- function(data) {
    data |>
      mutate(
        fac_sex      = as.numeric(as.factor(fac_sex)),
        fac_district = as.numeric(as.factor(fac_district))
      ) |>
      na.omit()
  }
  
  prepare_nwtco <- function(data) {
    data |>
      mutate(
        fac_stage        = as.numeric(as.factor(fac_stage)),
        fac_study        = as.numeric(as.factor(fac_study)),
        fac_in_subcohort = as.numeric(as.factor(fac_in_subcohort)),
        fac_instit       = as.numeric(as.factor(fac_instit)),
        fac_histol       = as.numeric(as.factor(fac_histol))
      ) |>
      na.omit()
  }
  
  prepare_retinopathy <- function(data) {
    data |>
      mutate(
        fac_laser = as.numeric(as.factor(fac_laser)),
        fac_eye   = as.numeric(as.factor(fac_eye)),
        fac_type  = as.numeric(as.factor(fac_type)),
        fac_trt   = as.numeric(as.factor(fac_trt)),
        fac_risk  = as.numeric(as.factor(fac_risk))
      ) |>
      na.omit()
  }
  
  prepare_Framingham <- function(data) {
    data |>
      mutate(
        fac_sex   = as.numeric(as.factor(fac_sex)),
        fac_month = as.numeric(as.factor(fac_month))
      ) |>
      na.omit()
  }
  
  prepare_csl <- function(data) {
    data |>
      mutate(
        fac_sex   = as.numeric(as.factor(fac_sex)),
        fac_treat = as.numeric(as.factor(fac_treat))
      ) |>
      na.omit()
  }
  
  prepare_cancer <- function(data) {
    data |>
      mutate(
        fac_inst    = as.numeric(as.factor(fac_inst)),
        fac_ph_ecog = as.numeric(as.factor(fac_ph_ecog)),
        fac_sex     = as.numeric(as.factor(fac_sex))
      ) |>
      na.omit()
  }
  
  prepare_whas500 <- function(data) {
    data |>
      mutate(
        fac_gender = as.numeric(as.factor(fac_gender)),
        fac_cvd    = as.numeric(as.factor(fac_cvd)),
        fac_afb    = as.numeric(as.factor(fac_afb)),
        fac_sho    = as.numeric(as.factor(fac_sho)),
        fac_chf    = as.numeric(as.factor(fac_chf)),
        fac_av3    = as.numeric(as.factor(fac_av3)),
        fac_miord  = as.numeric(as.factor(fac_miord)),
        fac_mitype = as.numeric(as.factor(fac_mitype)),
        fac_year   = as.numeric(as.factor(fac_year))
      ) |>
      na.omit()
  }
  
  prepare_cgd <- function(data) {
    data |>
      mutate(
        fac_center   = as.numeric(as.factor(fac_center)),
        fac_treat    = as.numeric(as.factor(fac_treat)),
        fac_sex      = as.numeric(as.factor(fac_sex)),
        fac_inherit  = as.numeric(as.factor(fac_inherit)),
        fac_steroids = as.numeric(as.factor(fac_steroids)),
        fac_propylac = as.numeric(as.factor(fac_propylac)),
        fac_hos_cat  = as.numeric(as.factor(fac_hos_cat))
      ) |>
      na.omit()
  }
  
  prepare_Pbc3 <- function(data) {
    data |>
      mutate(
        fac_unit    = as.numeric(as.factor(fac_unit)),
        fac_tment   = as.numeric(as.factor(fac_tment)),
        fac_sex     = as.numeric(as.factor(fac_sex)),
        fac_stage   = as.numeric(as.factor(fac_stage)),
        fac_gibleed = as.numeric(as.factor(fac_gibleed))
      ) |>
      na.omit()
  }
  
  prepare_GBSG2 <- function(data) {
    data |>
      mutate(
        fac_horTh    = as.numeric(as.factor(fac_horTh)),
        fac_menostat = as.numeric(as.factor(fac_menostat)),
        fac_tgrade   = as.numeric(as.factor(fac_tgrade))
      ) |>
      na.omit()
  }
  
  prepare_colon <- function(data) {
    data |>
      mutate(
        fac_rx       = as.numeric(as.factor(fac_rx)),
        fac_sex      = as.numeric(as.factor(fac_sex)),
        fac_differ   = as.numeric(as.factor(fac_differ)),
        fac_obstruct = as.numeric(as.factor(fac_obstruct)),
        fac_perfor   = as.numeric(as.factor(fac_perfor)),
        fac_adhere   = as.numeric(as.factor(fac_adhere)),
        fac_node4    = as.numeric(as.factor(fac_node4))
      ) |>
      na.omit()
  }
  
  prepare_mgus <- function(data) {
    data |>
      mutate(
        fac_sex  = as.numeric(as.factor(fac_sex)),
        fac_pcdx = as.numeric(as.factor(fac_pcdx))
      ) |>
      na.omit()
  }
  
  prepare_flchain <- function(data) {
    data |>
      mutate(
        fac_sex       = as.numeric(as.factor(fac_sex)),
        fac_chapter   = as.numeric(as.factor(fac_chapter)),
        fac_sample_yr = as.numeric(as.factor(fac_sample_yr)),
        fac_mgus      = as.numeric(as.factor(fac_mgus))
      ) |>
      na.omit()
  }

  
  prepare_Rossi <- function(data) {
    data |>
      mutate(
        fac_fin  = as.numeric(as.factor(fac_fin)),
        fac_race = as.numeric(as.factor(fac_race)),
        fac_wexp = as.numeric(as.factor(fac_wexp)),
        fac_mar  = as.numeric(as.factor(fac_mar)),
        fac_paro = as.numeric(as.factor(fac_paro)),
        fac_prio = as.numeric(as.factor(fac_prio)),
        fac_educ = as.numeric(as.factor(fac_educ)),
        fac_emp  = as.numeric(as.factor(fac_emp))
      ) |>
      na.omit()
  }
  
  prepare_burn <- function(data) {
    data |>
      mutate(
        fac_burn_buttock   = as.numeric(as.factor(fac_burn_buttock)),
        fac_burn_head      = as.numeric(as.factor(fac_burn_head)),
        fac_burn_lower_leg = as.numeric(as.factor(fac_burn_lower_leg)),
        fac_burn_resp      = as.numeric(as.factor(fac_burn_resp)),
        fac_burn_trunk     = as.numeric(as.factor(fac_burn_trunk)),
        fac_burn_type      = as.numeric(as.factor(fac_burn_type)),
        fac_burn_upper_leg = as.numeric(as.factor(fac_burn_upper_leg)),
        fac_race           = as.numeric(as.factor(fac_race)),
        fac_sex            = as.numeric(as.factor(fac_sex)),
        fac_treatment      = as.numeric(as.factor(fac_treatment))
      ) |>
      na.omit()
  }
  
  prepare_actg <- function(data) {
    data |>
      mutate(
        fac_tx       = as.numeric(as.factor(fac_tx)),
        fac_txgrp    = as.numeric(as.factor(fac_txgrp)),
        fac_strat2   = as.numeric(as.factor(fac_strat2)),
        fac_sex      = as.numeric(as.factor(fac_sex)),
        fac_raceth   = as.numeric(as.factor(fac_raceth)),
        fac_ivdrug   = as.numeric(as.factor(fac_ivdrug)),
        fac_hemophil = as.numeric(as.factor(fac_hemophil))
      ) |>
      na.omit()
  }
  
  prepare_rott2 <- function(data) {
    data |>
      mutate(
        fac_meno   = as.numeric(as.factor(fac_meno)),
        fac_tsize  = as.numeric(as.factor(fac_tsize)),
        fac_grade  = as.numeric(as.factor(fac_grade)),
        fac_hormon = as.numeric(as.factor(fac_hormon)),
        fac_chemo  = as.numeric(as.factor(fac_chemo)),
        fac_recent = as.numeric(as.factor(fac_recent))
      ) |>
      na.omit()
  }
  
  prepare_d_oropha_rec <- function(data) {
    data |>
      mutate(
        fac_inst   = as.numeric(as.factor(fac_inst)),
        fac_sex    = as.numeric(as.factor(fac_sex)),
        fac_treatm = as.numeric(as.factor(fac_treatm)),
        fac_grade  = as.numeric(as.factor(fac_grade)),
        fac_cond   = as.numeric(as.factor(fac_cond)),
        fac_site   = as.numeric(as.factor(fac_site)),
        fac_tstage = as.numeric(as.factor(fac_tstage)),
        fac_nstage = as.numeric(as.factor(fac_nstage))
      ) |>
      na.omit()
  }
  
  prepare_rdata <- function(data) {
    data |>
      mutate(
        fac_sex   = as.numeric(as.factor(fac_sex)),
        fac_agegr = as.numeric(as.factor(fac_agegr))
      ) |>
      na.omit()
  }





