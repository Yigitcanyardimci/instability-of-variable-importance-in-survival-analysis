time_col <- "_times_"

exclude_cols <- c(
  time_col, "_full_model_", "_baseline_", "_permutation_", "label", "event"
)

trapz <- function(x, y) {
  o <- order(x)
  x <- x[o]
  y <- y[o]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2, na.rm = TRUE)
}

vimp_to_long <- function(vimp_obj) {
  res <- as.data.frame(vimp_obj$result)
  vimp_vars <- setdiff(names(res), exclude_cols)
  
  res %>%
    dplyr::select(all_of(c(time_col, vimp_vars))) %>%
    tidyr::pivot_longer(
      -all_of(time_col),
      names_to  = "variable",
      values_to = "vimp"
    ) %>%
    dplyr::arrange(variable, .data[[time_col]])
}

calculate_vimp_summary <- function(vimp_obj, eps = 0.01) {
  vimp_to_long(vimp_obj) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      
      vimp_area = trapz(.data[[time_col]], pmax(vimp, 0)),
      
      normalized_vimp_area = {
        t <- .data[[time_col]]
        y <- pmax(vimp, 0)
        trapz(t, y) / (max(t) - min(t))
      },
      
      vimp_instability = {
        y <- vimp[!is.na(vimp)]
        d <- diff(y)
        d[abs(d) < eps] <- 0
        d <- sign(d)
        n <- length(d)
        
        if (n < 2) {
          NA_real_
        } else {
          sum(d[-n] != d[-1]) / n
        }
      },
      
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(normalized_vimp_area))
}

# 5 model için wrapper
calculate_all_vimp_summaries <- function(vimp_list, eps = 0.01) {
  lapply(names(vimp_list), function(nm) {
    if (is.null(vimp_list[[nm]])) return(NULL)
    
    tryCatch(
      calculate_vimp_summary(vimp_list[[nm]], eps = eps) %>%
        dplyr::mutate(model = nm, eps = eps),
      error = function(e) {
        message(nm, " vimp_summary hata: ", e$message)
        NULL
      }
    )
  }) |>
    Filter(Negate(is.null), x = _) |>
    dplyr::bind_rows()
}
