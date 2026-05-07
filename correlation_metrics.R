calculate_cor_metrics <- function(data) {
  
  X <- data |>
    dplyr::select(-dplyr::any_of(c("time", "event", "pid"))) |>
    dplyr::select(where(is.numeric))
  
  p <- ncol(X)
  
  cor_mat <- cor(X, use = "pairwise.complete.obs")
  
  # 1. Determinant of Correlation Matrix
  det_R <- det(cor_mat)
  
  # 2. GCD için özdeğerler üzerinden hesap
  eig_vals <- eigen(cor_mat)$values
  R_eigen  <- prod(eig_vals)
  gcd      <- 1 - R_eigen
  
  # 3. Average Absolute Correlation
  upper_tri <- cor_mat[upper.tri(cor_mat)]
  avg_abs_cor <- (2 / (p * (p - 1))) * sum(abs(upper_tri))
  
  return(list(
    det_R       = det_R,
    gcd         = gcd,
    avg_abs_cor = avg_abs_cor,
    cor_matrix  = cor_mat
  ))
}
