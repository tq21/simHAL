#' @title Make repeated measures data for pooled logistic regression
#' for estimating conditional density using hazard regression.
#'
#' @param W A matrix of covariates.
#' @param A A vector of discretized treatments.
#'
#' @export
make_rep_data <- function(W, A) {

  tau <- length(unique(A))

  # make repeated data for pooled logistic regression
  rep_data <- data.table(W, A = A)
  rep_data <- rep_data[rep(1:.N, each = tau)]
  rep_data$id <- rep(seq(length(A)), each = tau)
  rep_data$a <- rep(seq(tau), length(A))
  rep_data <- rep_data[, A_a := as.numeric(A == a)]
  rep_data_small <- copy(rep_data)
  rep_data_small <- rep_data_small[, filter_rows(.SD, "A_a"), by = id]

  return(list(rep_data = rep_data,
              rep_data_small = rep_data_small))
}
