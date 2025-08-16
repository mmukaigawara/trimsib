#' G-K Mortality Estimator with Robust SEs
#'
#' @description
#' A function that computes the G-K mortality rate estimator
#'
#' @param dat_tr A data frame containing at least the numeric columns
#'   \code{Dj}, \code{Sj}, \code{Wj}, and \code{Mj}.
#' @param max_Sj Integer scalar. Maximum \code{S_j} to include in the regression
#'   used to estimate \eqn{\alpha}. Defaults to \code{15}.
#' @param se_type Character scalar passed to \code{estimatr::lm_robust} (e.g., \code{"HC2"}).
#'
#' @return A tibble with columns: mortality rate (point estimates), lower and upper 95% CI, and the variance.
#'
#' @details
#' The internal regression is \code{logsumDSratio ~ Sj + I(Sj^2)} fit by
#' \code{estimatr::lm_robust} on aggregated data by \code{Sj}.
#'
#' @export
#' @importFrom dplyr mutate group_by summarise filter
#' @importFrom tibble tibble
#' @importFrom stats var cov mean
#' @importFrom rlang .data
#' @import estimatr

get_mort <- function(dat_tr, max_Sj = 15, se_type = "HC2") {

  sumdat <- dat_tr |>
    dplyr::mutate(DSratio = .data$Dj / .data$Sj) |>
    dplyr::group_by(.data$Sj) |>
    dplyr::summarise(logsumDSr = log(sum(.data$DSratio)), .groups = "drop") |>
    dplyr::filter(.data$Sj <= max_Sj, is.finite(.data$logsumDSr))

  fit <- estimatr::lm_robust(logsumDSr ~ Sj + I(Sj^2),
                             se_type = se_type, data = sumdat)

  # Obtain alpha
  alpha    <- unname(fit$coefficients[1])
  alpha_se <- unname(fit$std.error[1])

  # Obtain xi
  xi <- exp(alpha + (alpha_se^2) / 2)

  # Obtain variance
  J     <- nrow(dat_tr)
  varX  <- J * stats::var(dat_tr$Wj * dat_tr$Mj, na.rm = TRUE)
  varY  <- J * stats::var(dat_tr$Wj, na.rm = TRUE)
  covXY <- J * stats::cov(dat_tr$Wj, dat_tr$Mj * dat_tr$Wj, use = "complete.obs")

  muX <- J * mean(dat_tr$Wj * dat_tr$Mj, na.rm = TRUE) + xi
  muY <- J * mean(dat_tr$Wj, na.rm = TRUE) + xi

  var_gk <- (varX / muY^2) - 2 * (muX / (muY^3)) * covXY + (muX^2) * varY / (muY^4)

  # Obtain the point estimate and 95% CI
  point_est <- (sum(dat_tr$Wj * dat_tr$Mj, na.rm = TRUE) + xi) /
    (sum(dat_tr$Wj, na.rm = TRUE) + xi)

  se    <- sqrt(var_gk)
  ci_lo <- point_est - 1.96 * se
  ci_hi <- point_est + 1.96 * se

  tibble::tibble(
    `Mortality rate` = point_est,
    `Lower 95%CI`    = ci_lo,
    `Upper 95%CI`    = ci_hi,
    Variance         = var_gk
  )
}
