#' Cluster-wise trimming diagnostics and suggested trim levels
#'
#' @description
#' A funciton that computes trimmed means
#'
#' @param data A data frame.
#' @param vec_var String. Name of the numeric column to trim (e.g., \code{"Vj"}).
#' @param formula_rhs String. RHS of a formula in terms of \code{x}
#'   (e.g., \code{"x + I(x^2)"}). The response is internally set to \code{val}.
#' @param cl_var String. Cluster column name (default \code{"cluster"}).
#' @param j_grid Numeric vector of candidate trim thresholds \eqn{j}.
#'   Defaults to \code{seq(0.10, 0.49, by = 0.05)} to ensure \eqn{\ge 3} points
#'   for quadratic fits.
#'
#' @return
#' \itemize{
#'   \item \code{get_cl_to_trim()}: A numeric vector of average absolute errors
#'         (same length/order as \code{j_grid}).
#'   \item \code{suggest_trim()}: A list with:
#'     \describe{
#'       \item{\code{original_data}}{Input \code{data} with an added/standardized \code{cluster} column.}
#'       \item{\code{data_for_trimming}}{Tibble with columns \code{cluster}, \code{trim_level}, \code{abs_error}.}
#'     }
#' }
#'
#' @examples
#' # Example:
#' # suggest_trim(dat, vec_var = "Vj", cl_var = "cluster", formula_rhs = "x + I(x^2)")
#'
#' @importFrom stats lm predict
#' @importFrom dplyr group_by group_modify rename bind_cols filter %>%
#' @importFrom rlang .data sym
#' @importFrom purrr map_dbl
#' @export
get_cl_to_trim <- function(data,
                           vec_var,
                           formula) {

  # Identify the vector for trimming
  vec <- data[[vec_var]]

  # Trimmed means
  vec_trim <- sapply(seq(0, 0.5, by = 0.005), function(x) mean(vec, trim = x, na.rm = TRUE))
  trimdata <- data.frame(val = vec_trim, x = seq(0, 1, by = 0.01)) # Trimmed data

  # For each j, predict and compare
  form <- as.formula(paste("val ~", formula))

  purrr::map_dbl(seq(0.1, 0.5, by = 0.05), function(j) {

    trimdata_pred <- trimdata |> dplyr::filter(x >= j)
    x_pred <- rev(j - 0.01 * 2 * (1:5))

    model <- stats::lm(form, data = trimdata_pred)

    new_preds <- data.frame(
      x = x_pred,
      val = stats::predict(model, newdata = data.frame(x = x_pred))
    )

    res <- dplyr::bind_cols(new_preds, trimdata |> dplyr::filter(round(x, 4) %in% round(x_pred, 4)))

    mean(abs(res[, 2] - res[, 3]))
  })

}

#' @rdname get_cl_to_trim
#' @export
suggest_trim <- function(data, vec_var = "Vj", cl_var = "cluster", formula = "x + I(x^2)") {

  dat_st <- data |>
    dplyr::group_by(!!rlang::sym(cl_var)) |>
    dplyr::group_modify(~ {
      errors <- get_cl_to_trim(.x, vec_var, formula)
      tibble::tibble(trim_level = seq(0.1, 0.5, by = 0.05), abs_error = errors)
    })

  data <- data |> dplyr::rename(cluster = !!(sym(cl_var)))

  return(list(original_data = data, data_for_trimming = dat_st))

}
