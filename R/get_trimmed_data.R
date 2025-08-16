#' Select clusters to trim and return a quantile-trimmed dataset
#'
#' @description
#' A function that performs within-cluster symmetric trimming on
#' \code{vec_var} at level \code{trim_level}
#'
#' @param data_list Either:
#'   \itemize{
#'     \item the list returned by \code{suggest_trim()} (containing
#'           \code{original_data} and \code{data_for_trimming}); OR
#'     \item a data frame like \code{data_for_trimming} (with columns
#'           \code{cluster}, \code{trim_level}, \code{abs_error}); OR
#'     \item a named list/data frame where element \code{df_name_trim} is the
#'           trimming diagnostics and \code{df_name_orig} is the original data.
#'   }
#' @param criteria Numeric scalar threshold applied to per-cluster
#'   \code{max(abs_error)}; clusters with \code{max(abs_error) >= criteria}
#'   are trimmed. If \code{NULL} and \code{top_n} is provided, \code{top_n}
#'   rule is used. (Default \code{NULL}.)
#' @param trim_level Numeric in \eqn{(0, 1)}. Symmetric trim proportion (e.g.,
#'   \code{0.10} drops 5% in each tail).
#' @param vec_var String. Name of the numeric column to be trimmed within
#'   selected clusters (default \code{"Vj"}).
#'
#' @return
#' A data frame identical to the original input data (column-wise), plus a
#' logical/numeric \code{trim} column (\code{1} = trimmed cluster, \code{0} = not
#' trimmed). The vector of cluster IDs that were trimmed is attached as
#' attribute \code{"trimmed_clusters"}.
#'
#' @importFrom dplyr group_by summarise filter mutate bind_rows ungroup pull distinct %>%
#' @importFrom rlang .data sym
#' @importFrom stats quantile
#' @export

get_trimmed_data <- function(data_list, criteria, trim_level, vec_var = "Vj") {

  data <- as.data.frame(data_list[[2]])
  dat_original <- as.data.frame(data_list[[1]])

  trim_clusters <- data |>
    dplyr::group_by(.data$cluster) |>
    dplyr::filter(max(.data$abs_error) >= criteria) |>
    dplyr::pull(.data$cluster) |>
    unique()

  dat_original <- dat_original |>
    dplyr::mutate(trim = ifelse(.data$cluster %in% trim_clusters, 1, 0)) # 1 = trim

  # For clusters with trim == 1, obtain vectors and coresp obs, trim them, and return
  dat_trimmed <- dat_original |>
    dplyr::filter(.data$trim == 1) |>
    dplyr::group_by(.data$cluster) |>
    dplyr::filter(
      dplyr::between(
        .data[[vec_var]],
        stats::quantile(.data[[vec_var]], probs = trim_level/2, na.rm = TRUE, names = FALSE),
        stats::quantile(.data[[vec_var]], probs = 1 - trim_level/2, na.rm = TRUE, names = FALSE)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::bind_rows(dat_original |> dplyr::filter(.data$trim == 0)) # Merge with original with trim == 0


  return(dat_trimmed)

}
