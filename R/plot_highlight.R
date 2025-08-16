#' Plot per-cluster trim diagnostics with highlighted clusters
#'
#' @description
#' A function that draws per-cluster error curves and highlights clusters whose
#' maximum absolute error meets/exceeds a threshold \code{criteria}
#'
#' @param data_list Either:
#'   \itemize{
#'     \item A list like the return of \code{suggest_trim()}, in which case
#'           \code{data_for_trimming} (or \code{df_name}) will be used; or
#'     \item A data frame containing \code{cluster}, \code{trim_level}, \code{abs_error}.
#'   }
#' @param criteria Numeric scalar. Threshold applied to per-cluster
#'   \code{max(abs_error)} for highlighting. If \code{NULL} and \code{top_n}
#'   is provided, \code{top_n} highlighting is used instead. Default: \code{NULL}.
#'
#' @return A \code{ggplot} object. The vector of highlighted cluster IDs is
#'   attached as attribute \code{"highlighted_clusters"} to the returned plot.
#'
#' @importFrom dplyr group_by summarise mutate filter arrange slice_head distinct pull %>%
#' @importFrom rlang .data
#' @import ggplot2
#' @export

plot_highlight <- function(data_list, criteria) {

  data <- as.data.frame(data_list[[2]]) # Pick dat_st for plot

  highlight_clusters <- data |>
    dplyr::group_by(.data$cluster) |>
    dplyr::filter(max(.data$abs_error) >= .data$criteria) |>
    dplyr::pull(.data$cluster) |>
    unique()

  data <- data |>
    dplyr::mutate(color_group = ifelse(.data$cluster %in% highlight_clusters, "highlight", "normal"))

  ggplot() +
    ggplot2::geom_line(data = data, # trim_level = both bottom and above combined (10% = 5% each)
                       ggplot2::aes(x = .data$trim_level * 100, y = .data$abs_error, group = .data$cluster,
                                    color = .data$color_group, alpha = .data$color_group)) +
    ggplot2::scale_color_manual(values = c("highlight" = "orange", "normal" = "grey50")) +
    ggplot2::scale_alpha_manual(values = c("highlight" = 1, "normal" = 0.2)) +
    ggplot2::xlim(10, 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("% of observations trimmed") +
    ggplot2::ylab("Mean absolute prediction error") +
    ggplot2::theme(legend.position = "none")

}
