#' Create a link data.frame
#' @description
#' At present, this function is still in the experimental stage. `mantel_df` only
#' supports the results of \code{\link[gghpccl]{tidy_mantel}} or \code{\link[gghpcc]{tidy_mantel_pair}}, and `corr_df` only
#' supports the results of  \code{\link[ggcorrr]{fortify_corr}}.
#' @param mantel_df mantel's test data.frame, must be returned by \code{\link[gghpcc]{fortify_mantel}}.
#' @param corr_df correalation's test data.frame, must be returned by \code{\link[ggcorrr]{fortify_corr}}.
#' @param type correalation matrix plot's type.
#' @param var_grp_name the column name of link group variable in `mantel_df`.
#' @param var_grp_name the column name of link matrix variable in `mantel_df`.
#' @param show_diag logical, if TRUE, correalation matrix plot will don't show diagonal.
#' @param grp_hjust a numeric vector to adjust the horizonal position of group points.
#' @param grp_vjust a numeric vector to adjust the vertical position of group points.
#' @param mat_hjust a numeric vector to adjust the horizonal position of correalation matrixgroup points.
#' @param mat_vjust a numeric vector to adjust the vertical position of correalation matrixgroup points.
#' @return a data.frame.
#' @seealso [gghpcc::tidy_mantel()], [gghpcc::tidy_mantel_pair()], [ggcorrr::fortify_corr().
#' @export
get_link_data <- function(
                          mantel_df,
                          corr_df,
                          type = c("upper", "lower"),
                          var_grp_name = NULL,
                          var_mat_name = NULL,
                          show_diag = FALSE,
                          grp_hjust = NULL,
                          grp_vjust = NULL,
                          mat_hjust = NULL,
                          mat_vjust = NULL)
{
  type <- match.arg(type)
  stopifnot(inherits(mantel_df, "mantel_df"))
  stopifnot(inherits(corr_df, "ggcorrr_df"))
  var_grp_name <- var_grp_name %||% "spec"
  var_mat_name <- var_mat_name %||% "env"
  grp_name <- unique(mantel_df[[var_grp_name]])
  mat_name <- levels(corr_df[["y"]])
  n <- length(mat_name)
  grp_link <- link_grp_data(name = grp_name, n = n, type = type,
                         hjust = grp_hjust, vjust = grp_vjust)
  mat_link <- link_mat_data(name = mat_name, type = type, show_diag = show_diag,
                          hjust = mat_hjust, vjust = mat_vjust)
  out <- mantel_df %>%
    dplyr::left_join(mat_link, by = setNames("mat_name", var_mat_name)) %>%
    dplyr::left_join(grp_link, by = setNames("grp_name", var_grp_name))
  class(out) <- c("mantel_link_df", "mantel_df", "data.frame")
  out
}

#' @noRd
link_grp_data <- function(name, n, type, hjust = NULL, vjust = NULL)
{
  len <- length(name)
  if(!is.null(hjust) && length(hjust) != len)
    hjust <- rep_len(hjust, len)
  if(!is.null(vjust) && length(hjust) != len)
    vjust <- rep_len(vjust, len)
  if(type == "upper") {
    if(len == 1) {
      x <- 0.5 + 0.18 * n
      y <- 0.5 + 0.3 * n
    } else if(len == 2) {
      x <- c(0.5 - 0.02 * n, 0.5 + 0.2 * n)
      y <- c(0.5 + 0.46 * n, 0.5 + 0.2 * n)
    } else {
      y0 <- 0.5 + n * (1 - 0.3)
      y1 <- 0.5 + n * 0.1
      x0 <- 0.5 - 0.25 * n
      x1 <- 0.5 + 0.3 * n
      y <- seq(y0, y1, length.out = len)
      x <- seq(x0, x1, length.out = len)
    }
  } else {
    if(len == 1) {
      x <- 0.5 + 0.82 * n
      y <- 0.5 + 0.7 * n
    } else if(len == 2) {
      x <- c(0.5 + 0.8 * n, 0.5 + 1.02 * n)
      y <- c(0.5 + 0.8 * n, 0.5 + 0.54 * n)
    } else {
      y0 <- 0.5 + n * (1 - 0.1)
      y1 <- 0.5 + n * 0.3
      x0 <- 0.5 + 0.75 * n
      x1 <- 0.5 + 1.3 * n
      y <- seq(y0, y1, length.out = len)
      x <- seq(x0, x1, length.out = len)
    }
  }
  if(!is.null(hjust))
    x <- x + hjust
  if(!is.null(vjust))
    y <- y + vjust
  data.frame(grp_x = x,
             grp_y = y,
             grp_name = name,
             stringsAsFactors = FALSE)
}

#' @noRd
link_mat_data <- function(name, type = "upper", show_diag = FALSE,
                          hjust = NULL, vjust = NULL)
{
  n <- length(name)
  if(!is.null(hjust) && length(hjust) != n)
    hjust <- rep_len(hjust, n)
  if(!is.null(vjust) && length(hjust) != n)
    vjust <- rep_len(vjust, n)

  x <- n:1
  y <- 1:n
  if(type == "upper") {
    if(show_diag)
      x <- x - 1
  } else {
    if(show_diag)
      x <- x + 1
  }
  if(!is.null(hjust))
    x <- x + hjust
  if(!is.null(vjust))
    y <- y + vjust
  data.frame(mat_x = x,
             mat_y = y,
             mat_name = name,
             stringsAsFactors = FALSE)
}

