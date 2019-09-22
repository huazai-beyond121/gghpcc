#' Fortify a mantel's test data
#' @description
#' \code{fortify_mantel()}  provides a unified interface for mantel's test, and supports for specifying different
#' distance function (now support \code{\link[stats]{dist}} and \code{\link[vegan]{vegdist}}) and different mantel's test functions (
#' (now support \code{\link[vegan]{mantel}}, \code{\link[vegan]{mantel.partial}}.
#'
#' @param spec_df a data.frame of species.
#' @param env_df a data.frame of environment.
#' @param env_ctrl a list of dissimilarity matrices or a data.frame.
#' @param spec_select a list of numeric vector to split `spec_df` by columns.
#' @param spec_group variable name string to split `spec_df` to a list by rows.
#' @param env_group variable name string to split `env_df` to a list by rows.
#' @param env_ctrl_group variable name string to split `env_ctrl` data.frame to a list by rows.
#' @param pair_test logical, see details.
#' @param spec_fun a string of distance function, it is used to handle `gdis` parameter.
#' @param env_fun a string of distance function, it is used to handle `mdis` parameter.
#' @param mantel_fun a string of mantel's test function.
#' @param spec_params a list of extra params passing to `gdist_fun`.
#' @param env_params a list of extra params passing to `mdist_fun`.
#' @param env_corr_params a list of extra params passing to `ggcorrr::fortify_corr()`.
#' @param get_link_params a list of extra params passing to `get_link_data()`.
#' @param process logical, whether to print progress information.
#' @param ... extra params passing to `mantel_fun`.
#' @details
#' `spec_df`, `env_df` is splitted to list by row or column. `pair_test` parameter is used to distinguish two different modes.
#' If TRUE, mantel's test is performed on the i-th element of `spec_df` list  and each element
#' in the i-th element of `env_df` list. The defaults is FALSE, mantel's test is
#' performed on the i-th element of `spec_df` list and each elements in `env_df` list.
#' @return a list of `env_df` correalation data, link line data and correalation matrix plot type.
#' @seealso [vegan::vegdist()], [vegan::mantel()], [vegan::mantel.partial()],
#' [gghpcc::get_link_data()] and [ggcorrr::fortify_corr()].
#' @examples
#' library(dplyr)
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' ## four group
#' df4 <- fortify_mantel(varespec, varechem,
#'                       spec_select = list(spec01 = 22:25,
#'                                          spec02 = 1:4,
#'                                          spec03 = 38:43,
#'                                          spec04 = 15:20),
#'                       mantel_fun = "mantel",
#'                       env_corr_params = list(type = "upper",
#'                       show_diag = FALSE,
#'                       corr_test = TRUE,
#'                       cluster = TRUE))
#' ggmantel(df4, corr_type = "pie")
#' ## split by columns
#' spec_df <- varespec %>%
#'              mutate(grp = rep(LETTERS[1:3], 8))
#'
#' env_df <- varechem %>%
#'             mutate(grp = rep(LETTERS[1:3], 8))
#'
#' df_grp <- fortify_mantel(spec_df, env_df,
#'                          spec_group = "grp",
#'                          env_group = "grp",
#'                          mantel_fun = "mantel",
#'                          pair_test = TRUE,
#'                          env_corr_params = list(type = "upper",
#'                                                 show_diag = FALSE,
#'                                                 corr_test = TRUE,
#'                                                 cluster = TRUE))
#' ggmantel(df_grp)
#' @importFrom stats dist
#' @importFrom vegan vegdist
#' @importFrom vegan mantel
#' @importFrom vegan mantel.partial
#' @importFrom ggcorrr fortify_corr
#' @export
fortify_mantel <- function(spec_df,
                           env_df,
                           env_ctrl = NULL,
                           spec_select = NULL, # a list of index vector
                           spec_group = NULL,
                           env_group = NULL,
                           env_ctrl_group = NULL,
                           pair_test = FALSE,
                           spec_fun = "vegdist",
                           env_fun = "vegdist",
                           mantel_fun = "mantel",
                           spec_params = list(method = "bray"),
                           env_params = list(method = "euclidean"),
                           env_corr_params = list(type = "upper", show_diag = FALSE),
                           get_link_params = list(),
                           process = TRUE,
                           ...
)
{
  if(!is.data.frame(spec_df))
    spec_df <- as.data.frame(spec_df)
  if(!is.data.frame(env_df))
    env_df <- as.data.frame(env_df)
  corr_df <- env_df
  if(!is.null(env_group)) {
    corr_df[[env_group]] <- NULL
  }
  env_corr <- do.call("fortify_corr", modify_list(x = corr_df, params = env_corr_params))
  if(!is.null(spec_select) && !is.null(spec_group)) {
    warning("Just supports `spec_select` or `spec_group`, `spec_group` is droped", call. = FALSE)
    spec_group <- NULL
  }
  if(!is.null(spec_select))
    spec_df <- lapply(spec_select, function(x) {
      subset(spec_df, select = x, drop = FALSE)
    })
  if(!is.null(spec_group)) {
    xgrp <- spec_df[[spec_group]]
    spec_df[[spec_group]] <- NULL # remove group variable
    spec_df <- split(spec_df, xgrp, drop = FALSE)
  }
  if(!is.null(env_group)) {
    ygrp <- env_df[[env_group]]
    env_df[[env_group]] <- NULL # remove group variable
    env_df <- split(env_df, xgrp, drop = FALSE)
  }
  spec_df <- make_list_names(spec_df, pre = "spec")
  env_df <- make_list_names(env_df, pre = "env")
  if(mantel_fun == "mantel.partial" && !is.null(env_ctrl)) {
    if(!is.data.frame(env_ctrl) && !is.list(env_ctrl))
      env_ctrl <- as.data.frame(env_ctrl)
    if(!is.null(env_ctrl_group) && is.data.frame(env_ctrl)) {
      zgrp <- env_ctrl[[env_ctrl_group]]
      env_ctrl[[env_ctrl_group]] <- NULL # remove group variable
      env_ctrl_list <- split(env_ctrl, zgrp, drop = FALSE)
    }
    if(is.data.frame(env_ctrl))
      env_ctrl_list <- rep_len(env_ctrl, length(env_df))
  }
  if(pair_test) {
    mantel <- tidy_mantel_pair(spec_df, env_df, env_ctrl_list,
                               spec_fun = spec_fun,
                               env_fun = env_fun,
                               mantel_fun = mantel_fun,
                               spec_params = spec_params,
                               env_params = env_params,
                               process = process,
                               ...)
  } else {
    mantel <- tidy_mantel(spec_df, env_df, env_ctrl_list,
                          spec_fun = spec_fun,
                          env_fun = env_fun,
                          mantel_fun = mantel_fun,
                          spec_params = spec_params,
                          env_params = env_params,
                          process = process,
                          ...)
  }
  get_link_data_params <- modify_list(
    mantel_df = mantel,
    corr_df = env_corr,
    type = env_corr_params[["type"]],
    show_diag = env_corr_params[["show_diag"]],
    params = get_link_params
  )
  link_data <- do.call("get_link_data", get_link_data_params)

  structure(.Data = list(env_corr = env_corr,
                         link_data = link_data,
                         type = env_corr_params[["type"]]),
            class = "ggmantel")
}


#' @noRd
#' @export
tidy_mantel <- function(spec_list,
                        env_list,
                        env_ctrl_list = NULL,
                        spec_fun = "vegdist",
                        env_fun = "vegdist",
                        mantel_fun = c("mantel", "mantel.partial"),
                        spec_params = list(),
                        env_params = list(),
                        process = TRUE,
                        ...
)
{
  mantel_fun <- match.arg(mantel_fun)
  if(mantel_fun == "mantel.partial") {
    if(!is.null(env_ctrl_list) && length(env_list) != length(env_ctrl_list))
      env_ctrl_list <- rep_len(env_ctrl_list, length(env_list))
  }

  nm_spec <- names(spec_list)
  nm_env <- names(env_list)
  n <- length(spec_list)
  m <- length(env_list)
  len <- n * m
  spec_dist <- lapply(spec_list, function(x) {
    do.call(spec_fun, modify_list(x = x, params = spec_params))
  })
  env_dist <- lapply(env_list, function(x) {
    do.call(env_fun, modify_list(x = x, params = env_params))
  })
  if(mantel_fun == "mantel.partial" && !is.null(env_ctrl_list)) {
    env_ctrl_dist <- lapply(env_ctrl_list, function(x) {
      do.call(env_fun, modify_list(x = x, params = env_params))
    })
  }
  stat <- rep_len(NA, len)
  sign <- rep_len(NA, len)
  if(process)
    cat("Compute mantel statistic, please wait a moment:\n")
  for(i in seq_len(n)) {
    xs <- spec_dist[[i]]
    #    xs <- do.call(spec_fun, modify_list(x = spec_list[[i]], params = spec_params))
    for (j in seq_len(m)) {
      idx <- (i - 1) * m + j
      if(process)
        cat_message(idx, len)
      ys <- env_dist[[j]]
      #      ys <- do.call(env_fun, modify_list(x = env_list[[j]], params = env_params))
      if(mantel_fun == "mantel.partial") {
        if(is.null(env_ctrl_list)) {
          zs <- do.call(env_fun, modify_list(x = env_list[ , -j, drop = FALSE], params = env_params))
        } else {
          zs <- env_ctrl_dist[[j]]
        }
      }
      mantel_data_params <- switch (mantel_fun,
                                    mantel = modify_list(xdis = xs, ydis = ys, params = list(...)),
                                    mantel.partial = modify_list(xdis = xs, ydis = ys, zdis = zs, params = list(...)))
      mantel <- do.call(mantel_fun, mantel_data_params)
      stat[idx] <- mantel[["statistic"]]
      sign[idx] <- mantel[["signif"]]
    }
  }
  df <- data.frame(spec = rep(nm_spec, each = m),
                   env = rep(nm_env, n),
                   statistic = stat,
                   signif = sign,
                   stringsAsFactors = FALSE)
  class(df) <- c("mantel_df", "data.frame")
  df
}

#' @noRd
#' @export
tidy_mantel_pair <- function(spec_list,
                             env_list,
                             env_ctrl_list = NULL,
                             spec_fun = "vegdist",
                             env_fun = "vegdist",
                             mantel_fun = "mantel",
                             spec_params = list(),
                             env_params = list(),
                             process = TRUE,
                             ...
)
{
  stopifnot(length(spec_list) == length(env_list))
  spec_list <- lapply(spec_list, function(x) {
    if(!inherits(x, "data.frame")) {
      as.data.frame(x)
    } else x
  })
  env_list <- lapply(env_list, function(x) {
    if(!inherits(x, "data.frame")) {
      as.data.frame(x)
    } else x
  })
  spec_rows <- vapply(spec_list, nrow, numeric(1), USE.NAMES = FALSE)
  env_rows <- vapply(env_list, nrow, numeric(1), USE.NAMES = FALSE)
  if(any(spec_rows != env_rows))
    stop("The elements of `spec_list` and `env_list` not with the same rows.", call. = FALSE)
  nm_spec <- names(spec_list)
  nm_env <- unlist(lapply(env_list, names))
  n <- length(spec_list)
  m <- vapply(env_list, ncol, numeric(1), USE.NAMES = FALSE)
  spec_dist <- lapply(spec_list, function(x) {
    do.call(spec_fun, modify_list(x = x, params = spec_params))
  })
  if(mantel_fun == "mantel.partial" && !is.null(env_ctrl_list)) {
    if(length(env_list) != length(env_ctrl_list))
      env_ctrl_list <- rep_len(env_ctrl_list, length(env_list))
    env_ctrl_rows <- vapply(env_ctrl_list, nrow, numeric(1), USE.NAMES = FALSE)
    if(any(env_rows != env_ctrl_rows))
      stop("The elements of `env_list` and `env_ctrl_list` not with the same rows.", call. = FALSE)
    env_ctrl_dist <- lapply(env_ctrl_list, function(x) {
      do.call(env_fun, modify_list(x = x, params = env_params))
    })
  }
  stat <- rep_len(NA, sum(m))
  sign <- rep_len(NA, sum(m))
  if(process)
    cat("Compute mantel statistic, please wait a moment:\n")
  for(i in seq_len(n)) {
    xs <- spec_dist[[i]]
    yy <- lapply(env_list[[i]], function(x) {
      do.call(env_fun, modify_list(x = x, params = env_params))
    })
    for (j in seq_len(length(yy))) {
      idx <- if(i == 1) j else sum(m[1:(i - 1)]) + j
      if(process)
        cat_message(idx, sum(m))
      ys <- yy[[j]]
      if(mantel_fun == "mantel.partial") {
        if(is.null(env_ctrl_list)) {
          zs <- do.call(env_fun, modify_list(x = env_list[[i]][ , -j, drop = FALSE], params = env_params))
        } else
          zs <- env_ctrl_dist[[i]]
      }
      mantel_data_params <- switch (mantel_fun,
                                    mantel = modify_list(xdis = xs, ydis = ys, params = list(...)),
                                    mantel.partial = modify_list(xdis = xs, ydis = ys, zdis = zs, params = list(...)))
      mantel <- do.call(mantel_fun, mantel_data_params)
      stat[idx] <- mantel[["statistic"]]
      sign[idx] <- mantel[["signif"]]
    }
  }
  df <- data.frame(spec = rep(nm_spec, times = m),
                   env = nm_env,
                   statistic = stat,
                   signif = sign,
                   row.names = NULL,
                   check.rows = FALSE,
                   stringsAsFactors = FALSE)
  class(df) <- c("mantel_df", "data.frame")
  df
}
