#' @noRd
make_list_names <- function(x, pre = "X", sep = "_")
{
  stopifnot(is.list(x))
  n <- length(x)
  name <- names(x)
  if(!is.null(name) && all(name != "" & !is.na(name)))
    return(x)
  if(is.null(x)) {
    names(x) <- paste0(pre, sep, seq_len(n))
  }
  if(all(name == "" | is.na(name))) {
    names(x) <- paste0(pre, sep, seq_len(n))
  } else {
    idx <- name == "" | is.na(name)
    name[idx] <- paste0(pre, sep, sum(idx))
    names(x) <- make.unique(name)
  }
  return(x)
}

#' @noRd
modify_list <- function (..., params, keep.null = TRUE)
{
  ll <- list(...)

  modifyList(ll, params, keep.null = keep.null)
}

#' @noRd
modify_list2 <- function (x1, x2, keep.null = FALSE)
{

  modifyList(x1, x2, keep.null = keep.null)
}

#' @noRd
`%||%` <- function(x, y)
{
  if(is.null(x)) y else x
}

#' @noRd
link_color_pal <- function(n)
{
  stopifnot(n <= 7)
  colors <- c("#7A0177", "#D95F02", "#1B9E77", "#7570B3",
              "#E7298A", "#A6761D", "#CCCCCC")
  if(n == 1)
    return(colors[1])
  col <- c(colors[1:(n - 1)], colors[7])
  col

}
#' @noRd
cat_message <- function(idx, totle, msg = ".", width = 60)
{
  if(idx %% width == 0) {
    pct <- format(idx / totle * 100, digits = 1, nsmall = 1)
    msg <- paste0(msg, pct, "%\n")
  } else
    msg <- msg
  if(idx == totle && idx %% width != 0) {
    msg <- paste0(msg, "100%\n")
  }
  cat(msg)
}
