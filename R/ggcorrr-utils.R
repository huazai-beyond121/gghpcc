#' @noRd
corr_switch_fun <- function(type)
{
  type <- gsub("\\s+", "", type)
  type <- unlist(strsplit(type, "+", TRUE))
  stopifnot(all(type %in% c("square", "colour", "circle", "ellipse",
                            "pie", "text", "confbox", "mark", "shade", "cross")))
  fun <- lapply(type, function(x) {switch (x,
                                           "square"  = "geom_square",
                                           "colour"  = "geom_colour",
                                           "circle"  = "geom_circle2",
                                           "ellipse" = "geom_ellipse2",
                                           "pie"     = "geom_pie2",
                                           "text"    = "geom_num",
                                           "confbox" = "geom_confbox",
                                           "mark"    = "geom_mark",
                                           "shade"   = "geom_shade",
                                           "cross"   = "geom_cross"
  )})
  fun
}

#' @importFrom ggplot2 aes
#' @noRd
corr_switch_aes <- function(type, data)
{
  lapply(type,
         function(x) {switch (x,
           "geom_square"   = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), r = r, fill = r), data = data),
           "geom_colour"   = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), fill = r), data = data),
           "geom_circle2"  = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), r = r, fill = r), data = data),
           "geom_ellipse2" = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), r = r, fill = r), data = data),
           "geom_pie2"     = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), r = r, fill = r), data = data),
           "geom_num"      = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), num = r), data = data),
           "geom_confbox"  = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), r = r, low = low, upp = upp, fill = r), data = data),
           "geom_mark"     = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), r = r, p = p), data = data),
           "geom_shade"    = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), r = r), data = data),
           "geom_cross"    = list(mapping = aes(x = as.numeric(x), y = as.numeric(y), p = p), data = data)
  )})
}

#' @noRd
#' @export
corr_params <- function(...)
{
  params <- list(...)
  stopifnot(all(vapply(params, is.list, logical(1))) || length(params) == 0)
  if(length(params) == 0)
    params <- list(list())
  params
}





