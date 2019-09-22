#' Draw mantel test plot
#'
#' @description `ggmantel()` is highly dependent on `ggcorrr` packages, and it provides a simple,
#' flexible and easy-to-use function interface to draw mantel's test graphs. I strongly recommend
#' that people who are not familiar with `ggplot2`'s grammar use `ggmantel()` directly to draw mantel's
#' test plot. And at the same time, in order to maintain flexibility and customization, people
#' who are very familiar with `ggplot2`'s grammar can also just use `fortify_mantel()` to handle data. .
#' @param data mantel's test data, now it just support the data returned by `fortify_mantel()`.
#' @param corr_type the symbols of correalation matrix plot, see @details .
#' @param corr_extra_params extra params pass to correalation matrix plot's layer, see @details .
#' @param corr_backgroud background colour of correalation matrix plotting area.
#' @param corr_grid_colour correalation matrix plot's grid line colour.
#' @param stat_breaks a numeric vector of unique cut points giving the number of intervals into which mantel's
#' statistic is to be cut, the defaults values is (-Inf, 0.25, 0.5, Inf).
#' @param stat_labels labels for the levels of the mantel's statistic cut resulting category.
#' @param signif_breaks a numeric vector of unique cut points giving the number of intervals into which mantel's
#' test p-values is to be cut, the defaults values is (-Inf, 0.001, 0.01, 0.05, Inf).
#' @param signif_labels labels for the levels of the mantel's test p-values cut resulting category.
#' @param curvature a numeric value giving the amount of curvature. if `type = "upper"`, the default value is -0.1,
#' else is 0.1.
#' @param grp_mark logical. Should add mark symbols on group link points.
#' @param grp_mark_shape group mark symbol, the defualt value is 21.
#' @param grp_mark_size group mark symbol's size, the defualt value is 4.
#' @param grp_mark_col group mark symbol's edge colour, the defualt value is "blue".
#' @param grp_mark_fill group mark symbol's fill colour, the defualt value is "blue".
#' @param mat_mark logical. Should add mark symbols on  correalation matrix plot's link points.
#' @param mat_mark_shape mat mark symbol, the defualt value is 21.
#' @param mat_mark_size mat mark symbol's size, the defualt value is 4.
#' @param mat_mark_col mat mark symbol's edge colour, the defualt value is "grey60".
#' @param mat_mark_fill mat mark symbol's fill colour, the defualt value is "grey60".
#' @param grp_label character vecter, group link labels.
#' @param grp_label_colour group link labels' colour.
#' @param grp_label_size group link labels' size.
#' @param grp_label_family group link labels' text family.
#' @param grp_label_fontface group link labels' text font face.
#' @param grp_label_hspace the horizontal space between group link points and labels.
#' @param grp_label_vspace the vertical space between group link points and labels.
#' @param grp_label_hjust group link labels' horizontal justification.
#' @param grp_label_vjust group link labels' vertical justification.
#' @param scale_fill_col a colour string vector, the default value is `ggcorrr:::.default_colors`.
#' @param scale_colour_col a colour string vector.
#' @param scale_size_value a numeric vector.
#' @param legend_position the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector).
#' @param legend_mat_title title of legend colourbar.
#' @param legend_mat_breaks a numeric vector of positions.
#' @param legend_mat_labels a character vector giving labels (must be same length as legend_mat_breaks).
#' @param x_label_position the position of x axis. "top" or "bottom".
#' @param y_label_position the position of y axis. "left" or "right".
#' @param xlim a numeric vector of length two providing limits of the x axis scale.
#' @param ylim a numeric vector of length two providing limits of the y axis scale.
#'
#' @details
#' `corr_type` supports the following nine styles and combinations.
#' \itemize{
#'     \item{"square", @seealso [ggcorrr::geom_square()].}
#'     \item{"colour", @seealso [ggcorrr::geom_colour].}
#'     \item{"circle", @seealso [ggcorrr::geom_circle2()].}
#'     \item{"ellipse", @seealso [ggcorrr::geom_ellipse2()].}
#'     \item{"pie", @seealso [ggcorrr::geom_pie2()].}
#'     \item{"text", @seealso [ggcorrr::geom_num()].}
#'     \item{"confbox", @seealso [ggcorrr::geom_confbox()].}
#'     \item{"mark", @seealso [ggcorrr::geom_mark()].}
#'     \item{"shade", @seealso [ggcorrr::geom_shade()].}
#'     \item{"cross", @seealso [ggcorrr::geom_cross()].}
#' }
#'
#' Using "+" symbols to combine multiple layers, overlapping from left to right.
#'
#' `corr_extra_params` must be list. If there are multiple layers, we can set additional params for each layer with a list.
#' When a layer does not need additional parameters, an empty list needs to be added.
#'
#' @examples
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
#' ggmantel(df4)

#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_curve
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_colourbar
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggcorrr theme_corr
#' @importFrom ggcorrr geom_square
#' @importFrom ggcorrr geom_colour
#' @importFrom ggcorrr geom_circle2
#' @importFrom ggcorrr geom_ellipse2
#' @importFrom ggcorrr geom_pie2
#' @importFrom ggcorrr geom_num
#' @importFrom ggcorrr geom_confbox
#' @importFrom ggcorrr geom_mark
#' @importFrom ggcorrr geom_shade
#' @importFrom ggcorrr geom_cross
#' @importFrom ggcorrr scale_fill_gradient2n
#' @export

ggmantel <- function(data,
                     corr_type = "square",
                     corr_extra_params = corr_params(),
                     corr_backgroud = NA,
                     corr_grid_colour = "grey60",
                     stat_breaks = c(-Inf, 0.25, 0.5, Inf),
                     stat_labels = c("< 0.25", "0.25 - 0.5", ">= 0.5"),
                     signif_breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                     signif_labels = c("< 0.001", "0.001 - 0.01", "0.01 - 0.05", ">= 0.05"),
                     curvature = NULL,
                     grp_mark = TRUE,
                     grp_mark_shape = 21,
                     grp_mark_size = 4,
                     grp_mark_col = "blue",
                     grp_mark_fill = "blue",
                     mat_mark = TRUE,
                     mat_mark_shape = 21,
                     mat_mark_size = 4,
                     mat_mark_col = "grey60",
                     mat_mark_fill = "grey60",
                     grp_label = NULL,
                     grp_label_colour = "black",
                     grp_label_size = 6.5,
                     grp_label_family = "",
                     grp_label_fontface = 1,
                     grp_label_hspace = 0.2,
                     grp_label_vspace = 0,
                     grp_label_hjust = NULL,
                     grp_label_vjust = 0.5,
                     scale_fill_col = NULL,
                     scale_colour_col = NULL,
                     scale_size_value = c(0.5, 1.5, 4),
                     legend_position = NULL,
                     legend_mat_title = NULL,
                     legend_mat_breaks = NULL,
                     legend_mat_labels = NULL,
                     x_label_position = NULL,
                     y_label_position = NULL,
                     xlim = NULL,
                     ylim = NULL)
{
  if(!inherits(data, "ggmantel"))
    stop("Not support class of `data`.", call. = FALSE)
  mantel_df <- data[["link_data"]]
  corr_df <- data[["env_corr"]]
  type <- data[["type"]]
  corr_name <- names(corr_df)
  corr_fun <- corr_switch_fun(corr_type)
  extra_corr_params_len <- length(corr_extra_params)
  if(extra_corr_params_len < length(corr_fun))
    corr_extra_params <- rep_len(corr_extra_params, length(corr_fun))
  if(extra_corr_params_len > length(corr_fun)) {
    warning("`corr_extra_params` too long, extra params are trimed.")
    corr_extra_params <- corr_extra_params[1:length(corr_fun)]
  }
  if(grepl("confbox", corr_type))
    if(!all(c("p", "low", "upp") %in% corr_name))
      stop("`corr_df` seems to not contain 'p', 'low' or 'upp'.", call. = FALSE)
  if(grepl("mark", corr_type))
    if(! "p" %in% corr_name)
      stop("`corr_df` seems to not contain 'p'.", call. = FALSE)
  corr_aes <- corr_switch_aes(corr_fun, data = corr_df)
  corr_params <- purrr::map2(corr_aes, corr_extra_params, modify_list2)
  corr_geom_list <- purrr::map2(corr_fun, corr_params, do.call)
  n <- length(unique(mantel_df$spec))
  mantel_df$`Mantel's r` <- cut(mantel_df$statistic, breaks = stat_breaks,
                                labels = stat_labels)
  mantel_df$`P values` <- cut(mantel_df$signif, breaks = signif_breaks, labels = signif_labels)
  if(is.null(curvature))
    curvature <- if(type == "upper") -0.1 else 0.1
  if(is.null(scale_fill_col))
    scale_fill_col <- ggcorrr:::.default_colors
  if(is.null(scale_colour_col))
    scale_colour_col <- link_color_pal(length(signif_breaks) - 1)

  if(type == "upper")
    grp_label_hspace <- - grp_label_hspace
  if(type != "upper")
    grp_label_vspace <- - grp_label_vspace

  if(is.null(grp_label_hjust))
    grp_label_hjust <- if(type == "upper") 1 else 0

  if(is.null(legend_position))
    legend_position <- if(type == "upper") "right" else "left"
  if(is.null(legend_mat_title))
    legend_mat_title <- "Pearson's r"
  if(is.null(legend_mat_breaks))
    legend_mat_breaks <- seq(-1, 1, length.out = 5)
  if(is.null(legend_mat_labels))
    legend_mat_labels <- seq(-1, 1, length.out = 5)
  xlab <- levels(corr_df$x)
  ylab <- levels(corr_df$y)
  if(is.null(xlim)) {
    if(type == "upper") {
      xmin <- min(mantel_df$grp_x) - 3
      xmax <- length(xlab) + 0.5
    } else {
      xmin <- 0.5
      xmax <- max(mantel_df$grp_x) + 3
    }
    xlim <- c(xmin, xmax)
  }
  if(is.null(ylim))
    ylim <- c(0.5, length(ylab) + 0.5)
  if(is.null(x_label_position))
    x_label_position <- if(type == "upper") "top" else "bottom"
  if(is.null(y_label_position))
    y_label_position <- if(type == "upper") "right" else "left"

  grp_name_df <- mantel_df[!duplicated(mantel_df$spec), , drop = FALSE]
  mat_name_df <- mantel_df[!duplicated(mantel_df$env), , drop = FALSE]

  p <- ggplot() +
    geom_curve(aes(x = grp_x, y = grp_y, xend = mat_x, yend = mat_y,
                   size = `Mantel's r`, colour = `P values`), mantel_df,
               curvature = curvature) +
    geom_text(aes(x = grp_x + grp_label_hspace, y = grp_y + grp_label_vspace, label = spec ),
              grp_name_df, hjust = grp_label_hjust, vjust = grp_label_vjust, colour = grp_label_colour,
              size = grp_label_size, family = grp_label_family, fontface = grp_label_fontface)
  if(grp_mark)
    p <- p + geom_point(aes(x = grp_x, y = grp_y), grp_name_df, shape = grp_mark_shape, colour = grp_mark_col,
               fill = grp_mark_fill, size = grp_mark_size)
  if(mat_mark)
    p <- p + geom_point(aes(x = mat_x, y = mat_y), data = mat_name_df, shape = mat_mark_shape, colour = mat_mark_col,
                        fill = mat_mark_fill, size = mat_mark_size)

  # add matrix corrplot
  ## add corrplot background and grid
  p <- p + geom_tile(aes(x = as.numeric(x), y = as.numeric(y)), corr_df,
                     colour = corr_grid_colour, fill = corr_backgroud)

  ## add corrplot geom
  p <- p + corr_geom_list
  # add scale
  p <- p +
    scale_fill_gradient2n(breaks = legend_mat_breaks,
                          labels = legend_mat_labels,
                          expand = TRUE,
                          colours = scale_fill_col,
                          limits = c(-1, 1)) +
    scale_colour_manual(drop = FALSE, values = scale_colour_col) +
    scale_size_manual(drop = FALSE, values = scale_size_value) +
    scale_x_continuous(expand = FALSE, limits = xlim, breaks = 1:length(xlab),
                       labels = xlab, position = x_label_position) +
    scale_y_continuous(expand = FALSE, limits = ylim, breaks = 1:length(ylab),
                       labels = ylab,  position = y_label_position)

  # add guide box
  p <- p + guides(fill = guide_colourbar(title = legend_mat_title, nbin = 30, order = 3),
              colour = guide_legend(override.aes = list(size = 2), order = 2),
              size = guide_legend(override.aes = list(colour = "grey35"), order = 1))

  # add coord and theme
  p <- p + coord_fixed(expand = FALSE) +
    theme_corr(legend.position = legend_position, legend.box.spacing = grid::unit(1, "mm"),
               legend.spacing = grid::unit(2, "mm"))

  p
}

