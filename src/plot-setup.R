## define plotting function for dimension reduction of MNIST dataset
library(ggplot2)

#' Plot representation of observations in reduced dimension space.
#'
#' @param df `data.frame` or `matrix` with observations in rows and low
#' dimensional features in columns (named as `dim1` and `dim2`) along with
#' a `class` column indicating.
#' @param title (optional) character, plot title.
#' @return a `ggplot2` graph.
draw_plot <- function(df, title = NULL, legend = FALSE) {

    gg <- ggplot(df, aes(dim1, dim2, col = class, shape = class)) +
        geom_point() +
        coord_fixed() +
        theme_void()

    if(!is.null(title)) gg <- gg + ggtitle(title)
    if(!legend) gg <- gg + theme(legend.position="none")

    return(gg)
}
