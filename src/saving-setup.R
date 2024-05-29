## define function to save and load result data frame
library(readr)

#' Save a data frame into a csv file
#'
#' @description
#' Wrapper around [readr::write_delim()] function (to use `;` separator in
#' output csv files).
#'
#' @param df `data.frame` or `matrix` with observations in rows and low
#' dimensional features in columns.
#' @param filename character, name of the file to be saved.
save_res <- function(df, filename) {
    write_delim(df, file = filename, delim = ";")
}

#' Load a data frame from a csv file
#'
#' @description
#' Wrapper around [readr::read_delim()] function (to use `;` separator in
#' output csv files).
#'
#' @param filename character, name of the file to be saved.
#'
#' @return df `data.frame` or `matrix` with observations in rows and low
#' dimensional features in columns.
load_res <- function(filename) {
    out <- read_delim(file = filename, delim = ";", show_col_types = FALSE) %>%
        mutate(class = as.factor(as.character(class)))
    return(out)
}
