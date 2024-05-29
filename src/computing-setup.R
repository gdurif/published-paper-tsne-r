## define functions to apply each dimension reduction method

# T-SNE
library(Rtsne)

#' Run t-SNE and format results
#'
#' @description
#' Wrapper around [Rtsne::Rtsne()] function.
#'
#' @param X `data.frame` or `matrix` with observations in rows and features
#' in columns.
#' @param n_comp integer, number of dimension for the reduced dimension space,
#' default is 2.
#' @param perplexity numeric, perplexity input parameter for t-SNE.
#' @param class (optional) `data frame` containing the observations
#' classes/groups in a column named `class`.
#'
#' @return a `data.frame` containing the representation of observations
#' in the first two directions in the reduced dimension space and the
#' corresponding group/class for each observations (if provided in input).
run_tsne <- function(X, n_comp = 2, perplexity = 40, class = NULL) {
    # run t-sne
    out <- Rtsne(X, dims = n_comp, perplexity = perplexity, pca = TRUE)
    # extract results
    res <- data.frame(dim1 = out$Y[,1], dim2 = out$Y[,2])
    # add class/group of observations
    if(!is.null(class)) res <- res %>% bind_cols(class)
    # output
    return(res)
}

# Sammon mapping
library(MASS)

#' Run Sammon mappitng and format results
#'
#' @description
#' Wrapper around [MASS::sammon()] function.
#'
#' @param X `data.frame` or `matrix` with observations in rows and features
#' in columns.
#' @param n_comp integer, number of dimension for the reduced dimension space,
#' default is 2.
#' @param class (optional) `data frame` containing the observations
#' classes/groups in a column named `class`.
#'
#' @return a `data.frame` containing the representation of observations
#' in the first two directions in the reduced dimension space and the
#' corresponding group/class for each observations (if provided in input).
run_sammon <- function(X, n_comp = 2, class = NULL) {
    # run sammon
    out <- sammon(dist(X), k = n_comp)
    # extract results
    res <- data.frame(dim1 = out$points[,1], dim2 = out$points[,2])
    # add class/group of observations
    if(!is.null(class)) res <- res %>% bind_cols(class)
    # output
    return(res)
}

# ISOMAP
library(RDRToolbox)

#' Run isomap and format results
#'
#' @description
#' Wrapper around [RDRToolbox::Isomap()] function.
#'
#' @param X `data.frame` or `matrix` with observations in rows and features
#' in columns.
#' @param n_comp integer, number of dimension for the reduced dimension space,
#' default is 2.
#' @param k integer, number of neighbors input parameter for
#' [RDRToolbox::Isomap()].
#' @param class (optional) `data frame` containing the observations
#' classes/groups in a column named `class`.
#'
#' @return a `data.frame` containing the representation of observations
#' in the first two directions in the reduced dimension space and the
#' corresponding group/class for each observations (if provided in input).
run_isomap <- function(X, n_comp = 2, k = 12, class = NULL) {
    # run isomap
    out <- Isomap(as.matrix(X), dims = n_comp, k = k)
    # extract results
    res <- data.frame(dim1 = out$dim2[,1], dim2 = out$dim2[,2])
    # add class/group of observations
    if(!is.null(class)) res <- res %>% bind_cols(class)
    # output
    return(res)
}

# LLE

#' Run LLE and format results
#'
#' @description
#' Wrapper around [RDRToolbox::LLE()] function.
#'
#' @param X `data.frame` or `matrix` with observations in rows and features
#' in columns.
#' @param n_comp integer, number of dimension for the reduced dimension space,
#' default is 2.
#' @param k integer, number of neighbors input parameter for
#' [RDRToolbox::LLE()].
#' @param class (optional) vector of observations classes/groups.
#'
#' @return a `data.frame` containing the representation of observations
#' in the first two directions in the reduced dimension space and the
#' corresponding group/class for each observations (if provided in input).
run_lle <- function(X, n_comp = 2, k = 12, class = NULL) {
    # run isomap
    out <- LLE(as.matrix(X), dim = n_comp, k = k)
    # extract results
    res <- data.frame(dim1 = out[,1], dim2 = out[,2])
    # add class/group of observations
    if(!is.null(class)) res <- res %>% bind_cols(class)
    # output
    return(res)
}
