## Source: https://rdrr.io/github/gbasulto/solefinder/src/R/compute_moments.R

######################################################################

##' Centroid of an image
##'
##' Computes the centroid of an image.
##' @param img An image represented by a matrix
##' @param x_grid Vector with the coordinates of x
##' @param y_grid Vector with coordinates of y
##' @return A vector of size two with the centroid.
##' @author Guillermo Basulto-Elias
compute_centroid <- function (img, x_grid, y_grid) {

    m10 <- sum(t(x_grid) %*% img)
    m01 <- sum(img %*% y_grid)
    m00 <- sum(img)

    ## Add a little tolerance to the denominator.
    return (c(m10, m01)/(m00 + 3*.Machine$double.eps))
}


##' Normalized central moments of an image
##'
##' This function provides the normalized central moments of an image.
##' @param img An image stored in a matrix.
##' @param p x-moment.
##' @param q y-moment.
##' @param x_centered Vector of centered coordinates.
##' @param y_centered Vector of centered coordinates.
##' @return A real value with the qp normalized moment.
##' @author Guillermo Basulto-Elias
calculate_central_moments <- function(img, p, q,
                                      x_centered, y_centered){

    ## Compute pq and 00 moments
    mu_pq <- t(x_centered^p) %*% img %*% (y_centered^q)
    mu_00 <- sum((img))

    ## Compute normalize pq moment
    gam <- (p + q + 2)/2
    nu_pq<- mu_pq/mu_00^gam

    return (nu_pq)
}


##' Hu moments
##'
##' This function computes Hu's moments from an image
##' @param img A matrix representing and image
##' @return A numeric vector of size 7 with the moments.
##' @author Guillermo Basulto-Elias
##' @export
compute_moments <- function (img) {

    ## Define grids (both are vectors)
    n_row <- nrow(img)
    n_col <- ncol(img)
    x_grid <- -floor(n_row/2):(ceiling(n_row/2) - 1)
    y_grid <- -floor(n_col/2):(ceiling(n_col/2) - 1)

    ## Compute centroid
    centroid <- compute_centroid(img, x_grid, y_grid)

    ## Grids centered at centroid.
    x_ctrd <- x_grid - centroid[1]
    y_ctrd <- y_grid - centroid[2]

    ## Normalized central moments
    nu_11 <- calculate_central_moments(img, 1, 1, x_ctrd, y_ctrd);
    nu_20 <- calculate_central_moments(img, 2, 0, x_ctrd, y_ctrd);
    nu_02 <- calculate_central_moments(img, 0, 2, x_ctrd, y_ctrd);
    nu_21 <- calculate_central_moments(img, 2, 1, x_ctrd, y_ctrd);
    nu_12 <- calculate_central_moments(img, 1, 2, x_ctrd, y_ctrd);
    nu_03 <- calculate_central_moments(img, 0, 3, x_ctrd, y_ctrd);
    nu_30 <- calculate_central_moments(img, 3, 0, x_ctrd, y_ctrd);

    ## Hu moments
    phi1 <- nu_20 + nu_02
    phi2 <- (nu_20 - nu_02)^2 + 4*nu_11^2
    phi3 <- (nu_30 - 3*nu_12)^2 + (nu_03 - 3*nu_21)^2
    phi4 <- (nu_30 + nu_12)^2 + (nu_03 + nu_21)^2
    phi5 <- (nu_30 - 3*nu_12)*(nu_30 + nu_12)*
        ((nu_30 + nu_12)^2 - 3*(nu_21 + nu_03)^2) +
        (3*nu_21 - nu_03)*(nu_21 + nu_03)*
        (3*(nu_30 + nu_12)^2 - (nu_03 + nu_21)^2)
    phi6  <- (nu_20 - nu_02)*((nu_30 + nu_12)^2 -
                              (nu_21 + nu_03)^2) +
        4*nu_11*(nu_30 + nu_12)*(nu_21 + nu_03)
    phi7 <- (3*nu_21 - nu_03)*(nu_30 + nu_12)*
        ((nu_30 + nu_12)^2 - 3*(nu_21 + nu_03)^2) +
        (nu_30 - 3*nu_12)*(nu_21 + nu_03)*
        (3*(nu_30 + nu_12)^2 - (nu_03 + nu_21)^2);
    phi8 <- nu_11*((nu_30 + nu_12)^2 - (nu_03 + nu_21)^2) -
        (nu_20 - nu_02)*(nu_30 + nu_12)*(nu_21 + nu_03);

    hu_moments_vector <- c(phi1, phi2, phi3, phi4,
                           phi5, phi6, phi7, phi8)


    return (hu_moments_vector)
}