#' Fix a Matrix to Ensure Positive Semi-Definiteness
#'
#' This function adjusts a given matrix to ensure it is positive semi-definite (PSD).
#' If the matrix has negative eigenvalues, they are replaced with a small positive value
#' (ridge adjustment) to make the matrix PSD.
#'
#' @param X A symmetric matrix to be fixed.
#' @param ridge A small positive value used as the minimum eigenvalue. Default is \code{1e-20}.
#' @return A symmetric positive semi-definite matrix.
#' @details
#' The function checks the eigenvalues of the input matrix. If all eigenvalues are positive,
#' the matrix is returned as is. If there are negative eigenvalues, the function adjusts them
#' to ensure the matrix is PSD. If the smallest positive eigenvalue is smaller than the ridge,
#' a ridge adjustment is applied.
#' 
#' If the matrix cannot be fixed (i.e., it still has negative eigenvalues after adjustment),
#' the function throws an error.
#' 
#' @examples
#' # Example usage:
#' mat <- matrix(c(2, -1, -1, 2), nrow = 2)
#' fixed_mat <- fix_mat(mat)
#' 
#' @export

fix_mat <- function(X, ridge = 1e-20){
  
  dm <- dimnames(X)
  e  <- eigen(X, symmetric = TRUE)
  if(all(e$values > 0)){
    return(X)
  }
  
  min.pos <- min(e$values[e$values > 0], na.rm = TRUE) 
  if(min.pos<ridge){
    message("making ridge adjustment,\n
            since minimum positive eigen-value is small")
    ridge <- min.pos/10
  }
  
  fixed.e <- pmax(e$values, ridge)          # floor at `ridge`
  X       <- tcrossprod(e$vectors %*% diag(fixed.e, nrow(X)), e$vectors)
  
  if(any(eigen(X)$values<0)) {
    stop("eigen values are still negative after fixing,\n
         need to change the ridge")
  }
  dimnames(X) <- dm
  
  return(X)
}

#' Ensure Positive Semi-Definiteness of Variance-Covariance Matrix
#'
#' This function adjusts the variance-covariance matrix of a model object to ensure it is
#' positive semi-definite (PSD).
#'
#' @param object A model object for which the variance-covariance matrix is to be fixed.
#' @param ... Additional arguments passed to \code{vcov}.
#' @return A symmetric positive semi-definite variance-covariance matrix.
#' @details
#' This function retrieves the variance-covariance matrix of the given model object using
#' \code{vcov}, and then applies \code{fix_mat} to ensure the matrix is PSD.
#' 
#' @examples
#' # Example usage:
#' # Assuming `model` is a fitted model object:
#' # fixed_vcov <- vcov_psd(model)
#' 
#' @export
vcov_psd <- function(object, ...) {
  fix_mat(vcov(object, ...))
}
