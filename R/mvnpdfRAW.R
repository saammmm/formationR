#' Fonction de densité d'une loi normale multivariée
#'
#' Cette fonction permet de calculer la densité d'une loi normale multivariée
#'
#' @param x Une matrice de n colonnes et p lignes
#' @param mean Un vecteur contenant les n moyennes de colonnes de x
#' @param varcovM La matrice de variance covariance
#' @param Log True par défault
#'
#' @return Cette fonction retourne la matrice x et le vecteur des densités
#' @export
#'
#' @examples
mvnpdf <- function(x, mean =  rep(0, nrow(x)),
                   varcovM = diag(nrow(x)), Log = TRUE) {
  n <- ncol(x)
  p <- nrow(x)
  x0 <- x - mean
  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))

  y <- NULL
  for (j in 1:n) {
    yj <- - p/2 * log(2*pi) - 0.5 * LogDetvarcovM -
      0.5 * t(x0[, j]) %*% Rinv %*% x0[, j]
    y <- c(y, yj)
  }

  if (!Log) {
    y <- exp(y)
  }

  return(list(x=x,y=y))
}


