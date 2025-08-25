`_sphere_norm` <- function(x) {
  x <- as.matrix(x);
  r <- nrow(x);
  output <- matrix(0,r,ncol(x));
  
  for (p in 1:r) {
    vlength <- (sum(x[p,]^2))^(1/2)
    if (!is.na(vlength)) {
      if (vlength>0) {
        output[p,] <- ( x[p,] / vlength )
      }
    }
  }

  return(output);
}
