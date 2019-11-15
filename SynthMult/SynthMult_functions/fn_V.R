##fn_V.r

##The optimization function for the Vs

fn_V <- function(
                 variables.v = stop("variables.v missing"), X0.scaled = stop("X0.scaled missing"),
                 X1.scaled = stop("X1.scaled missing"), Z0 = stop("Z0 missing"),
                 Z1 = stop("Z1 missing")) {

  ##The V matrix
  V <- diag(x = as.numeric(abs(variables.v) / sum(abs(variables.v))))

  ##H <- t(X0.scaled) %*% V %*% (X0.scaled)
  H <- crossprod(X0.scaled, V) %*% X0.scaled  ##Use crossprod to speed things up
  ##a <- X1.scaled
  c <- -1 * c(crossprod(X1.scaled, V) %*% X0.scaled)
  A <- t(rep(1, length(c)))
  b <- 1
  l <- rep(0, length(c))
  u <- rep(1, length(c))
  r <- 0

  ## -- ipop --##
  ##slower
  ## res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u,
  ##             r = r, bound = 10, margin = 5e-04,
  ##             maxiter = 1000, sigf = 5)
  ## solution.w <- as.matrix(primal(res))

  ## -- LowRankQP -- ##
  ##Use capture.output() to prevent printing
  capture.output(
    res <- LowRankQP(Vmat=H, dvec=c, Amat=A, bvec=1, uvec=rep(1,length(c)),
                     method="LU")
  )

  solution.w <- matrix(res$alpha)

  ##loss.v <- as.numeric(t(Z1 - (Z0 %*% solution.w)) %*% (Z1 - (Z0 %*% solution.w)))
  ##Use crossprod() to speed things up
  loss.v <- as.numeric(crossprod(Z1 - (Z0 %*% solution.w)))
  loss.v <- loss.v / nrow(Z0)

  ## -- Mirror Descent C++ version -- ##
  ## solution.w.md <- MirrorDescSynth::mirror_desc(X0.scaled, X1.scaled,
  ##                                               V = V)

  ## loss.v.md <- as.numeric(crossprod(Z1 - (Z0 %*% solution.w.md)))
  ## loss.v.md <- loss.v.md / nrow(Z0)

  ## loss.v <- min(loss.v, loss.v.md)

  return(loss.v)
}


