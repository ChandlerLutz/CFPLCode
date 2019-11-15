##synth_optim.R

##to conduct the synthetic control esitmation
synth_optim <- function(X0.scaled, X1.scaled,
                        Z0 = NULL, Z1 = NULL, nvarsV,
                        custom.v = NULL,
                        optimxmethod = c("BFGS"),
                        seed)  {

  assign(".Random.seed", seed, envir = .GlobalEnv)

  if (!is.null(custom.v)) {
    if (length(custom.v) != nvarsV) {
      stop("Error: custom.V misspecified: length(custom.V) != nrow(X)")
    }
    solution.v <- abs(custom.v)/sum(abs(custom.v))
  } else {

    ##The starting values
    SV1 <- self$v.starting.values

    ##Run the optimization to the V -- the weights on the predictor variables
    ## -- R Cocde -- ##
    rgV.optim <- optimx(
      par=SV1,fn=private$fn_V,gr=NULL,hess=NULL,method=self$optim.method,itnmax=NULL,
      hessian=FALSE,
      control=list(kkt=FALSE,starttests=FALSE,
                   dowarn=FALSE,all.methods=FALSE),
      X0.scaled = X0.scaled,
      X1.scaled = X1.scaled,
      Z0 = Z0,
      Z1 = Z1
    )
    rgV.optim.par <- as.numeric(rgV.optim[1:nrow(self$X.scaled)])

    ## -- C++ Code ipopCPP -- ##
    ## rgV.optim <- optim(matrix(SV1), fn = private$fn_v_cpp,
    ##                    method = self$optim.method,
    ##                    X0_scaled = X0.scaled,
    ##                    X1_scaled = X1.scaled,
    ##                    Z0 = Z0,
    ##                    Z1 = Z1)
    ## rgV.optim.par <- rgV.optim$par
    ## loss.v <- rgV.optim$value
    ## rgV.optim <- optimx(
    ##     par=matrix(SV1),fn=private$fn_v_cpp,
    ##     gr=NULL,hess=NULL,method=self$optim.method,itnmax=NULL,
    ##     hessian=FALSE,
    ##     control=list(kkt=FALSE,starttests=FALSE,
    ##                  dowarn=FALSE,all.methods=FALSE),
    ##     X0_scaled = X0.scaled,
    ##     X1_scaled = X1.scaled,
    ##     Z0 = Z0,
    ##     Z1 = Z1
    ## )
    ## rgV.optim.par <- as.numeric(rgV.optim[1:nrow(self$X.scaled)])

    solution.v <- abs(rgV.optim.par)/sum(abs(rgV.optim.par))

  }

  ##Now that we have V, Solve the linear quadratic programming problem
  ##and get the weights

  ## -- R Code -- ##
  solution.v <- abs(solution.v) / sum(abs(solution.v))
  V <- diag(x = as.numeric(solution.v), nrow = nvarsV, ncol = nvarsV)

  ##t(X0.scaled) %*% V %*% (X0.scaled)
  ##H <- t(self$X.scaled[,temp.control.ids]) %*% V %*% (self$X.scaled[,temp.control.ids])
  ##Use crossprod to speed things up
  H <- crossprod(X0.scaled, V) %*% X0.scaled
  ##X1.scaled
  ##a <- X1.scaled
  ##-1 * c(t(a) %*% V %*% (X0.scaled))
  ##which equals
  ##-1 * c(t(X1.scaled) %*% V %*% (X0.scaled))
  c <- -1 * c( crossprod(X1.scaled, V) %*% (X0.scaled))
  A <- t(rep(1, length(c)))
  b <- 1
  l <- rep(0, length(c))
  u <- rep(1, length(c))
  r <- 0

  ## -- Ipop -- ##
  ## res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u,
  ##         r = r, bound = 10, margin = 5e-04,
  ##         maxiter = 1000, sigf = 5)
  ## solution.w <- as.matrix(primal(res))  ##row vector

  ## -- LowRank QP -- ##
  ##Use capture.output() to prevent printing
  capture.output(
    res <- LowRankQP(Vmat=H, dvec=c, Amat=A, bvec=1, uvec=rep(1,length(c)), method="LU",
                     niter = 1000)
  )

  solution.w.LowRankQP <- matrix(res$alpha)

  loss.v.LowRankQP <- as.numeric(crossprod(Z1 - (Z0 %*% solution.w.LowRankQP)))
  loss.v.LowRankQP <- loss.v.LowRankQP / nrow(Z0)

  ## -- Mirror Descent C++ version -- ##
  solution.w.md <- MirrorDescSynth::mirror_desc(X0.scaled, X1.scaled,
                                                V = V)

  rownames(solution.w.md) <- colnames(X0.scaled)
  colnames(solution.w.md) <- "w.weight"

  loss.v.md <- as.numeric(crossprod(Z1 - (Z0 %*% solution.w.md)))
  loss.v.md <- loss.v.md / nrow(Z0)

  ## -- C++ Version ipop version -- ##
  ## solution.w <- private$solution_w_cpp(matrix(as.numeric(solution.v)),
  ##                                      X0.scaled,
  ##                                      X1.scaled
  ##                                      )

  ## -- For w -- ##

  if (loss.v.LowRankQP < loss.v.md) {
    solution.w <- solution.w.LowRankQP
  } else {
    solution.w <- solution.w.md
  }

  ## -- For v -- ##
  rownames(solution.w) <- colnames(X0.scaled)
  colnames(solution.w) <- "w.weight"
  names(solution.v) <- rownames(self$X.scaled)

  return(list(solution.v = solution.v, solution.w = solution.w))
}
