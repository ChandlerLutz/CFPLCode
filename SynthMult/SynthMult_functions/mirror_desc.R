## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/mirror_desc.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-25

mirror_dec <- function(v, alpha, grad){
  h <- v * exp(-alpha*grad) ## Exponentiated Gradient Descent (or mirror-descent for entropy regularizer)
  h <- h/sum(h)
  return (h)
}

f_mirror_dec <- function(X0.scaled, X1.scaled, niter = 10000, rel.tol = 1e-8) {

  n <- ncol(X0.scaled)
  w <- (1 / n) * matrix(1, nrow = n, ncol = 1)

  J <- crossprod(X0.scaled)
  g <- crossprod(X0.scaled, X1.scaled)
  h <- crossprod(X1.scaled)

  obj.val <- t(w) %*% J %*% w - 2 * t(w) %*% g + h

  alpha <- 1
  for (t in 1:niter){
    step.size <- alpha
    grad = 2*(J %*% w - g)
    w.np <- private$mirror_dec(w,step.size,grad)
    obj.val.n <- t(w.np) %*% J %*% w.np - 2* t(w.np) %*% g + h
    rel.imp = (obj.val-obj.val.n)/obj.val
    if(obj.val.n < 1e-14){
      w <- w.np
      break
    }
    if( rel.imp <= 0 ){
      alpha <- 0.95 * alpha
    } else{
      w <- w.np
      obj.val <- obj.val.n
    }
    if( (rel.imp > 0) && (rel.imp < rel.tol) ){
      w = w.np
      break
    }
  }
  return(w)
}


