predict.SSModelSFA29 <- function (object, exploit = NULL, Catch = NULL, g.parm, m.avg = 5, 
          Max.P = 6, ...) 
{
  pB0 <- B.change <- mu.p <- B.p <- vector(mode = "list", 
                                           length = length(object$labels))
  names(B.p) <- names(mu.p) <- names(B.change) <- names(pB0) <- object$labels
  H <- object$data$H
  test.col <- array("", c(7, H))
  for (h in 1:H) {
    test.col[1, h] <- paste("Bh[", object$data$N, ",", 
                            h, "]", sep = "")
    test.col[2, h] <- paste("BBh[", object$data$N - 
                              1, ",", h, "]", sep = "")
    test.col[3, h] <- paste("Ph[", object$data$N, ",", 
                            h, "]", sep = "")
    test.col[4, h] <- paste("Rh[", object$data$N, ",", 
                            h, "]", sep = "")
    test.col[5, h] <- paste("m[", object$data$N, ",", 
                            h, "]", sep = "")
    test.col[6, h] <- paste("Kh[", h, "]", sep = "")
    test.col[7, h] <- paste("sigma[", h, "]", 
                            sep = "")
  }
  Bh <- object$sims.matrix[, test.col[1, ]]
  BBh <- object$sims.matrix[, test.col[2, ]]
  Ph <- object$sims.matrix[, test.col[3, ]]
  Rh <- object$sims.matrix[, test.col[4, ]]
  m <- object$sims.matrix[, test.col[5, ]]
  Kh <- object$sims.matrix[, test.col[6, ]]
  sigma <- object$sims.matrix[, test.col[7, ]]
  if (m.avg > 1) {
    for (i in 1:(m.avg - 1)) {
      for (h in 1:H) {
        test.col[5, h] <- paste("m[", object$data$N - 
                                  i, ",", h, "]", sep = "")
      }
      m <- m + object$sims.matrix[, test.col[5, ]]
    }
    m <- m/m.avg
  }
  Catchm <- matrix(NA, dim(Bh)[1], dim(Bh)[2])
  Bptot <- rep(0, dim(Bh)[1])
  SDM.num <- length(object$labels)
  mn.Bh <- apply(Bh + Rh * Kh, 2, mean)
  if (!is.null(exploit)) 
    temp.exploit <- Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                 exploit, object$e.parms, object$data$Area)
  if (!is.null(Catch)) {
    temp.exploit <- list(exploit = sweep(sweep(sweep(Bh, 
                                                     2, object$data$Area, "/")/apply(Bh, 1, sum), 
                                               1, object$e.parms[H + 1], "*"), 2, object$e.parms[1:H], 
                                         "+"))
    PCatch <- mn.Bh * temp.exploit$exploit
    PCatch <- Catch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                            "/")/Kh
  }
  for (h in 1:H) {
    Pmed.p <- exp(-m[, h]) * (g.parm[h] * (Ph[, h] + Rh[, 
                                                        h]))
    if (is.null(Catch)) 
      PCatch.h <- Pmed.p * temp.exploit$exploit[h]
    else PCatch.h <- PCatch[, h]
    if (temp.exploit$exploit[h] > 0) {
      Pmed.p <- Pmed.p - PCatch.h
    }
    Pmed.p[Pmed.p < 0.00001] <- 0.0001
    Pmed.p <- log(Pmed.p)
    P.p <- gen.lnorm(Pmed.p, sigma = sigma[, h], Max.P)
    B.p[[h]] <- P.p * Kh[, h]
    Catchm[, h] <- PCatch.h * Kh[, h]
    mu.p[[h]] <- Catchm[, h]/(B.p[[h]] + Catchm[, h])
    Bptot <- Bptot + B.p[[h]]
  }
  out <- list(Bh.next = B.p, mu.p = mu.p, Catch = Catchm, Bh.last = Bh, 
              m = m, labels = object$labels, Area = object$data$Area)
  class(out) <- "predict.ssmodelSFA29"
  out
}