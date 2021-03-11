predict.SSModeltest.2y <- function (object, exploit = NULL, Catch = NULL, catch2 = NULL, g.parm, m.avg = 5, 
                                    Max.P = 6, two.year=T, m.2 = NULL, m.multiplier = NULL,...) 
{
  
  predict.call <- match.call()
  Cur.year <- length(object$Years)
  
  if(two.year==T) years <- c(Cur.year, Cur.year+1)
  if(two.year==F) years <- Cur.year
  if(two.year==T) g.parm <- rbind(g.parm, g.parm)
  
  out <- NULL
  
  # we're going to loop through the following code in case we're doing a 2-year projection.
  for(i in 1:length(years)){
    # if we're projecting for the first time, we just take the data from the model output. If it's the second time, we use the values from the previous projection (which is done at the end of the 1st iteration of the loop)
    if(i==1) {
      pB0 <- B.change <- mu.p <- B.p <- P.p <- vector(mode = "list", 
                                                      length = length(object$labels))
      names(B.p) <- names(P.p) <- names(mu.p) <- names(B.change) <- names(pB0) <- object$labels
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
    }
  
    # keep K and sigma the same regardless of year (no r for 29W)
    Kh <- object$sims.matrix[, test.col[6, ]]
    sigma <- object$sims.matrix[, test.col[7, ]]
    
    # get mortality and apply multiplier if you want
    m <- object$sims.matrix[, test.col[5, ]]
    
    if(!is.null(m.multiplier) & is.null(m.2) & i > 1) m <- m * m.multiplier
    if(is.null(m.multiplier) & !is.null(m.2) & i > 1) {
      m <- m.2
      m.avg <- NULL
    }
    
    if (m.avg > 1) {
      for (j in 1:(m.avg - 1)) {
        for (h in 1:H) {
          test.col[5, h] <- paste("m[", object$data$N - 
                                    j, ",", h, "]", sep = "")
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
      temp.exploit <- SSModeltest:::Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                                 exploit, object$e.parms, object$data$Area)
    if (!is.null(Catch)) {
      
      if(i>1) Catch <- catch2
      
      # this is the exploitation for a particular strata
      temp.exploit <- list(exploit = 
                             sweep(
                               sweep(
                                 sweep(Bh,2, object$data$Area, "/")/ # biomass density
                                   apply(Bh, 1, sum), # proportional to entire area
                                 1, object$e.parms[H + 1], "*"), # by slope
                               2, object$e.parms[1:H], "+")) # plus intercept
      
      PCatch <- mn.Bh * temp.exploit$exploit
      PCatch <- Catch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                              "/")/Kh
    }
    
    for (h in 1:H) {
      Pmed.p <- exp(-m[, h]) * (g.parm[i, h] * (Ph[, h] + Rh[, 
                                                          h]))
      if (is.null(Catch)) 
        PCatch.h <- Pmed.p * temp.exploit$exploit[h]
      else PCatch.h <- PCatch[, h]
      if (length(temp.exploit$exploit[[h]]) > 0) {
        Pmed.p <- Pmed.p - PCatch.h
      }
      Pmed.p[Pmed.p < 1e-05] <- 1e-04
      Pmed.p <- log(Pmed.p)
      P.p[[h]] <- SSModeltest:::gen.lnorm(Pmed.p, sigma = sigma[, h], Max.P)
      B.p[[h]] <- P.p[[h]] * Kh[, h]
      Catchm[, h] <- PCatch.h * Kh[, h]
      mu.p[[h]] <- Catchm[, h]/(B.p[[h]] + Catchm[, h])
      Bptot <- Bptot + B.p[[h]]
    }
    
    out[[i]] <- list(Bh.next = B.p, P.out = P.p, mu.p = mu.p, Catch = Catchm, Bh.last = Bh, g.parm = g.parm[i,],
                     m = m, labels = object$labels, Area = object$data$Area)
    class(out) <- "predict.ssmodelSFA29"
    
    # if you're in the 1st iteration, and there's going to be another one, store these values for the next year. 
    if(length(years)>1 & i==1) {
      Ph <- data.frame(Low=P.p$Low, Medium=P.p$Medium, High=P.p$High)
      Bh <- data.frame(Low=B.p$Low, Medium=B.p$Medium, High=B.p$High)
    }
  } # end the 2y loop
  
  out
  
}
