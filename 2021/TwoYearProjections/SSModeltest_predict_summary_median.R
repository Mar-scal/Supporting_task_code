# builds the 2y decision tables for single strata from 29W subarea

SSModeltest_predict_summary_median <- function (object, signif = 3, LRP, USR, ...) 
{
  
  if(!is.na(LRP) & !is.na(USR)){
    strata <- "High"
  }
  if(is.na(LRP) & is.na(USR)){
    strata <- "Medium"
  }
  browser()
  postmedianB <- median(object$B.next[[which(object$labels==strata)]])
  
  out <- data.frame(Catch = rowSums(object$Catch), 
                    Exploit = round(object$Catch[[which(object$labels==strata)]]/(postmedianB + object$Catch[[which(object$labels==strata)]]), signif),
                    Density = round(apply(object$B.next, 2, median)/object$Area, 2),
                    B.change = round(apply(100 * (object$B.next - object$B.cur)/object$B.cur, 2, median), 1),
                    Prob.B = round(apply(0 < (object$B.next - object$B.cur), 2, mean), 2))
  
  # don't do it for 29A!
  if(!is.na(LRP) & !is.na(USR)){
    out$Prob_above_LRP <- signif(apply((object$B.next/object$Area) >= LRP, 2, sum)/apply(object$B.next, 2, length),digits=2)
    out$Prob_above_USR <- signif(apply((object$B.next/object$Area) >= USR, 2, sum)/apply(object$B.next, 2, length),digits=2)
  }

  list(Next.year = out)
}


function (object, signif = 3, ...) 
{
  out <- as.data.frame(matrix(NA, 1, 7))
  len.label <- length(object$labels)
  names(out)[c(1, 3:5)] <- paste(c("Exploit", "Density", 
                                   "B.change", "Prob.B"), object$label[len.label], 
                                 sep = ".")
  names(out)[c(2, 6, 7)] <- paste(c("Catch", "B.change", 
                                    "Prob.B"), "All", sep = ".")
  out[1, 1] <- round(sapply(object$mu.p, median)[len.label], 
                     2)
  out[1, 2] <- round(median(apply(object$Catch, 1, sum)))
  out[1, 3] <- round(sapply(object$Bh.next, median)[len.label]/object$Area[len.label], 
                     2)
  out[1, 4] <- round(median(100 * (object$Bh.next[[len.label]] - 
                                     object$Bh.last[, len.label])/object$Bh.last[, len.label]), 
                     1)
  out[1, 5] <- round(mean(0 < (object$Bh.next[[len.label]] - 
                                 object$Bh.last[, len.label])), 2)
  Bh.next.tot <- apply(sapply(object$Bh.next, function(x) {
    x
  }), 1, sum)
  Bh.last.tot <- apply(object$Bh.last, 1, sum)
  out[1, 6] <- round(median(100 * (Bh.next.tot - Bh.last.tot)/Bh.last.tot), 
                     1)
  out[1, 7] <- round(mean(0 < (Bh.next.tot - Bh.last.tot)), 
                     2)
  temp <- round(as.vector(unlist(apply(object$m, 2, mean))), 
                3)
  out2 <- as.data.frame(matrix(NA, 1, length(object$label)))
  out2 <- temp
  names(out2) <- paste("m", object$label, sep = ".")
  list(Next.year = out, Mean.m = out2)
}
  