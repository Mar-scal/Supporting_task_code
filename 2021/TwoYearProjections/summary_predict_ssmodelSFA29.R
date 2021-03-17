summary.predict.ssmodelSFA29 <- function (object, signif = 3, ...) 
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