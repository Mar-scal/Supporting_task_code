SSModel_predict_summary_median <- function (object, LRP, USR, RRP = NULL, signif = 3, ...) 
{
  out <- data.frame(Catch = object$Catch)
  if (is.matrix(object$B.next)) {
    postmedianB <- apply(object$B.next, 2, median) # posterior median biomass
    out$Exploit <- round(object$Catch/(postmedianB + object$Catch), #exploitation = catch/median biomass + catch
                         signif)
    
    # Percent change:
    # sweep function below is doing the following:
    # 1: the differences between B.next and B.cur: (object$B.next - object$B.cur)[1:3,1]
    # dim(object$B.next)
    # [1] 52500    51
    # length(object$B.cur)
    # [1] 52500  
    # dim(object$B.next - object$B.cur)
    # [1] 52500    51
    # 2: Each of the columns (that correspond to a catch level) are divided by object$B.cur
    # (object$B.next - object$B.cur)[1,1]/object$B.cur[1] == sweep(object$B.next - object$B.cur, 1, object$B.cur, "/")[1,1]
    # (object$B.next - object$B.cur)[2,1]/object$B.cur[2] == sweep(object$B.next - object$B.cur, 1, object$B.cur, "/")[2,1]
    # (object$B.next - object$B.cur)[3,1]/object$B.cur[3] == sweep(object$B.next - object$B.cur, 1, object$B.cur, "/")[3,1]
    # 3: Then multiply those by 100 to get the % difference
    # 4: then calculate the median within each column, and take the signif digits
    # median(100 * sweep(object$B.next - object$B.cur, 1, object$B.cur, "/")[,1]) == apply(100 * sweep(object$B.next - object$B.cur, 1, object$B.cur, "/"), 2, median)[1]
    # median(100 * sweep(object$B.next - object$B.cur, 1, object$B.cur, "/")[,2]) == apply(100 * sweep(object$B.next - object$B.cur, 1, object$B.cur, "/"), 2, median)[2]
    
    out$B.change <- round(apply(100 * sweep(object$B.next - 
                                              object$B.cur, 1, object$B.cur, "/"), 2, median), signif)
    
    # probability of a biomass increase 
    # 1: summing up the number of differences > 0 (signifies increase)
    # sum(((object$B.next - object$B.cur) > 0 )[,1]) == apply((object$B.next - object$B.cur) > 0, 2, sum)[1]
    # sum(((object$B.next - object$B.cur) > 0 )[,2]) == apply((object$B.next - object$B.cur) > 0, 2, sum)[2]
    # 2: then divide by the number of iterations
    out$pB0 <- round(apply((object$B.next - object$B.cur) > 
                             0, 2, sum)/length(object$B.cur), signif)
    
    if (!(missing(LRP) | missing(USR))) {
      # if you have an LRP and USR, then the probability of exceeding them. 
      out$p.LRP <- round(apply(object$B.next > LRP, 2, 
                               sum)/length(object$B.cur), signif)
      out$p.USR <- round(apply(object$B.next > USR, 2, 
                               sum)/length(object$B.cur), signif)
    }
    
    if (!is.null(RRP)) {
    # if you have a removal reference point, the catch at different probabilities exploitation exceeding your RRP
      # read this like: for catch of 127t, there is a 10% chance of exceeding the reference exploitation level of 0.15
      out2 <- t(apply(object$B.next * RRP, 2, quantile, 
                      probs = seq(0.1, 0.6, by = 0.1)))
      out2 <- cbind(object$Catch, round(out2))
      dimnames(out2)[[2]] <- c("Catch", as.character(seq(0.1, 
                                                         0.6, by = 0.1)))
    }
  }
  # if B.next is not a matrix...
  if(!is.matrix(object$B.next)) {
    postmedianB <- median(object$B.next)
    out$Exploit <- round(object$Catch/(postmedianB + object$Catch), 
                         signif)
    out$B.change <- round(median(100 * (object$B.next - object$B.cur)/object$B.cur), 
                          signif)
    out$pB0 <- round(median((object$B.next - object$B.cur) > 
                            0), signif)
    if (!(missing(LRP) | missing(USR))) {
      out$p.LRP <- round(median(object$B.next > LRP), signif)
      out$p.USR <- round(median(object$B.next > USR), signif)
    }
    if (!is.null(RRP)) {
      out2 <- c(object$Catch, round(quantile(object$B.next * 
                                               RRP, probs = seq(0.1, 0.6, by = 0.1)), 0))
      names(out2) <- c("Catch", as.character(seq(0.1, 0.6, 
                                                 by = 0.1)))
    }
  }
  if (exists("out2")) 
    return(list(Next.year = out, Interim.RRP = out2))
  else return(list(Next.year = out))
}