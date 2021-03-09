# This function performs 2 year projections by multiplying the one year projection by the expected exploitation rate. 
# It uses a "zero surplus production" scenario which does not factor in mortality, recruitment, growth etc. 
# This is akin to the interim table used in the BoF Update documents

# only input required is the model RData object

simple_2y_proj <- function(object, area){
  #browser()
  # extra catch values for 2yr projections
  # length(object$Years) == length(object$data$C)
  object$data$C <- c(object$data$C, object$data$C[length(object$data$C)])
  
  year.start <- object$data$maxY-9
  
  sub.obj <- NULL
  Max.P <- NULL
  m.avg <- 5
  B.next0 <- NULL
  B.next1 <- NULL
  B.next2 <- NULL
  
  for (i in year.start:length(object$Years)){
    sub.obj$sims.matrix <- cbind(object$sims.matrix[,paste0("B[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("R[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("P[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("m[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("r[", 1:i, "]")])
   
    if(area %in% c("GBa", "BBn")) sub.obj$sims.matrix  <- cbind(sub.obj$sims.matrix, object$sims.matrix[,paste0("mR[", 1:i, "]")])
    
    sub.obj$Years <- object$Years[1:i]
    
    q <- object$sims.matrix[,paste0("q")]
    r <- sub.obj$sims.matrix[,paste0("r[", i, "]")]
    K <- object$sims.matrix[,paste0("K")]
    sigma <- object$sims.matrix[,paste0("sigma")]
    
    P <- sub.obj$sims.matrix[,paste0("P[", i, "]")]
    R <- sub.obj$sims.matrix[,paste0("R[", i,"]")]
    
    m <- object$sims.matrix[,paste0("m[", 1:i, "]")]
    m.avg <- 5
    m <- apply(m[, length(sub.obj$Years) - (0:(m.avg - 1))], 1, mean)
    
    if(area %in% c("GBa", "BBn")) {
      mR <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]],
                                                      1, 3), "mR[")]
      mR.avg <- 5
      mR <- apply(mR[, i - (0:(m.avg - 1))], 1, mean)
    }
    
    #BoF and Offshore
    if(!grepl(x=area, "29")){
      if(!area %in% c("GBa", "BBn")) Pmed <- exp(-m) * object$data$g[i] * (P - object$data$C[i]/K) + exp(-m) * 
        object$data$gR[i] * r
      if(area %in% c("GBa", "BBn")) Pmed <- exp(-m) * object$data$g[i] * (P - object$data$C[i]/K) + exp(-mR) * 
          object$data$gR[i] * r
      Pmed <- Pmed <- ifelse(Pmed > 0.001, Pmed, 0.001)
      Pmed <- log(Pmed)
      Max.P <- 8
      set.seed(1)
      P.out <- SSModel:::gen.lnorm(Pmed, sigma, Max.P)
    }
    
    #29W
    if(grepl(x=area, "29")){
      # process equation
      Pmed.p <- exp(-m) * (object$data$g[i] * (P + R))
      # remove catch
      #Pcatch <- Pmed.p * exploit
      Pmed.p <- Pmed.p - object$data$C[i]/K
      
      Pmed.p[Pmed.p < 1e-05] <- 1e-04
      Pmed.p <- log(Pmed.p)
      Max.P <- 6
      P.out <- SSModeltest:::gen.lnorm(Pmed.p, sigma, Max.P)
      # mu.p <- object$data$C[i]/(P.out*K + object$data$C[i])
    }
    
    #B.next is your 1 year projection 
    B.next <- P.out * K
    
    #Figure out 2 year projection 
    #Range of exploitation levels 
    RRP.next <- seq(0.01,0.60,by=0.01)
    
    #Remaining biomass distribution for range of exploitation levels: 
    biomass.interim.dist <- list()
    #posterior distribution of the Catch for range of exploitation levels: 
    Catch.next.dist <- list()
    
    for (k in 1:length(RRP.next)){
      biomass.interim.dist[[k]] <- B.next * (1-RRP.next[k]) 
      Catch.next.dist[[k]] <- B.next * RRP.next[k]
    }
    
    postmedianBiomass <- lapply(biomass.interim.dist, median)
    postmedianCatch <- lapply(Catch.next.dist, median)
    
    #Convert to dataframe
    postmedianBiomass.df <- do.call(rbind.data.frame, postmedianBiomass)
    names(postmedianBiomass.df) <- "Interim.Biomass"
    postmedianCatch.df <- do.call(rbind.data.frame, postmedianCatch)
    names(postmedianCatch.df) <- "Interim.Catch"
    
    interim.table <- cbind(RRP.next, postmedianBiomass.df, postmedianCatch.df)
    interim.table$calcexp <- interim.table$Interim.Catch/(interim.table$Interim.Biomass + interim.table$Interim.Catch)
    interim.table$originalbiomass <- interim.table$Interim.Biomass + interim.table$Interim.Catch
    #interim.table
    
    # catch.2yr <- catch.by.year[6,grep(end.year+2,names(catch.by.year))]
    # catch.2yr  #catch that should be associated with the 2 year projection 
    # 
    #Find closest value in interim.table to catch.2yr and pull out index from the table to then get associated posterior which is the 2 year projection 
    #match.closest(catch.2yr, interim.table$Interim.Catch)
    B.2yr <- biomass.interim.dist[[MALDIquant::match.closest(object$data$C[i+1], interim.table$Interim.Catch)]]
    
    #boxplot(B.last, B.next, B.2yr)
    # if(object$Years[i] == 2018) browser()
    #df1 <- data.frame(Biomass = object$sims.matrix[,paste0("B[", i, "]")], year.id = 0, year = object$Years[i], catch=object$data$C[i-1])
    B.next0[[paste0(object$Years[i])]] <- data.frame(Biomass = object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 1, 2), "B[")][, i], 
                                                     year =  object$Years[i], catch=object$data$C[i-1], exp=NA, proj=0)
    B.next1[[paste0(object$Years[i])]] <- data.frame(Biomass = B.next, year =  object$Years[i]+1, catch=object$data$C[i], exp=NA, proj=1)
    B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = B.2yr, year =  object$Years[i]+2, catch=object$data$C[i+1], exp=interim.table$RRP.next[MALDIquant::match.closest(object$data$C[i+1], interim.table$Interim.Catch)], proj=2)
    
    #posteriors.projections[[paste0(object$Years[i])]] <- rbind(df1, df2, df3)
    #print(object$Years[i])
  }
  
  return(output=list(B.next0=B.next0, B.next1=B.next1, B.next2=B.next2))
}