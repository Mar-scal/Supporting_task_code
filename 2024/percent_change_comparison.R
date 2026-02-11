# condition variability

load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2024/Survey_summary_output/Survey_all_results.Rdata")


for(b in names(survey.obj)){
  if(b %in% "Ger") {
    survey.obj[[b]] <- lined.survey.obj
    survey.obj[[b]]$model.dat <- left_join(survey.obj[[b]]$model.dat, cf.data$Ger$CFyrs)
    survey.obj[[b]]$model.dat$CF <- survey.obj[[b]]$model.dat$CF2
  }
  print(b)
  standdev <- sd(survey.obj[[b]]$model.dat$CF, na.rm=T)
  meanc <- mean(survey.obj[[b]]$model.dat$CF, na.rm=T)
  
  last <- survey.obj[[b]]$model.dat$CF[1:(length(survey.obj[[b]]$model.dat$CF)-3)]
  this <- survey.obj[[b]]$model.dat$CF[2:(length(survey.obj[[b]]$model.dat$CF)-2)]
  df <- as.data.frame(cbind(last, this))
  df$word <- NA
  
  df$word[as.numeric(df$this) < as.numeric(df$last)] <- "decreased"
  df$word[as.numeric(df$this) > as.numeric(df$last)] <- "increased"
  
  df <- df[!is.na(df$last) & !is.na(df$this),]
  
  df$perc <- NA
  
  df$perc[df$word=="increased"] <- 
    (as.numeric(df$this[df$word=="increased"]) - as.numeric(df$last[df$word=="increased"]))/
    as.numeric(df$last[df$word=="increased"]) *100
  
  df$perc[df$word=="decreased"] <-   
    (as.numeric(df$last[df$word=="decreased"]) - as.numeric(df$this[df$word=="decreased"]))/
    as.numeric(df$last[df$word=="decreased"]) *100
  
  out <- df %>% dplyr::group_by(word) %>% dplyr::summarize(max(perc)) %>% dplyr::mutate(bank=b) 
  
  
  
  l22 <- survey.obj[[b]]$model.dat$CF[survey.obj[[b]]$model.dat$year==2022]
  t23 <- survey.obj[[b]]$model.dat$CF[survey.obj[[b]]$model.dat$year==2023]
  df <- as.data.frame(cbind(l22, t23))
  df$word <- NA
  
  df$word[as.numeric(df$t23) < as.numeric(df$l22)] <- "decreased"
  df$word[as.numeric(df$t23) > as.numeric(df$l22)] <- "increased"
  
  df <- df[!is.na(df$l22) & !is.na(df$t23),]
  
  df$perc <- NA
  
  df$perc[df$word=="increased"] <- 
    (as.numeric(df$t23[df$word=="increased"]) - as.numeric(df$l22[df$word=="increased"]))/
    as.numeric(df$l22[df$word=="increased"]) *100
  
  df$perc[df$word=="decreased"] <-   
    (as.numeric(df$l22[df$word=="decreased"]) - as.numeric(df$t23[df$word=="decreased"]))/
    as.numeric(df$l22[df$word=="decreased"]) *100
  
  
  
  l23 <- survey.obj[[b]]$model.dat$CF[survey.obj[[b]]$model.dat$year==2023]
  t24 <- survey.obj[[b]]$model.dat$CF[survey.obj[[b]]$model.dat$year==2024]
  df2 <- as.data.frame(cbind(l23, t24))
  df2$word <- NA
  
  df2$word[as.numeric(df2$t24) < as.numeric(df2$l23)] <- "decreased"
  df2$word[as.numeric(df2$t24) > as.numeric(df2$l23)] <- "increased"
  
  df2 <- df2[!is.na(df2$l23) & !is.na(df2$t24),]
  
  df2$perc <- NA
  
  df2$perc[df2$word=="increased"] <- 
    (as.numeric(df2$t24[df2$word=="increased"]) - as.numeric(df2$l23[df2$word=="increased"]))/
    as.numeric(df2$l23[df2$word=="increased"]) *100
  
  df2$perc[df2$word=="decreased"] <-   
    (as.numeric(df2$l23[df2$word=="decreased"]) - as.numeric(df2$t24[df2$word=="decreased"]))/
    as.numeric(df2$l23[df2$word=="decreased"]) *100

  
  out$word_23 <- df$word
  out$perc_23 <- df$perc
  
  out$word_24 <- df2$word
  out$perc_24 <- df2$perc
  out$sd <- standdev
  out$var <- standdev/meanc
  print(out)
}

