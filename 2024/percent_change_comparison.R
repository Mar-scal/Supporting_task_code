# condition variability

load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2024/Survey_summary_output/Survey_all_results.Rdata")


for(b in names(survey.obj)){
  if(b %in% "Ger") survey.obj[[b]] <- lined.survey.obj
  
  standdev <- sd(survey.obj[[b]]$model.dat$CF, na.rm=T)
  meanc <- mean(survey.obj[[b]]$model.dat$CF, na.rm=T)
  
  last <- survey.obj[[b]]$model.dat$CF[1:(length(survey.obj[[b]]$model.dat$CF)-2)]
  this <- survey.obj[[b]]$model.dat$CF[2:(length(survey.obj[[b]]$model.dat$CF)-1)]
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
  
  l23 <- survey.obj[[b]]$model.dat$CF[survey.obj[[b]]$model.dat$year==2023]
  t24 <- survey.obj[[b]]$model.dat$CF[survey.obj[[b]]$model.dat$year==2024]
  df <- as.data.frame(cbind(l23, t24))
  df$word <- NA
  
  df$word[as.numeric(df$t24) < as.numeric(df$l23)] <- "decreased"
  df$word[as.numeric(df$t24) > as.numeric(df$l23)] <- "increased"
  
  df <- df[!is.na(df$l23) & !is.na(df$t24),]
  
  df$perc <- NA
  
  df$perc[df$word=="increased"] <- 
    (as.numeric(df$t24[df$word=="increased"]) - as.numeric(df$l23[df$word=="increased"]))/
    as.numeric(df$l23[df$word=="increased"]) *100
  
  df$perc[df$word=="decreased"] <-   
    (as.numeric(df$l23[df$word=="decreased"]) - as.numeric(df$t24[df$word=="decreased"]))/
    as.numeric(df$l23[df$word=="decreased"]) *100
  
  out$curr_word <- df$word
  out$curr_perc <- df$perc
  out$standdev <- standdev
  out$var <- standdev/meanc
  print(out)
  
}

