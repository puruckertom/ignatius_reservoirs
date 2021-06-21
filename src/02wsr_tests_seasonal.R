dim(ari50)

ndays <- ncol(ari50)-2
col_strings <- colnames(ari50[3:ndays+2])
year <- substr(col_strings,2,5)
day <- substr(col_strings,6,8)
sample_date <- as.Date(paste(day,year), format="%j %Y")
head(sample_date)
sample_season <- factor(quarters(as.chron(sample_date)), 
                        labels = c("winter", "spring", "summer", "fall"))
head(sample_season)
season_date <- cbind(sample_season, sample_date)
#extract season columns for later use
spring_cols <- which(sample_season=="spring")
summer_cols <- which(sample_season=="summer")
fall_cols <- which(sample_season=="fall")
winter_cols <- which(sample_season=="winter")

#create long vectors for comparison, drop observation when both are NAs
headwaters <- vector()
neardam <- vector()
season <- vector()
#create storage vectors for p-value resutls
location_sig <- vector(mode="character", length=240)
season_sig <- vector(mode="character", length=240)
pvalue <- vector(mode="numeric", length=240)
# non-parameteric wilcoxon signed rank test for paired samples
# https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
counter <- 0
for(i in 1:nrow(ari50)){
  if(is.odd(i)){ # Headwaters, NearDam 
    for(j in 1:4){
      counter <- counter + 1
      if(j==1){
        extract_cols <- spring_cols
        this_season <- 'spring'
      }else if(j==2){
        extract_cols <- summer_cols
        this_season <- 'summer'        
      }
      }else if(j==3){
        extract_cols <- fall_cols
        this_season <- 'fall'        
      }
      }else if(j==4){
        extract_cols <- winter_cols
        this_season <- 'winter'        
      }
      as.character(ari50[i,1])
      #grab the data for this reservoir
      reservoir_temp <- as.matrix(ari50[i:(i+1),3:ndays+2])
      View(reservoir_temp)
      #grab the data for this season in this reservoir
      reservoir_temp <- reservoir_temp
      colnames(reservoir_temp) <- NULL
      location_sig[counter] <- as.character(ari50[i,1])
      #extract all
      headwaters_temp <- reservoir_temp[1,]
      neardam_temp <- reservoir_temp[2,]
      #find those not NA
      keepers_headwaters <- which(!is.na(headwaters_temp))
      keepers_neardam <- which(!is.na(neardam_temp))
      #ID those where headwater OR neardam not NA
      keepers_temp <- union(keepers_headwaters, keepers_neardam)
      #extract the keepers
      headwaters_kept <- headwaters_temp[keepers_temp]
      neardam_kept <- neardam_temp[keepers_temp]
      seasons_kept <- sample_season[keepers_temp]
      n_length <- length(headwaters_kept)
      #build a dataframe
      bloom_compare <- data.frame(
        cbind(
          concentration = c(headwaters_kept, neardam_kept), 
          seasons = c(seasons_kept, seasons_kept),
          location = rep(c("headwaters", "neardam"), each = n_length)))
      bloom_compare$concentration <- as.numeric(bloom_compare$concentration)
      bloom_compare$location <- as.factor(bloom_compare$location)
      #summary(bloom_compare)
      #wsr test
      hw_nd_test <- wilcox.test(x=headwaters_kept, y=neardam_kept,
                                alternative = "greater",
                                mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
                                conf.int = FALSE, conf.level = 0.95)
      reservoir_name <- as.character(ari50$Reservoir[i])
      print(hw_nd_test)
      p_value_text <- paste("p=", signif(hw_nd_test$p.value,4))
      pvalue[counter] <- signif(hw_nd_test$p.value,4)
      #boxplot
      bxp <- ggboxplot(bloom_compare, x = "location", y = "concentration",  
                       color = "location", palette = c("#00AFBB", "#E7B800"),
                       order = c("headwaters", "neardam"),
                       title= reservoir_name,
                       ylab = "Conc", xlab = p_value_text) + 
        theme(legend.position = "none")
      bxp
    }
  }
}

location_pvalues <- cbind(location_sig, pvalue)
#View(location_pvalues)
sum(pvalue>0.05)

write.csv(location_pvalues, paste(ari_data_out, "/location_pvalues.csv", sep=""))
