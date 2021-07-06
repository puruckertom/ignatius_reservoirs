dim(ari50)

nd_proxy <- min(ari50[3:ndays+2], na.rm=T)/sqrt(2)

ndays <- ncol(ari50)-2
col_strings <- colnames(ari50[3:ndays+2])
year <- substr(col_strings,2,5)
day <- substr(col_strings,6,8)
sample_date <- as.Date(paste(day,year), format="%j %Y")
head(sample_date)
sample_season <- factor(quarters(as.chron(sample_date)), 
                        labels = c("winter", "spring", "summer", "fall"))
head(sample_season)
cbind(sample_season, sample_date)
length(sample_season)

#create long vectors for comparison, drop observation when both are NAs
headwaters <- vector()
neardam <- vector()
season <- vector()
lakes <- vector()

#create storage vectors for p-value resutls
location_sig <- vector(mode="character", length=60)
pvalue <- vector(mode="numeric", length=60)
pvalue_spring <- vector(mode="numeric", length=60)
pvalue_summer <- vector(mode="numeric", length=60)
pvalue_fall <- vector(mode="numeric", length=60)
pvalue_winter <- vector(mode="numeric", length=60)
# non-parameteric wilcoxon signed rank test for paired samples
# https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples

ari_wsr_test_filename <- paste(ari_graphics,"/ari_wsr_lakes_nocloudcoverobs.pdf",sep="")
pdf(ari_wsr_test_filename, width = 8.5, height = 11, onefile = T)
  counter <- 0
  for(i in 1:nrow(ari50)){
    if(is.odd(i)){ # Headwaters, NearDam repeating
      counter <- counter + 1
      
      ##### extract data for the ith lake
      #save and print lake name
      location_sig[counter] <- as.character(ari50[i,1])
      print(location_sig[counter])
      #grab the data for this reservoir
      reservoir_temp <- as.matrix(ari50[i:(i+1),3:ndays+2])
      colnames(reservoir_temp) <- NULL
      #and separately extract for headwaters and neardam
      headwaters_temp <- reservoir_temp[1,]
      neardam_temp <- reservoir_temp[2,]
      print(paste("we start with", length(headwaters_temp), "daily observations"))
      
      ##### amber separately resolved whether nondetects were real non-detects or not observed
      #grab the NAs for this reservoir (cloud cover)
      reservoir_cloudcover_temp <- as.matrix(ari_legit_NAs[i:(i+1),3:ndays+2])
      colnames(reservoir_cloudcover_temp) <- NULL
      #and separately extract for headwaters and neardam
      headwaters_cloudcover_temp <- reservoir_cloudcover_temp[1,]
      neardam_cloudcover_temp <- reservoir_cloudcover_temp[2,]
      #grab the NDs for this reservoir (non-detects)
      reservoir_nondetects_temp <- as.matrix(ari_legit_nds[i:(i+1),3:ndays+2])
      colnames(reservoir_nondetects_temp) <- NULL
      #and separately extract for headwaters and neardam
      headwaters_nondetects_temp <- reservoir_nondetects_temp[1,]
      neardam_nondetects_temp <- reservoir_nondetects_temp[2,]
      # QA: inspect temp objects
      #View(rbind(headwaters_temp, headwaters_cloudcover_temp, headwaters_nondetects_temp))
      #View(rbind(neardam_temp, neardam_cloudcover_temp, neardam_nondetects_temp))
      
      ##### handle NAs, NaNs and non-detects for this lake
      #find those not NA/ND (and not zero) to begin with
      keepers_headwaters <- which(!is.na(headwaters_temp))
      keepers_neardam <- which(!is.na(neardam_temp))
      #ID those where headwater OR neardam not NA with union
      keepers_temp <- sort(union(keepers_headwaters, keepers_neardam))
      print(paste("we now have", length(keepers_temp), "daily observations after dropping days with jointly missing values"))
      #View(rbind(headwaters_temp[keepers_temp], neardam_temp[keepers_temp]))
      
      #####any remaining pair with a cloudcover observation should be dropped
      # cloudcover observations are NAs in headwaters_cloudcover_temp or neardam_cloudcover_temp
      headwaters_drop_cloudcover <- which(!is.nan(headwaters_cloudcover_temp[keepers_temp]))
      keepers_temp[headwaters_drop_cloudcover] #tricky, tricky
      neardam_drop_cloudcover <- which(!is.nan(neardam_cloudcover_temp[keepers_temp]))
      keepers_temp[neardam_drop_cloudcover] #tricky, tricky
      cloudcover_drop_these <- sort(union(headwaters_drop_cloudcover,neardam_drop_cloudcover))
      cloudcover_drops <- keepers_temp[cloudcover_drop_these] # tricky, tricky
      # remove cloudcover_drops from keepers_temp
      keepers_temp2 <- keepers_temp[!(keepers_temp %in% cloudcover_drops)]
      print(paste("we now have", length(keepers_temp2), "daily observations after dropping missing observations due to cloud cover"))
      #View(rbind(headwaters_temp[keepers_temp2], neardam_temp[keepers_temp2]))
      
      ##### drop any ties due to same value
      #find ties so we can drop them (for the test)
      #View(rbind(headwaters_temp, neardam_temp))
      which_detected_ties <- which(headwaters_temp[keepers_temp2]==neardam_temp[keepers_temp2])
      n_detected_ties <- length(which_detected_ties)
      if(n_detected_ties>0){
          keepers_temp3 <- keepers_temp2[-which_detected_ties]
        } else {
          keepers_temp3 <- keepers_temp2
      }
      #headwaters_temp[keepers_temp3]
      #neardam_temp[keepers_temp3]
      headwaters_temp3 <- headwaters_temp[keepers_temp3]
      neardam_temp3 <- neardam_temp[keepers_temp3]
      #which(headwaters_kept==neardam_kept)
      #View(rbind(headwaters_kept, neardam_kept))
      print(paste("we now have", length(keepers_temp3), "daily observations after dropping remaining ties"))
      
      ##### extract the keepers that will be used for testing
      keepers_final <- keepers_temp3
      headwaters_kept <- headwaters_temp[keepers_final]
      neardam_kept <- neardam_temp[keepers_final]
      seasons_kept <- sample_season[keepers_final]
      dates_kept <- sample_date[keepers_final]
      #cbind(headwaters_kept, neardam_kept, seasons_kept, dates_kept)

      ##### only now change the remaining NAs to a non-detect proxy
      headwaters_kept <- headwaters_kept %>% replace_na(nd_proxy)
      neardam_kept <- neardam_kept %>% replace_na(nd_proxy)
      #cbind(headwaters_kept, neardam_kept, seasons_kept, dates_kept)
      
      ##### identify season row ids
      season_which_spring <- which(seasons_kept=='spring')
      season_which_summer <- which(seasons_kept=='summer')
      season_which_fall <- which(seasons_kept=='fall')
      season_which_winter <- which(seasons_kept=='winter')
      
      ####
      #build a dataframe for the entire time series
      n_length <- length(headwaters_kept)
      reservoir_name <- as.character(ari50$Reservoir[i])
      bloom_compare <- data.frame(
                          cbind(
                          concentration = c(headwaters_kept, neardam_kept), 
                          seasons = c(seasons_kept, seasons_kept),
                          location = rep(c("headwaters", "neardam"), each = n_length),
                          lake = rep(reservoir_name, n_length*2)))
      bloom_compare$concentration <- as.numeric(bloom_compare$concentration)
      bloom_compare$location <- as.factor(bloom_compare$location)
      
      #thin to weekly with move
      
      
      #summary(bloom_compare)
      #wsr test for all
      hw_nd_test <- wilcox.test(x=headwaters_kept, y=neardam_kept,
                                alternative = "greater",
                                mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
                                conf.int = FALSE, conf.level = 0.95)
      print(hw_nd_test)
      p_value_text <- paste("p=", signif(hw_nd_test$p.value,4))
      pvalue[counter] <- signif(hw_nd_test$p.value,4)
      #boxplot
      bxp <- ggboxplot(bloom_compare, x = "location", y = "concentration",  
                color = "location", palette = c("#00AFBB", "#E7B800"),
                order = c("headwaters", "neardam"),
                title= paste(reservoir_name, "; n = ", n_length),
                ylab = "Conc", xlab = p_value_text) + 
                theme(legend.position = "none")
      ####

      ####
      #build a dataframe for spring only
      n_length_spring <- length(headwaters_kept[season_which_spring])
      headwaters_kept_spring <- headwaters_kept[season_which_spring]
      neardam_kept_spring <- neardam_kept[season_which_spring]
      seasons_kept_spring <- seasons_kept[season_which_spring]
      bloom_compare_spring <- data.frame(
        cbind(
          concentration = c(headwaters_kept_spring, neardam_kept_spring), 
          seasons = c(seasons_kept_spring, seasons_kept_spring),
          location = rep(c("hw", "nd"), each = n_length_spring)))
      bloom_compare_spring$concentration <- as.numeric(bloom_compare_spring$concentration)
      bloom_compare_spring$location <- as.factor(bloom_compare_spring$location)
      #summary(bloom_compare)
      #wsr test for all
      hw_nd_test_spring <- wilcox.test(x=headwaters_kept_spring, y=neardam_kept_spring,
                                alternative = "greater",
                                mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
                                conf.int = FALSE, conf.level = 0.95)
      reservoir_name <- as.character(ari50$Reservoir[i])
      print(hw_nd_test_spring)
      p_value_text_spring <- paste("p=", signif(hw_nd_test_spring$p.value,4))
      pvalue_spring[counter] <- signif(hw_nd_test_spring$p.value,4)
      #boxplot
      bxp_spring <- ggboxplot(bloom_compare_spring, x = "location", y = "concentration",  
                       color = "location", palette = c("#00AFBB", "#E7B800"),
                       order = c("hw", "nd"),
                       title= paste("spr; n=",n_length_spring),
                       ylab = "Conc", xlab = p_value_text_spring) + 
        theme(legend.position = "none")
      ####

      ####
      #build a dataframe for summer only
      n_length_summer <- length(headwaters_kept[season_which_summer])
      headwaters_kept_summer <- headwaters_kept[season_which_summer]
      neardam_kept_summer <- neardam_kept[season_which_summer]
      seasons_kept_summer <- seasons_kept[season_which_summer]
      bloom_compare_summer <- data.frame(
        cbind(
          concentration = c(headwaters_kept_summer, neardam_kept_summer), 
          seasons = c(seasons_kept_summer, seasons_kept_summer),
          location = rep(c("hw", "nd"), each = n_length_summer)))
      bloom_compare_summer$concentration <- as.numeric(bloom_compare_summer$concentration)
      bloom_compare_summer$location <- as.factor(bloom_compare_summer$location)
      #summary(bloom_compare)
      #wsr test for all
      hw_nd_test_summer <- wilcox.test(x=headwaters_kept_summer, y=neardam_kept_summer,
                                       alternative = "greater",
                                       mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
                                       conf.int = FALSE, conf.level = 0.95)
      reservoir_name <- as.character(ari50$Reservoir[i])
      print(hw_nd_test_summer)
      p_value_text_summer <- paste("p=", signif(hw_nd_test_summer$p.value,4))
      pvalue_summer[counter] <- signif(hw_nd_test_summer$p.value,4)
      #boxplot
      bxp_summer <- ggboxplot(bloom_compare_summer, x = "location", y = "concentration",  
                              color = "location", palette = c("#00AFBB", "#E7B800"),
                              order = c("hw", "nd"),
                              title= paste("sum; n=",n_length_summer),
                              ylab = "Conc", xlab = p_value_text_summer) + 
        theme(legend.position = "none")
      ####
      
      ####
      #build a dataframe for fall only
      n_length_fall <- length(headwaters_kept[season_which_fall])
      headwaters_kept_fall <- headwaters_kept[season_which_fall]
      neardam_kept_fall <- neardam_kept[season_which_fall]
      seasons_kept_fall <- seasons_kept[season_which_fall]
      bloom_compare_fall <- data.frame(
        cbind(
          concentration = c(headwaters_kept_fall, neardam_kept_fall), 
          seasons = c(seasons_kept_fall, seasons_kept_fall),
          location = rep(c("hw", "nd"), each = n_length_fall)))
      bloom_compare_fall$concentration <- as.numeric(bloom_compare_fall$concentration)
      bloom_compare_fall$location <- as.factor(bloom_compare_fall$location)
      #summary(bloom_compare)
      #wsr test for all
      hw_nd_test_fall <- wilcox.test(x=headwaters_kept_fall, y=neardam_kept_fall,
                                       alternative = "greater",
                                       mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
                                       conf.int = FALSE, conf.level = 0.95)
      reservoir_name <- as.character(ari50$Reservoir[i])
      print(hw_nd_test_fall)
      p_value_text_fall <- paste("p=", signif(hw_nd_test_fall$p.value,4))
      pvalue_fall[counter] <- signif(hw_nd_test_fall$p.value,4)
      #boxplot
      bxp_fall <- ggboxplot(bloom_compare_fall, x = "location", y = "concentration",  
                              color = "location", palette = c("#00AFBB", "#E7B800"),
                              order = c("hw", "nd"),
                              title= paste("fall; n=",n_length_fall),
                              ylab = "Conc", xlab = p_value_text_fall) + 
        theme(legend.position = "none")
      ####
      
      ####
      #build a dataframe for winter only
      n_length_winter <- length(headwaters_kept[season_which_winter])
      headwaters_kept_winter <- headwaters_kept[season_which_winter]
      neardam_kept_winter <- neardam_kept[season_which_winter]
      seasons_kept_winter <- seasons_kept[season_which_winter]
      bloom_compare_winter <- data.frame(
        cbind(
          concentration = c(headwaters_kept_winter, neardam_kept_winter), 
          seasons = c(seasons_kept_winter, seasons_kept_winter),
          location = rep(c("hw", "nd"), each = n_length_winter)))
      bloom_compare_winter$concentration <- as.numeric(bloom_compare_winter$concentration)
      bloom_compare_winter$location <- as.factor(bloom_compare_winter$location)
      #summary(bloom_compare)
      #wsr test for all
      hw_nd_test_winter <- wilcox.test(x=headwaters_kept_winter, y=neardam_kept_winter,
                                       alternative = "greater",
                                       mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
                                       conf.int = FALSE, conf.level = 0.95)
      reservoir_name <- as.character(ari50$Reservoir[i])
      print(hw_nd_test_winter)
      p_value_text_winter <- paste("p=", signif(hw_nd_test_winter$p.value,4))
      pvalue_winter[counter] <- signif(hw_nd_test_winter$p.value,4)
      #boxplot
      bxp_winter <- ggboxplot(bloom_compare_winter, x = "location", y = "concentration",  
                              color = "location", palette = c("#00AFBB", "#E7B800"),
                              order = c("hw", "nd"),
                              title= paste("wint; n =",n_length_winter),
                              ylab = "Conc", xlab = p_value_text_winter) + 
        theme(legend.position = "none")
      ####
      #acfs for the entire time series
      acf_neardam <- ggPacf(neardam_kept)
      acf_headwaters <- ggPacf(headwaters_kept)
      
      #using patchwork library
      #(acf_headwaters | acf_neardam)
      plots_combined <-  (bxp | (acf_headwaters / acf_neardam)) / (bxp_spring | bxp_summer | bxp_fall | bxp_winter)
      plot(plots_combined)
      
      # progressively build a large output file of the data used across all lakes
      if(counter==1){
        bloom_compare_all <- bloom_compare
      } else {
        bloom_compare_all <- rbind(bloom_compare_all, bloom_compare)
      }
    }
  }
dev.off()

location_pvalues <- cbind(location_sig, pvalue)
#View(location_pvalues)
sum(pvalue>0.05)
sum(pvalue_spring>0.05)
sum(pvalue_summer>0.05)
sum(pvalue_fall>0.05)
sum(pvalue_winter>0.05)

write.csv(location_pvalues, paste(ari_data_out, "/location_pvalues.csv", sep=""))

write.csv(bloom_compare_all, paste(ari_data_out, "/bloom_compare_all.csv", sep=""))
