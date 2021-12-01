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
cbind(sample_season, sample_date)

#create long vectors for comparison, drop observation when both are NAs
headwaters <- vector()
neardam <- vector()
season <- vector()
for(i in 1:nrow(ari50)){
  if(is.odd(i)){ # Headwaters, NearDam repeating
    #grab the data for this reservoir
    reservoir_temp <- as.matrix(ari50[i:(i+1),3:ndays+2])
    colnames(reservoir_temp) <- NULL
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
    #normalize by max bloom pixel size
    max_normalize <- max(cbind(headwaters_kept, neardam_kept), na.rm=T)
    headwaters <- append(headwaters, headwaters_kept) #/max_normalize
    neardam <- append(neardam, neardam_kept) #/max_normalize
    season <- append(season, seasons_kept)
  }
}
cbind(headwaters, neardam)
summary(headwaters)
summary(neardam)
hist(headwaters/neardam, breaks=100)
summary(headwaters/neardam)
hist(season)
unique(season)

spring_which <- which(season==2)
length(spring_which)
summer_which <- which(season==3)
length(summer_which)
fall_which <- which(season==4)
length(fall_which)
winter_which <- which(season==1)
length(winter_which)

raincloud_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# spring
n_hw_spring <- sum(!is.na(headwaters[spring_which]))
n_nd_spring <- sum(!is.na(neardam[spring_which]))
#
spring_df_1x1 <- data_1x1(
  array_1 = headwaters[spring_which],
  array_2 = neardam[spring_which],
  jit_distance = 0.09, # 0.2 is max allowed
  jit_seed = 321)
spring_raincloud_1_h <- raincloud_1x1(
  data = spring_df_1x1, 
  colors = (c('goldenrod1','lightskyblue2')), 
  fills = (c('darkorange', 'dodgerblue')), 
  size = 0.3, 
  alpha = 0.8, 
  ort = 'h') +
  scale_x_continuous(breaks=c(1.3,2.3), 
                     labels=c(paste("Headwaters\n n=",n_hw_spring), paste("Near Dam\n n=",n_nd_spring)), 
                     limits=c(0.9, 3)) +
  scale_y_continuous(labels=c("","","","",""),
                     limits=c(5000,400000)) +
  xlab("") + 
  ylab("") +
  annotate(geom='text', x=3, y=60000, label="Spring", fontface="bold") +
  theme_classic()
  #raincloud_theme +
  
  #raincloud_theme
spring_raincloud_1_h

# summer
n_hw_summer <- sum(!is.na(headwaters[summer_which]))
n_nd_summer <- sum(!is.na(neardam[summer_which]))
#
summer_df_1x1 <- data_1x1(
  array_1 = headwaters[summer_which],
  array_2 = neardam[summer_which],
  jit_distance = .09,
  jit_seed = 321)
#View(summer_df_1x1)
summer_raincloud_1_h <- raincloud_1x1(
  data = summer_df_1x1, 
  colors = (c('goldenrod1','lightskyblue2')), 
  fills = (c('darkorange', 'dodgerblue')), 
  size = 0.3, 
  alpha = 0.8,  
  ort = 'h') +
  scale_x_continuous(breaks=c(1.3,2.3), 
                     labels=c(paste("Headwaters\n n=",n_hw_summer), paste("Near Dam\n n=",n_nd_summer)), 
                     limits=c(0.9, 3)) +
  scale_y_continuous(labels=c("","","","",""),
                     limits=c(5000,400000)) +
  xlab("") + 
  ylab("") +
  annotate(geom='text', x=3, y=60000, label="Summer", fontface="bold") +
  theme_classic()
  #raincloud_theme +
  
  #raincloud_theme
summer_raincloud_1_h

# fall
n_hw_fall <- sum(!is.na(headwaters[fall_which]))
n_nd_fall <- sum(!is.na(neardam[fall_which]))
#
fall_df_1x1 <- data_1x1(
  array_1 = headwaters[fall_which],
  array_2 = neardam[fall_which],
  jit_distance = .09,
  jit_seed = 321)
fall_raincloud_1_h <- raincloud_1x1(
  data = fall_df_1x1, 
  colors = (c('goldenrod1','lightskyblue2')), 
  fills = (c('darkorange', 'dodgerblue')), 
  size = 0.3, 
  alpha = 0.8, 
  ort = 'h') +
  scale_x_continuous(breaks=c(1.3,2.3), 
                     labels=c(paste("Headwaters\n n=",n_hw_fall), paste("Near Dam\n n=",n_nd_fall)),
                     limits=c(0.9, 3)) +
  scale_y_continuous(labels=c("","","","",""),
                     limits=c(5000,400000)) +
  xlab("") + 
  ylab("") +
  annotate(geom='text', x=3, y=60000, label="Fall", fontface="bold") +
  theme_classic()
  
fall_raincloud_1_h

# winter
n_hw_winter <- sum(!is.na(headwaters[winter_which]))
n_nd_winter <- sum(!is.na(neardam[winter_which]))
#thin by a factor of 4
hw_winter_display <- headwaters[winter_which]
nd_winter_display <- neardam[winter_which]
winter_df_1x1 <- data_1x1(
  array_1 = headwaters[winter_which],
  array_2 = neardam[winter_which],
  jit_distance = .09,
  jit_seed = 321)
winter_raincloud_1_h <- raincloud_1x1(
  data = winter_df_1x1, 
  colors = (c('goldenrod1','lightskyblue2')), 
  fills = (c('darkorange', 'dodgerblue')), 
  size = 0.3, 
  alpha = 0.8,  
  ort = 'h') +
  scale_x_continuous(breaks=c(1.3,2.3), 
                     labels=c(paste("Headwaters\n n=",n_hw_winter), paste("Near Dam\n n=",n_nd_winter)),
                     limits=c(0.9, 3)) +
  scale_y_continuous(limits=c(5000,400000)) +
  xlab("") +
  ylab("Cyanobacteria concentration (cells/ml)") +
  annotate(geom='text', x=3, y=60000, label="Winter", fontface="bold") +
  theme_classic()
  
winter_raincloud_1_h

ignatius_winter_jpg <- paste(ari_graphics,"/ari_winter.jpg",sep="")
jpeg(ignatius_winter_jpg, width = 8, height = 4, units = "in",res=600)
par(mfrow=c(1,1))
  winter_raincloud_1_h
dev.off()

###Multiplot choices
#ggpubr
ggarrange(spring_raincloud_1_h, summer_raincloud_1_h, fall_raincloud_1_h, winter_raincloud_1_h,
          ncol = 1, nrow = 4)

#cowplot
plot_grid(spring_raincloud_1_h, summer_raincloud_1_h, fall_raincloud_1_h, winter_raincloud_1_h,
          ncol = 1, nrow = 4)


#gridExtra
grid.arrange(spring_raincloud_1_h, summer_raincloud_1_h, fall_raincloud_1_h, winter_raincloud_1_h,
             ncol = 1, nrow = 4)


# combined figure 
ignatius_all_season_jpg <- paste(ari_graphics,"/ari_all_seasons_p50.jpg",sep="")
jpeg(ignatius_all_season_jpg, width = 5, height = 8, units = "in",res=600)
par(mfrow=c(1,1))
  ggarrange(spring_raincloud_1_h, summer_raincloud_1_h, fall_raincloud_1_h, winter_raincloud_1_h,
            ncol = 1, nrow = 4)
dev.off()

ignatius_all_season_tif <- paste(ari_graphics,"/ari_all_seasons_p50.tif",sep="")
tiff(ignatius_all_season_tif, width = 5, height = 8, units = "in",res=600)
par(mfrow=c(1,1))
ggarrange(spring_raincloud_1_h, summer_raincloud_1_h, fall_raincloud_1_h, winter_raincloud_1_h,
          ncol = 1, nrow = 4)
dev.off()
