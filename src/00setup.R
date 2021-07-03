#Install and load supporting libraries.
print(Sys.info()[4])

library(cowplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(remotes)
library(rstatix)
library(schoolmath)
library(chron)
library(tidyverse)
library(patchwork)
library(forecast)
library(clusrank)
library(move)
#remotes::install_github('jorvlan/raincloudplots')

library(raincloudplots)

print("list of loaded packages: ")
print((.packages()))

#tom epa windows
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  ari_root <- file.path("c:","git","ignatius_reservoirs")
}

print(paste("Root directory location: ", ari_root, sep=""))

ari_data_in <- file.path(ari_root, "data_in")
ari_data_out <- file.path(ari_root, "data_out")
ari_graphics <- file.path(ari_root, "graphics")

#check to see if directories are accessible
boo = file.exists(file.path(ari_data_in,"/DailyPercentiles_countAnyIntersectingPixel.csv"))
print(paste("check to see if R can access GSF file OK: ", boo))

ari_pixels <- read.csv(file.path(ari_data_in,"/DailyPercentiles_countAnyIntersectingPixel.csv"), stringsAsFactors = TRUE)
dim(ari_pixels)
summary(ari_pixels)
colnames(ari_pixels)
#View(ari_pixels)

#pull out reservoir and location as factor
ari_factors <- ari_pixels[,c(1:2)]
dim(ari_factors)
ari_factors$Reservoir <- as.factor(ari_factors$Reservoir)
ari_factors$DamLocation <- as.factor(ari_factors$DamLocation)

# create 50th percentiles
p50_list <- which(grepl("_p50",colnames(ari_pixels)))
length(p50_list)
head(colnames(ari_pixels[,p50_list]))
p50_values <- ari_pixels %>% select(all_of(p50_list))
dim(p50_values)
#View(p50_values)
p50_concs <- 10^(3.0 / 250.0 * p50_values - 4.2) * 1.0e+8
#View(p50_concs)

ari50 <- cbind(ari_factors, p50_concs)
dim(ari50)
head(colnames(ari50))

#non-detects for converting into proxy value, can screw up acfs so wait
#these will be used inside loop later
ari_legit_nds <- read.csv(file.path(ari_data_in,"/legit_nds_by_lake.csv"), stringsAsFactors = TRUE)
dim(ari_legit_nds)
summary(ari_legit_nds)
colnames(ari_legit_nds)

#import cloud cover NAs from Amber and reassign as NAs to drop later for testing
#these will be used inside loop later
ari_legit_NAs <- read.csv(file.path(ari_data_in,"/legit_NAs_by_lake.csv"), stringsAsFactors = TRUE)
dim(ari_legit_NAs)
summary(ari_legit_NAs)
colnames(ari_legit_NAs)

# create 75th percentiles
p75_list <- which(grepl("_p75",colnames(ari_pixels)))
length(p75_list)
head(colnames(ari_pixels[,p75_list]))
p75_values <- ari_pixels %>% select(all_of(p75_list))
dim(p75_values)
max(p75_values, na.rm=T)
#View(p75_values)
p75_concs <- 10^(3.0 / 250.0 * p75_values - 4.2) * 1.0e+8
#View(p75_concs)
max(p75_concs, na.rm=T)
ari75 <- cbind(ari_factors, p75_concs)
dim(ari75)
head(colnames(ari75))
