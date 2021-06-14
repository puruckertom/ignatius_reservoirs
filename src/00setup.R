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

remotes::install_github('jorvlan/raincloudplots')

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
p50_list <- which(grepl("_p50",colnames(ari50)))
#not sure why this fails
length(p50_list)
colnames(ari_pixels[,p50_list])
p50_list2 <- seq(from=6, to=9112, by=5)
p50_list3 = p50_list2[-1]
length(p50_list3)
#use this instead
#View(colnames(ari_pixels[,p50_list3]))
ari50 <- ari_pixels[,c(1:2,p50_list3)]
ari50$Reservoir <- as.factor(ari50$Reservoir)
ari50$DamLocation <- as.factor(ari50$DamLocation)
dim(ari50)
colnames(ari50)
