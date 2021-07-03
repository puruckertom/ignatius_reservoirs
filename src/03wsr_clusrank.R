#View(bloom_compare_all)

#https://stackoverflow.com/questions/57136655/using-clusrank-by-group

bloom_compare_all <- read.csv(paste(ari_data_out, "/bloom_compare_all.csv", sep=""))

dim(bloom_compare_all)
unique(bloom_compare_all$lake)

# clusrank package example
data(amd)
summary(amd)
View(amd)
#[1] 69482     4
clusWilcox.test(CARMS ~ Variant + cluster(ID), data = amd,
                subset = CARMS %in% c(1, 2, 3, 4), method = "rgl", alternative = "two")
clusWilcox.test(CARMS ~ Variant + cluster(ID), data = amd,
                subset = CARMS %in% c(1, 2, 3, 4), method = "ds", alternative = "two")
clusWilcox.test(CARMS ~ Variant + cluster(ID) + stratum(AgeSex), data = amd,
                subset = CARMS %in% c(1, 2, 3, 4), alternative = "two")


summary(bloom_compare_all)
#concentration       seasons                location         lake          
#Min.   :   6486   Length:69482       headwaters:34741   Length:69482      
#1st Qu.:  13304   Class :character   neardam   :34741   Class :character  
#Median :  47424   Mode  :character                      Mode  :character  
#Mean   :  77345                                                           
#3rd Qu.: 108643                                                           
#Max.   :2147831 

#clusrank does not like factors or text
bloom_compare_all$lake <- as.numeric(as.factor(bloom_compare_all$lake))
bloom_compare_all$location <- as.numeric(as.factor(bloom_compare_all$location))

clusWilcox.test(concentration ~ location + cluster(lake), data = bloom_compare_all,
                method = "rgl", alternative = "greater")
