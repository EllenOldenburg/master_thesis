#install.packages("latticeExtra")

###############
## Libraries ##
###############
library(foreach)
library(readr) # read_delim
library(latticeExtra) #plot
library(doParallel)
library(data.table)

#########
## pwd ##
#########
#setwd("/Users/ellen/iCloud Drive (Archiv)/Documents/Uni/MA/")


load("BestClusterSizes.RData")
depth = c(5,25,45,75,100,125,150,175,200,225,250,500)


loaded_packages = .packages()
clu = makeCluster(12)
clusterExport(clu, ls())
registerDoParallel(clu)
##########
## data ##
##########

foreach (i = 1:12, .packages = loaded_packages)%dopar%{
  # Korrelation175all <- read_delim(paste0("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/best_forecast/python/Korrelation",depth[i],".csv"), "\t", escape_double = FALSE, trim_ws = TRUE)
  # print(head(Korrelation175all))
  Korrelation175all <- fread(paste0("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/best_forecast/python/Korrelation",depth[i],".csv"), sep = "\t", nThread = 4)

  
  CorClust_5 = foreach (j = 1:max(bestClusterForEachDepth_wo_ENV[[i]])) %do% {
    cluNames = paste0(depth[i],"_",names(bestClusterForEachDepth_wo_ENV[[i]][bestClusterForEachDepth_wo_ENV[[i]] == j]))
    CorClust = Korrelation175all[Korrelation175all$Org1 %in% cluNames & Korrelation175all$Org2 %in% cluNames, 1:5] 
  }
  print(str(CorClust_5))
  save(CorClust_5, file = paste0("CorClusts/CorClust_",depth[i],".RData"))
  
  test = foreach(k = 1:length(CorClust_5), .combine = cbind) %do% {
    lag_total = table(CorClust_5[[k]]$Lag_train)
    lag_totalee = rep(0,35)
    names(lag_totalee) = -17:17
    lag_totalee[names(lag_totalee) %in% names(lag_total)] = lag_total
    return(lag_totalee)
  }
  
  
  test = log(test)                           #log 
  test[is.infinite(test)]=0                  #inf to null
  test = test[,order(colSums(test))]         #order from smal to big
  colnames(test)= c(length(CorClust_5):1)  #cluster as colnames
  
  
  # A function generating colors
  cols<-function(n) {
    colorRampPalette(c("#3399CC", "#000066"))(40)                                 # 20 distinct colors
  }
  
  png(filename = paste0("Lags/lags",depth[i],".png"), width = 1920 , height = 1080 )
  
  cloud(test, panel.3d.cloud = panel.3dbars,                      # white borders for bars
        xbase = 1, ybase = 1, zlim = c(0, max(test)+1),                         # No space around the bars
        scales = list(arrows = FALSE, just = "right"), xlab = NULL, ylab = NULL, 
        col.facet = level.colors(test, at = do.breaks(range(test), 20),        
                                 col.regions = cols,                                   # color ramp for filling the bars
                                 colors = TRUE),
        colorkey = list(col = cols, at = do.breaks(range(test), 20)),
        screen = list(z = 25, x = -50))     
  
  
  dev.off()
  
  
}

stopCluster(clu)
