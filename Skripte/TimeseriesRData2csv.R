###libarys
# library(stats)
# library(tibble)
# library(dplyr)
# library(tidyr)

load("wahrsagen_phagen.RData")
load("wahrsagen_bac.RData")
load("wahrsagen_env.RData")
#load("wahrsagen_phagen100_175.RData")

timeseriesAll5 <- (rbind(wahrsagen2_pha[[1]], wahrsagen2_bac[[1]], wahrsagen2_env[[1]]))
timeseriesAll25 <- (rbind(wahrsagen2_pha[[2]], wahrsagen2_bac[[2]], wahrsagen2_env[[2]]))
timeseriesAll45 <- (rbind(wahrsagen2_pha[[3]], wahrsagen2_bac[[3]], wahrsagen2_env[[3]]))
timeseriesAll75 <- (rbind(wahrsagen2_pha[[4]], wahrsagen2_bac[[4]], wahrsagen2_env[[4]]))
timeseriesAll100 <- (rbind(wahrsagen2_pha[[5]], wahrsagen2_bac[[5]], wahrsagen2_env[[5]]))
timeseriesAll125 <- (rbind(wahrsagen2_pha[[6]], wahrsagen2_bac[[6]], wahrsagen2_env[[6]]))
timeseriesAll150 <- (rbind(wahrsagen2_pha[[7]], wahrsagen2_bac[[7]], wahrsagen2_env[[7]]))
timeseriesAll175 <- (rbind(wahrsagen2_pha[[8]], wahrsagen2_bac[[8]], wahrsagen2_env[[8]]))
timeseriesAll200 <- (rbind(wahrsagen2_pha[[9]], wahrsagen2_bac[[9]], wahrsagen2_env[[9]]))
timeseriesAll225 <- (rbind(wahrsagen2_pha[[10]], wahrsagen2_bac[[10]], wahrsagen2_env[[10]]))
timeseriesAll250 <- (rbind(wahrsagen2_pha[[11]], wahrsagen2_bac[[11]], wahrsagen2_env[[11]]))
timeseriesAll500 <- (rbind(wahrsagen2_pha[[12]], wahrsagen2_bac[[12]], wahrsagen2_env[[12]]))

############## test daten
# load("../wahrsagen_phagen_test.RData")
# load("../wahrsagen_bac_test.RData")
# load("wahrsagen_env.RData")
# 
# timeseriesAll5 <- (rbind(wahrsagen2[[1]], wahrsagen2_bac[[1]], wahrsagen2_env[[1]]))
# timeseriesAll25 <- (rbind(wahrsagen2[[2]], wahrsagen2_bac[[2]], wahrsagen2_env[[2]]))
# timeseriesAll45 <- (rbind(wahrsagen2[[3]], wahrsagen2_bac[[3]], wahrsagen2_env[[3]]))
# timeseriesAll75 <- (rbind(wahrsagen2[[4]], wahrsagen2_bac[[4]], wahrsagen2_env[[4]]))
# timeseriesAll100 <- (rbind(wahrsagen2[[5]], wahrsagen2_bac[[5]], wahrsagen2_env[[5]]))
# timeseriesAll125 <- (rbind(wahrsagen2[[6]], wahrsagen2_bac[[6]], wahrsagen2_env[[6]]))
# timeseriesAll150 <- (rbind(wahrsagen2[[7]], wahrsagen2_bac[[7]], wahrsagen2_env[[7]]))
# timeseriesAll175 <- (rbind(wahrsagen2[[8]], wahrsagen2_bac[[8]], wahrsagen2_env[[8]]))
# timeseriesAll200 <- (rbind(wahrsagen2[[9]], wahrsagen2_bac[[9]], wahrsagen2_env[[9]]))
# timeseriesAll225 <- (rbind(wahrsagen2[[10]], wahrsagen2_bac[[10]], wahrsagen2_env[[10]]))
# timeseriesAll250 <- (rbind(wahrsagen2[[11]], wahrsagen2_bac[[11]], wahrsagen2_env[[11]]))
# timeseriesAll500 <- (rbind(wahrsagen2[[12]], wahrsagen2_bac[[12]], wahrsagen2_env[[12]]))
#############

timeseriesAll = c("timeseriesAll5", "timeseriesAll25","timeseriesAll45",
                  "timeseriesAll75", "timeseriesAll100", "timeseriesAll125",
                  "timeseriesAll150", "timeseriesAll175", "timeseriesAll200",
                  "timeseriesAll225", "timeseriesAll250", "timeseriesAll500")

for (i in timeseriesAll) {
  df = get(i)
  df$fit_mae = sapply(df$fit_mae, function(x) paste(unlist(x), collapse='/'))
  df$fit_RMSE = sapply(df$fit_RMSE, function(x) paste(unlist(x), collapse='/'))
  df$train_data = sapply(df$train_data, function(x) paste(unlist(x), collapse='/'))
  df$best_forecast_mae = gsub("ï","i",df$best_forecast_mae)  # das ï muss weg!!!!!!!!!
  df$best_forecast_RMSE = gsub("ï","i",df$best_forecast_RMSE)  # Hier auch!!!!
  
  write.table(df, file = paste0("../python/csvFiles/",i,".csv"), col.names = F, row.names = F, sep = "\t")
}
