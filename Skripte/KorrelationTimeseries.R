###libarys
library(stats)
library(tibble)
library(dplyr)
library(tidyr)
library(igraph)
library(foreach)
library(doParallel)

loaded_packages = .packages()

load("wahrsagen_phagen.RData")
load("wahrsagen_bac.RData")
load("wahrsagen_env.RData")

#Phagen nach tiefe
timeseries5 <- (rbind(wahrsagen2_pha[[1]], wahrsagen2_bac[[1]]))
timeseries25 <- (rbind(wahrsagen2_pha[[2]], wahrsagen2_bac[[2]]))
timeseries45 <- (rbind(wahrsagen2_pha[[3]], wahrsagen2_bac[[3]]))
timeseries75 <- (rbind(wahrsagen2_pha[[4]], wahrsagen2_bac[[4]]))
timeseries100 <- (rbind(wahrsagen2_pha[[5]], wahrsagen2_bac[[5]]))
timeseries125 <- (rbind(wahrsagen2_pha[[6]], wahrsagen2_bac[[6]]))
timeseries150 <- (rbind(wahrsagen2_pha[[7]], wahrsagen2_bac[[7]]))
timeseries175 <- (rbind(wahrsagen2_pha[[8]], wahrsagen2_bac[[8]]))
timeseries200 <- (rbind(wahrsagen2_pha[[9]], wahrsagen2_bac[[9]]))
timeseries225 <- (rbind(wahrsagen2_pha[[10]], wahrsagen2_bac[[10]]))
timeseries250 <- (rbind(wahrsagen2_pha[[11]], wahrsagen2_bac[[11]]))
timeseries500 <- (rbind(wahrsagen2_pha[[12]], wahrsagen2_bac[[12]]))

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


#leeren datensatz erstellen

timeseriesAll = c("timeseriesAll5", "timeseriesAll25","timeseriesAll45",
                  "timeseriesAll75", "timeseriesAll100", "timeseriesAll125",
                  "timeseriesAll150",  "timeseriesAll175", "timeseriesAll200",
                  "timeseriesAll225",   "timeseriesAll250", "timeseriesAll500" )

timeseries = c("timeseries5", "timeseries25", "timeseries45",
               "timeseries75", "timeseries100", "timeseries125",
               "timeseries150",  "timeseries175", "timeseries200",
               "timeseries225",   "timeseries250", "timeseries500") 


#cl = makeCluster(12, outfile = "") # outfile = "" print in %dopar% möglich
#clusterExport(cl, ls())
#registerDoParallel(cl)


dftimeseriesAll = foreach (i = timeseriesAll[1], .combine = rbind, .packages = loaded_packages) %do% {
  start = Sys.time()
  x = get(i)
  traindatcor <- data.frame(matrix(ncol = nrow(x), nrow = 18))
  forecastdatcor <- data.frame(matrix(ncol = nrow(x), nrow = 27))

  #spaltennamen erstellen
  x$colname <- paste(x$i,x$org)
  
  #spaltennamen leerem dataframe zuordnen
  y <- c(x$colname)
  colnames(traindatcor) <- y
  colnames(forecastdatcor) <- y
  
  print("cor")
  for (j in 1:nrow(x)){
    traindatcor[j] <-as.numeric(x$train_data[[j]])
  }
  
  # cor jeder mit jedem im timeseries + forecast dataframe
  for (k in 1:nrow(x)){
    timeData = as.numeric(x$train_data[[k]])
    foreca = x$fit_RMSE[[k]][5:length(x$fit_RMSE[[k]])]
    z = c(timeData,foreca)
    
    forecastdatcor[k] <- z
  }
  
  print("d3")
  #cor wert für jeden mit jedem
  d3 <- forecastdatcor %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  
  print("d3lag")
  #lag hinzufügen
  d3$lag = apply(d3, 1, function(x){
    r = ccf(forecastdatcor[,x[[1]]],forecastdatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    r$lag[which(abs(max(r$acf))==r$acf)]
  })
  
  print("d3maxcor")
  d3$maxcor = apply(d3, 1, function(x){
    r = ccf(forecastdatcor[,x[[1]]],forecastdatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    
    r$acf[which(abs(max(r$acf))==r$acf)]
    
  })
  
  print("d2")
  #cor wert für jeden mit jedem
  d2train <- traindatcor %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  
  #lag hinzufügen
  d2train$lag = apply(d2train, 1, function(x){
    r = ccf(traindatcor[,x[[1]]],traindatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    r$lag[which(abs(max(r$acf))==r$acf)]
  })
  
  
  d2train$maxcor = apply(d2train, 1, function(x){
    r = ccf(traindatcor[,x[[1]]],traindatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    
    r$acf[which(abs(max(r$acf))==r$acf)]
    
  })
  
  print("dist")
  d2train$dist = 1-d2train$maxcor
  d3$dist = 1-d3$maxcor
  
  M <- data.frame(d2train$var1, d2train$var2, d2train$dist, stringsAsFactors = F)
  G <- graph.data.frame(M,directed=FALSE)
  A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="d2train.dist",type='lower')
  
  MF <- data.frame(d3$var1, d3$var2, d3$dist, stringsAsFactors = F)
  GF <- graph.data.frame(MF,directed=FALSE)
  AF <- as_adjacency_matrix(GF,names=TRUE,sparse=FALSE,attr="d3.dist",type='lower')

  
  distMatrixfor <- as.dist(AF)
  distMatrixtrain <- as.dist(A)
  
  #png(file = paste0("Cor_all_plots/hclustRMSE/",i,".png"), width = 1600, height = 847)
  #plot(hclust(distMatrixfor))
  #dev.off()
  
  #png(file = paste0("Cor_all_plots/hclust/",i,".png"), width = 1600, height = 847)
  #plot(hclust(distMatrixtrain))
  #dev.off()
  
  
  df_results = data.frame(depth = x$i[1],
                          corMatrixTrain = "",
                          corMatrixForecast = "",
                          distMatrixTrain = "",
                          distMatrixForecast = "",
                          corLagTrain = "",
                          corLagForecast = "")
  df_results$corMatrixTrain = list(A)
  df_results$corMatrixForecast = list(AF)
  df_results$distMatrixTrain = list(distMatrixtrain)
  df_results$distMatrixForecast = list(distMatrixfor)
  df_results$corLagTrain = list(d2train)
  df_results$corLagForecast = list(d3)
  
  end = Sys.time()
  print(end-start)
  
  return(df_results)
}

'''
dftimeseries = foreach (i = timeseries, .combine = rbind, .packages = loaded_packages) %dopar% {
  x = get(i)
  traindatcor <- data.frame(matrix(ncol = nrow(x), nrow = 18))
  forecastdatcor <- data.frame(matrix(ncol = nrow(x), nrow = 27))
  
  #spaltennamen erstellen
  x$colname <- paste(x$i,x$org)
  
  #spaltennamen leerem dataframe zuordnen
  y <- c(x$colname)
  colnames(traindatcor) <- y
  colnames(forecastdatcor) <- y
  
  for (j in 1:nrow(x)){
    traindatcor[j] <-as.numeric(x$train_data[[j]])
  }
  
  # cor jeder mit jedem im timeseries + forecast dataframe
  for (k in 1:nrow(x)){
    timeData = as.numeric(x$train_data[[k]])
    foreca = x$fit_RMSE[[k]][5:length(x$fit_RMSE[[k]])]
    z = c(timeData,foreca)
    
    forecastdatcor[k] <- z
  }
  
  #cor wert für jeden mit jedem
  d3 <- forecastdatcor %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  
  
  #lag hinzufügen
  d3$lag = apply(d3, 1, function(x){
    r = ccf(forecastdatcor[,x[[1]]],forecastdatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    r$lag[which(abs(max(r$acf))==r$acf)]
  })
  
  d3$maxcor = apply(d3, 1, function(x){
    r = ccf(forecastdatcor[,x[[1]]],forecastdatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    
    r$acf[which(abs(max(r$acf))==r$acf)]
    
  })

  
  
  #cor wert für jeden mit jedem
  d2train <- traindatcor %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  
  #lag hinzufügen
  d2train$lag = apply(d2train, 1, function(x){
    r = ccf(traindatcor[,x[[1]]],traindatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    r$lag[which(abs(max(r$acf))==r$acf)]
  })
  
  
  d2train$maxcor = apply(d2train, 1, function(x){
    r = ccf(traindatcor[,x[[1]]],traindatcor[,x[[2]]], lag.max = 20, type= "correlation",plot = F)
    
    r$acf[which(abs(max(r$acf))==r$acf)]
    
  })
  
  d2train$dist = 1-d2train$maxcor
  d3$dist = 1-d3$maxcor
  
  M <- data.frame(d2train$var1, d2train$var2, d2train$dist, stringsAsFactors = F)
  G <- graph.data.frame(M,directed=FALSE)
  A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="d2train.dist",type='lower')
  
  MF <- data.frame(d3$var1, d3$var2, d3$dist, stringsAsFactors = F)
  GF <- graph.data.frame(MF,directed=FALSE)
  AF <- as_adjacency_matrix(GF,names=TRUE,sparse=FALSE,attr="d3.dist",type='lower')
  
  
  distMatrixfor <- as.dist(AF)
  distMatrixtrain <- as.dist(A)
  
  df_results = data.frame(depth = x$i[1],
                          corMatrixTrain = "",
                          corMatrixForecast = "",
                          distMatrixTrain = "",
                          distMatrixForecast = "",
                          corLagTrain = "",
                          corLagForecast = "")
  df_results$corMatrixTrain = list(A)
  df_results$corMatrixForecast = list(AF)
  df_results$distMatrixTrain = list(distMatrixtrain)
  df_results$distMatrixForecast = list(distMatrixfor)
  df_results$corLagTrain = list(d2train)
  df_results$corLagForecast = list(d3)
  
  end = Sys.time()
  print(end-start)
  
  return(df_results)
}

save(dftimeseries, dftimeseriesAll, file = "Korrelation_timeseries.RData")
'''
