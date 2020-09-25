library(forecast)
library(expsmooth)
library(ggplot2)
library(seasonal)
library(rugarch)
library(dlm)
library(readr)
library(dplyr)
library(zoo)
library(chron)
library(xts)
library(tibble)
library(tidyverse)
library(prophet)
library(TSstudio)
library(foreach)
library(doParallel)

loaded_packages = .packages()
load("uniqueabmProphage.Rdata")

my_forecast_plot = function(model, model_name, org, depth, ...){
  fit = model(window(train_data, end=c(2015,12)),h=h)
  if (is.null(fit$series) == FALSE) {
    fit$series=as.character(fit$series)
  }
  if (...  == "RMSE") {
    autoplot(window(train_data, start = c(2014,11),end=c(2016,4))) +
      autolayer(fit, PI=TRUE,alpha = 0.25,color = "red") + 
      autolayer(fit, PI=FALSE, series=model_name,size = 1) + 
      xlab("Year") + ylab("New orders index") + 
      xlab("month") + ylab("abundance") +
      ggtitle(paste(model_name, ": ", org, depth, "depth (RMSE)")) +
      guides(colour=guide_legend(title="Forecast"))
  } else if (... == "mae") {
    autoplot(window(train_data, start = c(2014,11),end=c(2016,4))) +
      autolayer(fit, PI=TRUE,alpha = 0.25,color = "red") + 
      autolayer(fit, PI=FALSE, series=model_name,size = 1) + 
      xlab("Year") + ylab("New orders index") + 
      xlab("month") + ylab("abundance") +
      ggtitle(paste(model_name, ": ", org, depth, "depth (mae)")) +
      guides(colour=guide_legend(title="Forecast"))
  }
}


print_scores1 = function(models_RMSE){
  for (model_name in unique(models_RMSE$model)){
    print(paste(model_name,":",mean(models_RMSE[models_RMSE$model==model_name,"RMSE"],na.rm = T)))
  }
}


plot_scores1 = function(models_RMSE,naive = TRUE){
  if(naive){
    ggplot(models_RMSE,aes(x=horizon,y=RMSE,factor=model,color = model))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = 1:12) 
  }else{
    ggplot(models_RMSE[(models_RMSE$model != "Naïve") & (models_RMSE$model != "Seasonal Naïve"),],aes(x=horizon,y=RMSE,factor=model,color = model))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = 1:12) 
  }
}


next_time_step = function(y, is_end = F){
  if(is_end){
    new_start = y
  }else{
    new_start =  end(y)
  }
  if(new_start[2]==12){
    new_start[1]=new_start[1]+1
    new_start[2]=1
  }else{
    new_start[2]=new_start[2]+1
  }
  return(new_start)
}


previous_time_step = function(y){
  new_start =  end(y)
  if(new_start[2]==1){
    new_start[1]=new_start[1]-1
    new_start[2]=12
  }else{
    new_start[2]=new_start[2]-1
  }
  return(new_start)
}

print_scores = function(models_mae){
  for (model_name in unique(models_mae$model)){
    print(paste(model_name,":",mean(models_mae[models_mae$model==model_name,"mae"])))
  }
}


plot_scores = function(models_mae,naive = TRUE){
  if(naive){
    ggplot(models_mae,aes(x=horizon,y=mae,factor=model,color = model))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = 1:12) 
  }else{
    ggplot(models_mae[(models_mae$model != "Naïve") & (models_mae$model != "Seasonal Naïve"),],aes(x=horizon,y=mae,factor=model,color = model))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = 1:12) 
  }
}
get_best_models = function(models_mae){
  best_models = NULL
  for(i in 1:13){
    tmp = models_mae[models_mae$horizon==i,]
    best_models  = rbind(best_models,tmp[tmp$mae==min(tmp$mae),])
  }
  rownames(best_models)=NULL
  return(best_models)
}

get_best_models1 = function(models_RMSE){
  best_models = NULL
  for(i in 1:13){
    tmp = models_RMSE[models_RMSE$horizon==i,]
    best_models  = rbind(best_models,tmp[tmp$RMSE==min(tmp$RMSE),])
  }
  rownames(best_models)=NULL
  return(best_models)
}


evaluate_test_residuals1 = function(fitted_model, model_function,data,...){
  errors = NULL
  for(i in 1:35){
    data[1:(length(train_data)-12+i)] %>%
      ts(frequency = 12,start = start(data)) %>%
      model_function(model = fitted_model) %>%   # use the fitted model to forecast test data
      forecast(h=h,...) -> pred
    
    true_values = c(window(data,start = start(pred$mean),end=end(pred$mean)))
    pred_mean = c(pred$mean)
    if(i < 12){
      pred_mean[1:(12-i)]=NA
    }
    if(length(true_values) < 12){
      true_values = c(true_values, rep(NA,12-length(true_values)))
    }
    errors = rbind(errors,true_values-pred_mean)
  }
  return(errors)
}


choose_model = function(model_name){
  if (model_name == "Naïve") {
    model = naive
  } else if (model_name == "Seasonal Naïve") {
    model = snaive
  } else if (model_name == "RW+Decomposition") {
    model = function(y, h){
      stl_decomposition = stl(y,s.window = "per")
      return(forecast(stl_decomposition,method = "naive",h=h))
    }
  } else if (model_name == "Exp Smoothing+Decomposition") {
    model = function(y, h){
      stl_decomposition = stl(y, s.window="periodic",robust=TRUE)
      return(forecast(stl_decomposition,method = "ets",h=h))
    }
  } else if (model_name == "ARIMA + Decomposition") {
    model = function(y, h){
      fit = stlm(y,s.window="periodic", robust=TRUE,modelfunction=Arima, order=c(yyy))
      return(forecast(fit,h=h))
    }
  } else if (model_name == "GARCH + Decomposition") {
    model = function(y, h){
      # decomposition
      stl_decomposition = stl(y, s.window="periodic",robust=TRUE)
      
      # garch on y_adjusted 
      y_adjusted = seasadj(stl_decomposition)
      diff_y_adjusted = diff(y_adjusted)
      ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                 mean.model = list(armaOrder = c(yyy[c(1,3)]))) %>%
        ugarchfit(diff_y_adjusted, solver = 'hybrid',numderiv.control = list(hess.zero.tol=1e-7)) %>%
        ugarchforecast(n.ahead = h) %>% 
        fitted %>%
        ts(frequency = h,start = next_time_step(y)) ->output_garch
      
      # add last observation:
      output_garch = output_garch + y_adjusted[length(y_adjusted)]
      
      # get last year seasonality 
      tmp = end(y)
      tmp[1] = tmp[1]-1
      new_start = next_time_step(tmp,is_end = T)
      last_year_seasonality = window(seasonal(stl_decomposition),start = new_start)
      
      # sum them up
      fcast = output_garch+ts(last_year_seasonality,frequency = h,start = start(output_garch))
      
      # make it a forecast object
      fcast = structure(list(mean = fcast), class='forecast')
      return(fcast)
    }
  } else if (model_name == "DLM + Decomposition") {
    model = function(y, h){
      # decomposition
      stl_decomposition = stl(y, s.window="periodic",robust=TRUE)
      
      # dlm on y_adjusted 
      y_adjusted = seasadj(stl_decomposition)
      
      # fit the model 
      fit <- dlmMLE(y_adjusted, parm = rep(0,7), build = buildFun)
      my_model = buildFun(fit$par)
      
      # Forecast
      my_Fore = dlmForecast(my_model,nAhead = h)
      output = ts(my_Fore$f[,1], frequency = h,start = next_time_step(y))
      print(output)
      
      # get last year seasonality 
      tmp = end(y)
      tmp[1] = tmp[1]-1
      new_start = next_time_step(tmp,is_end = T)
      last_year_seasonality = window(seasonal(stl_decomposition),start = new_start)
      
      # sum them up
      fcast = output+ts(last_year_seasonality,frequency = h,start = start(output))
      
      # make it a forecast object
      fcast = structure(list(mean = fcast), class='forecast')
      
      return(fcast)
    }
  } else if (model_name == "NNETAR + Decomposition") {
    model = function(y, h){
      fit = stlm(y,s.window="periodic", robust=TRUE,modelfunction=nnetar, p=2,P=0)
      return(forecast(fit,h=h))
    }
  } else if (model_name == "Exp Smoothing") {
    model = function(y, h){
      fitted_model = ets(y)
      return(forecast(fitted_model,h=h))
    }
  } else if (model_name == "SARIMA") {
    model = function(y, h){
      forecast(Arima(y, order=c(xxx$arma[1],xxx$arma[2],xxx$arma[3])), h=h)
    } #, seasonal = c(0,1,1))
  } else if (model_name == "SARIMA_ML") {
    model = function(y, h){
      forecast(Arima(y, order=c(xxx$arma[1],xxx$arma[2],xxx$arma[3]), method = "ML"), h=h)
    } #, seasonal = c(0,1,1)) 
  } else if (model_name == "TBATS") {
    model = function(y, h){
      return(forecast(tbats(y,use.box.cox = TRUE,use.damped.trend = F,seasonal.periods = c(12),start.p=2,max.p=2,start.q=1,max.q=1), h=h))
    }
    
  } else if (model_name == "NNETAR") {
    model = function(y, h){
      forecast(nnetar(y), h=h)
    }    
  } else if (model_name == "Prophet") {
    model = function(y, h){
      #df = ts_to_prophet(y)
      # fit the model
      m <- prophet(df, n.changepoints=13,yearly.seasonality = F,
                   weekly.seasonality = F,
                   daily.seasonality = T)
      # make predictions
      future <- make_future_dataframe(m, freq= "month", include_history = F, periods = h)
      m %>% predict(future) -> pred
      pred$yhat %>% ts(start = next_time_step(y),frequency = h) -> fcast
      # make it a forecast object
      fcast = structure(list(mean = fcast), class='forecast')
      
      # print(end(y))
      return(fcast)
    }
  return(model)
  }
}

# normalize abundance values
depths = unique(uniqueabmProphage$Depth)

#uaP = uniqueabmProphage
#uaP_norm = apply(uaP[,3:ncol(uaP)], 2, function(x) (unlist(x))/max(unlist(x)))
#uniqueabmProphage = cbind(uaP[,1:2],uaP_norm)
#uniqueabmProphage[is.na(uniqueabmProphage)] = 0 # nan to 0

c = 1
cl = makeCluster(20, outfile = "") # outfile = "" print in %dopar% möglich
clusterExport(cl, ls())
registerDoParallel(cl)

start = Sys.time()
wahrsagen2_pha = foreach (i = depths[6:length(depths)]) %do% {
  # print(c)
  DepthabmProphage <- uniqueabmProphage[uniqueabmProphage$Depth == i,]
  subProphage <- as.data.frame(DepthabmProphage)
  uaP = subProphage                                                                                                                                          
  uaP_norm = apply(uaP[,3:ncol(uaP)], 2, function(x) (unlist(x))/max(unlist(x)))                                                                                   
  subProphage = cbind(uaP[,1:2],uaP_norm)                                                                                                                    
  subProphage[is.na(subProphage)] = 0 # nan to 0                                                                                                       
              
  subProphage = subProphage[colSums(subProphage != 0) > 7]  # nur Zeilen mit mehr als sieben Werten über 0


  wahrsagen = foreach(j = 3:ncol(subProphage), .combine = "rbind", .packages = loaded_packages) %dopar% {
    print(paste(c,j))
    subsubProphage<-subProphage[,c(2,j)]
    subsubProphage$Date <- as.factor(subProphage$Date)
    mutate(subsubProphage, Date = as.Date(Date, format = "%y%m%d"))

    # make data to date and ts
    z <- as.data.frame(read.zoo(subsubProphage, format = "%y%m%d"))
    z$Date <- as.yearmon(rownames(z))

    full = as.yearmon(2014 + seq(10, 27)/12)
    zm = data.frame(Date=full, `read.zoo(subsubProphage, format = "%y%m%d")`=with(z, `read.zoo(subsubProphage, format = "%y%m%d")`[match(full, Date)]))
    zx = read.zoo(zm)
    # plot.ts(zx)

    Prophage<-ts(zx, start = c(2014,11), end = c(2016,4), frequency = 12)
    attr(Prophage,"index")=NULL

    train_data = Prophage
    train_data = na.interp(train_data)
    # assign("train_data",train_data,pos=.GlobalEnv)

    # autoplot(train_data) +
    #   ggtitle(paste0(colnames(subsubProphage)[2]," ",i," depth") )+
    #   xlab("month") + ylab("abundance")
    # ggsave(paste0(colnames(subsubProphage)[2]," ",i," depth.png"))


    # ggseasonplot(train_data, year.labels=TRUE, year.labels.left=TRUE) + ylab("abundance") +
    #   #scale_x_continuous(labels = c(1:12), breaks = 1:12)+
    #   ggtitle(paste0("Seasonal plot: ", colnames(subsubProphage)[2]," ",i," depth"))
    # ggsave(paste0("Seasonal plot: ", colnames(subsubProphage)[2]," ",i," depth.png"))
# 
#     ggseasonplot(train_data, polar=TRUE) + ylab("abundance") +
#       ggtitle(paste0("Polar plot: ", colnames(subsubProphage)[2]," ",i," depth"))
      #ggsave(paste0("Polar plot: ", colnames(subsubProphage)[2]," ",i," depth.png"))

    
    ## NAIVE (Benchmark)
    model_name = "Naïve"
    model = choose_model(model_name)
    h = 13
    org = colnames(subsubProphage)[2]
    depth = i

    #my_forecast_plot(model,model_name, org, depth)
    #ggsave(paste0("NAIVE (Benchmark) forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))


    models_RMSE = NULL
    models_mae = NULL
    initial_obs = 0

    # compute CV RMSE
    cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    RMSE = RMSE[!is.nan(RMSE)]
    # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    mae = colMeans(abs(cv_residuals), na.rm = T)
    mae = mae[!is.nan(mae)]
    # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))

    # store MAE
    models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))

    # store RMSE
    models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))




    ### Seasonal Naïve
    model_name = "Seasonal Naïve"
    model = choose_model(model_name)
    h = 13

    #my_forecast_plot(model,model_name, org, depth)
    #ggsave(paste0("Seasonal Naïve forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))


    # compute CV RMSE
    cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    RMSE = RMSE[!is.nan(RMSE)]
    # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    mae = colMeans(abs(cv_residuals), na.rm = T)
    mae = mae[!is.nan(mae)]
    # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))

    # store MAE
    models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))

    # store RMSE
    models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))

    #2) Saisonale Zersetzung (+ jedes Modell)

    #Überglätten vermeiden


    #png(paste0("ggAcf: ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # ggAcf(train_data)
    #dev.off()

    #png(paste0("pacf: ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # pacf(train_data)
    #dev.off()



    # # Decomposition
    # test_error = try({fit = stl(train_data, s.window = "per")})
    # if (class(test_error)!="try-error") {
    # 
    #   # ggsubseriesplot(train_data)  + ylab("abundance") +
    #   #   ggtitle(paste0("Seasonal subseries plot: ", colnames(subsubProphage)[2]," ",i," depth"))
    #   # 
    #   # #ggsave(paste0("Seasonal subseries plot: ", colnames(subsubProphage)[2]," ",i," depth.png"))
    #   # 
    #   # #png(paste0("fit: ", colnames(subsubProphage)[2]," ",i," depth.png"))
    #   # autoplot(fit)
    #   # #dev.off()
    #   # 
    #   # # Seasonally adjusted time serie
    #   # autoplot(train_data, series="Data") +
    #   #   autolayer(trendcycle(fit), series="Trend") +
    #   #   autolayer(seasadj(fit), series="Seasonally Adjusted") +
    #   #   xlab("month") + ylab("abundance") +
    #   #   ggtitle(paste0("Seasonally adjusted plot: ", colnames(subsubProphage)[2]," ",i," depth")) +
    #   #   scale_colour_manual(values=c("gray","blue","red"),breaks=c("Data","Seasonally Adjusted","Trend"))
    # 
    #   #ggsave(paste0("Seasonally adjusted plot: ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # 
    # 
    #   ### RW + DECOMPOSITION
    #   model_name = "RW+Decomposition"
    #   model = choose_model(model_name)
    # 
    #   # perform decomposition
    #   #my_forecast_plot(model,model_name, org, depth)
    #   #ggsave(paste0("RW+Decomposition forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # 
    # 
    #   # compute CV RMSE
    #   cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    #   RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    #   RMSE = RMSE[!is.nan(RMSE)]
    #   # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    #   mae = colMeans(abs(cv_residuals), na.rm = T)
    #   mae = mae[!is.nan(mae)]
    #   # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))
    # 
    #   # store MAE
    #   models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))
    # 
    #   # store RMSE
    #   models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))
    # 
    # 
    # 
    # 
    #   ####### Exponential smoothing + DECOMPOSITION
    #   model_name = "Exp Smoothing+Decomposition"
    #   model = choose_model(model_name)
    # 
    # 
    #   # perform decomposition
    #   #my_forecast_plot(model,model_name, org, depth)
    #   #ggsave(paste0("Exp Smoothing+Decomposition forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # 
    # 
    #   #png(paste0("Exp Smoothing+Decomposition forecast 24month: ", colnames(subsubProphage)[2]," ",i," depth.png"))
    #   autoplot(model(train_data,24))
    #   #dev.off()
    # 
    #   # compute CV RMSE
    #   cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    #   RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    #   RMSE = RMSE[!is.nan(RMSE)]
    #   # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    #   mae = colMeans(abs(cv_residuals), na.rm = T)
    #   mae = mae[!is.nan(mae)]
    #   # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))
    # 
    #   # store MAE
    #   models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))
    # 
    #   # store RMSE
    #   models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))
    # 
    # 
    # 
    # 
    #   ####### ARIMA + Decomposition
    #   stl_decomposition = stl(train_data, s.window="periodic",robust=TRUE)
    #   xxx = auto.arima(seasadj(stl_decomposition),stepwise = FALSE,approximation = FALSE)
    #   yyy =arimaorder(xxx)
    # 
    #   model_name = "ARIMA + Decomposition"
    #   model = choose_model(model_name)
    # 
    #   #my_forecast_plot(model,model_name, org, depth)
    #   #ggsave(paste0("ARIMA + Decomposition forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # 
    # 
    # 
    #   # compute CV RMSE
    #   cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    #   RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    #   RMSE = RMSE[!is.nan(RMSE)]
    #   # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    #   mae = colMeans(abs(cv_residuals), na.rm = T)
    #   mae = mae[!is.nan(mae)]
    #   # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))
    # 
    #   # store MAE
    #   models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))
    # 
    #   # store RMSE
    #   models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))
    # 
    # 
    # 
    #   #5) GARBE
    # 
    #   ####### GARCH
    #   h=13
    #   model_name = "GARCH + Decomposition"
    #   model = choose_model(model_name)
    # 
    #   #my_forecast_plot(model,model_name, org, depth)+
    #     # autolayer(ProphageFeb, series="Feb")+
    #     # autolayer(ProphageApr, series="AprMar")
    #   #ggsave(paste0("GARCH + Decomposition forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # 
    # 
    #   # compute CV RMSE
    #   cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    #   RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    #   RMSE = RMSE[!is.nan(RMSE)]
    #   # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    #   mae = colMeans(abs(cv_residuals), na.rm = T)
    #   mae = mae[!is.nan(mae)]
    #   # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))
    # 
    #   # store MAE
    #   models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))
    # 
    #   # store RMSE
    #   models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))
    # 
    # 
    # 
    # 
    #   #6) Dynamische lineare Modelle
    # 
    # 
    #   ######## Dynamic linear model + Decomposition
    #   # define the dlm model
    #   buildFun <- function(x) {
    #     m = dlmModPoly(2, dV = exp(x[1]), dW = exp(c(x[2],x[3])), m0 = c(x[5],x[6]), C0 = exp(x[7])*diag(nrow = 2) )
    #     return(m)
    #   }
    # 
    #   model_name = "DLM + Decomposition"
    #   model = choose_model(model_name)
    # 
    #   #my_forecast_plot(model,model_name, org, depth)
    #   #ggsave(paste0("DLM + Decomposition forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # 
    # 
    #   # compute CV RMSE
    #   cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    #   RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    #   RMSE = RMSE[!is.nan(RMSE)]
    #   # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    #   mae = colMeans(abs(cv_residuals), na.rm = T)
    #   mae = mae[!is.nan(mae)]
    #   # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))
    # 
    #   # store MAE
    #   models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))
    # 
    #   # store RMSE
    #   models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))
    # 
    # 
    # 
    #   ####### NNETAR + Decomposition
    #   model_name = "NNETAR + Decomposition"
    #   model = choose_model(model_name)
    # 
    #   #my_forecast_plot(model,model_name, org, depth)
    #   #ggsave(paste0("NNETAR + Decomposition forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))
    # 
    # 
    #   # compute CV RMSE
    #   cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    #   RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    #   RMSE = RMSE[!is.nan(RMSE)]
    #   # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    #   mae = colMeans(abs(cv_residuals), na.rm = T)
    #   mae = mae[!is.nan(mae)]
    #   # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))
    # 
    #   # store MAE
    #   models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))
    # 
    #   # store RMSE
    #   models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))
    # }


    #3) Exponentielle Glättung

    ####### Exponential smoothing
    model_name = "Exp Smoothing"
    model = choose_model(model_name)

    # perform decomposition
    #my_forecast_plot(model,model_name, org, depth)
    #ggsave(paste0("Exp Smoothing forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))


    # compute CV RMSE
    cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    RMSE = RMSE[!is.nan(RMSE)]
    # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    mae = colMeans(abs(cv_residuals), na.rm = T)
    mae = mae[!is.nan(mae)]
    # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))

    # store MAE
    models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))

    # store RMSE
    models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))







    #4) ARIMA, SARIMA


    ####### SARIMA
    xxx = auto.arima(train_data,stepwise = FALSE,approximation = FALSE, seasonal = F)
    model_name = "SARIMA_ML"
    # model = function(y, h){forecast(Arima(y, order=c(xxx$arma[1],xxx$arma[2],xxx$arma[3])), h=h)} #, seasonal = c(0,1,1))
    model = choose_model(model_name)

    #my_forecast_plot(model,model_name, org, depth)
    #ggsave(paste0("SARIMA forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))


    # compute CV RMSE
    cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    RMSE = RMSE[!is.nan(RMSE)]
    # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    mae = colMeans(abs(cv_residuals), na.rm = T)
    mae = mae[!is.nan(mae)]
    # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))

    
    
    # store MAE
    models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))

    # store RMSE
    models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))




    #7) TBATS
    ####### TBATS
    m = tbats(train_data,seasonal.periods = c(12))

    model_name = "TBATS"
    model = choose_model(model_name)


    #my_forecast_plot(model,model_name, org, depth)
    #ggsave(paste0("TBATS forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))


    # compute CV RMSE
    cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    RMSE = RMSE[!is.nan(RMSE)]
    # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    mae = colMeans(abs(cv_residuals), na.rm = T)
    mae = mae[!is.nan(mae)]
    # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))

    # store MAE
    models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))

    # store RMSE
    models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))


    #9) NNETAR


    ####### NNETAR
    model_name = "NNETAR"
    # model = function(y, h){forecast(nnetar(y), h=h)}
    model = choose_model(model_name)

    #my_forecast_plot(model,model_name, org, depth)
    #ggsave(paste0("NNETAR forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))


    # compute CV RMSE
    cv_residuals = tsCV(train_data,model,initial = 0,h=h)
    RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    RMSE = RMSE[!is.nan(RMSE)]
    # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    mae = colMeans(abs(cv_residuals), na.rm = T)
    mae = mae[!is.nan(mae)]
    # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))

    # store MAE
    models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))

    # store RMSE
    models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))




    ######## Prophet
    y = as.data.frame(train_data)
    ds = full
    df = data.frame(ds,y=y, stringsAsFactors = F)
    colnames(df)=c("ds","y")
    df = mutate(df, ds = as.Date(ds, format = "%Y-%m-%d"))


    model_name = "Prophet"
    model = choose_model(model_name)

    #my_forecast_plot(model,model_name, org, depth)
    #ggsave(paste0("Prophet forecast : ", colnames(subsubProphage)[2]," ",i," depth.png"))
    

    # compute CV RMSE
    cv_residuals = tsCV(train_data,model, initial = 0, h=h)
    RMSE = sqrt(colMeans((cv_residuals^2), na.rm = T))
    RMSE = RMSE[!is.nan(RMSE)]
    # print(paste("Average RMSE across all the horizons for model",model_name,":",mean(RMSE)))
    mae = colMeans(abs(cv_residuals), na.rm = T)
    mae = mae[!is.nan(mae)]
    # print(paste("Average MAE across all the horizons for model",model_name,":",mean(mae)))

    # store MAE
    models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:length(mae),row.names = NULL))

    # store RMSE
    models_RMSE = rbind(models_RMSE,data.frame(model = model_name, RMSE,horizon = 1:length(RMSE),row.names = NULL))




    # print_scores1(models_RMSE)
    # get_best_models1(models_RMSE)

    best_model_table = table(get_best_models1(models_RMSE)[1])
    best_forecast_RMSE = names(best_model_table[max(best_model_table) == best_model_table])
    if (length(best_forecast_RMSE) > 1) {
      mean_models = sapply(best_forecast_RMSE, function(k) mean(models_RMSE$RMSE[models_RMSE$model == k]))
      best_forecast_RMSE = names(mean_models[mean_models == min(mean_models)])
    }
    model = choose_model(best_forecast_RMSE)
    fit_RMSE = model(window(train_data, end=c(2015,12)),h=h)

    # my_forecast_plot(model,best_forecast_RMSE, org, depth, "RMSE")
    # ggsave(paste0("RMSE_plots/", best_forecast_RMSE, " forecast - ", colnames(subsubProphage)[2]," ",i," depth.png"))



    # plot_scores1(models_RMSE)


    # print_scores(models_mae)
    # get_best_models(models_mae)
    best_model_table = table(get_best_models(models_mae)[1])
    best_forecast_mae = names(best_model_table[max(best_model_table) == best_model_table])
    if (length(best_forecast_mae) > 1) {
      mean_models = sapply(best_forecast_mae, function(k) mean(models_mae$mae[models_mae$model == k]))
      best_forecast_mae = names(mean_models[mean_models == min(mean_models)])
    }
    model = choose_model(best_forecast_mae)
    fit_mae = model(window(train_data, end=c(2015,12)),h=h)

    my_forecast_plot(model,best_forecast_mae, org, depth, "mae")
    # ggsave(paste0("mae_plots/", best_forecast_mae, " forecast - ", colnames(subsubProphage)[2]," ",i," depth.png"))



    # plot_scores(models_mae)


    # c = c + 1
    df_results = data.frame(i,org,best_forecast_mae, fit_mae = "", best_forecast_RMSE, fit_RMSE = "", train_data = "")
    df_results$fit_mae = as.list(fit_mae$mean)
    df_results$fit_RMSE = as.list(fit_RMSE$mean)
    df_results$train_data = as.list(train_data)


    return(df_results)
  }
  c = c + 1
  
  save(wahrsagen, file = paste0("wahrsagen_phagen", i , ".RData"))
  return(wahrsagen)
}

end = Sys.time()

print(end-start)

save(wahrsagen2_pha, file = "wahrsagen_phagen.RData")
# # load("wahrsagen_phagen.RData")
stopCluster(cl)
