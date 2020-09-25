# -*- coding: utf-8 -*-
"""
Spyder Editor

Korrelationsskript von Elmas.
"""

import numpy as np
from itertools import combinations 
import scipy.signal as ss
from multiprocessing import Pool
import time

def ccf1(values, lag_max = 20):
    nam_x = values[0]
    nam_y = values[1]

    ####### train

    x = org2trainall[nam_x]
    y = org2trainall[nam_y]

    result = ss.correlate(y - np.mean(y), x - np.mean(x), method='direct') / (np.std(y) * np.std(x) * len(y))
    length = (len(result) - 1) // 2
    lag = np.argmax(abs(result)) - length
    cor = np.max(abs(result))
    dist = (cor+1)/2
    out_train = nam_x + "\t" + nam_y + "\t" + str(lag) + "\t" + str(cor) + "\t" + str(dist) + "\t"

    ####### forecast

    x = org2forecastall[nam_x]
    y = org2forecastall[nam_y]

    result = ss.correlate(y - np.mean(y), x - np.mean(x), method='direct') / (np.std(y) * np.std(x) * len(y))
    length = (len(result) - 1) // 2
    lo = length - lag_max
    hi = length + (lag_max + 1)
    result = result[lo:hi]
    length = (len(result) - 1) // 2
    lag = np.argmax(abs(result)) - length
    cor = result[np.argmax(abs(result))]
    dist = (cor+1)/2
    out = out_train + str(lag) + "\t" + str(cor) + "\t" + str(dist) + "\n"

    return out


# def get_next_line():
#     with open("combs_all.csv", 'r') as f:
#         for line in f:
#             yield line


#data_list_all = ["timeseriesAll5", "timeseriesAll25","timeseriesAll45","timeseriesAll75"]
data_list_all = ["timeseriesAll100", "timeseriesAll125","timeseriesAll150", "timeseriesAll175"]
#data_list_all = ["timeseriesAll200", "timeseriesAll225", "timeseriesAll250", "timeseriesAll500"]


for data in data_list_all:
    data_out = "Korrelation" + data[13:] + ".csv"

    start = time.time()
    try:
        open(data_out)
        print(f"Die Datei" ,data_out, "ist bereits vorhanden")

    except FileNotFoundError:
        with open(data_out,"w") as aus1:
            print(data)
            aus1.write("Org1\tOrg2\tLag_train\tCor_train\tDist_train\tLag_forecast\tCor_forecast\tDist_forecast\n")
               
            org2trainall = {}
            org2forecastall = {}
            namsall = []
            with open("csvFiles/" + data + ".csv") as ein1:
                for i in ein1:
                    depth, org, model_mae, fit_mae, model_RMSE, fit_RMSE, train_data = i.rstrip().replace("\"","").split("\t")
                    fit_mae = fit_mae.split("/")
                    fit_RMSE = fit_RMSE.split("/")
                    train_data = train_data.split("/")
                        
                    fit_mae = [float(j) for j in fit_mae]
                    fit_RMSE = [float(j) for j in fit_RMSE] 
                    train_data = [float(j) for j in train_data]
                        
                    nam = depth + "_" + org
                    org2trainall[nam] = train_data
                    org2forecastall[nam] = train_data + fit_RMSE[4:]
                    namsall.append(nam)
        
            if __name__ == '__main__':
                pool = Pool(60) # Create a multiprocessing Pooli               
    
                for result in pool.imap(ccf1, combinations(namsall, 2)):
                    aus1.write(result)
    
                pool.close()
                pool.join()    
                
    end = time.time()
    print(end-start)
