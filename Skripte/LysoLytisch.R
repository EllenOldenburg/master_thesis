library(foreach)



cps = read.csv("/mnt/data_scratch/thomas/NetworkFiles/cps.txt", header=T, sep = "\t")
load("wahrsagen_phagen.RData")



cpsPhage = as.character(cps$phage)


lysoLyti = foreach(i = 1:12)%do%{
  phagen = wahrsagen2_pha[[i]][wahrsagen2_pha[[i]]$org %in% cpsPhage,]
  phagen$lysogen = cps$temperate[cps$phage %in% phagen$org]
  return(phagen)
}


save(lysoLyti, file = "LysoPhagen.RData")