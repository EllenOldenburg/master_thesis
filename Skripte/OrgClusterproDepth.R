###############
## Libraries ##
###############
library(devtools)
library(segmenTier) # for clustering 
library(segmenTools) # for plots
library(foreach)

#########
## pwd ##
#########
setwd("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/")




##########
## data ##
##########
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster5.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster25.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster45.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster75.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster100.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster125.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster150.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster175.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster200.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster225.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster250.RData")
load("/Users/ellen/Documents/Uni/MA/ZeitserienCluster/zeitserienCluster_ohne_env/zeitserienCluster500.RData")

plotClusters(tset125norm, csetnorm125,k=17, type = "all", axes = F)



tsetnorm = tset5norm
depth = c("","25","45","75","100", "125", "150", "175", "200", "225", "250", "500")

names = foreach (i = depth, .combine = "c")%do%{
  x = get(paste0("csetnorm",i))
  rownames(x$clusters)
}

names1 = unique(names)

OrgClu = foreach (i = depth, .combine = "cbind")%do%{
  x = get(paste0("csetnorm",i))
  y = get(paste0("tset",i,"norm"))
  bestCluster = names(x$bic[which.max(x$bic)])
  z = x$clusters[,as.numeric(bestCluster)-1]
  OrgCluDepth = sapply(names1,function(x) z[match(x,names(z))])
}



###########
### pwd ###
###########
setwd("~/Documents/Uni/MA")


############
### Data ###
############
bacTax <- read_delim("bacTax.txt", ";", escape_double = FALSE, trim_ws = TRUE)


iBac = grep("[A-Z]{2}[0-9]{6}$", names1)
iProphagen = grep("[A-Z]{2}[0-9]{6}_", names1)

PhagenNamen = names1[-c(iBac,iProphagen)]
ProPhagenNamen = names1[iProphagen]

BacNamen = names1[iBac]
attr(BacNamen, "names")=NULL

phasplit = sapply(PhagenNamen, function(x) strsplit(x, "_")[[1]][1])
attr(phasplit, "names")=NULL

ProPhasplit = sapply(ProPhagenNamen, function(x) strsplit(x, "_")[[1]][1])
attr(ProPhasplit, "names")=NULL


ProPhaPhylum = unlist(sapply(ProPhasplit, function(x) bacTax$phylum[bacTax$accession==x]))
attr(ProPhaPhylum, "names")=NULL


BacPhylum = unlist(sapply(BacNamen, function(x) bacTax$phylum[bacTax$accession==x]))
attr(BacPhylum, "names")=NULL

PhaPhylum = unlist(sapply(phasplit, function(x) bacTax$phylum[match(x,bacTax$genus)]))
attr(PhaPhylum, "names")=NULL

OrgClu = data.frame(OrgClu)
OrgClu$phylum = NA
OrgClu$phylum[iBac] = BacPhylum
OrgClu$phylum[iProphagen] = ProPhaPhylum
OrgClu$phylum[-c(iBac,iProphagen)] = PhaPhylum
colnames(OrgClu) = c(depth,"phylum")

OrgClu = data.frame(Org = names1, OrgClu$phylum, OrgClu[,1:12])
rownames(OrgClu) = 1:nrow(OrgClu)
colnames(OrgClu) = c("Org","phylum",depth)


save(OrgClu, file = "/Users/ellen/Documents/Uni/MA/OrgClu.RData")










