###############
## Libraries ##
###############
library(plyr)
library(data.table)
library(readr)
library(ggplot2)
library(parallel)



###########################
## Protein to organismus ##
###########################

#read files
data = fread("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/pOTU_readCount/SRR9178536.txt")
# remove last row from file (number of unmapped reads)
data=data[1:(nrow(data)-1),]
#extract org name 


########
#cluster_info = fread("/mnt/data_scratch/thomas/NetworkFiles/cps.txt")
prot2pp = fread("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/protein2prophage.tsv", header = FALSE, sep = "\t")
show(dim(prot2pp))
prot2pp = unique(prot2pp)

#prot2pp$V1=strsplit(prot2pp$V1,'\\|')  # split first column in accesion id and locus tag (protein 2 prophage data)
#prot2pp$V3=sapply(prot2pp$V1,function(x)x[[2]])  # add new column with locus tag (protein 2 prophage data)
#prot2pp$V1=sapply(prot2pp$V1,function(x)x[[1]])  # change the first column to accesion id only (protein 2 prophage data)
prot2pp$V3=gsub("\\.fa[a$&]",'',prot2pp$V3)  # remove file ending (.faa,.fa$,.fa&) 

# match mcl cluster to prophage annotation
mi = match(data$V1,prot2pp$V2)  # index prophages in prot2pp 
org_names = prot2pp$V3[mi]  # add prophages names, phages names as NA
i1 = unlist(which(is.na(org_names)))  # get index of phages 
data$V5 = org_names  # add prophages names, phages names are NA
data$V5[i1] = gsub("&.*","",data$V1[i1])  # change phagenames (NAs) to it's name


# set the steps for the data
steps = seq(178068,178536,1)

BAI=data$V5
#unique bac accession ids
ubai=unique(BAI)

abm_phage=matrix(NaN,nrow = length(steps),ncol=length(ubai))
rownames(abm_phage)=paste0("SRR9", steps)
colnames(abm_phage)=ubai

show(length(ubai))

for (i in steps) {
show(i)
  data = fread(paste0("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/pOTU_readCount/SRR9", i,".txt"))
  sm=sum(data$V3+data$V4)
  absz = data$V3/sm
  absz = absz[1:length(absz)-1]
  data = data[1:(nrow(data)-1),]
  data = cbind(data,BAI)
  
  # 02
  # berechnung rel Abund für alle organismen
  rb_non_pOTU = aggregate(absz,list(data$BAI),mean)
  
  # check the average number of reads per organism
  tmp=rb_non_pOTU$x*sm
  # keep only the species which have on average 1 read per whole genome
  rb_non_pOTU$x[which(tmp<1)]=0
  
  # speichern in Matrix
  abm_phage[paste0("SRR9", i),rb_non_pOTU$Group.1]=rb_non_pOTU$x
}

save(abm_phage, file = "/mnt/data_scratch/ovidiu/Ellen/MasterThesis/phageInfo/abund_phage.RData")



