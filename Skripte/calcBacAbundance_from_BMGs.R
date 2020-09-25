### calculate read abundance for prokaryotes from marker genes

library(data.table)

#srrinfo 
#srrinfo=fread("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/phageInfo/Tabelle_date_death.txt")

files=dir("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/BMG_readCount/")

## read first file and define the size of the abundance matrix
## size is number of srr X number of unique bac accession IDs

path=paste0("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/BMG_readCount/",files[1])

#read files
ff=fread(path,fill=T,sep="\t")
#remove last row from file (number of unmapped reads)
ff=ff[1:(nrow(ff)-1),]
#extract bac accession ID
BAI=unlist(lapply(strsplit(ff$V1,'\\|'),function(x)x[4]))
#unique bac accession ids
ubai=unique(BAI)

# define abundance matrix
abm=matrix(NaN,nrow = length(files),ncol=length(ubai))
rownames(abm)=files
colnames(abm)=ubai
for(i in 1:length(files)){
    # read all files and calc relative abundance
     path=paste0("/mnt/data_scratch/ovidiu/Ellen/MasterThesis/BMG_readCount/",files[i])
    #read files
    ff=fread(path,fill=T,sep="\t")
    #abundance relative to sample size
    absz=ff$V3/sum(ff$V3+ff$V4)
    BAI=unlist(lapply(strsplit(ff$V1,'\\|'),function(x)x[4]))
    #calc mean abundance per genome from all marker genes
    rb=aggregate(absz,list(BAI),mean)
    
    # check the average number of reads per organism
    tmp=rb$x*sum(ff$V3+ff$V4)
    # keep only the species which have on average 1 read per whole genome
    rb$x[which(tmp<1)]=0
    abm[files[i],rb$Group.1]=rb$x
}

abm=abm[,which(!is.na(colnames(abm)))]
save(abm,file="/mnt/data_scratch/ovidiu/Ellen/MasterThesis/ReadAbundance_BMG.Rdata")


