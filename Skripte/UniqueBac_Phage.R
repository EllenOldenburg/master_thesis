#Clear environment
rm(list=ls())
#clear any occupied memory by running garbage collector 
gc()

# library
libraries = c("forecast","dplyr","zoo","chron", "xts", "dplyr","tibble","tidyverse")
for (lib_name in libraries) {
  if (require(lib_name, character.only = TRUE) == F) {
    install.packages(lib_name)
    library(lib_name, character.only = TRUE)
  } else {
    library(lib_name, character.only = TRUE)
  }
}


# load abundance table phage and bacteria
load("~/Documents/uni/MA/abund_phage.RData")
load("~/Documents/uni/MA/ReadAbundance_BMG.Rdata")
load("~/Documents/uni/MA/uniqueabmProphage.Rdata")



# load date and depth
DateDepth <- read_csv("~/Documents/Uni/MA/Tabelle_date_death.txt")
colnames(DateDepth) <- c("Index", "SRR", "Depth", "Date")

abm = abm[rownames(abm) %in% DateDepth$SRR,]
abm_phage = abm_phage[rownames(abm_phage) %in% DateDepth$SRR,]

# combine abundance Phage and prophage with SRR, Depth and Date
abmProphageDeDa <- cbind(DateDepth[,3:4],abm_phage)

# combine Date and Depth to one col
abmProphageDeDa$DaDe <- as.numeric(paste0(abmProphageDeDa$Date, abmProphageDeDa$Depth))

save(abmProphageDeDa, file = "~/Documents/Uni/MA/abmProphageDeDa.Rdata")


# mean of the abundance if there are duplicates in Depth and Date
df = abmProphageDeDa


df = aggregate(df[,3:(ncol(df)-1)], list(df$DaDe), mean)

Date = as.numeric(substr(df$Group.1, 1, 6))
Depth = as.numeric(substr(df$Group.1, 7, nchar(df$Group.1)))

uniqueabmProphage = cbind(Depth,Date,df[,2:ncol(df)])

dim(uniqueabmProphage)

save(uniqueabmProphage, file = "~/Documents/Uni/MA/uniqueabmProphage.Rdata")



####Bac

# load date and depth
DateDepth <- read_csv("~/Documents/Uni/MA/Tabelle_date_death.txt")
colnames(DateDepth) <- c("Index", "SRR", "Depth", "Date")

abm = abm[rownames(abm) %in% DateDepth$SRR,]
abm_Bac = abm[rownames(abm) %in% DateDepth$SRR,]

# combine abundance Phage and prophage with SRR, Depth and Date
abmBacDeDa <- cbind(DateDepth[,3:4],abm_Bac)

# combine Date and Depth to one col
abmBacDeDa$DaDe <- as.numeric(paste0(abmBacDeDa$Date, abmBacDeDa$Depth))

save(abmBacDeDa, file = "~/Documents/Uni/MA/abmBacDeDa.Rdata")


# mean of the abundance if there are duplicates in Depth and Date
df = abmBacDeDa


df = aggregate(df[,3:(ncol(df)-1)], list(df$DaDe), mean)

Date = as.numeric(substr(df$Group.1, 1, 6))
Depth = as.numeric(substr(df$Group.1, 7, nchar(df$Group.1)))

uniqueabmBac = cbind(Depth,Date,df[,2:ncol(df)])

dim(uniqueabmBac)

save(uniqueabmBac, file = "~/Documents/Uni/MA/uniqueabmBac.Rdata")