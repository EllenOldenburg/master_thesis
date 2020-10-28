###########
# library #
###########
library(tidyr)
library(ggplot2)
library(ggExtra)
library(reshape2)
library(foreach)

########
# Data #
########
load("/Users/ellen/Documents/Uni/MA/ClusterTaxa.RData")

colnames(OrgClu_with_tax) = c("Org", "5", "25", "45",
                              "75", "100", "125", "150",
                              "175", "200", "225", "250",
                              "500", "phylum", "family", "genus" )

table(OrgClu_with_tax$phylum)

OrgTaxCyan = OrgClu_with_tax[(OrgClu_with_tax$phylum == "Cyanobacteria"),]
ind <- apply(OrgTaxCyan, 1, function(x) all(is.na(x)))
OrgTaxCyan <- OrgTaxCyan[ !ind, ]



pro5 = (sum(complete.cases(OrgTaxCyan$`5`))/sum(complete.cases(OrgClu_with_tax$`5`)))*100
pro25 = (sum(complete.cases(OrgTaxCyan$`25`))/sum(complete.cases(OrgClu_with_tax$`25`)))*100
pro45 = (sum(complete.cases(OrgTaxCyan$`45`))/sum(complete.cases(OrgClu_with_tax$`45`)))*100
pro75 = (sum(complete.cases(OrgTaxCyan$`75`))/sum(complete.cases(OrgClu_with_tax$`75`)))*100
pro100 = (sum(complete.cases(OrgTaxCyan$`100`))/sum(complete.cases(OrgClu_with_tax$`100`)))*100
pro125 = (sum(complete.cases(OrgTaxCyan$`125`))/sum(complete.cases(OrgClu_with_tax$`125`)))*100
pro150 = (sum(complete.cases(OrgTaxCyan$`150`))/sum(complete.cases(OrgClu_with_tax$`150`)))*100
pro175 = (sum(complete.cases(OrgTaxCyan$`175`))/sum(complete.cases(OrgClu_with_tax$`175`)))*100
pro200 = (sum(complete.cases(OrgTaxCyan$`200`))/sum(complete.cases(OrgClu_with_tax$`200`)))*100
pro225 = (sum(complete.cases(OrgTaxCyan$`225`))/sum(complete.cases(OrgClu_with_tax$`225`)))*100
pro250 = (sum(complete.cases(OrgTaxCyan$`250`))/sum(complete.cases(OrgClu_with_tax$`250`)))*100
pro500 = (sum(complete.cases(OrgTaxCyan$`500`))/sum(complete.cases(OrgClu_with_tax$`500`)))*100

depth = c("5", "25", "45", "75", 
          "100", "125", "150", "175",
          "200", "225", "250", "500")
percent = c(pro5, pro25, pro45, pro75,
            pro100, pro125, pro150, pro175,
            pro200, pro225, pro250, pro500)



dfper = data.frame(depth= depth, percent = percent)



colnames(OrgClu_with_tax)[2] = 5

i = grep("Cyano", OrgClu_with_tax$phylum)
cyanos = OrgClu_with_tax[i,]
fam = unique(cyanos$family)
cyanos1 = cyanos[,c(2:13,15)]
cyanos1$family = sapply(strsplit(cyanos1$family," "), `[`, 1)

bla = melt(cyanos1, id.vars = c("family"))

dep = as.character(unique(bla$variable))

hehe = foreach(i = dep, .combine = rbind) %do% {
  sub = bla[bla$variable == i,]
  sub = na.omit(sub)
  bla1 = table(sub$family)
  bla2 = data.frame(bla1, depth = i)
}



hehe$depth = factor(hehe$depth, levels = dep)

ggplot(hehe, aes(Var1, depth, fill= Freq)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90))

p =ggplot(data = hehe, aes(x=Var1, y=depth, size=Freq, color=Var1)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Depth") +
  xlab("Families") +
  theme(legend.position = "none")




hehe2 = foreach(i = dep, .combine = rbind) %do% {
  sub = bla[bla$variable == i,]
  sub = na.omit(sub)
  sub = sub[,c(1,3)]
  testo = melt(sub, id.vars = c("value", "family"))
  bla1 = table(testo)
  bla2 = data.frame(bla1, depth = i)
}

hehe2$depth = as.numeric(hehe2$depth)
hehe2$value = as.character(hehe2$value)

sub1 = hehe2[hehe2$depth %in% dep[1:3],]
sub2 = hehe2[hehe2$depth %in% dep[4:6],]
sub3 = hehe2[hehe2$depth %in% dep[7:9],]
sub4 = hehe2[hehe2$depth %in% dep[10:12],]

p1 <-ggplot(sub1,aes(family,value,fill=Freq))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="TNumber",option ="C")
p1 <-p1 + facet_grid(depth~.)
p1 <-p1 + scale_x_discrete(guide = guide_axis(angle = 45)) 
p1 <-p1 + scale_y_discrete( breaks = unique(sub1$value))
p1

p2 <-ggplot(sub2,aes(family,value,fill=Freq))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="TNumber",option ="C")
p2 <-p2 + facet_grid(depth~.)
p2 <-p2 + scale_x_discrete(guide = guide_axis(angle = 45)) 
p2 <-p2 + scale_y_discrete( breaks = unique(sub2$value))
p2

p3 <-ggplot(sub3,aes(family,value,fill=Freq))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="TNumber",option ="C")
p3 <-p3 + facet_grid(depth~.)
p3 <-p3 + scale_x_discrete(guide = guide_axis(angle = 45)) 
p3 <-p3 + scale_y_discrete( breaks = unique(sub3$value))
p3

p4 <-ggplot(sub4,aes(family,value,fill=Freq))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="TNumber",option ="C")
p4 <-p4 + facet_grid(depth~.)
p4 <-p4 + scale_x_discrete(guide = guide_axis(angle = 45)) 
p4 <-p4 + scale_y_discrete( breaks = unique(sub4$value))
p4



p <-ggplot(hehe2,aes(family,value,fill=Freq))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="TNumber",option ="C")
p <-p + facet_grid(depth~.)
p <-p + scale_y_discrete( breaks = unique(hehe2$value))




sub1 = hehe2[hehe2$depth %in% dep[1],]
sub2 = hehe2[hehe2$depth %in% dep[2],]
sub3 = hehe2[hehe2$depth %in% dep[3],]
sub4 = hehe2[hehe2$depth %in% dep[4],]
sub5 = hehe2[hehe2$depth %in% dep[5],]
sub6 = hehe2[hehe2$depth %in% dep[6],]
sub7 = hehe2[hehe2$depth %in% dep[7],]
sub8 = hehe2[hehe2$depth %in% dep[8],]
sub9 = hehe2[hehe2$depth %in% dep[9],]
sub10 = hehe2[hehe2$depth %in% dep[10],]
sub11 = hehe2[hehe2$depth %in% dep[11],]
sub12 = hehe2[hehe2$depth %in% dep[12],]



p1 =ggplot(data = sub1, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p2 =ggplot(data = sub2, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p3 =ggplot(data = sub3, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p4 =ggplot(data = sub4, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p5 =ggplot(data = sub5, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p6 =ggplot(data = sub6, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p7 =ggplot(data = sub7, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")


p8 =ggplot(data = sub8, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p9 =ggplot(data = sub9, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")


p10 =ggplot(data = sub10, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p11 =ggplot(data = sub11, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size(range = c(.1, 24), name="tada")+
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")



library(patchwork)

testo = aggregate(Freq~family, data = sub12, FUN = sum)
testo$family = as.character(testo$family)
testo$family = factor(testo$family, levels = rev(testo[order(testo$Freq, decreasing = T),1]))
p12_2 = ggplot(testo, aes(x = family, y = Freq)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size=15)) + coord_flip()

sub12$family = factor(sub12$family, levels = rev(testo[order(testo$Freq, decreasing = T),1]))

p12_1 =ggplot(data = sub12, aes(x=value, y=family, size=Freq, color=depth)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="tada")+
  scale_y_discrete(expand=c(0, 1, 0, 4)) +
  theme(legend.position="bottom") +
  ylab("Families") +
  xlab("Cluster") +
  theme(legend.position = "none")

p12_1 | p12_2




