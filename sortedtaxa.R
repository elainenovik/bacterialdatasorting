library(readxl)
library(dplyr)
library(VennDiagram)
library(RColorBrewer)
library(writexl)

#fresh_data<-read_excel("/home/enovik/R/Thesis/divided-level-6.xlsx")

fresh_data2<-read_excel("/home/enovik/R/V2divided-level-6.xlsx")
View(fresh_data2)

fresh_data2<-V2divided_level_6
View(fresh_data2)

#new_data<-fresh_data
#View(new_data)

new_data2<-fresh_data2
View(new_data2)

#Filter out chloroplast and mitochondria counts
fgenusdata2<-new_data2[!grepl("g__Chloroplast", new_data2$Genus),]
fgenusdata2<-fgenusdata2[!grepl("g__Mitochondria", fgenusdata2$Genus),]
fgenusdata2<-fgenusdata2[!grepl("f__uncultured", fgenusdata2$Family),]


names(fgenusdata2)[7] <- "Ban_Max"
names(fgenusdata2)[8] <- "BLO_02"
names(fgenusdata2)[9] <- "BLO_16"
names(fgenusdata2)[11] <- "NF_F"


fgenusdata2$Genus <- as.character(fgenusdata2$Genus)
fgenusdata2$Genus<- gsub("^.{0,3}", "", fgenusdata2$Genus)

fgenusdata2$Family <- as.character(fgenusdata2$Family)
fgenusdata2$Family<- gsub("^.{0,3}", "", fgenusdata2$Family)

fgenusdata2$Phylum <- as.character(fgenusdata2$Phylum)
fgenusdata2$Phylum<- gsub("^.{0,3}", "", fgenusdata2$Phylum)
View(fgenusdata2)

#fgenusdata$Family <- as.character(fgenusdata$Family)
#fgenusdata$Family<- gsub("^.{0,3}", "", fgenusdata$Family)

#fgenusdata<-fgenusdata[!(is.na(fgenusdata$Family) | fgenusdata$Family==""), ]

#replacing uncultured or unavailable genera names with the family names
#family<-"Uncultured from family"
#for (i in 5:ncol(fgenusdata)){
#  for (j in 1:nrow(fgenusdata)){
#    if (i == 6 && (fgenusdata[j,i] == '' | fgenusdata[j,i] == "uncultured")){
#      fgenusdata[j,i]<-fgenusdata[j,i-1]
#      fgenusdata[j,i]<-paste(family, fgenusdata[j,i], sep = " ")
#    }
#  }
#}


fdata <- select(fgenusdata2,"Ban_Max", "BLO_02", "BLO_16", "Clad", "NF_F")
View(fdata)
View(fgenusdata2)

#unique genera for all 5 samples
BMcount<-0
B2count<-0
B16count<-0
Clcount<-0
NFcount<-0
BMnum <- character(0)
B2num <- character(0)
B16num <- character(0)
Clnum <- character(0)
NFnum <- character(0)
for (i in rownames(fdata)){
  x <-fdata[i,]
  if (x[1]>0&&x[2]==0&&x[3]==0&&x[4]==0&&x[5]==0){
    BMcount<-BMcount+1
    BMnum<-c(BMnum, i)
  }
  else if (x[1]==0&&x[2]>0&&x[3]==0&&x[4]==0&&x[5]==0){
    B2count<-B2count+1
    B2num<-c(B2num, i)
  }
  else if (x[1]==0&&x[2]==0&&x[3]>0&&x[4]==0&&x[5]==0){
    B16count<-B16count+1
    B16num<-c(B16num, i)
  }
  else if (x[1]==0&&x[2]==0&&x[3]==0&&x[4]>0&&x[5]==0){
    Clcount<-Clcount+1
    Clnum<-c(Clnum, i)
  }
  else if (x[1]==0&&x[2]==0&&x[3]==0&&x[4]==0&&x[5]>0){
    NFcount<-NFcount+1
    NFnum<-c(NFnum, i)
  }
}
print (c("Total Unique Genera in Ban Max: ", BMcount, BMnum))
print (c("Total Unique Genera in Bangia 02: ", B2count, B2num))
print (c("Total Unique Genera in Bangia 16: ", B16count, B16num))
print (c("Total Unique Genera in Clad: ", Clcount, Clnum))
print (c("Total Unique Genera in NF: ", NFcount, NFnum))

Cl_total<-data.frame()
for (i in Clnum){
  model<-fgenusdata2[i,2:6]
 
  df<-data.frame(model)
  Cl_total<- rbind(Cl_total,df)
}
 
View(Cl_total)
 
write_xlsx(B2_total,"/home/enovik/R/Thesis/B2_total.xlsx")

NF_total<-data.frame()
for (i in NFnum){
  model<-fgenusdata2[i,2:6]
  
  df<-data.frame(model)
  NF_total<- rbind(NF_total,df)
}

View(NF_total)


write_xlsx(NF_total,"/home/enovik/R/Thesis/NF_total.xlsx")

#unique genera for "freshwater reds #2,3", "marine reds #1,5", "freshwater green #4"
#for example FRcount is the genera that either of the freshwater red alga samples have, 
#that the marine red or green samples don't

#subset<-fdata[1:10,]
#View(subset)
FRcount <- 0
MRcount <- 0
FGcount <- 0
FRnum <- character(0)
MRnum <- character(0)
FGnum <- character(0)
extras <- 0
rownumex<-character(0)
for (i in rownames(fdata)){
  x <-fdata[i,]
  if ((x[2]>0|x[3]>0)&&x[1]==0&&x[4]==0&&x[5]==0){
    FRcount<-FRcount+1
    FRnum<-c(FRnum, i)
  }
  else if ((x[1]>0|x[5]>0)&&x[3]==0&&x[4]==0&&x[2]==0){
    MRcount<-MRcount+1
    MRnum<-c(MRnum,i)
  }
  else if (x[1]==0&&x[2]==0&&x[3]==0&&x[4]>0&&x[5]==0){
    FGcount<-FGcount+1
    FGnum<-c(FGnum,i)
  }
  else{
    extras<-extras+1
    rownumex<-c(rownumex, i)
  }
}

print(c("extras:", extras, rownumex))

print (c("Total Unique Genera in Fresh reds: ", FRcount, FRnum))
print (c("Total Unique Genera in Marine reds: ", MRcount, MRnum))
print (c("Total Unique Genera in Fresh greens: ", FGcount, FGnum))

MR_total<-data.frame()
for (i in MRnum){
  model<-fgenusdata2[i,2:6]
  
  df<-data.frame(model)
  MR_total<- rbind(MR_total,df)
}

write_xlsx(MR_total,"/home/enovik/R/Thesis/MR_total.xlsx")

#View(FR_total)

#overlap between the three groups (FR&MR (2,3,1,5), MR&FG (1,5,4), FR&FG (2,3,4))
#for example in FR/MR overlap: the genus must be in either one of the red freshwater samples
#AND in either one of the red marine samples, but NOT in the fresh green sample.


FR_MRcount <- 0
MR_FGcount <- 0
FG_FRcount <- 0
totalcount<-0
extracount <- 0
rownum1<-character(0)
rownum2<-character(0)
rownum3<-character(0)
rownum4<-character(0)
rownum5<-character(0)

for (i in rownames(fdata)){
  x <-fdata[i,]
  
  if ((x[2]>0|x[3]>0)&&(x[1]>0|x[5]>0)&&(x[4]==0)){
    FR_MRcount<-FR_MRcount+1 #Fresh reds and Marine reds genus overlap
    rownum1<-c(rownum1,i)
  }
  
  else if ((x[1]>0|x[5]>0)&&(x[4]>0)&&(x[2] == 0 && x[3] == 0)){
    MR_FGcount<-MR_FGcount+1 #Marine reds and Fresh greens overlap
    rownum2<-c(rownum2,i)
  }
  
  else if ((x[2]>0|x[3]>0)&&(x[4]>0)&&(x[1]==0&&x[5]==0)){
    FG_FRcount<-FG_FRcount+1 #Fresh greens and Fresh reds overlap
    rownum3<-c(rownum3,i)
  }
  
  else if ((x[2]>0|x[3]>0)&&(x[4]>0)&&(x[1]>0|x[5]>0)){
    totalcount<-totalcount+1 #Fresh reds, marine reds, and fresh green overlap
    rownum4<-c(rownum4,i)
  }
}


print (c("Total Genera in common between Fresh reds and Marine reds: ", FR_MRcount, rownum1))
print (c("Total Genera in common between Marine reds and Fresh greens: ", MR_FGcount, rownum2))
print (c("Total Genera in common between Fresh greens and Fresh reds: ", FG_FRcount, rownum3))
print (c("Total Genera in common between all 3 groups: ", totalcount, rownum4))


FG_FR_total<-data.frame()
for (i in rownum3){
  model<-fgenusdata2[i,2:6]
  
  df<-data.frame(model)
  FG_FR_total<- rbind(FG_FR_total,df)
}

write_xlsx(FG_FR_total,"/home/enovik/R/Thesis/FG_FR_total.xlsx")

View(FR_MR_total)



#venn diagram


colours <- c("#E31A1C","#1F78B4","#33A02C")

alga_venn <- draw.triple.venn(
  area1 = FRcount+FR_MRcount+FG_FRcount+totalcount,
  area2 = MRcount+FR_MRcount+MR_FGcount+totalcount, 
  area3 = FGcount+FG_FRcount+MR_FGcount+totalcount, 
  n12 = FR_MRcount+totalcount, 
  n23 = MR_FGcount+totalcount, 
  n13 = FG_FRcount+totalcount, 
  n123 = totalcount, 
  category = c("Freshwater Rhodophyta", "Marine Rhodophyta", "Freshwater Chlorophyta"),
  fill = colours,
  lty = "blank",
  cex = 1.5,
  cat.cex = 1,
  cat.col = "black")

grid.draw(alga_venn)
#overlap within freshwater reds:
ban02count<-0
ban16count<-0
bothcount<-0
ban02num<-character(0)
ban16num<-character(0)
bothnum<-character(0)
for (i in rownames(fdata)){
  x <-fdata[i,]
  if (x[2]>0 && x[3] == 0){
    ban02count<-ban02count+1
    ban02num<-c(ban02num, i)
  }
  else if (x[2]==0 && x[3]>0){
    ban16count<-ban16count+1
    ban16num<-c(ban16num, i)
  }
  else if (x[2]>0 && x[3]>0){
    bothcount<-bothcount+1
    bothnum<-c(bothnum, i)
  }
}

print (c("Total Genera unique to Bangia 2002: ", ban02count, ban02num))
print (c("Total Genera unique to Bangia 2016: ", ban16count, ban16num))
print (c("Total Genera in common between Bangia 2002 and 2016 ", bothcount, bothnum))

ban16_total<-data.frame()
for (i in ban16num){
  model<-fgenusdata2[i,2:6]
  
  df<-data.frame(model)
  ban16_total<- rbind(ban16_total,df)
}

write_xlsx(ban16_total,"/home/enovik/R/Thesis/ban16_total.xlsx")
View(both_total)

colours3 <- c("#FDBF6F","#33A02C","#E31A1C")
choosecolour<-sample(colours3, 2)
atro_venn = draw.pairwise.venn(
  area1 = ban02count+bothcount,
  area2 = ban16count+bothcount,
  cross.area = bothcount,
  category=c("Bangia atropurpurea", "Bangia atropurpurea"),
  scaled = FALSE,
  fill = choosecolour,
  cex = 1.5,
  cat.cex=1,
  cat.col='black',
  cat.pos = c(-15,15),
  cat.fontface = rep("italic",2)
)
grid.draw(atro_venn)

#bangia atropurpurea 2002 vs bangia fuscopurpurea 1998
atrocount<-0
fuscocount<-0
togethercount<-0
atronum<-character(0)
fusconum<-character(0)
togethernum<-character(0)
for (i in rownames(fdata)){
  x <-fdata[i,]
  if (x[2]>0 && x[5] == 0){
    atrocount<-atrocount+1
    atronum<-c(atronum, i)
  }
  else if (x[2]==0 && x[5]>0){
    fuscocount<-fuscocount+1
    fusconum<-c(fusconum, i)
  }
  else if (x[2]>0 && x[5]>0){
    togethercount<-togethercount+1
    togethernum<-c(togethernum, i)
  }
}

print (c("Total Genera unique to Bangia atropurpurea: ", atrocount, atronum))
print (c("Total Genera unique to Bangia fuscopurpurea: ", fuscocount, fusconum))
print (c("Total Genera in common between B. atropurpurea and B. fuscopurpurea ", togethercount, togethernum))


fusco_total<-data.frame()
for (i in fusconum){
  model<-fgenusdata2[i,2:6]
  
  df<-data.frame(model)
  fusco_total<- rbind(fusco_total,df)
}

write_xlsx(fusco_total,"/home/enovik/R/Thesis/fusco_total.xlsx")

View(atrofusco_total)


colours2 <- c("#A6CEE3", "#B2DF8A","#FB9A99")
colours <- c("#E31A1C","#1F78B4","#33A02C")
choosecolour<-sample(colours2, 2)
bangia_venn = draw.pairwise.venn(
  area1 = atrocount+togethercount,
  area2 = fuscocount+togethercount,
  cross.area = togethercount,
  category=c("Bangia atropurpurea", "Bangia fuscopurpurea"),
  scaled = FALSE,
  fill = choosecolour,
  cex = 1.5,
  cat.cex=1,
  cat.col='black',
  cat.pos = c(-10,10),
  #cat.dist = 0.05,
  cat.fontface = rep("italic",2)
)
grid.draw(bangia_venn)

#cladophora 2005 vs bangia 2002
atrocount2<-0
cladcount<-0
togethercount2<-0
atronum2<-character(0)
cladnum<-character(0)
togethernum2<-character(0)
for (i in rownames(fdata)){
  x <-fdata[i,]
  if (x[2]>0 && x[4] == 0){
    atrocount2<-atrocount2+1
    atronum2<-c(atronum2, i)
  }
  else if (x[4]>0 && x[2]==0){
    cladcount<-cladcount+1
    cladnum<-c(cladnum, i)
  }
  else if (x[2]>0 && x[4]>0){
    togethercount2<-togethercount2+1
    togethernum2<-c(togethernum2, i)
  }
}

print (c("Total Genera unique to Bangia atropurpurea: ", atrocount2, atronum2))
print (c("Total Genera unique to Cladophora glomarata: ", cladcount, cladnum))
print (c("Total Genera in common between B. atropurpurea and C. glomerata ", togethercount2, togethernum2))

clad_total<-data.frame()
for (i in cladnum){
  model<-fgenusdata2[i,2:6]
  
  df<-data.frame(model)
  clad_total<- rbind(clad_total,df)
}

write_xlsx(clad_total,"/home/enovik/R/Thesis/clad_total.xlsx")

View(atroclad_total)


#venn diagram for atro vs clad

colours <- c( "#B2DF8A","#FB9A99","#A6CEE3")
choosecolours2<-sample(colours, 2)
lakeont_venn = draw.pairwise.venn(
  area1 = cladcount+togethercount2,
  area2 = atrocount2+togethercount2,
  cross.area = togethercount2,
  category=c("Cladophora glomerata", "Bangia atropurpurea"),
  scaled = FALSE,
  fill = choosecolours2,
  cex = 1.5,
  cat.cex=1,
  cat.col='black',
  cat.pos = c(-20,20),
  cat.dist = 0.05,
  cat.fontface = rep("italic",2)
)
grid.draw(lakeont_venn)

#cladophora 2005 vs bangia fuscopurpurea 1998
fuscocount2<-0
cladcount2<-0
togethercount3<-0
fusconum2<-character(0)
cladnum2<-character(0)
togethernum3<-character(0)
for (i in rownames(fdata)){
  x <-fdata[i,]
  if (x[5]>0 && x[4] == 0){
    fuscocount2<-fuscocount2+1
    fusconum2<-c(fusconum2, i)
  }
  else if (x[4]>0 && x[5]==0){
    cladcount2<-cladcount2+1
    cladnum2<-c(cladnum2, i)
  }
  else if (x[5]>0 && x[4]>0){
    togethercount3<-togethercount3+1
    togethernum3<-c(togethernum3, i)
  }
}

print (c("Total Genera unique to Bangia fuscopurpurea: ", fuscocount2, fusconum2))
print (c("Total Genera unique to Cladophora glomerata: ", cladcount2, cladnum2))
print (c("Total Genera in common between B. fusco and C. glomerata ", togethercount3, togethernum3))

both3_total<-data.frame()
for (i in togethernum3){
  model<-fgenusdata2[i,2:6]
  
  df<-data.frame(model)
  both3_total<- rbind(both3_total,df)
}

View(both3_total)


write_xlsx(both3_total,"/home/enovik/R/Thesis/both3_total.xlsx")

#venn diagram for fusco vs clad

colours <- c("#A6CEE3", "#B2DF8A","#A6CEE3")
choosecolours2<-sample(colours, 2)
cladfusco_venn = draw.pairwise.venn(
  area1 = cladcount2+togethercount3,
  area2 = fuscocount2+togethercount3,
  cross.area = togethercount3,
  category=c("Cladophora glomerata", "Bangia fuscopurpurea"),
  scaled = FALSE,
  fill = choosecolours2,
  cex = 1.5,
  cat.cex=1,
  cat.col='black',
  cat.pos = c(-20,20),
  cat.dist = 0.05,
  cat.fontface = rep("italic",2)
)
grid.draw(cladfusco_venn)


#total genera present in each sample
a<-0
b<-0
c<-0
d<-0
e<-0
for (i in rownames(fdata)){
  x <-fdata[i,]
  if(x[1]>0){
    a<-a+1
  }
  if(x[2]>0){
    b<-b+1
  }
  if(x[3]>0){
    c<-c+1
  }
  if(x[4]>0){
    d<-d+1
  }
  if(x[5]>0){
    e<-e+1
  }
}

print(c(a,b,c,d,e))

#List of genera in each sample (cross reference)
#Venn diagram 1; Fresh reds vs Marine reds vs Fresh green
#FRnum: genera in fresh reds
#MRnum: genera in marine reds 
#FGnum: genera in fresh greens

#rownum1: genera overlapping between FR and MR
#rownum2: genera overlapping between MR and FG
#rownum3: genera overlapping between FG and FR
#rownum4: genera overlapping between all 3 groups

#Venn #2: Bangia atro 2002 vs 2016
#ban02num: genera unique to bangia atro 2002
#ban16num: genera unique to bangia atro 2016
#bothnum: genera in both groups

#Venn #3: Atro vs Fusco
#atronum: genera only in atro
#fusconum: genera only in fusco
#togethernum: genera in both

#Venn #4: Clad vs Atro
#atronum2: Genera unique to ban atro
#cladnum: genera unique to clad
#togethernum2: genera in both

library(data.table)

freshwater_reds <- list()
marine_reds <- list()

for (i in FRnum){
  freshwater_reds<-c(freshwater_reds, as.character(fgenusdata[i,6]))
}
for (i in MRnum){
  marine_reds<- c(marine_reds, as.character(fgenusdata[i,6]))
}

fgenusdata$Genus[19]

View(fgenusdata)

