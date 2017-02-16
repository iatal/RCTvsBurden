#On va mettre dans une base de données, pour chaque région, les share across 28 maladies du burden et des essais

#1- On fait matrice burden par catégorie
#################################################################################
library(foreach)
library(doParallel)

#GBD 2005
#Data as downloaded from GBD 2010 study, not included in the repository
gbds <- list.files('/media/igna/Elements/HotelDieu/Cochrane/GBD 2010/GBD_2005_2010_by_cause_country_level_GBD2010/2005')
gbds <- gbds[grep('csv',gbds,ignore.case=TRUE)]

cl<-makeCluster(4)
registerDoParallel(cl)

t0 <- proc.time()
A <- foreach(k = gbds) %dopar% {
Mcause <- read.csv(paste('/media/igna/Elements/HotelDieu/Cochrane/GBD 2010/GBD_2005_2010_by_cause_country_level_GBD2010/2005/',k,sep=""))
#Restriction aux données: toutes les ages, tous les sexes, toutes les metriques (dalys, morts, yll, yld), nombre total (sans intervalle de confiance),nb par mill inhab
Mcause[Mcause$age_name=='All ages' & Mcause$sex=='Both sexes',c(1:8,11,12,18)]
}

stopCluster(cl)

DT <- do.call('rbind',A)

(proc.time()-t0)/60
#1.5min

#Taxonomie à 27 catégories
Mgbd <- read.table("./Data/27_gbd_groups.txt")

DT$causelevel5[is.na(DT$causelevel5)] <- ""

#On rajoute les maladies évaluées au niveau 27
dt <- DT[(DT$causelevel3%in%as.character(Mgbd$cause_name) & DT$causelevel4=="" & DT$causelevel5=="") |(DT$causelevel4%in%as.character(Mgbd$cause_name) & DT$causelevel5=="") | (DT$causelevel5%in%as.character(Mgbd$cause_name)),]

#Liste des maladies ajoutées
ML <- dt[!duplicated(paste(dt$causelevel1,dt$causelevel2,dt$causelevel3,dt$causelevel4,dt$causelevel5)),]

table(Mgbd$cause_name%in%c(as.character(dt$causelevel4),as.character(dt$causelevel5),as.character(dt$causelevel3)))
#FALSE  TRUE 
#   15    13 

#Il reste 15 à faire manuellement
Mgbd$cause_name[!Mgbd$cause_name%in%c(as.character(dt$causelevel4),as.character(dt$causelevel5),as.character(dt$causelevel3))]

#Maternal disorders
Aj <- DT[DT$causelevel3%in%c("Maternal disorders"),]
table(Aj$causelevel5=="")
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Neonatal disorders
Aj <- DT[DT$causelevel3%in%c("Neonatal disorders"),]
table(Aj$causelevel5=="")
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Nutritional deficiencies
Aj <- DT[DT$causelevel3%in%c("Nutritional deficiencies"),]
table(Aj$causelevel5=="")
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Neoplasms
Aj <- DT[DT$causelevel3%in%c("Neoplasms"),]
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
#1 seul cancer concerné (Liver cancer)
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")
#Estimé au niveau 4
#On peut supprimé l'estimation niveau 5 et ajouter niveau pays
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Cardiovascular and circulatory diseases
Aj <- DT[DT$causelevel3%in%c("Cardiovascular and circulatory diseases"),]
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Chronic respiratory diseases
Aj <- DT[DT$causelevel3%in%c("Chronic respiratory diseases"),]
table(Aj$causelevel5=="")
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Cirrhosis of the liver
Aj <- DT[DT$causelevel3%in%c("Cirrhosis of the liver"),]
table(Aj$causelevel5=="")
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Digestive diseases (except cirrhosis)
Aj <- DT[DT$causelevel3%in%c("Digestive diseases (except cirrhosis)"),]
table(Aj$causelevel5=="")
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Neurological disorders
Aj <- DT[DT$causelevel3%in%c("Neurological disorders"),]
table(Aj$causelevel5=="")
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Mental and behavioral disorders
Aj <- DT[DT$causelevel3%in%c("Mental and behavioral disorders"),]
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")/4
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Musculoskeletal disorders
Aj <- DT[DT$causelevel3%in%c("Musculoskeletal disorders"),]
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Injuries
Aj <- DT[DT$causelevel2%in%c("Injuries"),]
table(Aj$causelevel4=="")
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")/4
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
Mttp$causelevel3 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Diarrhea, lower respiratory infections, meningitis, and other common infectious diseases
Aj <- DT[DT$causelevel3%in%c("Diarrhea, lower respiratory infections, meningitis, and other common infectious diseases"),]
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- ""
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Neglected tropical diseases excluding malaria
Aj <- DT[DT$causelevel3%in%c("Neglected tropical diseases and malaria") & !DT$causelevel4%in%c("Malaria"),]
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")
length(unique(Aj$country[Aj$causelevel5!=""]))
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- "Neglected tropical diseases excluding malaria"
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

#Diabetes, urinary diseases and male infertility
Aj <- DT[DT$causelevel4%in%c("Diabetes mellitus","Acute glomerulonephritis","Urinary diseases and male infertility","Chronic kidney diseases"),]
table(Aj$causelevel5=="")
length(unique(Aj$causelevel4[Aj$causelevel5!=""]))
table(Aj$causelevel4%in%Aj$causelevel4[Aj$causelevel5!=""] & Aj$causelevel5=="")
Aj <- Aj[Aj$causelevel5=="",]
Mttp <- Aj[!duplicated(paste(Aj$country_name,Aj$measure)),]
Mttp <- Mttp[order(paste(Mttp$country_name,Mttp$measure)),]
Mttp$nm_mean <- tapply(Aj$nm_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$rt_mean <- tapply(Aj$rt_mean,paste(Aj$country_name,Aj$measure),sum)
Mttp$causelevel4 <- "Diabetes, urinary diseases and male infertility"
dt <- rbind(dt,Mttp)
ML <- rbind(ML,Aj[!duplicated(paste(Aj$causelevel1,Aj$causelevel2,Aj$causelevel3,Aj$causelevel4,Aj$causelevel5)),])

table(Mgbd$cause_name%in%c(as.character(dt$causelevel2[dt$causelevel3==""]),as.character(dt$causelevel3[dt$causelevel4==""]),as.character(dt$causelevel4[dt$causelevel5==""])))

table(Mgbd$cause_name%in%c(as.character(dt$causelevel2[dt$causelevel3!=""]),as.character(dt$causelevel3[dt$causelevel4!=""]),as.character(dt$causelevel4[dt$causelevel5!=""])))

U <- unique(paste(dt$causelevel1,dt$causelevel2,dt$causelevel3,dt$causelevel4,dt$causelevel5))
length(U)
#OK on a les 28

Umes <- unique(paste(dt$causelevel1,dt$causelevel2,dt$causelevel3,dt$causelevel4,dt$causelevel5,dt$measure))
length(Umes)/28
#3.857143

s <- tapply(dt$meas,paste(dt$causelevel1,dt$causelevel2,dt$causelevel3,dt$causelevel4,dt$causelevel5),function(x){length(unique(x))})
s[s!=4]

#Sense organ diseases and oral disorders: normal pas de deaths ni yll

L <- lapply(Mgbd$cause_name,function(x){grep(as.character(x),U,fixed=TRUE)})
table(unlist(lapply(L,length)))

Mgbd$cause_name[unlist(lapply(L,length))==2]
U[grep("HIV/AIDS",U)]
#OK

#On rajoute la variable Diseases = dernière diseases sans ""
dt$Disease <- as.character(dt$causelevel4)
dt$Disease[dt$Disease==""] <- as.character(dt$causelevel3[dt$Disease==""])
dt$Disease[dt$Disease==""] <- as.character(dt$causelevel2[dt$Disease==""])

table(dt$Disease%in%as.character(Mgbd$cause_name))
length(unique(dt$Dis))
#OK

dt$Sup_region <- dt$region
levels(dt$region)
# [1] "Andean Latin America"         "Australasia"                 
# [3] "Caribbean"                    "Central Asia"                
# [5] "Central Europe "              "Central Latin America"       
# [7] "Central Sub-Saharan Africa"   "East Asia"                   
# [9] "Eastern Europe"               "Eastern Sub-Saharan Africa"  
#[11] "High-income Asia Pacific"     "High-income North America"   
#[13] "North Africa and Middle East" "Oceania"                     
#[15] "South Asia"                   "Southeast Asia "             
#[17] "Southern Latin America"       "Southern Sub-Saharan Africa" 
#[19] "Tropical Latin America"       "Western Europe"              
#[21] "Western Sub-Saharan Africa"  

levels(dt$Sup_region) <- c(
"Latin America and Caribbean", "High-income", 
"Latin America and Caribbean", "Central Europe, Eastern Europe, and Central Asia", 
"Central Europe, Eastern Europe, and Central Asia", "Latin America and Caribbean",
"Sub-Saharian Africa", "Southeast Asia, East Asia and Oceania",
"Central Europe, Eastern Europe, and Central Asia", "Sub-Saharian Africa",
"High-income", "High-income",
"North Africa and Middle East", "Southeast Asia, East Asia and Oceania",
"South Asia", "Southeast Asia, East Asia and Oceania",
"High-income", "Sub-Saharian Africa",
"Latin America and Caribbean", "High-income",
"Sub-Saharian Africa")

#Region level
GBD <- dt
meas <- levels(GBD$mes)
dis <- levels(GBD$Dis)
reg <- levels(GBD$Sup_reg)

S <- tapply(GBD$nm_mean,paste(GBD$meas,GBD$Sup_region,GBD$Disease,sep="&"),sum)

L <- strsplit(names(S),"&")

M <- data.frame(do.call('rbind',L))
names(M) <- c("metr","Region","Disease")
M$burden <- as.numeric(S)

write.table(M,"./Data/DALY_YLL_deaths_per_region_and_27_diseases_2005.txt")

