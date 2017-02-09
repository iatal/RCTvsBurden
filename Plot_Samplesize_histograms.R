library(gdata)

#Upload database
data <- read.table("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Flowchart/database_all_diseases_final_ok.txt")
data <- data[!is.na(data$Sample),]
table(data$Sample>=10)
# FALSE   TRUE 
#  1629 107582
table(data$Sample>=150000)
# FALSE   TRUE 
#109177     34 

data <- data[data$Sample>=10 & data$Sample<=150000,]
N <- nrow(data)
names(data)

#Upload traduction names/label categories
Mgbd <- read.table("/home/igna/Desktop/Programs GBD/Classifier_Trial_GBD/Databases/Taxonomy_DL/GBD_data/GBD_ICD.txt")
grep("Injur",Mgbd$cause_name)

#We supress from GBD28 the label 28
GBD27 <- sapply(strsplit(as.character(data$GBD28),"&"),function(x){paste(x[x!="28"],collapse="&")})
data$GBD27 <- GBD27
#Number of trials relevant to the burden of diseases
table(GBD27=="")

regs <- sort(unique(unlist(strsplit(as.character(data$Regions),"&"))))
reg_labs <- c("Central Europe, Eastern\nEurope and Central Asia",
              "High-income\ncountries",
              "Latin America\nand Caribbean",
              "North Africa and\nMiddle East",
              "South Asia",
              "Southeast Asia,\nEast Asia and Oceania",
              "Sub-Saharian\nAfrica")

nb_ctrs <- lapply(strsplit(as.character(data$Nb_ctr_per_reg),'&'),as.numeric)
RGs <-strsplit(as.character(data$Regions),'&')
pats <- data.frame(TrialID = rep(data$TrialID,sapply(nb_ctrs,length)),
                   Nb_ctrs = unlist(nb_ctrs),
                   Region = unlist(RGs),
                   Tot_sample = rep(data$Sample,sapply(nb_ctrs,length)))

pats$tot_ctrs <- rep(sapply(nb_ctrs,sum),sapply(nb_ctrs,length))
pats$sample_per_reg <- pats$Tot_sample*pats$Nb_ctrs/pats$tot_ctrs

Lgbd <- lapply(as.character(data$GBD27),function(x){as.numeric(unlist(strsplit(x,"&")))})

spl_qt <- c(0,20,40,60,100,200,400,1000,2000,10000,20000,100000,200000)

data$Sple_cl <- cut(data$Sample,spl_qt,right=F)
data$Sple_cl <- as.character(data$Sple_cl)
data$Sple_cl <- as.factor(data$Sple_cl)

library(gdata)

data$Sple_cl <- reorder(data$Sple_cl,new.order=c('[0,20)',
                                                 '[20,40)',
                                                 '[40,60)',
                                                 '[60,100)',
                                                 '[100,200)',
                                                 '[200,400)',
                                                 '[400,1e+03)',
                                                 '[1e+03,2e+03)',
                                                 '[2e+03,1e+04)',
                                                 '[1e+04,2e+04)',
                                                 '[2e+04,1e+05)',
                                                 '[1e+05,2e+05)'
                                                 ))


pats$Sple_cl <- cut(pats$sample_per_reg,spl_qt,right=F)
pats$Sple_cl <- as.character(pats$Sple_cl)
pats$Sple_cl <- as.factor(pats$Sple_cl)
pats$Sple_cl <- reorder(pats$Sple_cl,new.order=c('[0,20)',
                                                 '[20,40)',
                                                 '[40,60)',
                                                 '[60,100)',
                                                 '[100,200)',
                                                 '[200,400)',
                                                 '[400,1e+03)',
                                                 '[1e+03,2e+03)',
                                                 '[2e+03,1e+04)',
                                                 '[1e+04,2e+04)',
                                                 '[2e+04,1e+05)',
                                                 '[1e+05,2e+05)'
                                                 ))


T <- table(pats$Sple_cl,pats$Region)
T <- cbind(T,"Non High-income"=table(data$Sple_cl)-T[,"High-income"])
Toth <- T[,!colnames(T)%in%c("High-income","Non High-income")]

dev.off()
x11(height=7,width=10)
par(mfrow=c(2,4))

barplot(t(T[,c("High-income","Non High-income")]),main="High-income among\nworld")
for(i in 1:ncol(Toth)) barplot(Toth[,i],ylim=c(0,max(Toth)),main=reg_labs[reg_labs!="High-income\ncountries"][i])


barplot(t(T))

