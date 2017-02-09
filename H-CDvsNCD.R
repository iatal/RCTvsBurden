Mgbd <- read.table("/home/igna/Desktop/Programs GBD/Classifier_Trial_GBD/Databases/Taxonomy_DL/GBD_data/GBD_ICD.txt")

Mgbd$BG <- NA
Mgbd$BG[1:11] <- "CD"
Mgbd$BG[12:27] <- "NCD"

Mgbd <- Mgbd[-28,]

GBD <- read.table("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Tables/GBD_data_per_region_and_27_diseases_2005.txt")

Mgbd <- Mgbd[order(Mgbd$cause_name),]
GBD$BG <- Mgbd$BG

do.call('rbind',by(GBD,as.factor(GBD$BG),function(x){apply(x[,-ncol(x)],2,sum)}))
