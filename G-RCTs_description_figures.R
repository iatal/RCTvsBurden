
data <- read.table("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Flowchart/database_all_diseases_final_ok.txt")

smp <- data$Sample
smp[smp<20 | smp>150000] <- NA

table(smp>=1000)
# FALSE   TRUE 
#101987    963
hist(smp[smp<=1000])
