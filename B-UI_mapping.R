library(data.table)

dis <- 1:27
#we suppress diseases without replicates:
dis <- setdiff(dis,c(9,11,21,23,27))

d <- sample(dis,1)
DF <- fread(paste(c("HotelDieu/MappingRCTs/Replicates/",as.character(d),"/Metrics_over_replicates.txt"),collapse=""))


dprp <- DF[DF$Dis=="dis",c("RCTs","Patients")]/DF[DF$Dis=="all",c("RCTs","Patients")]
dprp$Reg <- DF$Region[DF$Dis=="dis"]
tapply(dprp$RCTs,dprp$Reg,function(x){100*quantile(x,probs=c(0.025,0.5,0.975))})
tapply(dprp$Patients,dprp$Reg,function(x){100*quantile(x,probs=c(0.025,0.5,0.975))})

DF[,quantile(.SD,probs=c(0.025,0.5,0.975)),by=.(Dis,Region),.SDcols=c("RCTs","Patients")]


tapply(DF$RCTs[DF$Dis=="dis"],DF$Reg[DF$Dis=="dis"],function(x){quantile(x,probs=c(0.025,0.5,0.975))})


