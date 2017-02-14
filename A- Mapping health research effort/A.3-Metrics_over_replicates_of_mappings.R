library(data.table)
library(foreach)
library(doParallel)
options(warn = 2)

#Upload database
data <- read.table("/media/igna/Elements/HotelDieu/Cochrane/MappingRCTs_vs_Burden/database_RCTs_regions_27diseases.txt")

#Upload traduction names/label categories
Mgbd <- read.table("../Data/27_gbd_groups.txt")

#Regions per trial
regs <- sort(unique(unlist(strsplit(as.character(data$Regions),"&"))))
LR <- lapply(regs,function(x){1:nrow(data)%in%grep(x,data$Regions)})
LR <- do.call('cbind',LR)
LR <- data.table(LR)
LR$TrialID <- data$TrialID

#Nb of patients per region per trial
nb_ctrs <- lapply(strsplit(as.character(data$Nb_ctr_per_reg),'&'),as.numeric)
RGs <-strsplit(as.character(data$Regions),'&')
pats <- data.frame(TrialID = rep(data$TrialID,sapply(nb_ctrs,length)),
                   Nb_ctrs = unlist(nb_ctrs),
                   Region = unlist(RGs),
                   Tot_sample = rep(data$Sample,sapply(nb_ctrs,length)))

pats$tot_ctrs <- rep(sapply(nb_ctrs,sum),sapply(nb_ctrs,length))
pats$sample_per_reg <- pats$Tot_sample*pats$Nb_ctrs/pats$tot_ctrs
pats <- data.table(pats)
setkey(pats,TrialID)

dis <- 1:27

t0 <- proc.time()

for(d in dis){

tp0 <- proc.time()
print(paste("starting disease ",d,": ",as.character(Mgbd$x[d])),collapse="") 

SMs <- list.files(paste("/media/igna/Elements/HotelDieu/Cochrane/MappingRCTs_vs_Burden/Replicates/",as.character(d),sep=""))
SMs <- SMs[grep("Reclassif",SMs)]
if(length(SMs)<9000) {
print(paste(c("disease ",d,": ",as.character(Mgbd$x[d])," has only ",length(SMs)," replicates: we pass to next one"),collapse=""))
next
}

cl<-makeCluster(4)
registerDoParallel(cl)

A <- foreach(k = SMs, .packages="data.table") %dopar% {

	repl <- fread(paste(c("HotelDieu/MappingRCTs/Replicates/",as.character(d),"/",k),collapse=""))
	repl$TrialID <- LR$TrialID
	setkey(repl,TrialID)
	replpats <- merge(pats,repl)
	setkey(replpats,Region)
	
	#Output data
	df <- data.table(Region=c(sort(regs),"All","Non-HI"),Dis=rep(c("dis","all"),each=9),RCTs=as.integer(0),Patients=as.numeric(0))

	#Par région
	#Nb trials par region concernant la maladie and relevant to GBD
	df[Dis=="dis" & Region%in%regs,RCTs:=table(replpats[recl_dis==1,Region])]
	df[Dis=="all" & Region%in%regs,RCTs:=table(replpats[recl_dis+recl_oth>=1,Region])]
	#Nb patients par région concernant la maladie and relevant to GBD
	df[Dis=="dis" & Region%in%regs,Patients:=replpats[recl_dis==1,][regs,sum(sample_per_reg,na.rm=TRUE),by=.EACHI]$V1]
	df[Dis=="all" & Region%in%regs,Patients:=replpats[recl_dis+recl_oth>=1,][regs,sum(sample_per_reg,na.rm=TRUE),by=.EACHI]$V1]

	#WorldWide
	#Nb trials worldwide concernant la maladie and relevant to GBD
	df[Dis=="dis" & Region=="All",RCTs:=sum(repl$recl_dis)]
	df[Dis=="all" & Region=="All",RCTs:=sum(repl$recl_dis+repl$recl_oth>=1)]
	#Nb patients worldwide concernant la maladie and relevant to GBD
	df[Dis=="dis" & Region=="All",Patients:=sum(replpats[recl_dis==1,sample_per_reg],na.rm=TRUE)]
	df[Dis=="all" & Region=="All",Patients:=sum(replpats[recl_dis+recl_oth>=1,sample_per_reg],na.rm=TRUE)]

	#Non-HI countries
	#Nb trials worldwide concernant la maladie and relevant to GBD
	df[Dis=="dis" & Region=="Non-HI",RCTs:=replpats[Region!="High-income",][recl_dis==1,][!duplicated(TrialID),.N]]
	df[Dis=="all" & Region=="Non-HI",RCTs:=replpats[Region!="High-income",][recl_dis+recl_oth>=1,][!duplicated(TrialID),.N]]
	#Nb patients worldwide concernant la maladie and relevant to GBD
	df[Dis=="dis" & Region=="Non-HI",Patients:=sum(replpats[Region!="High-income",][recl_dis==1,sample_per_reg],na.rm=TRUE)]
	df[Dis=="all" & Region=="Non-HI",Patients:=sum(replpats[Region!="High-income",][recl_dis+recl_oth>=1,sample_per_reg],na.rm=TRUE)]

}

stopCluster(cl)

fwrite(rbindlist(A),paste(c("/media/igna/Elements/HotelDieu/Cochrane/MappingRCTs_vs_Burden/Replicates/Metrics_over_repl/Metrics_over_replicates_",as.character(d),".txt"),collapse=""))
rm(A)

tp1 <- proc.time()
print(paste(c("disease ",d,": ",as.character(Mgbd$x[d])," finished after (min):"),collapse=""))
print((tp1-tp0)/60)
}

t1 <- proc.time()

print("total time (hrs):")
print((t1-t0)/3600)

