#Upload database
data <- read.table("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Flowchart/database_all_diseases_final_ok.txt")
N <- nrow(data)
names(data)

#Upload traduction names/label categories
Mgbd <- read.table("/home/igna/Desktop/Programs GBD/Classifier_Trial_GBD/Databases/Taxonomy_DL/GBD_data/GBD_ICD.txt")
#And supress injuries from the causes of burden
grep("Injur",Mgbd$cause_name)
GBD27 <- sapply(strsplit(as.character(data$GBD28),"&"),function(x){paste(x[x!="28"],collapse="&")})
data$GBD27 <- GBD27

regs <- sort(unique(unlist(strsplit(as.character(data$Regions),"&"))))
LR <- lapply(regs,function(x){1:nrow(data)%in%grep(x,data$Regions)})
LR <- do.call('cbind',LR)

dis <- 1:27
d <- dis[1]

SMs <- list.files(paste("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Incertitude_mapping/Replicates/",as.character(d),sep=""))

ss_sp <- read.table(paste(c("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Incertitude_mapping/Replicates/",as.character(d),"/Sens_spec.txt"),collapse=""))


options(repr.plot.width=9, repr.plot.height=3)
par(mfrow=c(1,4))
for(i in 1:ncol(ss_sp)) hist(ss_sp[,i],xlim=c(0,1),main=colnames(ss_sp)[i],xlab=NULL)
    

    SMs <- SMs[grep("Reclassif",SMs)]
    
    k <- sample(SMs,1)
    
    recl <- read.table(paste(c("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Incertitude_mapping/Replicates/",as.character(d),"/",k),collapse=""),header=TRUE)
    
    apply(LR[as.logical(recl$recl_dis),],2,sum)
    
    t0 <- proc.time()
A <- 
lapply(SMs,function(k){
    recl <- read.table(paste(c("/home/igna/Desktop/",
                               as.character(d),"/",k),collapse=""),header=TRUE)
    
    tt <- sum(recl$recl_dis)
    cbind(ifelse(tt==0,rep(0,length(regs)),
          ifelse(tt==1,as.numeric(LR[as.logical(recl$recl_dis),]),
          apply(LR[as.logical(recl$recl_dis),],2,sum))),
    apply(LR[as.logical(recl$recl_dis+recl$recl_oth),],2,sum))
    
})
t1 <- proc.time()
    
    
    
    library(parallel)
    
    detectCores()
    
    library(foreach)
library(doParallel)
    
    
cl<-makeCluster(4)
registerDoParallel(cl)

t0 <- proc.time()
A <- foreach(k = SMs) %dopar% {
    recl <- read.table(paste(c("/home/igna/Desktop/",
                               as.character(d),"/",k),collapse=""),header=TRUE)
    
    tt <- sum(recl$recl_dis)
    cbind(ifelse(tt==0,rep(0,length(regs)),
          ifelse(tt==1,as.numeric(LR[as.logical(recl$recl_dis),]),
          apply(LR[as.logical(recl$recl_dis),],2,sum))),
    apply(LR[as.logical(recl$recl_dis+recl$recl_oth),],2,sum))
}

t1 <- proc.time()-t0

stopCluster(cl)



k <- SMs[1]
    recl <- read.table(paste(c("/media/igna/Elements/HotelDieu/Cochrane/Mapping_Cancer/Incertitude_mapping/Replicates/",
                               as.character(d),"/",k),collapse=""),header=TRUE)
    
    tt <- sum(recl$recl_dis)
    cbind(unlist(ifelse(tt==0,list(rep(0,length(regs))),
          ifelse(tt==1,list(as.numeric(LR[as.logical(recl$recl_dis),])),
          list(apply(LR[as.logical(recl$recl_dis),],2,sum))))),
    apply(LR[as.logical(recl$recl_dis+recl$recl_oth),],2,sum))



