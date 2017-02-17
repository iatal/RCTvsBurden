GBD <- read.table("../Data/DALY_YLL_deaths_per_region_and_27_diseases_2005.txt")
GBD <- GBD[order(GBD$Region,GBD$Disease),]
RCT <- read.table("../Data/RCTs_and_Patients_Nb_local_prop_median_UI_per_region_and_disease.txt")
RCT <- RCT[order(RCT$Region,RCT$Disease),]

regs <- levels(GBD$Region)
regs <- regs[regs!="All"]

r <- sample(regs,1)

#All burden in region r
burd_tot <- GBD$burden[GBD$metr=="daly" & GBD$Region==r & GBD$Disease=="all"]
#All research
res_tot <- RCT[RCT$Region==r & RCT$Dis=="All",]

dfb <- GBD[GBD$metr=="daly" & GBD$Region==r & GBD$Disease!="all",]
dfb$prop <- 100*dfb$burden/burd_tot
dfb$mes <- "burden"
dfb <- dfb[,c("Disease","prop","mes")]

dfr <- RCT[RCT$Region==r & RCT$Dis!="All",]
dfr$prop <- dfr$Prop_RCTs_med
dfr$mes <- "research"
dfr <- dfr[,c("Disease","prop","mes")]

dfre <- RCT[RCT$Region==r & RCT$Dis!="All",]
dfre$prop_low <- dfre$Prop_RCTs_low
dfre$prop_up <- dfre$Prop_RCTs_up
dfre$mes <- "research"
dfre <- dfre[,c("Disease","prop_low","prop_up","mes")]

dt <- rbind(dfb,dfr)

#10 Principales maladies
diss <- dfb$Disease[order(dfb$prop,decreasing=TRUE)][1:10]

dt <- droplevels(dt[dt$Disease%in%diss,])
dfre <- droplevels(dfre[dfre$Disease%in%diss,])

library(gdata)
dt$Disease <- reorder(dt$Disease,new.order=as.character(diss))
dfre$Disease <- reorder(dfre$Disease,new.order=as.character(diss))

library(ggplot2)

p <- ggplot(dt,aes(Disease))
p <- p + geom_bar(aes(fill=mes,y=prop),position="dodge",stat="identity",width=0.8)
p <- p + geom_segment(aes(x=as.numeric(Disease)-0.4,xend=as.numeric(Disease)+0.4,y=prop/2,yend=prop/2,size=5),linetype="dashed",data=dt[dt$mes=="burden",],lwd=0.1)
p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=prop_up,ymin=prop_low),width=0.2,data=dfre)





