##################################################################################
library(ggplot2)
library(gdata)

D <- read.table('Data/Local_props_RCTs_from_sim_and_burden_forgaps.txt')

D <- D[order(D$Region,D$Disease),]
D$GBD <- D$Tot_gbd/1e6

D$RCTs_sim <- D$SimMed_NbRCTs
D$RCTs_sim95 <- D$Sim95up_NbRCTs

#Order diseases: increasing burden
tt_gbd <- sort(tapply(D$GBD,D$Dis,sum))

#Number of RCTs per region
#Faudra le recalculer pour simulations simulant nombre d'essais dans chaque region
#Order regions
RCTs <- read.table('Data/Simulations_Alldis_NbProp_MedMn95Int_RCTs.txt')
RCTs <- RCTs[RCTs$Dis=="All" & RCTs$Region!="All",c(1,4)]
rownames(RCTs) <- RCTs$Region
tt_rcts <- RCTs[,2]
names(tt_rcts) <- rownames(RCTs)
tt_rcts <- sort(tt_rcts,decreasing=TRUE)

regs <- names(tt_rcts)
#Region labels
reg_labs <- c("High-income countries","Southeast Asia,\nEast Asia and Oceania",
"North Africa and\nMiddle East", "Central Europe, Eastern\nEurope and Central Asia",
"South Asia", "Latin America\nand Caribbean", "Sub-Saharian\nAfrica")

dpl <- D
#Normalizing regions: max RCts = max GBD
dpl$gpl <- (dpl$GBD/max(dpl$GBD))*max(dpl$RCTs_sim)

#Bar size = wdt*2
wdt <- 0.45
#Distance between regions
d_reg <- 400
esp_dis_nb <- 200

#Rectangles for a given region and disease
displt <- 
function(dis,Rg,rg){
data.frame(dis=dis,
           xmin = rep(which(dis==names(tt_gbd))-wdt,2),
           xmax = rep(which(dis==names(tt_gbd))+wdt,2),
           ymin = c(Rg+esp_dis_nb,Rg-esp_dis_nb),
           ymax = c(Rg+esp_dis_nb+dpl$RCTs_sim[dpl$Dis==dis & dpl$Region==rg],Rg-esp_dis_nb-dpl$gpl[dpl$Dis==dis & dpl$Region==rg]),
           ymax_95 = c(Rg+esp_dis_nb+dpl$RCTs_sim95[dpl$Dis==dis & dpl$Region==rg],Rg-esp_dis_nb-dpl$gpl[dpl$Dis==dis & dpl$Region==rg]),
           metr = c("RCT","GBD"),
           reg = rg, ycent = Rg, dis_nb = which(dis==names(tt_gbd)))
}

#Rectangles pour toutes les maladies, une région donnée
regplt <- function(Rg,rg) do.call('rbind',lapply(names(tt_gbd),function(x){displt(x,Rg,rg)}))

#Emplacement des régions
RG <- 0
for(i in 2:length(regs)){
RG <- c(RG,RG[i-1]-(2*esp_dis_nb+max(dpl$gpl[dpl$Region==regs[i-1]])+d_reg+max(dpl$RCTs_sim95[dpl$Region==regs[i]])))
}

#DataFrame Plot
DPLOT <- do.call('rbind',lapply(1:length(regs),function(i){regplt(RG[i],regs[i])}))

#Inner circle
IC <- 8
DPLOT$xmin <- DPLOT$xmin + IC
DPLOT$xmax <- DPLOT$xmax + IC
DPLOT$xcent <- DPLOT$dis_nb + IC

#REGION LABELS
#################
totalLength <- max(DPLOT$ymax_95)-min(DPLOT$ymax_95)+d_reg
alphaStart <- 2*pi*(max(D$RCTs_sim95[D$Region==names(tt_rcts)[1]]+d_reg/2)/totalLength)

  readableAngle<-function(x){
    angle<-x*(360/totalLength)
#    angle+ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,180,0)
  }
    familyLabelsDF<-data.frame(xmin=RG,label=reg_labs)
    familyLabelsDF$angle <- readableAngle(familyLabelsDF$xmin)

#Disease labels
###################
DPLOT$size_dis_lab = 2.3*(40+DPLOT$dis_nb)/(40+max(DPLOT$dis_nb))

#Tick marks
####################
#Pour RCTs
rcttks <- c(0,100,500,1000,2000,3000,5000,7500,10000)

maj_rcts <- function(nb){
x <- nb
k <- 0
while(x>=100){
x <- x%/%10
k <- k+1
}
res <- (x+1)*10^k
res
}

#Faire que les ticks aillent jusqu'au max des RCTs arrondi au sup
RCTtcks <- do.call('rbind',lapply(regs,function(x){
data.frame(breaks=
unique(DPLOT$ymin[DPLOT$metr=="RCT" & DPLOT$reg==x]) +
c(rcttks[2:findInterval(max(D$RCTs_sim95[D$Region==x]),rcttks)],maj_rcts(max(D$RCTs_sim95[D$Region==x]))),
labels=c(rcttks[2:findInterval(max(D$RCTs_sim95[D$Region==x]),rcttks)],maj_rcts(max(D$RCTs_sim95[D$Region==x]))))
}))
RCTtcks$col <- "1RCT"
#Adding 11200 and suppressing 12000
RCTtcks[RCTtcks$labels==12000,c(1,2)] <- RCTtcks[RCTtcks$labels==12000,c(1,2)] - 800
RCTtcks$labels <- as.character(RCTtcks$label)

#Pour GBD
#gbdtks <- c(0,1e7,2e7,3e7,5e7,7.5e7,1e8,1.5e8,2e8)
gbdtks <- c(0,1e7,2e7,3e7,5e7,7.5e7,1e8,1.5e8,2e8)/1e6

maj_gbd <- function(nb){
x <- nb
if(trunc(x)==x) return(x)
else return(trunc(x) + 1)
#k <- 0
#while(x>=100){
#x <- x%/%10
#k <- k+1
#}
#res <- (x+1)*10^k
#res
}

GBDtcks <- do.call('rbind',lapply(regs,function(x){
data.frame(breaks=
unique(DPLOT$ymin[DPLOT$metr=="GBD" & DPLOT$reg==x]) -
c(gbdtks[2:findInterval(max(D$GBD[D$Region==x]),gbdtks)],maj_gbd(max(D$GBD[D$Region==x])))*max(D$RCTs_sim)/max(D$GBD),
labels=c(gbdtks[2:findInterval(max(D$GBD[D$Region==x]),gbdtks)],maj_gbd(max(D$GBD[D$Region==x]))))
}))
GBDtcks$col <- "GBD"
#Changing 1.3e8 par 1.25e8 pour SSA
#GBDtcks[GBDtcks$labels==1.3e8,1] <- GBDtcks[GBDtcks$labels==1.3e8,1] + (0.05e8)*max(D$RCT)/max(D$GBD)
#GBDtcks[GBDtcks$labels==1.3e8,2] <- GBDtcks[GBDtcks$labels==1.3e8,2] - (0.05e8)

#Changing 1.4e8 par 1.3e8 pour SA
#GBDtcks[GBDtcks$labels==1.4e8,1] <- GBDtcks[GBDtcks$labels==1.4e8,1] + (0.1e8)*max(D$RCT)/max(D$GBD)
#GBDtcks[GBDtcks$labels==1.4e8,2] <- GBDtcks[GBDtcks$labels==1.4e8,2] - (0.1e8)
GBDtcks$labels <- as.character(GBDtcks$label)
#GBDtcks$labels <- gsub("0","",GBDtcks$label)

tcks <- rbind(RCTtcks,GBDtcks)
tcks$col <- as.factor(tcks$col)

#We suppress diseases for which Nb RCT simulation was not OK
DPLOT[DPLOT$dis%in%c("Sexually transmitted diseases excluding HIV","Leprosy","Hemoglobinopathies and hemolytic anemias","Congenital anomalies","Sudden infant death syndrome") & DPLOT$metr=="RCT",c(2:6)] <- NA

    
#GGPLOT OBJECT
####################
p <- ggplot(DPLOT) + geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=metr,label=dis)) +
 geom_text(aes(x=xcent,
        y=ycent,
        label=dis_nb,
        hjust=0.5),
size=DPLOT$size_dis_lab,col="#42442E")

    p <- p + geom_segment(aes(x=(xmin+xmax)/2,xend=(xmin+xmax)/2,y=ymax,yend=ymax_95),data=DPLOT[DPLOT$metr=="RCT",])
    p <- p + geom_segment(aes(x=xmin+wdt/2,xend=xmax-wdt/2,y=ymax_95,yend=ymax_95),data=DPLOT[DPLOT$metr=="RCT",])

p <- p + theme_minimal()
p <-p+theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank()
    )

#Y axis
p <- p+ scale_y_continuous(minor_breaks = tcks$breaks, breaks=tcks$breaks,
#                     labels=tcks$labels,limits=c(min(DPLOT$ymax)-d_reg/2,max(DPLOT$ymax)+d_reg/2))
                      labels=rep("",nrow(tcks)),limits=c(min(DPLOT$ymax_95)-d_reg/2,max(DPLOT$ymax_95)+d_reg/2))
#p <- p + theme(axis.text.x = element_text(size=7,colour=tcks$col))
#p <- p + theme(panel.grid.minor=element_line(color="grey"))
#p <- p + theme(panel.grid.major=element_line(color="black"))
p <- p + theme(panel.grid.minor=element_line(color="#D3D3D3",size=0.1))    
    
p <- p+geom_text(
       aes(x=length(tt_gbd)+IC+1.5,
        y=breaks,
        label=labels,
	hjust=0.5),
    data=tcks,
    size=2,
    col=as.numeric(tcks$col))

#X axis
p <- p +  scale_x_continuous(breaks = NULL,limits = c(0,max(DPLOT$xmax)+3))

#Region labels
    p<-p+geom_text(
      aes(x=length(tt_gbd)+IC+3,
#       aes(x=Inf,
        y=xmin,
        label=label,
        angle=angle,
	hjust=0.5,vjust=0),
    data=familyLabelsDF,
    size=4.3)

#Taking_out legend
p <- p + theme(legend.position = "none")

#POLAR COORDINATES
p <- p + coord_polar(theta="y",start=alphaStart,direction=-1)

x11(width=12,height=12)

ggsave(filename = "Figures/polar_absolute_numbers_simulation_new.pdf")
dev.off()


#################################################################################################
####################################################################################################
#PROPORTIONS PLOT
########################

dpl <- D
#Pas besoin de normaliser RCTs et GBD

dpl$Pr_GBD <- 100*dpl$Prp_gbd
dpl$Pr_RCTs_sim <- 100*dpl$SimMed_PrRCTs
dpl$Pr_RCTs_sim95 <- 100*dpl$Sim95up_PrRCTs
    
#Taille barres = wdt*2
wdt <- 0.45
#Distance entre régions
d_reg <- 5
esp_dis_nb <- 1.2

#Rectangles pour une maladie et région donnée
displt <- 
function(dis,Rg,rg){
data.frame(dis=dis,	xmin = rep(which(dis==names(tt_gbd))-wdt,2),
			xmax = rep(which(dis==names(tt_gbd))+wdt,2),
			ymin = c(Rg+esp_dis_nb,Rg-esp_dis_nb),
			ymax = c(Rg+esp_dis_nb+dpl$Pr_RCTs_sim[dpl$Dis==dis & dpl$Region==rg],Rg-esp_dis_nb-dpl$Pr_GBD[dpl$Dis==dis & dpl$Region==rg]),
            ymax_95 = c(Rg+esp_dis_nb+dpl$Pr_RCTs_sim95[dpl$Dis==dis & dpl$Region==rg],Rg-esp_dis_nb-dpl$gpl[dpl$Dis==dis & dpl$Region==rg]),
			metr = c("RCT","GBD"),
			reg = rg, ycent = Rg, dis_nb = which(dis==names(tt_gbd)))
}

#Rectangles pour toutes les maladies, une région donnée
regplt <- function(Rg,rg) do.call('rbind',lapply(names(tt_gbd),function(x){displt(x,Rg,rg)}))

#Emplacement des régions
RG <- 0
for(i in 2:length(regs)){
RG <- c(RG,RG[i-1]-(2*esp_dis_nb+max(dpl$Pr_GBD[dpl$Region==regs[i-1]])+d_reg+max(dpl$Pr_RCTs_sim95[dpl$Region==regs[i]])))
}

#DataFrame Plot
DPLOT <- do.call('rbind',lapply(1:length(regs),function(i){regplt(RG[i],regs[i])}))

#Inner circle
IC <- 8
DPLOT$xmin <- DPLOT$xmin + IC
DPLOT$xmax <- DPLOT$xmax + IC
DPLOT$xcent <- DPLOT$dis_nb + IC

#REGION LABELS
#################
totalLength <- max(DPLOT$ymax)-min(DPLOT$ymax)+d_reg
alphaStart <- 2*pi*(max(dpl$Pr_RCTs_sim95[dpl$Region==names(tt_rcts)[1]]+d_reg/2)/totalLength)

  readableAngle<-function(x){
    angle<-x*(360/totalLength)
#    angle+ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,180,0)
  }
    familyLabelsDF<-data.frame(xmin=RG,label=reg_labs)
    familyLabelsDF$angle <- readableAngle(familyLabelsDF$xmin)

#Disease labels
###################
DPLOT$size_dis_lab = 2.3*(40+DPLOT$dis_nb)/(40+max(DPLOT$dis_nb))

#Tick marks
####################
#Pour RCTs
rcttks <- c(0,1,5,10,15,20,25,30)
    
#Faire que les ticks aillent jusqu'au max des RCTs arrondi au sup
RCTtcks <- do.call('rbind',lapply(regs,function(x){
data.frame(breaks=
unique(DPLOT$ymin[DPLOT$metr=="RCT" & DPLOT$reg==x]) +
rcttks[1+1:findInterval(max(dpl$Pr_RCTs_sim95[dpl$Region==x]),rcttks)],
labels=rcttks[1+1:findInterval(max(dpl$Pr_RCTs_sim95[dpl$Region==x]),rcttks)],reg=x)
}))
RCTtcks$col <- "1RCT"
#RCTtcks <- RCTtcks[!(RCTtcks$reg=="Sub.Saharian.Africa" & RCTtcks$label==20),]
#RCTtcks <- RCTtcks[!(RCTtcks$reg=="Latin.America.and.Caribbean" & RCTtcks$label==20),]
#RCTtcks <- RCTtcks[!(RCTtcks$reg=="Central.Europe..Eastern.Europe..and.Central.Asia" & RCTtcks$label==25),]
#RCTtcks <- RCTtcks[!(RCTtcks$reg=="High.income" & RCTtcks$label==20),]
RCTtcks$labels <- as.character(RCTtcks$label)

#Pour GBD
gbdtks <- c(0,1,5,10,15,20,25,30,35)
GBDtcks <- do.call('rbind',lapply(regs,function(x){
data.frame(breaks=
unique(DPLOT$ymin[DPLOT$metr=="GBD" & DPLOT$reg==x]) -
gbdtks[1+1:findInterval(max(dpl$Pr_GBD[dpl$Region==x]),gbdtks)],
labels=gbdtks[1+1:findInterval(max(dpl$Pr_GBD[dpl$Region==x]),gbdtks)],reg=x)
}))
GBDtcks$col <- "GBD"
#GBDtcks[GBDtcks$reg=="Southeast.Asia..East.Asia.and.Oceania" & GBDtcks$label==20,c(1,2)] <- GBDtcks[GBDtcks$reg=="Southeast.Asia..East.Asia.and.Oceania" & GBDtcks$label==20,c(1,2)] - c(-2,2)
#GBDtcks[GBDtcks$reg=="South.Asia" & GBDtcks$label==25,c(1,2)] <- GBDtcks[GBDtcks$reg=="South.Asia" & GBDtcks$label==25,c(1,2)] - c(-3,3)
#GBDtcks[GBDtcks$reg=="Latin.America.and.Caribbean" & GBDtcks$label==15,c(1,2)] <- GBDtcks[GBDtcks$reg=="Latin.America.and.Caribbean" & GBDtcks$label==15,c(1,2)] - c(-2,2)
#GBDtcks[GBDtcks$reg=="Sub.Saharian.Africa" & GBDtcks$label==25,c(1,2)] <- GBDtcks[GBDtcks$reg=="Sub.Saharian.Africa" & GBDtcks$label==25,c(1,2)] - c(-2,2)
GBDtcks$labels <- as.character(GBDtcks$label)

tcks <- rbind(RCTtcks,GBDtcks)
tcks$col <- as.factor(tcks$col)

#We suppress diseases for which Nb RCT simulation was not OK
DPLOT[DPLOT$dis%in%c("Sexually transmitted diseases excluding HIV","Leprosy","Hemoglobinopathies and hemolytic anemias","Congenital anomalies","Sudden infant death syndrome") & DPLOT$metr=="RCT",c(2:6)] <- NA

#Gaps
DGaps <- DPLOT[paste(DPLOT$reg,DPLOT$dis)%in%paste(dpl$Region[dpl$GAP & dpl$Pr_GBD>=1],dpl$Disease[dpl$GAP &  dpl$Pr_GBD>=1]),]
DGaps <- DGaps[!DGaps$dis%in%c("Sexually transmitted diseases excluding HIV","Leprosy","Hemoglobinopathies and hemolytic anemias","Congenital anomalies","Sudden infant death syndrome"),]
DGaps$gap_shape <- paste(DGaps$reg,DGaps$dis)%in%paste(dpl$Region[dpl$GAP & dpl$Pr_GBD>=5],dpl$Disease[dpl$GAP &  dpl$Pr_GBD>=5])
DGaps <- DGaps[DGaps$metr=="GBD",]
DGaps$gap_shape <- factor(DGaps$gap_shape)
    
#GGPLOT OBJECT
####################
DPLOT$shape <- DPLOT$dis_nb
DPLOT$shape[DPLOT$shape==27] <- 0
DPLOT$shape[DPLOT$shape==26] <- 32
DPLOT$shape <- as.factor(DPLOT$shape)

p <- ggplot(DPLOT) + geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=metr,label=dis)) +
 geom_text(aes(x=xcent,
        y=ycent,
        label=dis_nb,
	hjust=0.5),
size=DPLOT$size_dis_lab,col="#42442E") 

    p <- p + geom_segment(aes(x=(xmin+xmax)/2,xend=(xmin+xmax)/2,y=ymax,yend=ymax_95),data=DPLOT[DPLOT$metr=="RCT",])
    p <- p + geom_segment(aes(x=xmin+wdt/2,xend=xmax-wdt/2,y=ymax_95,yend=ymax_95),data=DPLOT[DPLOT$metr=="RCT",])

    p <- p + geom_point(data=DGaps,aes(x=(xmin+xmax)/2,y=ymax - 2,shape=gap_shape))
    
p <- p + theme_minimal()
p <-p+theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank()
    )

#Y axis
p <- p+ scale_y_continuous(minor_breaks = tcks$breaks, breaks=tcks$breaks,
#                     labels=tcks$labels,limits=c(min(DPLOT$ymax)-d_reg/2,max(DPLOT$ymax)+d_reg/2))
                      labels=rep("",nrow(tcks)),limits=c(min(DPLOT$ymax)-d_reg/2,max(DPLOT$ymax)+d_reg/2))
#p <- p + theme(axis.text.x = element_text(size=7,colour=tcks$col))
p <- p + theme(panel.grid.minor=element_line(color="#D3D3D3",size=0.1))    

p <- p+geom_text(
       aes(x=length(tt_gbd)+IC+1.5,
        y=breaks,
        label=labels,
	hjust=0.5),
    data=tcks,
    size=2,
    col=as.numeric(tcks$col))

#X axis
p <- p +  scale_x_continuous(breaks = NULL,limits = c(0,max(DPLOT$xmax)+3))

#Region labels
    p<-p+geom_text(
      aes(x=length(tt_gbd)+IC+3,
#       aes(x=Inf,
        y=xmin,
        label=label,
        angle=angle,
	hjust=0.5,vjust=0),
    data=familyLabelsDF,
    size=4.5)


#POLAR COORDINATES
p <- p + coord_polar(theta="y",start=alphaStart,direction=-1)

p <- p + theme(legend.position = "none")

x11(width=12,height=12)

ggsave(filename = "Figures/polar_proportion_simulation_new.pdf")

dev.off()

