library(gdata)
library(ggplot2)

GBD <- read.table("../Data/DALY_YLL_deaths_per_region_and_27_diseases_2005.txt")
GBD <- GBD[order(GBD$Region,GBD$Disease),]
RCT <- read.table("../Data/RCTs_and_Patients_Nb_local_prop_median_UI_per_region_and_disease.txt")
RCT <- RCT[order(RCT$Region,RCT$Disease),]

#Disease labels for plot
GBD$Dis_lab <- GBD$Disease
levels(GBD$Dis_lab) <- c("",
               "Cardiovasc.\nand circulatory",
               "Chronic\nrespiratory",
               "Cirrhosis",
               "Congenital\nanomalies",
               "Diabetes, urinary\nmale infertility",
               "Common\ninfections",
               "Digestive",
               "Gynecol.",
               "Hemoglob. and\nhemolytic anemia",
               "Hepatitis",
               "HIV",
               "Leprosy",
               "Malaria",
               "Maternal\ndisorders",
               "Mental and\nbehavioral",
               "Musculosk.",
               "Neglected trop.",
               "Neonatal\ndisorders",
               "Neoplasms",
               "Neurological",
               "Nutritional",
               "Oral",
               "Sense organ",
               "STD",
               "Skin and\nsubcutaneous",
               "Sudden infant death",
               "Tuberculosis")

RCT$Dis_lab <- RCT$Disease
levels(RCT$Dis_lab) <- c("",
unique(as.character(GBD$Dis_lab))[levels(droplevels(GBD$Disease[GBD$Dis_lab!=""]))%in%levels(RCT$Disease)])

#Order of regions: decreasing nb of RCTs
rt <- RCT[RCT$Disease=="All" & !RCT$Region%in%c("All","Non-HI"),]
regs <- as.character(rt$Region[order(rt$Nb_RCTs_med,decreasing=TRUE)])


regional_plot <- function(region,
                          metric_burden="daly",
                          metric_dis="RCTs"){
    
    if(length(metric_dis)!=1 | 
       !metric_dis%in%c("RCTs","Patients")) stop("metric_dis must be RCTs of Patients")
    if(length(metric_burden)!=1 | 
       !metric_burden%in%c("daly","death","yld","yll")) stop("metric_burden must be daly, death, yld, or yll")
    
    r <- region
    #All burden in region r
    burd_tot <- GBD$burden[GBD$metr==metric_burden & GBD$Region==r & GBD$Disease=="all"]
    #All research
    res_tot <- RCT[RCT$Region==r & RCT$Dis=="All",]   

    #Local burden across diseases
    dfb <- GBD[GBD$metr==metric_burden & GBD$Region==r & GBD$Disease!="all",]
    dfb$prop <- 100*dfb$burden/burd_tot
    dfb$mes <- "burden"
    dfb <- dfb[,c("Disease","prop","mes","Dis_lab")] 

    #Local research across diseases
    dfr <- RCT[RCT$Region==r & RCT$Disease!="All",]
    if(metric_dis=="RCTs") dfr$prop <- dfr$Prop_RCTs_med
    else dfr$prop <- dfr$Prop_Patients_med
        
    dfr$mes <- "research"
    dfr <- dfr[,c("Disease","prop","mes","Dis_lab")]
    dt <- rbind(dfb,dfr)    

    #Uncertainty intervals for local research
    dfre <- RCT[RCT$Region==r & RCT$Disease!="All",]
    if(metric_dis=="RCTs"){ dfre$prop_low <- dfre$Prop_RCTs_low
    dfre$prop_up <- dfre$Prop_RCTs_up}
    else { dfre$prop_low <- dfre$Prop_Patients_low
    dfre$prop_up <- dfre$Prop_Patients_up}
    dfre$mes <- "research"
    dfre <- dfre[,c("Disease","prop_low","prop_up","mes")]    
    
    #10 Principales maladies
    diss <- dfb$Disease[order(dfb$prop,decreasing=TRUE)][1:10]
    dt <- droplevels(dt[dt$Disease%in%diss,])
    dfre <- droplevels(dfre[dfre$Disease%in%diss,])
    #Order of diseases
    dt$Disease <- reorder(dt$Disease,new.order=as.character(diss))
    dfre$Disease <- reorder(dfre$Disease,new.order=as.character(diss))
    
    #Highlighting GAPS
    dt$gap <- dt$Disease
    levels(dt$gap) <- unlist(lapply(levels(dt$Disease),
                                    function(x){
                                        ifelse(sum(dfre$Disease==x)==0,NA,
                                        dt$prop[dt$mes=="burden" & dt$Disease==x]/
                                        dfre$prop_up[dfre$Disease==x])}))
    dt$gap_col <- dt$mes
    dt$gap_col[as.numeric(as.character(dt$gap))>=2 & dt$gap_col=="burden"] <- "burden_gap"
    
    dlbl <- dt[order(dt$Disease),]
    dlbl <- dlbl[dlbl$mes=="burden",]
    dlbl$gap_text <- "plain"
    dlbl$gap_text[dlbl$gap_col=="burden_gap"] <- "bold"

    #GGPLOT Object
    p <- ggplot(dt,aes(Disease))
    p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
    p <- p + geom_segment(aes(x=as.numeric(Disease)-0.4,xend=as.numeric(Disease)+0.4,y=prop/2,yend=prop/2,size=5),linetype="dashed",data=dt[dt$mes=="burden",],lwd=0.1)
    p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=prop_up,ymin=prop_low),width=0.2,data=dfre)
    p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red","research"="blue"))

    p <- p + scale_x_discrete(label = dlbl$Dis_lab
#                              ,name=paste("10 Groups of diseases causing highest burden in",r,sep="\n")
                              )

    p <- p + scale_y_continuous(limits = c(0,40),breaks=c(0,5,seq(10,40,10)),
#                                name=paste("Local proportion (%) of burden and research in",r,sep="\n")
                                name=paste("Local proportion (%)\nof burden and research")
                                )

    p <- p + ggtitle(r)
    p <- p + theme( axis.text.x = element_text(
                    face=dlbl$gap_text,
                    angle=55,
                    hjust=1,
                    vjust=1.1,size=8)) +
             theme(panel.background = element_blank()) + 
             theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
             theme(panel.grid.major.x = element_blank()) + 
             theme(axis.ticks.x=element_blank()) + 
             theme(legend.position = "none") + 
             theme(axis.title.x=element_blank())


}






