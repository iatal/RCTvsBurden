
#For annotating with ratio of alignment
form_ratio <- function(x){
paste(format(round(x[2]),nsmall = 0),
           "% [",format(round(x[1]),nsmall = 0),
           "-",format(round(x[3]),nsmall = 0),"]",sep="")
    }


Within_Region_plot <- function(metr_burden = "daly",
                               metr_res="RCTs",
                               region = "High-income"){
    r <- region
    reg_lab <- unique(DT$regs_lab[DT$Region==r])


    #Max plot: maximum of local proportions

    max_plot <- 10*ceiling(max(DT[DT$Region==r & DT$Disease!="All",
                                  c(intersect(grep(metr_burden,names(DT)),
                                              grep("Prop_loc_burden",names(DT))),
                                    intersect(grep(metr_res,names(DT)),
                                              grep("Prop_loc_",names(DT)))),]/10,na.rm=TRUE))

    dtp <- DT[DT$Region==r & DT$Disease!="All",c(which(names(DT)%in%c("Disease","Dis_lab")),
                        intersect(grep(metr_burden,names(DT)),grep("Prop_loc_burden",names(DT))),
                        intersect(grep(metr_res,names(DT)),grep("Prop_loc_",names(DT)))),]
    
    dfb <- dtp[,c(1,2,3)]
    names(dfb) <- c("Disease","Dis_lab","prop")
    dfb$mes <- "burden"
    dfr <- dtp[,c(1,2,5)]
    names(dfr) <- c("Disease","Dis_lab","prop")
    dfr$mes <- "research"
    dt <- rbind(dfb,dfr)

    dfre <- dtp[,c(1,2,4,6)]
    names(dfre) <- c("Disease","Dis_lab","prop_low","prop_up")
    dfre$mes <- "research"
    
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
                              )

    p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                name=paste("Local share across groups of diseases\nof ",
                                           ifelse(metr_burden=="death",metr_burden,toupper(metr_burden)),
                                           " and ",
                                           metr_res,
                                           " (%)",sep="")
                                )


    p <- p + ggtitle(reg_lab)
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

    if(r!="All"){
    p <- p + annotate("text",x=(length(diss)+1)/2,y=max_plot-max_plot/10,label=
            paste("R =",
             form_ratio(ratio_align[ratio_align$Region==r & ratio_align$metr==metr_burden,paste(metr_res,"nogap",sep="_")]))
            )
        }
    
    return(p)
    }

######################################################################################################"
Within_2Regions_plot <- function(metr_burden = "daly",
                                 metr_res="RCTs",
                                 region1 = "High-income",
                                 region2 = "Sub-Saharian Africa"){
    
    max_plot <- 10*ceiling(max(DT[DT$Region%in%c(region1,region2) & DT$Disease!="All",
                                  c(intersect(grep(metr_burden,names(DT)),
                                              grep("Prop_loc_burden",names(DT))),
                                    intersect(grep(metr_res,names(DT)),
                                              grep("Prop_loc_",names(DT)))),]/10,na.rm=TRUE))

    #First region
    r <- region1
    reg_lab <- unique(DT$regs_lab[DT$Region==r])

    dtp <- DT[DT$Region==r & DT$Disease!="All",c(which(names(DT)%in%c("Disease","Dis_lab")),
                        intersect(grep(metr_burden,names(DT)),grep("Prop_loc_burden",names(DT))),
                        intersect(grep(metr_res,names(DT)),grep("Prop_loc_",names(DT)))),]
    
    dfb <- dtp[,c(1,2,3)]
    names(dfb) <- c("Disease","Dis_lab","prop")
    dfb$mes <- "burden"
    dfr <- dtp[,c(1,2,5)]
    names(dfr) <- c("Disease","Dis_lab","prop")
    dfr$mes <- "research"
    dt <- rbind(dfb,dfr)

    dfre <- dtp[,c(1,2,4,6)]
    names(dfre) <- c("Disease","Dis_lab","prop_low","prop_up")
    dfre$mes <- "research"
    
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
                              )

    p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                name=paste("Local share across groups of diseases\nof ",
                                           ifelse(metr_burden=="death",metr_burden,toupper(metr_burden)),
                                           " and ",
                                           metr_res,
                                           " (%)",sep="")
                                )


    p <- p + ggtitle(reg_lab)
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

    if(r!="All"){
    p <- p + annotate("text",x=(length(diss)+1)/2,y=max_plot-max_plot/10,label=
            paste("R =",
             form_ratio(ratio_align[ratio_align$Region==r & ratio_align$metr==metr_burden,paste(metr_res,"nogap",sep="_")]))
            )
        }

    p1 <- p
    
    #Second region
        #First region
    r <- region2
    reg_lab <- unique(DT$regs_lab[DT$Region==r])

    dtp <- DT[DT$Region==r & DT$Disease!="All",c(which(names(DT)%in%c("Disease","Dis_lab")),
                        intersect(grep(metr_burden,names(DT)),grep("Prop_loc_burden",names(DT))),
                        intersect(grep(metr_res,names(DT)),grep("Prop_loc_",names(DT)))),]
    
    dfb <- dtp[,c(1,2,3)]
    names(dfb) <- c("Disease","Dis_lab","prop")
    dfb$mes <- "burden"
    dfr <- dtp[,c(1,2,5)]
    names(dfr) <- c("Disease","Dis_lab","prop")
    dfr$mes <- "research"
    dt <- rbind(dfb,dfr)

    dfre <- dtp[,c(1,2,4,6)]
    names(dfre) <- c("Disease","Dis_lab","prop_low","prop_up")
    dfre$mes <- "research"
    
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
                              )

    p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                name=paste("\n"))

    p <- p + ggtitle(reg_lab)
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

    if(r!="All"){
    p <- p + annotate("text",x=(length(diss)+1)/2,y=max_plot-max_plot/10,label=
            paste("R =",
             form_ratio(ratio_align[ratio_align$Region==r & ratio_align$metr==metr_burden,paste(metr_res,"nogap",sep="_")]))
            )
        }

    return(list(p1,p))
    }
