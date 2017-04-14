#setwd("Desktop/Programs GBD/RCTvsBurden/Interactive_figure/")

library(shiny)
library(gdata)
library(ggplot2)
library(grid)
library(gridExtra)

DT <- read.table("./data/data.txt")

#input <- list(tabs="diseases",region="World",metr_burden="DALY",metr_res="RCTs",Nb_dis=10,
#all_nhi = "All regions",disease="All diseases",metr_burden2="DALY",metr_res2="RCTs",
#region1="High-income countries",region2="World",metr_burden_comp="DALY",metr_res_comp="RCTs",Nb_dis_comp=10)
#runApp(display.mode="showcase")

DT$regs_lab <- reorder(DT$regs_lab,new.order=c("World",
                                               "High-income countries",
                                               "Non-high-income countries",
                                               "Eastern Europe and Central Asia",
                                               "Latin America and Caribbean",
                                               "North Africa and Middle East",
                                               "South Asia",
                                               "Southeast Asia, East Asia and Oceania",
                                               "Sub-Saharian Africa"))
                       
dinit_reg <- DT[DT$Disease!="All",-c(grep("glob",names(DT)),grep("NHI",names(DT)))]
dinit_dis <- droplevels(DT[DT$Region!="All" & DT$Region!="Non-HI",-grep("loc",names(DT))])
levels(dinit_dis$regs_lab) <- c("High-income",
                                "Eastern Europe\nCentral Asia",
                                "Latin America",
                                "North Africa\nMiddle East",
                                "South Asia",
                                "Southeast Asia\nOceania",
                                "Sub-Saharian\nAfrica")

#For numbers in tooltip
form_ratio <- function(x,pourc=TRUE){
    if(is.na(x[1])) return(NA)
        else {
        paste(format(round(x[2]),nsmall = 0,big.mark=","),
               ifelse(pourc==TRUE,"% ["," ["),format(round(x[1]),nsmall = 0,big.mark=","),
               " - ",format(round(x[3]),nsmall = 0,big.mark=","),"]",sep="")
            }
    }


#User interface
#############################################################################################################
ui <- fluidPage(
    
    tags$a(href="http://www.theclinic.cl","The Clinic"),

    tags$hr(),
    
    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
    sidebarLayout(
        mainPanel(
            conditionalPanel(condition = "input.tabs == 'regions'",             
                             # this is an extra div used ONLY to create positioned ancestor for tooltip
                             # we don't change its position
                             div(style = "position:relative",
                                 plotOutput("plot_within_reg",# height = "100px",
                                            hover = hoverOpts("plot_hover", delay = 5, delayType = "debounce")),
                                 uiOutput("hover_info_regs")
                                 )
                             ),                        
            conditionalPanel(condition = "input.tabs == 'diseases'",      
                             # this is an extra div used ONLY to create positioned ancestor for tooltip
                             # we don't change its position
                             div(style = "position:relative",
                                 plotOutput("plot_within_dis", 
                                            hover = hoverOpts("plot_hover", delay = 5, delayType = "debounce")),
                                 uiOutput("hover_info_dis")
                                 )
                             ),
            conditionalPanel(condition = "input.tabs == 'compare' & input.across== 'Regions'",
                             plotOutput("plot_comp_reg1"),
                             plotOutput("plot_comp_reg2")
                            )
            
            ),
        
        sidebarPanel(
            
            tabsetPanel(id="tabs",
                tabPanel("Within regions",value="regions",
                    selectInput(inputId = "region",
                                label= "Elige una region",
                                choices = levels(DT$regs_lab),
                                selected = "World"),
                    splitLayout(
                                selectInput(inputId = "metr_burden",
                                            label= "Elige una metric pal burden",
                                            choices = c("DALY","YLL","YLD","Death"),
                                            selected = "DALY"),
                                selectInput(inputId = "metr_res",
                                            label= "Elige una metric pa la research",
                                            choices = c("RCTs","Patients"),
                                            selected = "RCTs")),
                    numericInput(inputId = "Nb_dis",
                                 label="Number of diseases",
                                 value = 10,
                                 min = 1,
                                 max = 27,
                                 step = 1),
                    checkboxInput(inputId = "hgp_reg",
                                  label="Highlight gaps",
                                  value=FALSE),
                    downloadButton('downloadPlot_within_reg', 'Download Plot')
                         ),
                tabPanel("Within diseases",value="diseases",
#                    splitLayout(
#                        actionButton(inputId = "allregs",label= "All regions"),
#                        actionButton(inputId = "nhionly",label= "Non-high-income only")
#                        ),
                    selectInput(inputId = "disease",
                                label= "Elige una maladie",
                                choices = levels(DT$Dis_tooltip),
                                selected = "All diseases"),
                    selectInput(inputId = "all_nhi",
                                label= "Across:",
                                choices = c("All regions","Non-high-income only"),
                                selected = "All regions"),
                    splitLayout(
                                selectInput(inputId = "metr_burden2",
                                            label= "Elige una metric pal burden",
                                            choices = c("DALY","YLL","YLD","Death"),
                                            selected = "DALY"),
                                selectInput(inputId = "metr_res2",
                                            label= "Elige una metric pa la research",
                                            choices = c("RCTs","Patients"),
                                            selected = "RCTs")),
                    checkboxInput(inputId = "hgp_dis",
                                  label="Highlight gaps",
                                  value=FALSE),
                    downloadButton('downloadPlot_within_dis', 'Download Plot')
                         ),
                 tabPanel("Compare",value="compare",
                    selectInput(inputId = "across",
                                label= "Across:",
                                choices = c("Regions","Diseases"),
                                selected = "Regions"),
                    conditionalPanel(condition = "input.tabs == 'compare' & input.across == 'Regions'",
                                     splitLayout(
                                         selectInput(inputId = "region1",
                                                 label= "Elige una region",
                                                 choices = levels(DT$regs_lab),
                                                 selected = "High-income countries"),
                                         selectInput(inputId = "region2",
                                                 label= "Elige una region",
                                                 choices = levels(DT$regs_lab),
                                                 selected = "Non-high-income countries")
                                         ),
                                     splitLayout(
                                         selectInput(inputId = "metr_burden_comp",
                                                 label= "Elige una metric pal burden",
                                                 choices = c("DALY","YLL","YLD","Death"),
                                                 selected = "DALY"),
                                         selectInput(inputId = "metr_res_comp",
                                                 label= "Elige una metric pa la research",
                                                 choices = c("RCTs","Patients"),
                                                 selected = "RCTs")),
                                     numericInput(inputId = "Nb_dis_comp",
                                                  label="Number of diseases",
                                                  value = 10,
                                                  min = 1,
                                                  max = 27,
                                                  step = 1),                                     
                                     checkboxInput(inputId = "hgp_comp_reg",
                                                   label="Highlight gaps",
                                                   value=FALSE),
                                     downloadButton('downloadPlot_comp_reg', 'Download Plot')
                                     ),
                     conditionalPanel(condition = "input.tabs == 'compare' & input.across == 'Diseases'",
                                      splitLayout(
                                         selectInput(inputId = "disease1",
                                                     label= "Elige una maladie",
                                                     choices = levels(DT$Dis_tooltip),
                                                     selected = "HIV"),
                                         selectInput(inputId = "disease2",
                                                     label= "Elige una maladie",
                                                     choices = levels(DT$Dis_tooltip),
                                                     selected = "Malaria")   
                                                 ),
                                      splitLayout(
                                          selectInput(inputId = "metr_burden_comp_dis",
                                                  label= "Elige una metric pal burden",
                                                  choices = c("DALY","YLL","YLD","Death"),
                                                  selected = "DALY"),
                                      selectInput(inputId = "metr_res_comp_dis",
                                                  label= "Elige una metric pa la research",
                                                  choices = c("RCTs","Patients"),
                                                  selected = "RCTs")),
                                      checkboxInput(inputId = "hgp_comp_dis",
                                                    label="Highlight gaps",
                                                    value=FALSE),
                                      downloadButton('downloadPlot_comp_dis', 'Download Plot')
                                      
                                      )

                          )
                        )#end tabset panel
            )#end sidebar panel
        )#end sidebar layout
    )#end fluid page

#Server
##################################################################################################################

server <- function(input,output){

    #Server functions Within regions
    ###########################################
    
    #subset according to region
    dreg <- reactive({
        dinit_reg[dinit_reg$regs_lab==input$region,-which(names(dinit_reg)%in%c("regs_lab","Region"))]
    })

    #subset according to burden metric
    dburden <- reactive({
        dfb <- dreg()[,c(which(names(dreg())%in%c("Disease","Dis_lab","Dis_tooltip")),
                                grep(tolower(input$metr_burden),names(dreg())))]
        names(dfb) <- c("Disease","Dis_lab","Dis_tooltip","Nb","prop")
        dfb$metr <- "burden"
        dfb
    })

    #subset according to research metric
    dres <- reactive({
        dfr <- dreg()[,c(which(names(dreg())%in%c("Disease","Dis_lab","Dis_tooltip")),
                         grep(input$metr_res,names(dreg())))]
        names(dfr) <- c("Disease","Dis_lab","Dis_tooltip","Nb_low","Nb_med","Nb_up","prop_low","prop_med","prop_up")
        dfr
    })

    #Choose the Nb_dis highest diseases
    diss <- reactive({dburden()$Disease[order(dburden()$prop,decreasing=TRUE)][1:input$Nb_dis]})
    
    #data for plot
    data_plot <- reactive({
    
        dfr_med <- dres()[,c("Disease","Dis_lab","Nb_med","prop_med")]
        names(dfr_med) <- c("Disease","Dis_lab","Nb","prop")
        dfr_med$metr <- "research"        
        dt <- rbind(dburden()[,c("Disease","Dis_lab","Nb","prop","metr")],dfr_med)
        dt <- droplevels(dt[dt$Disease%in%diss(),])
        dt$Disease <- reorder(dt$Disease,new.order=as.character(diss()))
        dt
        
    })
    
    #data for bar errors
    dfr_err <- reactive({
    
        d_err <- dres()[,c("Disease","Dis_lab","prop_low","prop_up")]
        d_err$metr <- "research"        
        d_err <- droplevels(d_err[d_err$Disease%in%diss(),])
        d_err$Disease <- reorder(d_err$Disease,new.order=as.character(diss()))
        d_err
    
    })
    
    #data for hover
    data_points <- reactive({
    
        dpts <- merge(dburden(),dres())
        dpts <- droplevels(dpts[dpts$Disease%in%diss(),])
        dpts$Disease <- reorder(dpts$Disease,new.order=as.character(diss()))
        dpts$Dis_pos <- as.numeric(dpts$Disease)
        
        #burden points
        dpts_bur <- dpts
        dpts_bur$prop_pos <- dpts_bur$prop
        dpts_bur$Dis_pos <- dpts_bur$Dis_pos-0.2
        D_bur <- do.call('rbind',
        lapply(1:nrow(dpts_bur),function(x){
            tt <- trunc(dpts_bur$prop_pos[x])
            dtp <- dpts_bur[rep(x,tt+2),]
            dtp$prop_pos <- c(0:tt,dpts_bur$prop_pos[x])
            dtp
        }))
        
        #research points
        dpts_res <- dpts[!is.na(dpts$prop_med),]
        dpts_res$prop_pos <- dpts_res$prop_med
        dpts_res$Dis_pos <- dpts_res$Dis_pos+0.2
        D_res <- do.call('rbind',
        lapply(1:nrow(dpts_res),function(x){
            tt <- trunc(dpts_res$prop_pos[x])
            dtp <- dpts_res[rep(x,tt+2),]
            dtp$prop_pos <- c(0:tt,dpts_res$prop_pos[x])
            dtp
        }))
        
        rbind(D_bur,D_res)
        
    })
    
    #Hover info
    output$hover_info_regs <- renderUI({
        
        hover <- input$plot_hover
        
        point <- nearPoints(data_points(), hover, yvar="prop_pos", xvar="Dis_pos",
                            threshold = (1005*0.2/(input$Nb_dis + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<b> Region: </b>", input$region, "<br/>",
                                   "<b> Group of diseases: </b>", point$Dis_tooltip, "<br/>",
                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
                                   " million ", input$metr_burden,"s (",format(round(point$prop),nsmall = 0),
                                   "% of ",ifelse(input$region=="World","global","local")," burden)<br/>",
                                   "<b> Research: </b>", 
                                   ifelse(input$metr_res=="RCTs",
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
                                                            pourc=FALSE)," ",input$metr_res),
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")] /1e3,
                                                            pourc=FALSE)," thousand patients")),
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],pourc=TRUE), " of ",
                                   ifelse(input$region=="World","global","local")," research)<br/>")))
                    )
        })

    #Plot
    plotInput_within_reg <- reactive({

            dt <- data_plot()
            max_plot <- 10*ceiling(max(c(dt$prop,dfr_err()$prop_up)/10,na.rm=TRUE))

            #Highlighting gaps
            dt$gap <- dt$Disease
            levels(dt$gap) <- unlist(lapply(levels(dt$Disease),
                                            function(x){
                                            ifelse(sum(dfr_err()$Disease==x)==0,NA,
                                            dt$prop[dt$metr=="burden" & dt$Disease==x]/
                                            dfr_err()$prop_up[dfr_err()$Disease==x])}))
            dt$gap_col <- dt$metr
            dt$gap_col[as.numeric(as.character(dt$gap))>=2 & dt$gap_col=="burden"] <- "burden_gap"

            #disease labels
            dlbl <- dt[order(dt$Disease),]
            dlbl <- dlbl[dlbl$metr=="burden",]
            dlbl$gap_text <- "plain"
            dlbl$gap_text[dlbl$gap_col=="burden_gap"] <- "bold"


            p <- ggplot(dt,aes(Disease))
            p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
            #p <- p + geom_segment(aes(x=as.numeric(Disease)-0.4,xend=as.numeric(Disease)+0.4,
            #                          y=prop/2,yend=prop/2,size=5),
            #                      linetype="dashed",data=dt[dt$metr=="burden",],lwd=0.1)
            p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=prop_up,ymin=prop_low),width=0.2,data=dfr_err())
            if(input$hgp_reg)  { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red",
                                                                       "research"="blue")) }
            if(!input$hgp_reg) { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="orange",
                                                                       "research"="blue")) }
            p <- p + scale_x_discrete(label = dlbl$Dis_lab)
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                        name=paste("Local share across groups of diseases\nof ",
                                                   input$metr_burden," and ",input$metr_res," (%)",sep=""))

            p <- p + theme( axis.text.x = element_text(
                            face=dlbl$gap_text,
                            angle=55,
                            hjust=1,
                            vjust=1.1,size=10, margin = unit(c(10, 0, 0, 0), "mm"))) +
                 theme(panel.background = element_blank()) + 
                 theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                 theme(panel.grid.major.x = element_blank()) + 
                 theme(axis.ticks.x=element_blank()) + 
                 theme(legend.position = "none") + 
                 theme(axis.title.x=element_blank())

            p
            
            })
    
    #Render Plot
    output$plot_within_reg <- renderPlot({ plotInput_within_reg() })
        
    output$downloadPlot_within_reg <- downloadHandler(
            filename = function() { paste(input$tabs, '.png', sep='') },
            content = function(file) { ggsave(file, plot = plotInput_within_reg(), device = "png") }
        )
    
        
    #Server functions Within diseases
    ######################################################################
    
#    all_nhi <- reactiveValues(ch="All regions")
    
#    observeEvent(input$allregs, {all_nhi$ch <- "All regions"})
#    observeEvent(input$nhionly, {all_nhi$ch <- "Non-high-income only"})
        
    #subset according to Glob vs across NHI
    ddis_reg <- reactive({
#        if(all_nhi$ch=="All regions") dinit_dis[,-grep("NHI",names(dinit_dis))]
        if(input$all_nhi=="All regions") dinit_dis[,-grep("NHI",names(dinit_dis))]        
            else dinit_dis[dinit_dis$Region!="High-income",-grep("glob",names(dinit_dis))]
    })
        
    #subset according to disease
    ddis <- reactive({
        ddis_reg()[ddis_reg()$Dis_tooltip==input$disease,-grep("Dis",names(ddis_reg()))]
    })

    #subset according to burden metric
    ddis_burden <- reactive({
        dfb <- ddis()[,c(which(names(ddis())%in%c("Region","regs_lab")),
                                grep(tolower(input$metr_burden2),names(ddis())))]
        names(dfb) <- c("Region","regs_lab","Nb","prop")
        dfb$metr <- "burden"
        dfb
    })

    #Order of regions: depending on burden of the disease: increasing burden. if High-income: we put it first
    regs_ord <- reactive({
        r <- as.character(ddis_burden()$Region[order(ddis_burden()$Nb)])
        if("High-income"%in%r) r <- c("High-income",r[r!="High-income"])
        r
    })

    #subset according to research metric
    ddis_res <- reactive({
        dfr <- ddis()[,c(which(names(ddis())%in%c("Region","regs_lab")),
                         grep(input$metr_res2,names(ddis())))]
        names(dfr) <- c("Region","regs_lab","Nb_low","Nb_med","Nb_up","prop_low","prop_med","prop_up")
        dfr
    })   
    
    #data for plot
    ddis_plot <- reactive({
    
        dfr_med <- ddis_res()[,c("Region","regs_lab","Nb_med","prop_med")]
        names(dfr_med) <- c("Region","regs_lab","Nb","prop")
        dfr_med$metr <- "research"        
        dt <- rbind(ddis_burden()[,c("Region","regs_lab","Nb","prop","metr")],dfr_med)
        dt$Region <- reorder(dt$Region,new.order=regs_ord())
        dt
        
    })
    
    #data for error bars
    ddisfr_err <- reactive({
    
        d_err <- ddis_res()[,c("Region","regs_lab","prop_low","prop_up")]
        d_err$metr <- "research"        
        d_err$Region <- reorder(d_err$Region,new.order=regs_ord())
        d_err
    
    })        
            
    #data for hover
    ddis_points <- reactive({
    
        dpts <- merge(ddis_burden(),ddis_res())
        dpts$Region <- reorder(dpts$Region,new.order=regs_ord())
        dpts$Reg_pos <- as.numeric(dpts$Region)
        
        #burden points
        dpts_bur <- dpts
        dpts_bur$prop_pos <- dpts_bur$prop
        dpts_bur$Reg_pos <- dpts_bur$Reg_pos-0.2
        D_bur <- do.call('rbind',
        lapply(1:nrow(dpts_bur),function(x){
            tt <- trunc(dpts_bur$prop_pos[x])
            dtp <- dpts_bur[rep(x,tt+2),]
            dtp$prop_pos <- c(0:tt,dpts_bur$prop_pos[x])
            dtp
        }))
        
        #research points
        dpts_res <- dpts[!is.na(dpts$prop_med),]
        dpts_res$prop_pos <- dpts_res$prop_med
        dpts_res$Reg_pos <- dpts_res$Reg_pos+0.2
        D_res <- do.call('rbind',
        lapply(1:nrow(dpts_res),function(x){
            tt <- trunc(dpts_res$prop_pos[x])
            dtp <- dpts_res[rep(x,tt+2),]
            dtp$prop_pos <- c(0:tt,dpts_res$prop_pos[x])
            dtp
        }))
        
        rbind(D_bur,D_res)
        
    })

    #Hover  
    output$hover_info_dis <- renderUI({    

        hover <- input$plot_hover
        
        point <- nearPoints(ddis_points(), hover, yvar="prop_pos", xvar="Reg_pos",
                            threshold = (1005*0.2/(length(regs_ord()) + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<b> Group of diseases: </b>", input$disease, "<br/>",
                                   "<b> Region: </b>", point$Region, "<br/>",
                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
                                   " million ", input$metr_burden2,"s (",format(round(point$prop),nsmall = 0),"%)<br/>",
                                   "<b> Research: </b>", 
                                   ifelse(input$metr_res2=="RCTs",
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
                                                            pourc=FALSE)," ",input$metr_res2),
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")]/1e3,
                                                            pourc=FALSE)," thousand patients")),                                  
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],
                                                   pourc=TRUE),")<br/>")))
                    )
        }) 

    #Plot
    plotInput_within_dis <- reactive({
        
        dt <- ddis_plot()
        max_plot <- 10*ceiling(max(c(dt$prop,ddisfr_err()$prop_up)/10,na.rm=TRUE))

        #Highlighting gaps
        dt$gap <- dt$Region
        levels(dt$gap) <- unlist(lapply(levels(dt$Region),
                                        function(x){
                                            ifelse(sum(ddisfr_err()$Region==x)==0,NA,
                                                   dt$prop[dt$metr=="burden" & dt$Region==x]/
                                                   ddisfr_err()$prop_up[ddisfr_err()$Region==x])}))
        dt$gap_col <- dt$metr
        dt$gap_col[as.numeric(as.character(dt$gap))>=2 & dt$gap_col=="burden"] <- "burden_gap"

        #disease labels
        dlbl <- dt[order(dt$Region),]
        dlbl <- dlbl[dlbl$metr=="burden",]
        dlbl$gap_text <- "plain"
        dlbl$gap_text[dlbl$gap_col=="burden_gap"] <- "bold"


        p <- ggplot(dt,aes(Region))
        p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
        p <- p + geom_errorbar(aes(x=as.numeric(Region)+0.2,ymax=prop_up,ymin=prop_low),width=0.2,data=ddisfr_err())
        if(input$hgp_dis)  { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red",
                                                                   "research"="blue")) }
        if(!input$hgp_dis) { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="orange",
                                                                   "research"="blue")) }        
        p <- p + scale_x_discrete(label = dlbl$regs_lab)
        p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
#                                    name=paste("Share across",ifelse(all_nhi$ch=="All regions",
                                    name=paste("Share across",ifelse(input$all_nhi=="All regions",                                    
                                                                     " "," Non-high-income "),
                                               "regions\nof ",input$metr_burden2," and ",input$metr_res2,
                                               " (%)",sep=""))

        p <- p + theme( axis.text.x = element_text(
            face=dlbl$gap_text,
            angle=55,
            hjust=1,
            vjust=1.1,size=10, margin = unit(c(10, 0, 0, 0), "mm"))) +
        theme(panel.background = element_blank()) + 
        theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
        theme(panel.grid.major.x = element_blank()) + 
        theme(axis.ticks.x=element_blank()) + 
        theme(legend.position = "none") + 
        theme(axis.title.x=element_blank())

        p
        
        })
    
    #Render Plot
    output$plot_within_dis <- renderPlot({ plotInput_within_dis() })
        
    output$downloadPlot_within_dis <- downloadHandler(
            filename = function() { paste(input$tabs, '.png', sep='') },
            content = function(file) { ggsave(file, plot = plotInput_within_dis(), device = "png") }
        )

        
    #Server functions Compare regions
    ###########################################
    
    #subset according to region
    dcomp_reg <- reactive({
        dinit_reg[dinit_reg$regs_lab%in%c(input$region1,input$region2),]
    })

    #subset according to burden metric
    dcomp_burden <- reactive({
        dfb <- dcomp_reg()[,c(which(names(dcomp_reg())%in%c("Disease","Dis_lab","Dis_tooltip","regs_lab")),
                                grep(tolower(input$metr_burden_comp),names(dcomp_reg())))]
        names(dfb) <- c("Disease","Dis_lab","Dis_tooltip","Region","Nb","prop")
        dfb$metr <- "burden"
        dfb
    })

    #subset according to research metric
    dcomp_res <- reactive({
        dfr <- dcomp_reg()[,c(which(names(dcomp_reg())%in%c("Disease","Dis_lab","Dis_tooltip","regs_lab")),
                         grep(input$metr_res_comp,names(dcomp_reg())))]
        names(dfr) <- c("Disease","Dis_lab","Dis_tooltip","Region",
                        "Nb_low","Nb_med","Nb_up","prop_low","prop_med","prop_up")
        dfr
    })

    #Choose the Nb_dis highest diseases
    diss_comp <- reactive({dord <- tapply(dcomp_burden()$prop,dcomp_burden()$Disease,sum)
                      names(dord)[order(dord,decreasing=TRUE)][1:input$Nb_dis_comp]})
    
    #data for plot
    dcomp_plot <- reactive({
    
        dfr_med <- dcomp_res()[,c("Disease","Dis_lab","Region","Nb_med","prop_med")]
        names(dfr_med) <- c("Disease","Dis_lab","Region","Nb","prop")
        dfr_med$metr <- "research"
        dtb <- dcomp_burden()[,c("Disease","Dis_lab","Region","Nb","prop","metr")]
        dt <- rbind(dtb,dfr_med)
        dt <- droplevels(dt[dt$Disease%in%diss_comp(),])
        dt$Disease <- reorder(dt$Disease,new.order=as.character(diss_comp()))
        dt
        
    })
    
    #data for bar errors
    dcomp_err <- reactive({
    
        d_err <- dcomp_res()[,c("Disease","Dis_lab","Region","prop_low","prop_up")]
        d_err$metr <- "research"        
        d_err <- droplevels(d_err[d_err$Disease%in%diss_comp(),])
        d_err$Disease <- reorder(d_err$Disease,new.order=as.character(diss_comp()))
        d_err
    
    })
    
    #data for hover
#    data_points <- reactive({
#    
#        dpts <- merge(dburden(),dres())
#        dpts <- droplevels(dpts[dpts$Disease%in%diss(),])
#        dpts$Disease <- reorder(dpts$Disease,new.order=as.character(diss()))
#        dpts$Dis_pos <- as.numeric(dpts$Disease)
#        
#        #burden points
#        dpts_bur <- dpts
#        dpts_bur$prop_pos <- dpts_bur$prop
#        dpts_bur$Dis_pos <- dpts_bur$Dis_pos-0.2
#        D_bur <- do.call('rbind',
#        lapply(1:nrow(dpts_bur),function(x){
#            tt <- trunc(dpts_bur$prop_pos[x])
#            dtp <- dpts_bur[rep(x,tt+2),]
#            dtp$prop_pos <- c(0:tt,dpts_bur$prop_pos[x])
#            dtp
#        }))
#        
#        #research points
#        dpts_res <- dpts[!is.na(dpts$prop_med),]
#        dpts_res$prop_pos <- dpts_res$prop_med
#        dpts_res$Dis_pos <- dpts_res$Dis_pos+0.2
#        D_res <- do.call('rbind',
#        lapply(1:nrow(dpts_res),function(x){
#            tt <- trunc(dpts_res$prop_pos[x])
#            dtp <- dpts_res[rep(x,tt+2),]
#            dtp$prop_pos <- c(0:tt,dpts_res$prop_pos[x])
#            dtp
#        }))
#        
#        rbind(D_bur,D_res)
#        
#    })
    
    #Hover info
#    output$hover_info_regs <- renderUI({
#        
#        hover <- input$plot_hover
#        
#        point <- nearPoints(data_points(), hover, yvar="prop_pos", xvar="Dis_pos",
#                            threshold = (1005*0.2/(input$Nb_dis + 0.4)), maxpoints = 1, addDist = TRUE)
#        if (nrow(point) == 0) return(NULL)
#
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
#            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
#                            "left:", 0, "px; top:", 0, "px;")
#
            # actual tooltip created as wellPanel
#            tags$div(class = "well well-sm",
#                     style = style,
#                     p(HTML(paste0("<b> Region: </b>", input$region, "<br/>",
#                                   "<b> Group of diseases: </b>", point$Dis_tooltip, "<br/>",
#                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
#                                   " million ", input$metr_burden,"s (",format(round(point$prop),nsmall = 0),
#                                   "% of ",ifelse(input$region=="World","global","local")," burden)<br/>",
#                                   "<b> Research: </b>", 
#                                   ifelse(input$metr_res=="RCTs",
#                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
#                                                            pourc=FALSE)," ",input$metr_res),
#                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")] /1e3,
#                                                            pourc=FALSE)," thousand patients")),
#                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],pourc=TRUE), " of ",
#                                   ifelse(input$region=="World","global","local")," research)<br/>")))
#                    )
#        })

    #Plot
    plotInput_comp_reg <- reactive({

            dt <- dcomp_plot()
            max_plot <- 10*ceiling(max(c(dt$prop,dcomp_err()$prop_up)/10,na.rm=TRUE))

            #Highlighting gaps
#            dt$gap <- factor(paste(dt$Region,dt$Disease))
#            levels(dt$gap) <- unlist(lapply(sort(paste(dt$Region,dt$Disease)),
#                                            function(x){
#                                            ifelse(sum(paste(dcomp_err()$Region,dcomp_err()$Disease)==x)==0,NA,
#                                            dt$prop[dt$metr=="burden" & paste(dt$Region,dt$Disease)==x]/
#                                            dcomp_err()$prop_up[paste(dcomp_err()$Region,dcomp_err()$Disease)==x])}))
#            dt$gap_col <- dt$metr
#            dt$gap_col[as.numeric(as.character(dt$gap))>=2 & dt$gap_col=="burden"] <- "burden_gap"

            #disease labels
            dlbl <- dt[order(dt$Disease),]
            dlbl <- dlbl[dlbl$metr=="burden",]
#            dlbl$gap_text <- "plain"
#            dlbl$gap_text[dlbl$gap_col=="burden_gap"] <- "bold"


            p <- ggplot(dt[dt$Region==input$region1,],aes(Disease))
#            p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
            p <- p + geom_bar(aes(fill=metr,y=prop),position="dodge",stat="identity",width=0.8)
            p <- p + geom_segment(aes(x=as.numeric(Disease)-0.4,xend=as.numeric(Disease)+0.4,
                                      y=prop/2,yend=prop/2,size=5),
                                  linetype="dashed",data=dt[dt$metr=="burden" & dt$Region==input$region1,],lwd=0.1)
            p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=prop_up,ymin=prop_low),
                                   width=0.2,data=dcomp_err()[dcomp_err()$Region==input$region1,])
#            p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red","research"="blue"))
            p <- p + scale_fill_manual(values = c("burden"="orange","research"="blue"))
            p <- p + scale_x_discrete(label = dlbl$Dis_lab[dlbl$Region==input$region1])
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                        name=paste("Local share across groups of diseases\nof ",
                                                   input$metr_burden_comp," and ",input$metr_res_comp," (%)",sep=""))

            p <- p + theme( axis.text.x = element_text(
                            face=dlbl$gap_text[dlbl$Region==input$region1],
                            angle=55,
                            hjust=1,
                            vjust=1.1,size=10, margin = unit(c(10, 0, 0, 0), "mm"))) +
                 theme(panel.background = element_blank()) + 
                 theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                 theme(panel.grid.major.x = element_blank()) + 
                 theme(axis.ticks.x=element_blank()) + 
                 theme(legend.position = "none") + 
                 theme(axis.title.x=element_blank())

            p1 <- p
        
            p <- ggplot(dt[dt$Region==input$region2,],aes(Disease))
#            p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
            p <- p + geom_bar(aes(fill=metr,y=-prop),position="dodge",stat="identity",width=0.8)
            p <- p + geom_segment(aes(x=as.numeric(Disease)-0.4,xend=as.numeric(Disease)+0.4,
                                      y=prop/2,yend=prop/2,size=5),
                                  linetype="dashed",data=dt[dt$metr=="burden" & dt$Region==input$region2,],lwd=0.1)
            p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=-prop_low,ymin=-prop_up),
                                   width=0.2,data=dcomp_err()[dcomp_err()$Region==input$region2,])
#            p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red","research"="blue"))
            p <- p + scale_fill_manual(values = c("burden"="orange","research"="blue"))
            p <- p + scale_x_discrete(label = dlbl$Dis_lab[dlbl$Region==input$region2])
            p <- p + scale_y_continuous(limits = -rev(c(0,max_plot)),breaks=-rev(c(0,5,seq(10,max_plot,10))),
#            p <- p + scale_y_continuous(limits = c(0,max_plot),trans="reverse",#breaks=rev(c(0,5,seq(10,max_plot,10))),        
                                        name=paste("Local share across groups of diseases\nof ",
                                                   input$metr_burden_comp," and ",input$metr_res_comp," (%)",sep=""))

            p <- p + theme( axis.text.x = element_text(
                            face=dlbl$gap_text[dlbl$Region==input$region2],
                            angle=55,
                            hjust=1,
                            vjust=1.1,size=10, margin = unit(c(10, 0, 0, 0), "mm"))) +
                 theme(panel.background = element_blank()) + 
                 theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                 theme(panel.grid.major.x = element_blank()) + 
                 theme(axis.ticks.x=element_blank()) + 
                 theme(legend.position = "none") + 
                 theme(axis.title.x=element_blank())

            p2 <- p
        
            return(list(p1,p2))
            
            })
    
    #Render Plot
    output$plot_comp_reg1 <- renderPlot({ plotInput_comp_reg()[[1]] })
    output$plot_comp_reg2 <- renderPlot({ plotInput_comp_reg()[[2]] })
        
    output$downloadPlot_comp_reg <- downloadHandler(
            filename = function() { paste(input$tabs, '.png', sep='') },
            content = function(file) { ggsave(file, plot = grid.arrange(plotInput_comp_reg()[[1]],
                                                                        plotInput_comp_reg()[[2]],ncol=1),
                                              device = "png") }
        )
    
        
}

        
shinyApp(ui = ui, server = server)