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
    if(is.na(x[1])) {
        paste0(format(round(x[2]),nsmall = 0,big.mark=","),
               ifelse(pourc==TRUE,"% [estimate not available]"," [estimate not available]"))
        }
        else {
        paste(format(round(x[2]),nsmall = 0,big.mark=","),
               ifelse(pourc==TRUE,"% ["," ["),format(round(x[1]),nsmall = 0,big.mark=","),
               " - ",format(round(x[3]),nsmall = 0,big.mark=","),"]",sep="")
            }
    }

Choix_dis <-
list(`All diseases` = "All diseases",
     `Communicable diseases` = c("Tuberculosis",
                                "HIV",
                                "Common infectious diseases",
                                "Malaria", 
                                "Neglected tropical diseases", 
                                "Sexually transmitted diseases excluding HIV", 
                                "Hepatitis", 
                                "Leprosy"),
     `Maternal, neonatal & nutritional`= c("Maternal disorders", 
                                           "Neonatal disorders", 
                                           "Nutritional deficiencies"),
     `Non-communicable diseases` = c("Neoplasms", 
                                    "Cardiovascular and circulatory diseases", 
                                    "Chronic respiratory diseases", 
                                    "Cirrhosis of the liver", 
                                    "Digestive diseases", 
                                    "Neurological disorders", 
                                    "Mental and behavioral disorders", 
                                    "Diabetes, urinary diseases and male infertility", 
                                    "Gynecological diseases", 
                                    "Hemoglobinopathies and hemolytic anemias", 
                                    "Musculoskeletal disorders", 
                                    "Congenital anomalies", 
                                    "Skin and subcutaneous diseases", 
                                    "Sense organ diseases", 
                                    "Oral disorders", 
                                    "Sudden infant death syndrome") 
)

#User interface
#############################################################################################################
ui <- fluidPage(
    
#    tags$a(href="http://www.theclinic.cl","The Clinic"),
    tags$h2("Does health research effort match health needs?"),
    tags$h4("A large scale comparison between the global conduct of randomized controlled trials and the global burden of diseases"),
    tags$hr(),
    
    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
    sidebarLayout(
        mainPanel(
            conditionalPanel(condition = "input.tabs == 'regions'",             
                             # this is an extra div used ONLY to create positioned ancestor for tooltip
                             # we don't change its position
                             htmlOutput("title_reg"),
                             div(style = "position:relative",     
                                 plotOutput("plot_within_reg",
                                            hover = hoverOpts("plot_hover_reg", delay = 5, delayType = "debounce")),
                                 plotOutput("plot_within_reg_labs", height="95px"),
                                 uiOutput("hover_info_regs")
                                 )
                             ),                        
            conditionalPanel(condition = "input.tabs == 'diseases'",      
                             # this is an extra div used ONLY to create positioned ancestor for tooltip
                             # we don't change its position
                             htmlOutput("title_dis"),
                             div(style = "position:relative",
                                 plotOutput("plot_within_dis", 
                                            hover = hoverOpts("plot_hover_dis", delay = 5, delayType = "debounce")),
                                 plotOutput("plot_within_dis_labs", height="95px"),
                                 uiOutput("hover_info_dis")
                                 )
                             ),
            conditionalPanel(condition = "input.tabs == 'compare' & input.across== 'Regions'",
                             htmlOutput("title_comp_reg"),
                             div(style = "position:relative",     
                             plotOutput("plot_comp_reg1", height="200px",
                                        hover = hoverOpts("plot_hover_reg1", delay = 5, delayType = "debounce")),
                             plotOutput("plot_comp_reg_labs", height="85px"),
                             plotOutput("plot_comp_reg2", height="200px",
                                        hover = hoverOpts("plot_hover_reg2", delay = 5, delayType = "debounce")),
                                 uiOutput("hover_info_reg1")
                                 )
                             ),
            conditionalPanel(condition = "input.tabs == 'compare' & input.across== 'Diseases'",
                             htmlOutput("title_comp_dis"),
                             div(style = "position:relative",     
                             plotOutput("plot_comp_dis1", height="200px",
                                        hover = hoverOpts("plot_hover_dis1", delay = 5, delayType = "debounce")),
                             plotOutput("plot_comp_dis_labs", height="30px"),
                             plotOutput("plot_comp_dis2", height="200px",
                                        hover = hoverOpts("plot_hover_dis2", delay = 5, delayType = "debounce")),
                                 uiOutput("hover_info_dis1")
                                 )
                             ),
            conditionalPanel(condition = "input.tabs == 'about'",             
                             # this is an extra div used ONLY to create positioned ancestor for tooltip
                             # we don't change its position
                             HTML(paste0( "<center><b>",
                                   "Local",
                                   " share",
                                   paste0(" in ","Sub-Saharian Africa"),
                                   " across groups of diseases<br/>of ",
                                   "DALY","s as of 2005 vs ",
                                          "Randomized controlled trials",
                                   " conducted in 2006-2015",
                                  "</b></center>")),
                             div(style = "position:relative",     
                                 plotOutput("plot_within_reg_ex",
                                            hover = hoverOpts("plot_hover_reg_ex", delay = 5, delayType = "debounce")),
                                 plotOutput("plot_within_reg_labs_ex", height="95px"),
                                 uiOutput("hover_info_regs_ex")
                                 )
                             )
            #Adding conditional panel = "About"
        ),
        
        sidebarPanel(
            
            tabsetPanel(id="tabs",
                tabPanel("Within regions",value="regions",
                    selectInput(inputId = "region",
                                label= "Region",
                                choices = levels(DT$regs_lab),
                                selected = "World"),
                    splitLayout(
                                selectInput(inputId = "metr_burden",
                                            label= "Burden measured as:",
                                            choices = c("DALY","YLL","YLD","Death"),
                                            selected = "DALY"),
                                selectInput(inputId = "metr_res",
                                            label= "Research measured as:",
                                            choices = c("RCTs","Patients"),
                                            selected = "RCTs")),
                    sliderInput(inputId = "Nb_dis", "Number of diseases", 5, 27, 10, step = 1),
                    checkboxInput(inputId = "hgp_reg",
                                  label="Highlight local gaps",
                                  value=FALSE),
                    downloadButton('downloadPlot_within_reg', 'Download Plot'),
                    tags$br(),
                    tags$br(),
                    HTML(
#                        <table style="Font-Family: 'Arial', Serif;">
                        "<table>
                            <tr>
                                <td><img src='carre1.png' alt='orange square'></td>
                                <td><b>Local health needs:</b><br/>Proportion of burden caused by the group of diseases in the region</td>
                            </tr>
                            <tr>
                                <td><img src='carre2.png' alt='blue square'></td>
                                <td><b>Local research effort:</b><br/>Proportion of RCTs concerning the group of diseases in the region (estimate [95% UI])</td>
                            </tr>
                            <tr>
                                <td><img src='carre3.png' alt='red square'></td>
                                <td><b>Local gap of research:</b><br/>Groups of diseases for which the local research effort was less than half the local health needs</td>
                            </tr>
                        </table>"
                        )
                         ),
                tabPanel("Within diseases",value="diseases",
                    selectInput(inputId = "disease",
                                label= "Group of diseases",
                                choices = Choix_dis,
                                selected = "All diseases"),
                    splitLayout(
                                selectInput(inputId = "metr_burden2",
                                            label= "Burden measured as:",
                                            choices = c("DALY","YLL","YLD","Death"),
                                            selected = "DALY"),
                                selectInput(inputId = "metr_res2",
                                            label= "Research measured as:",
                                            choices = c("RCTs","Patients"),
                                            selected = "RCTs")),
                    checkboxInput(inputId = "all_nhi",
                                label="Non-high-income regions only",
                                value=TRUE),                         
                    checkboxInput(inputId = "hgp_dis",
                                  label="Highlight disease-specific gaps",
                                  value=FALSE),
                    downloadButton('downloadPlot_within_dis', 'Download Plot'),
                    tags$br(),
                    tags$br(),
                    conditionalPanel(condition = "input.tabs == 'diseases' & input.all_nhi",
                    HTML(
                        "<table>
                            <tr>
                                <td><img src='carre1.png' alt='orange square'></td>
                                <td><b>Regional health needs:</b><br/>Proportion of burden caused by the group of diseases in the region among the burden caused in all non-high-income regions</td>
                            </tr>
                            <tr>
                                <td><img src='carre2.png' alt='blue square'></td>
                                <td><b>Regional research effort:</b><br/>Proportion of RCTs conducted in the region among RCTs conducted in non-high-income regions concerning the group of diseases (estimate [95% UI])</td>
                            </tr>
                            <tr>
                                <td><img src='carre3.png' alt='red square'></td>
                                <td><b>Regional gap of research:</b><br/>Regions for which the regional research effort was less than half the regional health needs</td>
                            </tr>
                        </table>"
                          )
                    ),
                    conditionalPanel(condition = "input.tabs == 'diseases' & !input.all_nhi",
                    HTML(
                        "<table>
                            <tr>
                                <td><img src='carre1.png' alt='orange square'></td>
                                <td><b>Regional health needs:</b><br/>Proportion of burden in the region among worlwide burden caused by the group of diseases</td>
                            </tr>
                            <tr>
                                <td><img src='carre2.png' alt='blue square'></td>
                                <td><b>Regional research effort:</b><br/>Proportion of RCTs conducted in the region among all RCTs concerning the group of diseases (estimate [95% UI])</td>
                            </tr>
                            <tr>
                                <td><img src='carre3.png' alt='red square'></td>
                                <td><b>Regional gap of research:</b><br/>Regions for which the regional research effort was less than half the regional health needs</td>
                            </tr>
                        </table>"
                        )
                    )
                         ),
                tabPanel("Compare",value="compare",
                         radioButtons(inputId = "across",                          
                                      label = NULL,
                                      choices = c("Regions","Diseases"),
                                      selected = "Regions",
                                      inline = TRUE),
                         conditionalPanel(condition = "input.tabs == 'compare' & input.across == 'Regions'",
                                     splitLayout(
                                         selectInput(inputId = "region1",
                                                 label= "Region 1:",
                                                 choices = levels(DT$regs_lab),
                                                 selected = "High-income countries"),
                                         selectInput(inputId = "region2",
                                                 label= "Region 2:",
                                                 choices = levels(DT$regs_lab),
                                                 selected = "Non-high-income countries")
                                         ),
                                     splitLayout(
                                         selectInput(inputId = "metr_burden_comp",
                                                 label= "Burden measured as:",
                                                 choices = c("DALY","YLL","YLD","Death"),
                                                 selected = "DALY"),
                                         selectInput(inputId = "metr_res_comp",
                                                 label= "Research measured as:",
                                                 choices = c("RCTs","Patients"),
                                                 selected = "RCTs")),
                                     sliderInput(inputId = "Nb_dis_comp", 
                                                 "Number of diseases", 5, 27, 10, step = 1),
#                                     checkboxInput(inputId = "hgp_comp_reg",
#                                                   label="Highlight gaps",
#                                                   value=FALSE),
                                     downloadButton('downloadPlot_comp_reg', 'Download Plot')
                                     ),
                     conditionalPanel(condition = "input.tabs == 'compare' & input.across == 'Diseases'",
                                      splitLayout(
                                         selectInput(inputId = "disease1",
                                                     label= "Group of diseases 1:",
                                                     choices = Choix_dis,
                                                     selected = "Neoplasms"),
                                         selectInput(inputId = "disease2",
                                                     label= "Group of diseases 2:",
                                                     choices = Choix_dis,
                                                     selected = "HIV")   
                                                 ),
#                                      selectInput(inputId = "all_nhi_comp",
#                                            label= "Across:",
#                                            choices = c("All regions","Non-high-income only"),
#                                            selected = "All regions"),
                                      splitLayout(
                                          selectInput(inputId = "metr_burden_comp_dis",
                                                  label= "Burden measured as:",
                                                  choices = c("DALY","YLL","YLD","Death"),
                                                  selected = "DALY"),
                                      selectInput(inputId = "metr_res_comp_dis",
                                                  label= "Research measured as:",
                                                  choices = c("RCTs","Patients"),
                                                  selected = "RCTs")),
                                      checkboxInput(inputId = "all_nhi_comp",
                                                    label="Non-high-income regions only",
                                                    value=TRUE),                       
#                                      checkboxInput(inputId = "hgp_comp_dis",
#                                                    label="Highlight gaps",
#                                                    value=FALSE),
                                      downloadButton('downloadPlot_comp_dis', 'Download Plot'),
                                      tags$br(),
                                    tags$br(),
                                    HTML(
                                        "<table>
                                            <tr>
                                                <td><img src='carre1.png' alt='orange square'></td>
                                                <td><b>Regional health needs:</b><br/>Proportion of burden caused by the group of diseases in the region among the burden caused in all non-high-income regions</td>
                                            </tr>
                                            <tr>
                                                <td><img src='carre2.png' alt='blue square'></td>
                                                <td><b>Regional research effort:</b><br/>Proportion of RCTs conducted in the region among RCTs conducted in non-high-income regions concerning the group of diseases (estimate [95% UI])</td>
                                            </tr>
                                            <tr>
                                                <td><img src='carre3.png' alt='red square'></td>
                                                <td><b>Regional gap of research:</b><br/>Regions for which the regional research effort was less than half the regional health needs</td>
                                            </tr>
                                        </table>"
                                                      
                                                      )
                            )

                          ),
                        tabPanel("About",value="about",
				 tags$br(),
				 "This visualization tool allows for comparison between health research effort and health needs for seven epidemiological regions and 27 major groups of diseases.",
                                 tags$h4("Research effort:"),
				 "Research effort was measured as the number of randomized controlles trials (RCTs) or patients planned to be enrolled in RCTs initiated in the 2006-2015 period. Data represents the mapping of 117,180 RCTs planning to enroll 44.0 million patients. Data was extracted from the ",tags$a(href="http://apps.who.int/trialsearch/","WHO International Clinical Trials Registry Platform."),
                                 tags$h4("Health needs:"),"Health needs were measured as the burden of diseases in 2005, as disability-adjusted life years (DALY), years of life lost (YLL), years lived with disability (YLD) or number of deaths. Data represents the mapping of 2,200 million DALY, 1,549 million YLL, 671 million YLD and 46.3 million deaths. Data was extracted from the ", tags$a(href="http://www.healthdata.org/gbd","Global Burden of Diseases 2010 study."),
				 tags$h4("Data and code availability"),"All aggregated data and code is available in a ", tags$a(href="http://www.github.com/iatal/RCTvsBurden","github repository."),
                        tags$br(),
                    tags$br(),
                    HTML(
#                        <table style="Font-Family: 'Arial', Serif;">
                        "<table>
                            <tr>
                                <td><img src='carre1.png' alt='orange square'></td>
                                <td><b>Local health needs:</b><br/>Proportion of burden caused by the group of diseases in the region</td>
                            </tr>
                            <tr>
                                <td><img src='carre2.png' alt='blue square'></td>
                                <td><b>Local research effort:</b><br/>Proportion of RCTs concerning the group of diseases in the region (estimate [95% UI])</td>
                            </tr>
                        </table>"
                        )
                                 )
                        )#end tabset panel
            )#end sidebar panel
        ),#end sidebar layout
    
    tags$hr(),
    HTML("<footer>
	<a href='http://cress-umr1153.fr/en/' title='cress website' target='_blank'><img src='cress.png' alt='CRESS' title='logo CRESS' width='200px'/></a> 
	<a href='http://www.inserm.fr/' title='inserm website' target='_blank'><img src='inserm.jpg' alt='INSERM' title='logo inserm' width='250px'/></a> 
	<a href='https://www.univ-paris5.fr/' title='paris descartes website' target='_blank'><img src='parisdescartes.png' alt='logo Paris Descartes' title='Paris Descartes University' width='200px'/></a> 
</footer>
")
    
    )#end fluid page


######################################################################################################
#About example
#subset according to region
    dreg_ex <- dinit_reg[dinit_reg$regs_lab=="Sub-Saharian Africa",-which(names(dinit_reg)%in%c("regs_lab","Region"))]

    #subset according to burden metric
    dburden_ex <- dreg_ex[,c(which(names(dreg_ex)%in%c("Disease","Dis_lab","Dis_tooltip")),
                                                                grep("daly",names(dreg_ex)))]
    names(dburden_ex) <- c("Disease","Dis_lab","Dis_tooltip","Nb","prop")
    dburden_ex$metr <- "burden"

    #subset according to research metric

    dres_ex <- dreg_ex[,c(which(names(dreg_ex)%in%c("Disease","Dis_lab","Dis_tooltip")),
                         grep("RCT",names(dreg_ex)))]
    names(dres_ex) <- c("Disease","Dis_lab","Dis_tooltip","Nb_low","Nb_med","Nb_up","prop_low","prop_med","prop_up")

    #Choose the Nb_dis highest diseases
    diss_ex <- dburden_ex$Disease[order(dburden_ex$prop,decreasing=TRUE)][1:10]
    
    #data for plot
    dfr_med_ex <- dres_ex[,c("Disease","Dis_lab","Nb_med","prop_med")]
    names(dfr_med_ex) <- c("Disease","Dis_lab","Nb","prop")
    dfr_med_ex$metr <- "research"        
    data_plot_ex <- rbind(dburden_ex[,c("Disease","Dis_lab","Nb","prop","metr")],dfr_med_ex)
    data_plot_ex <- droplevels(data_plot_ex[data_plot_ex$Disease%in%diss_ex,])
    data_plot_ex$Disease <- reorder(data_plot_ex$Disease,new.order=as.character(diss_ex))
    
    #data for bar errors
        dfr_err_ex <- dres_ex[,c("Disease","Dis_lab","prop_low","prop_up")]
        dfr_err_ex$metr <- "research"        
        dfr_err_ex <- droplevels(dfr_err_ex[dfr_err_ex$Disease%in%diss_ex,])
        dfr_err_ex$Disease <- reorder(dfr_err_ex$Disease,new.order=as.character(diss_ex))
        
    
    #data for hover
    
        dpts_ex <- merge(dburden_ex,dres_ex)
        dpts_ex <- droplevels(dpts_ex[dpts_ex$Disease%in%diss_ex,])
        dpts_ex$Disease <- reorder(dpts_ex$Disease,new.order=as.character(diss_ex))
        dpts_ex$Dis_pos <- as.numeric(dpts_ex$Disease)
        
        #burden points
        dpts_bur_ex <- dpts_ex
        dpts_bur_ex$prop_pos <- dpts_bur_ex$prop
        dpts_bur_ex$Dis_pos <- dpts_bur_ex$Dis_pos-0.2
        D_bur_ex <- do.call('rbind',
        lapply(1:nrow(dpts_bur_ex),function(x){
            tt <- trunc(dpts_bur_ex$prop_pos[x])
            dtp <- dpts_bur_ex[rep(x,tt+2),]
            dtp$prop_pos <- c(0:tt,dpts_bur_ex$prop_pos[x])
            dtp
        }))
        
        #research points
        dpts_res_ex <- dpts_ex[!is.na(dpts_ex$prop_med),]
        dpts_res_ex$prop_pos <- dpts_res_ex$prop_med
        dpts_res_ex$Dis_pos <- dpts_res_ex$Dis_pos+0.2
        D_res_ex <- do.call('rbind',
        lapply(1:nrow(dpts_res_ex),function(x){
            tt <- trunc(dpts_res_ex$prop_pos[x])
            dtp <- dpts_res_ex[rep(x,tt+2),]
            dtp$prop_pos <- c(0:tt,dpts_res_ex$prop_pos[x])
            dtp
        }))
        
        data_points_ex <- rbind(D_bur_ex,D_res_ex)


#Server
##################################################################################################################

server <- function(input,output){

    #Server functions Within regions
    ###############################################################################################################
    ###############################################################################################################
    
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
        
        hover <- input$plot_hover_reg
        
        point <- nearPoints(data_points(), hover, yvar="prop_pos", xvar="Dis_pos",
                            threshold = (1005*0.2/(input$Nb_dis + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) {
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<br/>",
                                   "<br/>",
                                   "<br/>",
                                   "<br/>")))
                    )}
        else{

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
            }
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
            if(max_plot>20 & max_plot<80) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                        name="%")
                            }
            if(max_plot>=80) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=unique(c(0,10,seq(20,max_plot,20),max_plot)),
                                        name="%")
                            }
            if(max_plot<=20) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,1,seq(5,max_plot,5)),
                                        name="%")
                            }
            p <- p + theme(axis.text.y=element_text(size=12))
            p <- p + theme(axis.title.y=element_text(size=20))    

            p <- p + theme(axis.text.x = element_blank()) + 
                     theme(panel.background = element_blank()) + 
                     theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                     theme(panel.grid.major.x = element_blank()) + 
                     theme(axis.ticks.x=element_blank()) + 
                     theme(legend.position = "none") + 
                     theme(axis.title.x=element_blank()) +
                     labs(x=NULL)

            plb <- ggplot(dt,aes(Disease)) 
            plb <- plb + scale_y_continuous(limits = c(0,0),breaks=c(0),label="   ",
                                        name=" ")
            plb <- plb + theme(axis.ticks.y = element_blank())
            plb <- plb + theme(axis.text.y=element_text(size=12))
            plb <- plb + theme(axis.title.y=element_text(size=20)) 
            if(input$hgp_reg)  { plb <- plb + theme(axis.text.x = element_text(face=dlbl$gap_text)) }

            plb <- plb + theme(axis.text.x = element_text(angle=55,
                                                          hjust=1,
                                                          vjust=1.1,size=10))
        
            plb <- plb + theme(panel.background = element_blank()) + 
                         theme(panel.grid.major.y = element_blank()) +
                         theme(panel.grid.major.x = element_blank()) + 
                         theme(axis.ticks.x=element_blank()) + 
                         theme(legend.position = "none") + 
                         theme(axis.title.x=element_blank())

            plb <- plb + scale_x_discrete(label = dlbl$Dis_lab)
            plb <- plb + theme(axis.text.x=element_text(size=12))

            list(p,plb)
            
            })
    
    #Render Plot
    output$plot_within_reg <- renderPlot({ plotInput_within_reg()[[1]] })
    output$plot_within_reg_labs <- renderPlot({ plotInput_within_reg()[[2]] })
        
    output$downloadPlot_within_reg <- downloadHandler(
            filename = function() { paste0('Across_',input$tabs,'_',
                                           gsub('[[:punct:]| ]','_',input$region),'_',
                                           input$metr_burden,'_vs_',input$metr_res,
                                           '.png', sep='') },
            content = function(file) { 
                P <-  plotInput_within_reg()
                p <- P[[1]]
                p <- p + ggtitle(paste0(ifelse(input$region=="World","Global","Local"),
                                        " share",ifelse(input$region=="World","",paste0(" in ",input$region)),
                                        " across groups of diseases\nof ",input$metr_burden,"s as of 2005 vs ",
                                        ifelse(input$metr_res=="RCTs",
                                               "Randomized controlled trials",
                                               "patients planned to be enrolled in RCTs"),
                                        " conducted in 2006-2015"))
                ggsave(file, plot = grid.arrange(p, P[[2]],
                                                 layout_matrix = matrix(c(1,1,1,1,2),ncol=1)),
                                                 device = "png", width = 12, height = 7, unit = "in") }
        )
        
        

    output$title_reg <- renderText({
       
        paste0( "<center><b>",
               ifelse(input$region=="World","Global","Local"),
               " share",
               ifelse(input$region=="World"," ",paste0(" in ",input$region)),
               " across groups of diseases<br/>of ",
               input$metr_burden,"s as of 2005 vs ",
               ifelse(input$metr_res=="RCTs",
                      "Randomized controlled trials",
                      "patients planned to be enrolled in Randomized controlled trials"),
               " conducted in 2006-2015",
              "</b></center>")
        })    
        
        
    #Server functions Within diseases
    ###############################################################################################################
    ###############################################################################################################
    
#    all_nhi <- reactiveValues(ch="All regions")

#    observeEvent(input$allregs, {all_nhi$ch <- "All regions"})
#    observeEvent(input$nhionly, {all_nhi$ch <- "Non-high-income only"})
        
    #subset according to Glob vs across NHI
    ddis_reg <- reactive({
#        if(all_nhi$ch=="All regions") dinit_dis[,-grep("NHI",names(dinit_dis))]
#        if(input$all_nhi=="All regions") dinit_dis[,-grep("NHI",names(dinit_dis))]        
          if(input$all_nhi==FALSE) dinit_dis[,-grep("NHI",names(dinit_dis))]        
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

        hover <- input$plot_hover_dis
        
        point <- nearPoints(ddis_points(), hover, yvar="prop_pos", xvar="Reg_pos",
                            threshold = (1005*0.2/(length(regs_ord()) + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) {
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<br/>",
                                   "<br/>",
                                   "<br/>",
                                   "<br/>")))
                    )}
        else{

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
                                   " million ", input$metr_burden2,"s (",format(round(point$prop),nsmall = 0),
                                   "% of burden among ",ifelse(input$all_nhi==TRUE,"non-high-income","all"),
                                   " regions)<br/>",
                                   "<b> Research: </b>", 
                                   ifelse(input$metr_res2=="RCTs",
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
                                                            pourc=FALSE)," ",input$metr_res2),
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")]/1e3,
                                                            pourc=FALSE)," thousand patients")),                                  
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],
                                                   pourc=TRUE)," of research among ",
                                   ifelse(input$all_nhi==TRUE,"non-high-income","all")," regions)<br/>")))
                    )
            }
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
        
        if(max_plot>20 & max_plot<80) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                        name="%")
                            }
        if(max_plot>=80) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=unique(c(0,10,seq(20,max_plot,20),max_plot)),
                                        name="%")
        }
        if(max_plot<=20) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,1,seq(5,max_plot,5)),
                                        name="%")
        }
        p <- p + theme(axis.text.y=element_text(size=12))
        p <- p + theme(axis.title.y=element_text(size=20)) 

        p <- p + theme(axis.text.x = element_blank()) + 
                 theme(panel.background = element_blank()) + 
                 theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                 theme(panel.grid.major.x = element_blank()) + 
                 theme(axis.ticks.x=element_blank()) + 
                 theme(legend.position = "none") + 
                 theme(axis.title.x=element_blank())+
                 labs(x=NULL)
                         
        plb <- ggplot(dt,aes(Region)) 
        plb <- plb + scale_y_continuous(limits = c(0,0),breaks=c(0),label="   ",
                                        name=" ")
        plb <- plb + theme(axis.ticks.y = element_blank())
        plb <- plb + theme(axis.text.y=element_text(size=12))
        plb <- plb + theme(axis.title.y=element_text(size=20)) 

        if(input$hgp_dis)  { plb <- plb + theme(axis.text.x = element_text(face=dlbl$gap_text)) }
        plb <- plb + theme(axis.text.x = element_text(angle=55,
                                                      hjust=1,
                                                      vjust=1.1,size=10))
        plb <- plb + theme(panel.background = element_blank()) + 
                     theme(panel.grid.major.y = element_blank()) +
                     theme(panel.grid.major.x = element_blank()) + 
                     theme(axis.ticks.x=element_blank()) + 
                     theme(legend.position = "none") + 
                     theme(axis.title.x=element_blank())

        plb <- plb + scale_x_discrete(label = dlbl$regs_lab)
        plb <- plb + theme(axis.text.x=element_text(size=12))
              
        list(p,plb)
        
        })
    
    #Render Plot
    output$plot_within_dis <- renderPlot({ plotInput_within_dis()[[1]] })
    output$plot_within_dis_labs <- renderPlot({ plotInput_within_dis()[[2]] })
        
    output$downloadPlot_within_dis <- downloadHandler(
            filename = function() { paste0('Across_',input$tabs,'_',
                                           gsub('[[:punct:]| ]','_',input$disease),'_',
                                           input$metr_burden2,'_vs_',input$metr_res2,
                                           '.png', sep='') },
            content = function(file) { 
                P <-  plotInput_within_dis()
                p <- P[[1]]
                p <- p + ggtitle(paste0("Share across ",
#                                        ifelse(input$all_nhi=="All regions","","non-high-income "),
                                        ifelse(input$all_nhi==FALSE,"","non-high-income "),
                                        "regions of ",input$metr_burden2,"s as of 2005 vs\n",
                                        ifelse(input$metr_res2=="RCTs",
                                               "Randomized controlled trials",
                                               "patients planned to be enrolled in RCTs"),
                                        " conducted in 2006-2015\nfor ",
                                        input$disease))
                ggsave(file, plot = grid.arrange(p, P[[2]],
                                                 layout_matrix = matrix(c(1,1,1,1,2),ncol=1)),
                                                 device = "png", width = 12, height = 7, unit = "in") }
        )

    output$title_dis <- renderText({
       
        paste0( "<center><b>",
               "Share across ",
#               ifelse(input$all_nhi=="All regions","","non-high-income"),
               ifelse(input$all_nhi==FALSE,"","non-high-income"),
               " regions of<br/>",
               input$metr_burden2,"s as of 2005 vs ",
               ifelse(input$metr_res2=="RCTs",
                      "Randomized controlled trials",
                      "patients planned to be enrolled in Randomized controlled trials"),
               " conducted in 2006-2015 for ",
               input$disease,
              "</b></center>")
        })    
        
    #Server functions Compare regions
    ###############################################################################################################
    ###############################################################################################################
    
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

#    diss_comp <- reactive({dord <- tapply(dcomp_burden()$prop,dcomp_burden()$Disease,sum)
#                      names(dord)[order(dord,decreasing=TRUE)][1:input$Nb_dis_comp]})

    #Choose the Nb_dis highest diseases for region 1
    diss_comp <- reactive({
            as.character(
                dcomp_burden()$Disease[dcomp_burden()$Region==input$region1][order(dcomp_burden()$prop[dcomp_burden()$Region==input$region1],decreasing=TRUE)][1:input$Nb_dis_comp])
        })
             
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
    datacomp_points <- reactive({
    
        dpts <- merge(dcomp_burden(),dcomp_res())
        dpts <- droplevels(dpts[dpts$Disease%in%diss_comp(),])
        dpts$Disease <- reorder(dpts$Disease,new.order=as.character(diss_comp()))
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
        
        D_bur$prop_pos[D_bur$Region==input$region2] <- -D_bur$prop_pos[D_bur$Region==input$region2]
        
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
        
        D_res$prop_pos[D_res$Region==input$region2] <- -D_res$prop_pos[D_res$Region==input$region2]
        
        rbind(D_bur,D_res)
        
    })
    
    #Hover info
    hover_info_reg1_r <- reactive({
        
        hover <- input$plot_hover_reg1
        
        point <- nearPoints(datacomp_points()[datacomp_points()$Region==input$region1,],
        hover, yvar="prop_pos", xvar="Dis_pos",
                            threshold = (1005*0.2/(input$Nb_dis_comp + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<b> Region: </b>", input$region1, "<br/>",
                                   "<b> Group of diseases: </b>", point$Dis_tooltip, "<br/>",
                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
                                   " million ", input$metr_burden_comp,"s (",format(round(point$prop),nsmall = 0),
                                   "% of ",ifelse(input$region1=="World","global","local")," burden)<br/>",
                                   "<b> Research: </b>", 
                                   ifelse(input$metr_res_comp=="RCTs",
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
                                                            pourc=FALSE)," ",input$metr_res_comp),
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")] /1e3,
                                                            pourc=FALSE)," thousand patients")),
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],pourc=TRUE), " of ",
                                   ifelse(input$region1=="World","global","local")," research)<br/>")))
                    )
        })

    hover_info_reg2_r <- reactive({
        
        hover <- input$plot_hover_reg2
        
        point <- nearPoints(datacomp_points()[datacomp_points()$Region==input$region2,],
        hover, yvar="prop_pos", xvar="Dis_pos",
                            threshold = (1005*0.2/(input$Nb_dis_comp + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<b> Region: </b>", input$region2, "<br/>",
                                   "<b> Group of diseases: </b>", point$Dis_tooltip, "<br/>",
                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
                                   " million ", input$metr_burden_comp,"s (",format(round(point$prop),nsmall = 0),
                                   "% of ",ifelse(input$region1=="World","global","local")," burden)<br/>",
                                   "<b> Research: </b>", 
                                   ifelse(input$metr_res_comp=="RCTs",
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
                                                            pourc=FALSE)," ",input$metr_res_comp),
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")] /1e3,
                                                            pourc=FALSE)," thousand patients")),
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],pourc=TRUE), " of ",
                                   ifelse(input$region1=="World","global","local")," research)<br/>")))
                    )
        })

    output$hover_info_reg1 <- renderUI({

        if(length(hover_info_reg1_r())==0 & length(hover_info_reg2_r())==0){
     style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<br/>",
                                   "<br/>",
                                   "<br/>",
                                   "<br/>")))
                    )
     }
     else { if(length(hover_info_reg1_r())!=0) return(hover_info_reg1_r())
            if(length(hover_info_reg2_r())!=0) return(hover_info_reg2_r())
                         }
        })
        
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
            p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=prop_up,ymin=prop_low),
                                   width=0.2,data=dcomp_err()[dcomp_err()$Region==input$region1,])
#            p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red","research"="blue"))
            p <- p + scale_fill_manual(values = c("burden"="orange","research"="blue"))

            if(max_plot>20 & max_plot<80) {
                p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                        name="%")
                            }
            if(max_plot>=80) {
                p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=unique(c(0,10,seq(20,max_plot,20),max_plot)),
                                        name="%")
                            }
            if(max_plot<=20) {
                p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,1,seq(5,max_plot,5)),
                                            name="%")
            }
            p <- p + theme(axis.text.y=element_text(size=12))
            p <- p + theme(axis.title.y=element_text(size=20)) 

            p <- p + theme(axis.text.x = element_blank()) + 
                     theme(panel.background = element_blank()) + 
                     theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                     theme(panel.grid.major.x = element_blank()) + 
                     theme(axis.ticks.x=element_blank()) + 
                     theme(legend.position = "none") + 
                     theme(axis.title.x=element_blank()) +
                     labs(x=NULL)

            p1 <- p

            plb <- ggplot(dt,aes(Disease)) 
            plb <- plb + scale_y_continuous(limits = c(0,0),breaks=c(0),label="   ",
                                        name=" ")
            plb <- plb + theme(axis.ticks.y = element_blank())
            plb <- plb + theme(axis.text.y=element_text(size=12))
            plb <- plb + theme(axis.title.y=element_text(size=20)) 

#            if(input$hgp_dis)  { plb <- plb + theme(axis.text.x = element_text(face=dlbl$gap_text)) }
            plb <- plb + theme(axis.text.x = element_text(angle=90,
                                                          hjust=0.5,
                                                          vjust=0.5,size=10))
            plb <- plb + theme(panel.background = element_blank()) + 
                         theme(panel.grid.major.y = element_blank()) +
                         theme(panel.grid.major.x = element_blank()) + 
                         theme(axis.ticks.x=element_blank()) + 
                         theme(legend.position = "none") + 
                         theme(axis.title.x=element_blank())

            plb <- plb + scale_x_discrete(label = dlbl$Dis_lab[dlbl$Region==input$region1])
            plb <- plb + theme(axis.text.x=element_text(size=12))
                
            p <- ggplot(dt[dt$Region==input$region2,],aes(Disease))
#            p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
            p <- p + geom_bar(aes(fill=metr,y=-prop),position="dodge",stat="identity",width=0.8)
            p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=-prop_low,ymin=-prop_up),
                                   width=0.2,data=dcomp_err()[dcomp_err()$Region==input$region2,])
#            p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red","research"="blue"))
            p <- p + scale_fill_manual(values = c("burden"="orange","research"="blue"))
        
            if(max_plot>20 & max_plot<80) {
            p <- p + scale_y_continuous(limits = -rev(c(0,max_plot)),breaks=-rev(c(0,5,seq(10,max_plot,10))),
                                        labels = rev(c(0,5,seq(10,max_plot,10))),
                                        name="%")
                            }
            if(max_plot>=80) {
            p <- p + scale_y_continuous(limits = -rev(c(0,max_plot)),
                                        breaks = -rev(unique(c(0,10,seq(20,max_plot,20),max_plot))),
                                        labels = rev(unique(c(0,10,seq(20,max_plot,20),max_plot))),
                                        name="%")
                            }
        
            if(max_plot<=20) {
                p <- p + scale_y_continuous(limits = -rev(c(0,max_plot)),breaks=-rev(c(0,1,seq(5,max_plot,5))),
                                            labels = rev(c(0,1,seq(5,max_plot,5))),name="%")
            }
            p <- p + theme(axis.text.y=element_text(size=12))
            p <- p + theme(axis.title.y=element_text(size=20)) 

            p <- p + theme(axis.text.x = element_blank()) + 
                     theme(panel.background = element_blank()) + 
                     theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                     theme(panel.grid.major.x = element_blank()) + 
                     theme(axis.ticks.x=element_blank()) + 
                     theme(legend.position = "none") + 
                     theme(axis.title.x=element_blank()) +
                     labs(x=NULL)

            p2 <- p
        
            return(list(p1,p2,plb))
            
            })

    #Render Plot
    output$plot_comp_reg1 <- renderPlot({ plotInput_comp_reg()[[1]] })    
    output$plot_comp_reg2 <- renderPlot({ plotInput_comp_reg()[[2]] })
    output$plot_comp_reg_labs <- renderPlot({ plotInput_comp_reg()[[3]] })
        
    output$downloadPlot_comp_reg <- downloadHandler(
            filename = function() { paste0(input$tabs,'_',
                                           gsub('[[:punct:]| ]','_',input$region1),'_vs_',
                                           gsub('[[:punct:]| ]','_',input$region2),'_',
                                           input$metr_burden_comp,'_vs_',input$metr_res_comp,
                                           '.png', sep='') },
            content = function(file) { 
                P <-  plotInput_comp_reg()
                ggsave(file, plot = grid.arrange(P[[1]], P[[3]], P[[2]],
                                                 layout_matrix = matrix(c(1,1,1,1,2,3,3,3,3),ncol=1),
                                                 top = textGrob(paste0("Local shares across groups of diseases\nin ",
                                                                       input$region1,
                                                                       " (top) & ", input$region2,
                                                                       " (bottom)\nof ",
                                                                      input$metr_burden_comp,"s as of 2005 vs ",
                                                                      ifelse(input$metr_res_comp=="RCTs",
                                                                             "Randomized controlled trials",
                                                                             "patients planned to be enrolled in RCTs"),
                                                                      " conducted in 2006-2015"),
                                                                       gp=gpar(fontsize=20))),
                device = "png", width = 15, height = 12, unit = "in") }
        )
        
        

    output$title_comp_reg <- renderText({
       
        paste0( "<center><b>",
               "Local shares across groups of diseases in ",
               input$region1,
               " (top) & ", input$region2,
               " (bottom)<br/>of ",
               input$metr_burden_comp,"s as of 2005 vs ",
               ifelse(input$metr_res_comp=="RCTs",
                      "Randomized controlled trials",
                      "patients planned to be enrolled in RCTs"),
               " conducted in 2006-2015",
               "</b></center>")
        })    

        
    #Server functions Compare diseases
    ###############################################################################################################
    ###############################################################################################################

    #subset according to Glob vs across NHI
    ddis_comp_reg <- reactive({
#        if(input$all_nhi_comp=="All regions") dinit_dis[,-grep("NHI",names(dinit_dis))]        
            if(input$all_nhi_comp==FALSE) dinit_dis[,-grep("NHI",names(dinit_dis))]        
            else dinit_dis[dinit_dis$Region!="High-income",-grep("glob",names(dinit_dis))]
    })
        
    #subset according to disease
    ddis_comp <- reactive({
        ddis_comp_reg()[ddis_comp_reg()$Dis_tooltip%in%c(input$disease1,input$disease2),]
    })

    #subset according to burden metric
    ddis_comp_burden <- reactive({
        dfb <- ddis_comp()[,c(which(names(ddis_comp())%in%c("Disease","Dis_lab","Dis_tooltip","Region","regs_lab")),
                                   grep(tolower(input$metr_burden_comp_dis),names(ddis_comp())))]
        names(dfb) <- c("Region","Disease","Dis_lab","Dis_tooltip","regs_lab","Nb","prop")
        dfb$metr <- "burden"
        dfb
    })

    #Order of regions: depending on burden of the disease: increasing burden. if High-income: we put it first
    regs_ord_comp <- reactive({
        r <- as.character(ddis_comp_burden()$Region[ddis_comp_burden()$Dis_tooltip==input$disease1][order(ddis_comp_burden()$Nb[ddis_comp_burden()$Dis_tooltip==input$disease1])])
        if("High-income"%in%r) r <- c("High-income",r[r!="High-income"])
        r
    })

    #subset according to research metric
    ddis_comp_res <- reactive({
        dfr <- ddis_comp()[,c(which(names(ddis_comp())%in%c("Disease","Dis_lab","Dis_tooltip","Region","regs_lab")),
                         grep(input$metr_res_comp_dis,names(ddis_comp())))]
        names(dfr) <- c("Region","Disease","Dis_lab","Dis_tooltip",
                        "regs_lab","Nb_low","Nb_med","Nb_up","prop_low","prop_med","prop_up")
        dfr
    })   
    
    #data for plot
    ddis_comp_plot <- reactive({
    
        dfr_med <- ddis_comp_res()[,c("Region","regs_lab","Disease","Dis_lab","Dis_tooltip","Nb_med","prop_med")]
        names(dfr_med) <- c("Region","regs_lab","Disease","Dis_lab","Dis_tooltip","Nb","prop")
        dfr_med$metr <- "research"        
        dt <- rbind(ddis_comp_burden()[,c("Region","regs_lab","Disease","Dis_lab","Dis_tooltip","Nb","prop","metr")],dfr_med)
        dt$Region <- reorder(dt$Region,new.order=regs_ord_comp())
        dt
        
    })
    
    #data for error bars
    ddis_comp_err <- reactive({
    
        d_err <- ddis_comp_res()[,c("Region","regs_lab","Disease","Dis_lab","Dis_tooltip","prop_low","prop_up")]
        d_err$metr <- "research"        
        d_err$Region <- reorder(d_err$Region,new.order=regs_ord_comp())
        d_err
    
    })        
            
    #data for hover
    ddis_comp_points <- reactive({
    
        dpts <- merge(ddis_comp_burden(),ddis_comp_res())
        dpts$Region <- reorder(dpts$Region,new.order=regs_ord_comp())
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
        
        D_bur$prop_pos[D_bur$Dis_tooltip==input$disease2] <- -D_bur$prop_pos[D_bur$Dis_tooltip==input$disease2]

        
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
        
        D_res$prop_pos[D_bur$Dis_tooltip==input$disease2] <- -D_res$prop_pos[D_bur$Dis_tooltip==input$disease2]
        
        rbind(D_bur,D_res)
        
    })
        
    
    #Hover info
#    output$hover_info_dis1 <- renderUI({
    hover_info_dis1_r <- reactive({
        
        hover <- input$plot_hover_dis1
        
        point <- nearPoints(ddis_comp_points()[ddis_comp_points()$Dis_tooltip==input$disease1,],
        hover, yvar="prop_pos", xvar="Reg_pos",
                            threshold = (1005*0.2/(length(regs_ord_comp()) + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<b> Group of diseases: </b>", input$disease1, "<br/>",
                                   "<b> Region: </b>", point$Region, "<br/>",
                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
                                   " million ", input$metr_burden_comp_dis,"s (",
                                   format(round(point$prop),nsmall = 0),"% of burden among ",
                                   ifelse(input$all_nhi_comp==TRUE,"non-high-income","all")," regions)<br/>",
                                   "<b> Research: </b>", 
                                   ifelse(input$metr_res_comp_dis=="RCTs",
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
                                                            pourc=FALSE)," ",input$metr_res_comp_dis),
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")]/1e3,
                                                            pourc=FALSE)," thousand patients")),                                  
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],
                                                   pourc=TRUE)," of research among ",
                                   ifelse(input$all_nhi_comp==TRUE,"non-high-income","all")," regions)<br/>")))
                    )
            
        })

    #Hover info
#    output$hover_info_dis2 <- renderUI({
    hover_info_dis2_r <- reactive({        
        hover <- input$plot_hover_dis2
        
        point <- nearPoints(ddis_comp_points()[ddis_comp_points()$Dis_tooltip==input$disease2,],
        hover, yvar="prop_pos", xvar="Reg_pos",
                            threshold = (1005*0.2/(length(regs_ord_comp()) + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<b> Group of diseases: </b>", input$disease2, "<br/>",
                                   "<b> Region: </b>", point$Region, "<br/>",
                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
                                   " million ", input$metr_burden_comp_dis,"s (",
                                   format(round(point$prop),nsmall = 0),"% of burden among ",
                                   ifelse(input$all_nhi_comp==TRUE,"non-high-income","all")," regions)<br/>",
                                   "<b> Research: </b>", 
                                   ifelse(input$metr_res_comp_dis=="RCTs",
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],
                                                            pourc=FALSE)," ",input$metr_res_comp_dis),
                                          paste0(form_ratio(point[,c("Nb_low","Nb_med","Nb_up")]/1e3,
                                                            pourc=FALSE)," thousand patients")),                                  
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],
                                                   pourc=TRUE)," of research among ",
                                   ifelse(input$all_nhi_comp==TRUE,"non-high-income","all")," regions)<br/>")))
                    )
        })

 output$hover_info_dis1 <- renderUI({
 if(length(hover_info_dis1_r())==0 & length(hover_info_dis2_r())==0){
     style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<br/>",
                                   "<br/>",
                                   "<br/>",
                                   "<br/>")))
                    )
     }
     else { if(length(hover_info_dis1_r())!=0) return(hover_info_dis1_r())
            if(length(hover_info_dis2_r())!=0) return(hover_info_dis2_r())
                         }
 })
        
    #Plot
    plotInput_comp_dis <- reactive({

            dt <- ddis_comp_plot()
            max_plot <- 10*ceiling(max(c(dt$prop,ddis_comp_err()$prop_up)/10,na.rm=TRUE))

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
        dlbl <- dt[order(dt$Region),]
        dlbl <- dlbl[dlbl$metr=="burden",]
#        dlbl$gap_text <- "plain"
#        dlbl$gap_text[dlbl$gap_col=="burden_gap"] <- "bold"

        p <- ggplot(dt[dt$Dis_tooltip==input$disease1,],aes(Region))
#        p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
        p <- p + geom_bar(aes(fill=metr,y=prop),position="dodge",stat="identity",width=0.8)
        p <- p + geom_errorbar(aes(x=as.numeric(Region)+0.2,ymax=prop_up,ymin=prop_low),width=0.2,
                               data=ddis_comp_err()[ddis_comp_err()$Dis_tooltip==input$disease1,])
#        if(input$hgp_dis)  { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red",
#                                                                   "research"="blue")) }
#        if(!input$hgp_dis) { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="orange",
#                                                                   "research"="blue")) }        
        p <- p + scale_fill_manual(values = c("burden"="orange","research"="blue"))
        
        if(max_plot>20 & max_plot<80) {
                p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                        name="%")
                            }
        if(max_plot>=80) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=unique(c(0,10,seq(20,max_plot,20),max_plot)),
                                        name="%")
        }
        if(max_plot<=20) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,1,seq(5,max_plot,5)),
                                        name="%")
        }
        p <- p + theme(axis.text.y=element_text(size=12))
        p <- p + theme(axis.title.y=element_text(size=20)) 

        p <- p + theme(axis.text.x = element_blank()) + 
                 theme(panel.background = element_blank()) + 
                 theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                 theme(panel.grid.major.x = element_blank()) + 
                 theme(axis.ticks.x=element_blank()) + 
                 theme(legend.position = "none") + 
                 theme(axis.title.x=element_blank())+
                 labs(x=NULL)
        
        p1 <- p
                         
        plb <- ggplot(dt,aes(Region)) 
        plb <- plb + scale_y_continuous(limits = c(0,0),breaks=c(0),label="   ",
                                        name=" ")
        plb <- plb + theme(axis.ticks.y = element_blank())
        plb <- plb + theme(axis.text.y=element_text(size=12))
        plb <- plb + theme(axis.title.y=element_text(size=20)) 

#        if(input$hgp_dis)  { plb <- plb + theme(axis.text.x = element_text(face=dlbl$gap_text)) }
        plb <- plb + theme(axis.text.x = element_text(angle=0,
                                                      hjust=0.5,
                                                      vjust=0.5,size=10))
        plb <- plb + theme(panel.background = element_blank()) + 
                     theme(panel.grid.major.y = element_blank()) +
                     theme(panel.grid.major.x = element_blank()) + 
                     theme(axis.ticks.x=element_blank()) + 
                     theme(legend.position = "none") + 
                     theme(axis.title.x=element_blank())

        plb <- plb + scale_x_discrete(label = dlbl$regs_lab[dlbl$Dis_tooltip==input$disease1])
        plb <- plb + theme(axis.text.x=element_text(size=12))

        p <- ggplot(dt[dt$Dis_tooltip==input$disease2,],aes(Region))
#        p <- p + geom_bar(aes(fill=gap_col,y=prop),position="dodge",stat="identity",width=0.8)
        p <- p + geom_bar(aes(fill=metr,y=-prop),position="dodge",stat="identity",width=0.8)
        p <- p + geom_errorbar(aes(x=as.numeric(Region)+0.2,ymax=-prop_low,ymin=-prop_up),width=0.2,
                               data=ddis_comp_err()[ddis_comp_err()$Dis_tooltip==input$disease2,])
#        if(input$hgp_dis)  { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red",
#                                                                   "research"="blue")) }
#        if(!input$hgp_dis) { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="orange",
#                                                                   "research"="blue")) }        
        p <- p + scale_fill_manual(values = c("burden"="orange","research"="blue"))
        
        if(max_plot>20 & max_plot<80) {
            p <- p + scale_y_continuous(limits = -rev(c(0,max_plot)),breaks=-rev(c(0,5,seq(10,max_plot,10))),
                                        labels = rev(c(0,5,seq(10,max_plot,10))),
                                        name="%")
                            }
        if(max_plot>=80) {
            p <- p + scale_y_continuous(limits = -rev(c(0,max_plot)),
                                        breaks = -rev(unique(c(0,10,seq(20,max_plot,20),max_plot))),
                                        labels = rev(unique(c(0,10,seq(20,max_plot,20),max_plot))),
                                        name="%")
        }
        if(max_plot<=20) {
            p <- p + scale_y_continuous(limits = -rev(c(0,max_plot)),breaks=-rev(c(0,1,seq(5,max_plot,5))),
                                        labels = rev(c(0,1,seq(5,max_plot,5))),name="%")
        }
        p <- p + theme(axis.text.y=element_text(size=12))
        p <- p + theme(axis.title.y=element_text(size=20)) 

        p <- p + theme(axis.text.x = element_blank()) + 
                 theme(panel.background = element_blank()) + 
                 theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                 theme(panel.grid.major.x = element_blank()) + 
                 theme(axis.ticks.x=element_blank()) + 
                 theme(legend.position = "none") + 
                 theme(axis.title.x=element_blank())+
                 labs(x=NULL)
        
        p2 <- p
        
        list(p1,p2,plb)
        
            
            })

    #Render Plot
    output$plot_comp_dis1 <- renderPlot({ plotInput_comp_dis()[[1]] })    
    output$plot_comp_dis2 <- renderPlot({ plotInput_comp_dis()[[2]] })
    output$plot_comp_dis_labs <- renderPlot({ plotInput_comp_dis()[[3]] })
        
    output$downloadPlot_comp_dis <- downloadHandler(
            filename = function() { paste0(input$tabs,'_',
                                           gsub('[[:punct:]| ]','_',input$disease1),'_vs_',
                                           gsub('[[:punct:]| ]','_',input$disease2),'_vs_',
                                           input$metr_burden_comp_dis,'_vs_',input$metr_res_comp_dis,
                                           '.png', sep='') },
            content = function(file) { 
                P <-  plotInput_comp_dis()
                ggsave(file, plot = grid.arrange(P[[1]], P[[3]], P[[2]],
                                                 layout_matrix = matrix(c(rep(1,8),2,rep(3,8)),ncol=1),
                                                 top = textGrob(paste0("Shares across ",
                                                                       ifelse(input$all_nhi_comp==FALSE,"",
                                                                              "non-high-income "),
                                                                       "regions of ",
                                                                       input$metr_burden_comp_dis,"s as of 2005 vs\n",
                                                                      ifelse(input$metr_res_comp_dis=="RCTs",
                                                                             "Randomized controlled trials",
                                                                             "patients planned to be enrolled in RCTs"),
                                                                      " conducted in 2006-2015\non ",
                                                                       input$disease1,
                                                                       " (top) & ", input$disease2,
                                                                       " (bottom)"),
                                                                       gp=gpar(fontsize=20))),
                device = "png", width = 15, height = 12, unit = "in") }
        )
                                                                                       
    output$title_comp_dis <- renderText({
       
        paste0( "<center><b>",
               "Shares across ", ifelse(input$all_nhi_comp==FALSE,"","non-high-income "),
               "regions of ",
               input$metr_burden_comp_dis,"s as of 2005 vs<br/>",
               ifelse(input$metr_res_comp_dis=="RCTs",
                      "Randomized controlled trials",
                      "patients planned to be enrolled in RCTs"),
               " conducted in 2006-2015<br/>on ",
               input$disease1,
               " (top) & ", input$disease2,
               " (bottom)",
               "</b></center>"
              )
        })

        #About Plot
        ######################################################################################################
        ######################################################################################################
        
        #Hover info
    output$hover_info_regs_ex <- renderUI({
        
        hover <- input$plot_hover_reg_ex
        
        point <- nearPoints(data_points_ex, hover, yvar="prop_pos", xvar="Dis_pos",
                            threshold = (1005*0.2/(10 + 0.4)), maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) {
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<br/>",
                                   "<br/>",
                                   "<br/>",
                                   "<br/>")))
                    )}
        else{

            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                            "left:", 0, "px; top:", 0, "px;")

            # actual tooltip created as wellPanel
            tags$div(class = "well well-sm",
                     style = style,
                     p(HTML(paste0("<b> Region: </b>", "Sub-Saharian Africa", "<br/>",
                                   "<b> Group of diseases: </b>", point$Dis_tooltip, "<br/>",
                                   "<b> Burden: </b>", format(round(point$Nb/1e6,1),nsmall = 1,big.mark=","),
                                   " million ", "DALY","s (",format(round(point$prop),nsmall = 0),
                                   "% of ","local"," burden)<br/>",
                                   "<b> Research: </b>",
                                   form_ratio(point[,c("Nb_low","Nb_med","Nb_up")],pourc=FALSE)," ","RCTs",
                                   " (",form_ratio(point[,c("prop_low","prop_med","prop_up")],pourc=TRUE), " of ",
                                   "local"," research)<br/>")))
                    )
            }
        })

    #Plot
    plotInput_within_reg_ex <- reactive({

            dt <- data_plot_ex
            max_plot <- 10*ceiling(max(c(dt$prop,dfr_err_ex$prop_up)/10,na.rm=TRUE))

            #Highlighting gaps
            dt$gap <- dt$Disease
            levels(dt$gap) <- unlist(lapply(levels(dt$Disease),
                                            function(x){
                                            ifelse(sum(dfr_err_ex$Disease==x)==0,NA,
                                            dt$prop[dt$metr=="burden" & dt$Disease==x]/
                                            dfr_err_ex$prop_up[dfr_err_ex$Disease==x])}))
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
            p <- p + geom_errorbar(aes(x=as.numeric(Disease)+0.2,ymax=prop_up,ymin=prop_low),width=0.2,data=dfr_err_ex)
            if(input$hgp_reg)  { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="red",
                                                                       "research"="blue")) }
            if(!input$hgp_reg) { p <- p + scale_fill_manual(values = c("burden"="orange","burden_gap"="orange",
                                                                       "research"="blue")) }
            p <- p + scale_x_discrete(label = dlbl$Dis_lab)
            if(max_plot>20 & max_plot<80) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,5,seq(10,max_plot,10)),
                                        name="%")
                            }
            if(max_plot>=80) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=unique(c(0,10,seq(20,max_plot,20),max_plot)),
                                        name="%")
                            }
            if(max_plot<=20) {
            p <- p + scale_y_continuous(limits = c(0,max_plot),breaks=c(0,1,seq(5,max_plot,5)),
                                        name="%")
                            }
            p <- p + theme(axis.text.y=element_text(size=12))
            p <- p + theme(axis.title.y=element_text(size=20))    

            p <- p + theme(axis.text.x = element_blank()) + 
                     theme(panel.background = element_blank()) + 
                     theme(panel.grid.major.y = element_line(colour = "grey",size=0.1)) +
                     theme(panel.grid.major.x = element_blank()) + 
                     theme(axis.ticks.x=element_blank()) + 
                     theme(legend.position = "none") + 
                     theme(axis.title.x=element_blank()) +
                     labs(x=NULL)

            plb <- ggplot(dt,aes(Disease)) 
            plb <- plb + scale_y_continuous(limits = c(0,0),breaks=c(0),label="   ",
                                        name=" ")
            plb <- plb + theme(axis.ticks.y = element_blank())
            plb <- plb + theme(axis.text.y=element_text(size=12))
            plb <- plb + theme(axis.title.y=element_text(size=20)) 
            if(input$hgp_reg)  { plb <- plb + theme(axis.text.x = element_text(face=dlbl$gap_text)) }

            plb <- plb + theme(axis.text.x = element_text(angle=55,
                                                          hjust=1,
                                                          vjust=1.1,size=10))
        
            plb <- plb + theme(panel.background = element_blank()) + 
                         theme(panel.grid.major.y = element_blank()) +
                         theme(panel.grid.major.x = element_blank()) + 
                         theme(axis.ticks.x=element_blank()) + 
                         theme(legend.position = "none") + 
                         theme(axis.title.x=element_blank())

            plb <- plb + scale_x_discrete(label = dlbl$Dis_lab)
            plb <- plb + theme(axis.text.x=element_text(size=12))

            list(p,plb)
            
            })
    
    #Render Plot
    output$plot_within_reg_ex <- renderPlot({ plotInput_within_reg_ex()[[1]] })
    output$plot_within_reg_labs_ex <- renderPlot({ plotInput_within_reg_ex()[[2]] })
        
        
        
}

        
            
        
shinyApp(ui = ui, server = server)
