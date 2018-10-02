#.libPaths(c(normalizePath("./libs"), .libPaths()))

library(shiny)
library(stringr)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)
library(V8)

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

ui <- dashboardPage(
  dashboardHeader(title = "Vendor eCOA Checker",
                  titleWidth = 270),
  dashboardSidebar(
    width = 270,
    sidebarMenu(
      menuItem("Data Download", icon = icon("cloud-download"), startExpanded = TRUE,
               div(style="display:inline-block;width:100%;text-align: center;",
                   downloadButton("downloadData", "Download Dummy eCOA File", class="downloadBtn"))
               ),
      menuItem("Data Upload", tabName = "home", icon = icon("file"),
               selectInput(inputId = "datatype", label = "What type of Portal Data is Available?", 
                           choices = c("Comprehensive Data Dump" = 1, "Raw Portal Data" = 2), selected = 1),
               uiOutput("showRaw"),
               uiOutput("showDD"),
               uiOutput("showDS"),
               uiOutput("showBP")
      ),
      menuItem("Portal Data Checker", tabName = "raw", icon = icon("check")),
      menuItem("Diary Data Viewer", tabName = "viewer", icon = icon("table")),
      menuItem("Questions/Need Assistance?", icon = icon("question-circle"),
               br(),
               div(span(icon("user-circle"), "Melanie Ren (renm1@gene.com)", style = "padding-left:10px"), style = "height:16px;"),
               br()
      ),
      br(),
      div(style="display:inline-block;width:100%;text-align: center;",downloadButton("dnBtn", "Download Comprehensive Report", class="downloadBtn"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    tags$head(
      tags$script(type="text/javascript",'$(document).ready(function(){
                             $(".main-sidebar").css("height","100%");
                  $(".main-sidebar .sidebar").css({"position":"relative","max-height": "100%","overflow": "auto"})
                  })'),
      tags$style(
        # Fix widths of infoboxes with html since the function call is bugged
        '#sfchk, #rbchk, #dupchk, #scrnfchk, #bwlendochk {width:20%}',
        HTML('
             /* logo */
             .skin-blue .main-header .logo {
             background: linear-gradient(to right, rgb(32, 193, 237), rgb(64,142,186));
             font-weight: bold;
             position: fixed;
             width: 270px;
             }
             
             /* logo when hovered */
             .skin-blue .main-header .logo:hover {
             background: linear-gradient(to right, rgb(32, 193, 237), rgb(64,142,186));
             }
             
             /* navbar (rest of the header) */
             .skin-blue .main-header .navbar {
             background: linear-gradient(to right, rgb(64,142,186),rgb(74, 109, 130));
             width: 100%;
             position: fixed;
             }        
             
             .content {
             padding-top: 60px;
             }
             
             /* navbar when hovered */
             .skin-blue .main-header .navbar:hover {
             background: linear-gradient(to right, rgb(64,142,186),rgb(32, 193, 237));
             }    
             
             /* collapse button when hovered */
             .skin-blue .main-header .navbar .sidebar-toggle:hover {
             background-color: rgb(64,142,186);
             }
             
             /* main sidebar */
             .skin-blue .main-sidebar {
             /*background: linear-gradient(217deg, rgba(10,10,10, 0.9), rgba(100,100,100, 0) 80%),
             linear-gradient(127deg, rgba(64,142,186,0.9), rgba(64,142,186,0) 70.71%),
             linear-gradient(336deg, rgba(32, 193, 237,0.9), rgba(32, 193, 237,0) 70.71%);*/
             background-color: rgb(36,49,55);
             position: fixed;
             white-space: nowrap;
             overflow: visible;
             }
             
             
             /* active selected tab in the sidebarmenu */
             .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
             background-color: rgba(0,0,0,0.3);
             border-left-color: rgb(32, 193, 237);
             }
             
             
             /* other links in the sidebarmenu when hovered */
             .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
             background-color: rgba(0,0,0,0.3);
             border-left-color: rgb(32, 193, 237);
             color: #fffff;
             }
             
             /* Change backgroud color*/
             .content-wrapper, .right-side {
             background-color: #def5fc; /*(18,34,38);*/
             }
             
             /* Custom Download Button */
             .downloadBtn {
             color:white;
             background-color:rgb(36,49,55);
             }
             
             ')
        )
        ),
    tabItems(
      tabItem(tabName = "raw",
              fluidRow(
                box(title = "Read Me First!", 
                    id = "readme",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    p("This app was created as part of my internship projects at Genentech. It is designed for the QC of received Electronic Clinical Outcome Assessments (eCOA) 
                      data provided by an external vendor for our phase II study in the Ulcerative Colitis indication. This is a modified version
                      of the app and is intended to be used as a sample for my project portfolio. The purpose of this application is to 
                      cross-check the vendor-derived data values (bundled in the eCOA file with raw data) with our own derivation of the 
                      same variables based off the same raw data. It also provides some generic data checks to test for the presence of illogical data.", style = "font-size:12pt"),
                    h4(span("Directions:", style="font-weight:bold")),
                    tags$ol(
                      tags$li("Obtain portal dump file(s) in .csv format"),
                      tags$ul(
                        tags$li("Normally the file(s) would be obtained directly from the vendor but for this showcase purpose 
                                I have provided a downloadable sample file to use in the sidebar.")
                      ),
                      tags$li("Select the type of data available"),
                      tags$ul(
                        tags$li("Choose", span("Comprehensive Data Dump", style = "color: rgb(64,142,186);font-weight:bold;"), 
                                "if the vendor single comprehensive data dump is available"),
                        tags$ul(
                          tags$li("This is the file with all testcodes stacked together in rows", 
                                  tags$strong("(Please choose this option and upload the sample I provided.)"))
                        ),
                        tags$li("Choose", span("Raw Portal Data", style = "color: rgb(64,142,186);font-weight:bold;"),
                                "if the separate data files from the portal are available"),
                        tags$ul(
                          tags$li("Three separate files downloaded from the ERT portal are required for this option"),
                          tags$li(span("Daily Diary Data", style = "color: rgb(64,142,186)"), " is the file with only raw diary entries"),
                          tags$li(span("Daily Diary Scores Data", style = "color: rgb(64,142,186)"), " is the file with the vendor-computed average diary scores"),
                          tags$li(span("Bowel Prep Date Data", style = "color: rgb(64,142,186)"), " is the file with bowel prep and endoscopy date information")
                        )
                      ),
                      tags$li("Upload the correct file(s) first before attempting to navigate to other pages of this app"),
                      tags$li("Use this page(", span("Portal Data Checker", style = "color: rgb(64,142,186)"), ")to check device algorithm (average daily diary score calculations)"),
                      tags$ul(
                        tags$li("Click the ",span("Download Comprehensive Report", style = "color: rgb(64,142,186)"), " button to download a PDF with a summary of the data QC")
                      ),
                      tags$li("Use the", span("Data Viewer", style = "color: rgb(64,142,186)"), "to view a summary of the computed scores and raw diary entries for uploaded data")
                    ),
                    p(span("Last Update:", style = "color: red"), " October 2, 2018"),
                    hr(),
                    p("Once the Vendor Portal data file is uploaded, this page will automatically refresh to load the data check 
                      and display a summary of whether the data checks have passed.", span("Aqua-colored tables",style="background:rgb(0,196,236); color:white"),"indicate checks passed while",
                      span("red-colored tables", style="background:rgb(239,69,59); color:white"), "indicate errors were found. 
                      If checks failed, data tables with the inconsistent records will appear below.", style="font-size:12pt")
                    )
                ),
              fluidRow(
                uiOutput("varbox")
              ),
              fluidRow(
                infoBoxOutput("sfchk"),
                infoBoxOutput("rbchk"),
                infoBoxOutput("scrnfchk"),
                infoBoxOutput("bwlendochk"),
                infoBoxOutput("dupchk")
              ),
              fluidRow(
                uiOutput("sfbox"),
                uiOutput("rbbox"),
                uiOutput("scrnfbox"),
                uiOutput("bwlendobox"),
                uiOutput("dupbox")
              )
                ),
      tabItem(tabName = "viewer",
              fluidRow(
                box(
                  width = 12,
                  solidHeader = TRUE,
                  tabBox(
                    title = h4("eCOA Data Tables"),
                    id = "dataTabs",
                    width = 12,
                    tabPanel(h4("Summary Table of Diary Scores by Visit"), value = "sum",
                             span(textOutput("sumEmpty"), align = "center", style = "color:red"),
                             DT::dataTableOutput("sumTbl")
                    ),
                    tabPanel(h4("Raw Diary Entries"), value = "raw",
                             span(textOutput("rawEmpty"), align = "center", style = "color:red"),
                             DT::dataTableOutput("rawTbl")
                    )
                  )
                )
              )
      )   
    )
    )
      )

server <- function(input, output) {
  # Raises limit on file upload size
  options(shiny.maxRequestSize=30*1024^2)
  hide("dnBtn")
  
  ###################################################,
  ##### Output Display
  ###################################################,
  
  ###########################,
  ##### Menu Sidebar
  ###########################,
  
  output$showRaw <- renderUI({
    if(input$datatype == 1){
      fileInput("raw", label = "Portal Dump Data (.csv)",
                accept = c("text/csv"))
    }
  })
  
  output$showDD <- renderUI({
    if(input$datatype == 2)
      fileInput("raw_dd", label = "Daily Diary Data (.csv)",
                accept = c("text/csv"))
  })
  
  output$showDS <- renderUI({
    if(input$datatype == 2)
      fileInput("raw_ds", label = "Daily Diary Scores Data (.csv)",
                accept = c("text/csv"))
  })
  
  output$showBP <- renderUI({
    if(input$datatype == 2)
      fileInput("raw_bp", label = "Bowel Prep Date Data (.csv)",
                accept = c("text/csv"))
  })
  
  output$dnBtn <- downloadHandler(
    filename = function() {
      paste0('Sample_QC_Report_', Sys.time(),".pdf")
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd')
      file.rename(out, file)
    }
  )
  
  ###########################,
  ##### ERT Checker
  ###########################,
  output$varbox <- renderUI({
    getVarData()
  })
  
  output$sfchk <- renderInfoBox({
    value = "Passed"
    icon = icon("check-square")
    color = "aqua"
    fill = FALSE
    if(nrow(getStlData())>0){
      value = paste(nrow(getStlData()), "Failed")
      icon = icon("exclamation-triangle")
      color = "red"
      fill = TRUE
    }
    
    infoBox(title = HTML(paste("Average Stool",br(), "Frequency")), 
            value = value, icon = icon, color = color, fill = fill, width = 6
    )
  })
  
  output$rbchk <- renderInfoBox({
    value = "Passed"
    icon = icon("check-square")
    color = "aqua"
    fill = FALSE
    if(nrow(getRbData())>0){
      value = paste(nrow(getRbData()), "Failed")
      icon = icon("exclamation-triangle")
      color = "red"
      fill = TRUE
    }
    infoBox(title = HTML(paste("Average Rectal",br(), "Bleeding")), 
            value = value, icon = icon, color = color, fill = fill, width = 6
    )
  })
  
  output$scrnfchk <- renderInfoBox({
    value = "Passed"
    icon = icon("check-square")
    color = "aqua"
    fill = FALSE
    if(nrow(getScrnFailRndData())>0){
      value = paste(nrow(getScrnFailRndData()), "Failed")
      icon = icon("exclamation-triangle")
      color = "red"
      fill = TRUE
    }
    
    infoBox(title = HTML(paste("Screen Fail",br(), "Randomization")), 
            value = value, icon = icon, color = color, fill = fill
    )
  })
  
  output$bwlendochk <- renderInfoBox({
    value = "Passed"
    icon = icon("check-square")
    color = "aqua"
    fill = FALSE
    if(nrow(getBwlEndoData())>0){
      value = paste(nrow(getBwlEndoData()), "Failed")
      icon = icon("exclamation-triangle")
      color = "red"
      fill = TRUE
    }
    
    infoBox(title = HTML(paste("Bowel Prep",br(),"Endoscopy")),
            value = value, icon = icon, color = color, fill = fill
    )
  })
  
  output$dupchk <- renderInfoBox({
    value = "Passed"
    icon = icon("check-square")
    color = "aqua"
    fill = FALSE
    if(nrow(getDupData())>0){
      value = paste(nrow(getDupData()), "Failed")
      icon = icon("exclamation-triangle")
      color = "red"
      fill = TRUE
    }
    
    infoBox(title = HTML(paste("Duplicate",br(), "Diary Entry")), 
            value = value, icon = icon, color = color, fill = fill
    )
  })
  
  output$sfbox <- renderUI({
    ct = nrow(getStlData())
    title = paste0("Stool Frequency Score Calculation Mismatches (", ct, " records found)")
    output$stTbl <- DT::renderDataTable(
      datatable(getStlData(),
                rownames = FALSE,
                extensions = c('Buttons','ColReorder'),
                options = list(scrollX=TRUE, dom ='Bfrtip', 
                               pageLength = 15,
                               orientation = 'landscape',
                               buttons = list('colvis',
                                              list(extend = 'csv', 
                                                   filename = paste0('sfchk_',Sys.Date())),
                                              list(
                                                extend = 'pdf',
                                                title = paste0('GA39925 Stool Frequency Average Miscalculations (',ct,' records found)'),
                                                messageTop = paste('This table was created on', Sys.time()),
                                                messageBottom = '\nGenerated with https://go.gene.com/ga39925datacheck',
                                                pageSize = 'A4',
                                                orientation = 'landscape',
                                                filename = paste0('sfchk_',Sys.Date()))), colReorder = TRUE), 
                filter = "top"),
      #Allows for download of full table instead of just what's displayed on page
      server = FALSE
    )
    box(
      title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = (ct==0),
      status = ifelse(ct > 0, "danger", "info"),
      solidHeader = TRUE,
      p("This table outputs all records found where the derived average stool frequency score does not match our algorithm.", style = "font-size:12pt; font-weight:bold"),
      br(),
      DT::dataTableOutput('stTbl')
    )
  })
  
  output$rbbox <- renderUI({
    ct = nrow(getRbData())
    title = paste0("Rectal Bleeding Score Calculation Mismatches (", ct, " records found)")
    output$rbTbl <- DT::renderDataTable(
      datatable(getRbData(),
                rownames = FALSE,
                extensions = c('Buttons','ColReorder'),
                options = list(scrollX=TRUE, dom ='Bfrtip', pageLength = 15,
                               buttons = list('colvis',
                                              list(
                                                extend = 'csv',
                                                filename = paste0('rbchk_',Sys.Date())),
                                              list(
                                                extend = 'pdf',
                                                title = paste0('GA39925 Rectal Bleeding Average Miscalculations (',ct,' records found)'),
                                                messageTop = paste('This table was created on', Sys.time()),
                                                messageBottom = '\nGenerated with https://go.gene.com/ga39925datacheck',
                                                pageSize = 'A4',
                                                orientation = 'landscape',
                                                filename = paste0('rbchk_',Sys.Date()))), colReorder = TRUE), 
                filter = "top"),
      #Allows for download of full table instead of just what's displayed on page
      server = FALSE
    )
    box(
      title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = (ct==0),
      status = ifelse(ct > 0, "danger", "info"),
      solidHeader = TRUE,
      p("This table outputs all records found where the derived average rectal bleeding score does not match our algorithm.", style = "font-size:12pt; font-weight:bold"),
      br(),
      DT::dataTableOutput('rbTbl')
    )
  })
  
  output$scrnfbox <- renderUI({
    ct = nrow(getScrnFailRndData())
    title = paste0("Randomized Screen Failure Subjects (", ct, " records found)")
    output$scrnfTbl <- DT::renderDataTable(
      datatable(getScrnFailRndData(),
                rownames = FALSE,
                extensions = c('Buttons','ColReorder'),
                options = list(scrollX=TRUE, dom ='Bfrtip', pageLength = 15,
                               buttons = list('colvis',
                                              list(
                                                extend = 'csv',
                                                filename = paste0('scrnfchk_',Sys.Date())),
                                              list(
                                                extend = 'pdf',
                                                title = paste0('GA39925 Screen Failure Patients with Randomization ID (',ct,' records found)'),
                                                messageTop = paste('This table was created on', Sys.time()),
                                                messageBottom = '\nGenerated with https://go.gene.com/ga39925datacheck',
                                                pageSize = 'A4',
                                                orientation = 'portrait',
                                                filename = paste0('scrnfchk_',Sys.Date()))), colReorder = TRUE), 
                filter = "top"),
      #Allows for download of full table instead of just what's displayed on page
      server = FALSE
    )
    box(
      title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = (ct == 0),
      status = ifelse(ct > 0, "danger", "info"),
      solidHeader = TRUE,
      p("This table outputs all records found where the subject failed screening but was still assigned a randomization ID.", style = "font-size:12pt; font-weight:bold"),
      br(),
      DT::dataTableOutput('scrnfTbl')
    )
  })
  
  output$bwlendobox <- renderUI({
    ct = nrow(getBwlEndoData())
    title = paste0("Bowel Prep Date vs. Endoscopy Date Mismatches (", ct, " records found)")
    output$bwlendoTbl <- DT::renderDataTable(
      datatable(getBwlEndoData(),
                rownames = FALSE,
                extensions = c('Buttons','ColReorder'),
                options = list(scrollX=TRUE, dom ='Bfrtip', pageLength = 15,
                               buttons = list('colvis',
                                              list(
                                                extend = 'csv',
                                                filename = paste0('bwlendochk_',Sys.Date())),
                                              list(
                                                extend = 'pdf',
                                                title = paste0('GA39925 Bowel Prep and Endoscopy Date Mismatches (',ct,' records found)'),
                                                messageTop = paste('This table was created on', Sys.time()),
                                                messageBottom = '\nGenerated with https://go.gene.com/ga39925datacheck',
                                                pageSize = 'A4',
                                                orientation = 'portrait',
                                                filename = paste0('bwlendochk_',Sys.Date()))), colReorder = TRUE), 
                filter = "top"),
      #Allows for download of full table instead of just what's displayed on page
      server = FALSE
    )
    box(
      title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = (ct == 0),
      status = ifelse(ct > 0, "danger", "info"),
      solidHeader = TRUE,
      p("This table outputs all records found where the bowel prep date and endoscopy date entered do not make sense:", style = "font-size:12pt; font-weight:bold"),
      tags$ul(
        tags$li("Record of either bowel prep date or endoscopy but not the other"),
        tags$li("Bowel prep that takes place after endoscopy"),
        tags$li("Bowel prep more than 1 day prior to endoscopy")
      ),
      br(),
      DT::dataTableOutput('bwlendoTbl')
    )
  })
  
  output$dupbox <- renderUI({
    ct = nrow(getDupData())
    title = paste0("Duplicate Date Diary Entries (", ct, " records found)")
    output$dupTbl <- DT::renderDataTable(
      datatable(getDupData(),
                rownames = FALSE,
                extensions = c('Buttons','ColReorder'),
                options = list(scrollX=TRUE, dom ='Bfrtip', pageLength = 15,
                               buttons = list('colvis',
                                              list(
                                                extend = 'csv',
                                                filename = paste0('dupchk_',Sys.Date())),
                                              list(
                                                extend = 'pdf',
                                                title = paste0('GA39925 Duplicate Daily Diary Entries (',ct,' records found)'),
                                                messageTop = paste('This table was created on', Sys.time()),
                                                messageBottom = '\nGenerated with https://go.gene.com/ga39925datacheck',
                                                pageSize = 'A4',
                                                orientation = 'portrait',
                                                filename = paste0('dupchk_',Sys.Date()))), colReorder = TRUE), 
                filter = "top"),
      #Allows for download of full table instead of just what's displayed on page
      server = FALSE
    )
    box(
      title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = (ct == 0),
      status = ifelse(ct > 0, "danger", "info"),
      solidHeader = TRUE,
      p("This table outputs all records found where a subject has multiple diary entries for the same test on the same date.", style = "font-size:12pt; font-weight:bold"),
      br(),
      DT::dataTableOutput('dupTbl')
    )
  })
  
  ###########################,
  ##### Data Viewer
  ###########################,
  output$sumEmpty <- renderText({
    if((input$datatype == 1 & is.null(input$raw)) | 
       (input$datatype == 2 & (is.null(input$raw_dd) | is.null(input$raw_ds) | is.null(input$raw_bp))
       )){
      return("ERT Portal data has not been uploaded yet. Please upload the file to view.")
    } else {
      return("")
    }
  })
  
  output$rawEmpty <- renderText({
    if((input$datatype == 1 & is.null(input$raw)) | 
       (input$datatype == 2 & (is.null(input$raw_dd) | is.null(input$raw_ds) | is.null(input$raw_bp))
       )){
      return("ERT Portal data has not been uploaded yet. Please upload the file to view.")
    } else {
      return("")
    }
  })
  
  output$sumTbl <- DT::renderDataTable(
    datatable(getData(),
              rownames = FALSE,
              extensions = c('Buttons','ColReorder'),
              options = list(scrollX=TRUE, dom ='Bfrtip', pageLength = 20,
                             buttons = list('colvis',list(extend = 'csv', 
                                                          filename = 'summaryscoretable')), 
                             colReorder = TRUE), 
              filter = "top"),
    server = FALSE
  )
  
  output$rawTbl <- DT::renderDataTable(
    datatable(getData(),
              rownames = FALSE,
              extensions = c('Buttons','ColReorder'),
              options = list(scrollX=TRUE, dom ='Bfrtip', pageLength = 20,
                             buttons = list('colvis',list(extend = 'csv', 
                                                          filename = 'rawdiaries')), 
                             colReorder = TRUE), 
              filter = "top"),
    server = FALSE
  )
  
  ###################################################,
  ##### Function Logic
  ###################################################,
  
  ###########################,
  ##### Custom functions
  ###########################,
  countDiary <- function(data, visit, currdate, actdate = ""){
    if(visit == 20){
      data <- data %>%
        dplyr::filter(Date < currdate) #& Date >= actdate)
    } else if(visit == 30 | visit == 40) {
      data <- data %>%
        dplyr::filter(Date < currdate & Date >= (currdate - 7))
    } else {
      data <- data %>%
        dplyr::filter(Date < currdate & Date >= (currdate - 14))
    }
    return(nrow(data))
  }
  
  sumDiaryScore <- function(df, subj, visit, currdate, assigndate = "", actdate = "") {
    data <- df %>%
      dplyr::filter(USUBJID == subj)
    
    # calculate total completed diaries in visit window
    drycmp <- countDiary(data,visit,currdate,actdate)
    
    # derive entries within visit window
    if(visit == 20) {
      data <- data %>%
        dplyr::filter(Date < currdate) # & Date >= assigndate)
      
    } else if(visit == 30 | visit == 40) {
      data <- data %>%
        dplyr::filter(Date < currdate & Date >= (currdate - 7)) 
    } else {
      data <- data %>%
        dplyr::filter(Date < currdate & Date >= (currdate - 14)) 
    }
    
    # only select most recent 3 entries
    data <- data %>%
      dplyr::filter(row_number() >= (nrow(data)-2))
    
    # total up entry scores
    sumScore <- sum(as.numeric(data$QSSTRESN))
    
    return(c(sumScore,drycmp))
  }
  
  # Check if diary date overlaps with an excluded day (bowel prep range)
  bwlEndoDay <- function(subj, dt) {
    bwl <- getBwlData() %>%
      dplyr::filter(USUBJID == subj[1])
    endo <- getEndoData() %>%
      dplyr::filter(USUBJID == subj[1])
    
    # Date range of endo + 1
    dtrange = as.Date(c((dt[1]-1): dt[1]), origin = "1970-1-1")
    
    # Boolean for whether dt is on bwl prep or endo or endo + 1
    res = ((dtrange %in% endo$EndoDt) | (dt %in% bwl$BwlPrepDt))
    
    if(TRUE %in% res){
      return(1)
    }
    return(0)
  }
  
  # Custom round function to round to nearest whole number
  round2 = function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    z*posneg
  }
  
  ###########################,
  ##### App functions
  ###########################,
  inputData <- read.csv("dummy_eCOA.csv", header = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "dummy_eCOA.csv"
    },
    content = function(file) {
      write.csv(inputData, file, row.names = FALSE)
    }
  )
  
  getVarData <- reactive({
    if(input$datatype == 1){
      req(input$raw)
    } else {
      req(input$raw_dd, input$raw_ds, input$raw_bp)
    }
    box(
      title = "Description of Table Variables",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "primary",
      solidHeader = TRUE,
      p("Variable names in CAPS are from the raw data, while variable names with mixed casing are derived from this application's data-check algorithm.", style = "font-size:14pt"),
      fluidRow(
        column(
          width = 4,
          tags$ul(
            tags$li(code("USUBJID"), span(": Unique subject identifier")),
            tags$li(code("Date"), ": Date of clinical visit/start of visit window"),
            tags$li(code("Time"), ": Time of clinical visit/start of visit window"),
            tags$li(code("VISITNUM"), ": Clinical visit number label"),
            tags$li(code("VISIT"), ": Clinical Visit type"),
            tags$li(code("BwlPrepDt"), ": Bowel prep date (if available) for the previous visit window"),
            tags$li(code("EndoDt"), ": Endoscopy date (if available) for the previous visit window"),
            tags$li(code("DRYCMP1N"), ": Count of total diaries (1 stool + 1 rectal bleeding is counted as 1 entry) completed in visit window")
          )
        ),
        column(
          width = 4,
          tags$ul(
            tags$li(code("QSTESTCD"), ": Short code notation for test category"),
            tags$li(code("QSTEST"), ": Extended test category label"),
            tags$li(code("StlDiaryTot"),": Count of total stool frequency diaries completed, excluding bowel prep date range",span("*",style = "color:#3A9BF0")),
            tags$li(code("StlSum"),": Sum of the most (2 or )3 recent stool frequency diary entry scores",span("*",style = "color:#3A9BF0")),
            tags$li(code("SFSSCR1N"), ": Sum of the most (2 or )3 recent stool frequency diary entry scores",span("*",style = "color:#3A9BF0")),
            tags$li(code("StlAvg"), ": Average of the most (2 or )3 recent stool frequency diary entry scores",span("*",style = "color:#3A9BF0")),
            tags$li(code("AVGSFS1N"), ": Average of the most (2 or )3 recent stool frequency diary entry scores",span("*",style = "color:#3A9BF0"))
          )
        ),
        column(
          width = 4,
          tags$ul(
            tags$li(code("QSSTRESC"), ": Test result (character)"),
            tags$li(code("QSSTRESN"), ": Test result (numeric)"),
            tags$li(code("RbDiaryTot"), ": Count of total rectal bleeding diaries completed, excluding bowel prep date range",span("*",style = "color:#3A9BF0")),
            tags$li(code("RbSum"), ": Sum of the most (2 or )3 recent rectal bleeding diary entry scores",span("*",style = "color:#3A9BF0")),
            tags$li(code("RBSSCR1N"), ": Sum of the most (2 or )3 recent rectal bleeding diary entry scores",span("*",style = "color:#3A9BF0")),
            tags$li(code("RbAvg"), ": Average of the most (2 or )3 recent rectal bleeding diary entry scores",span("*",style = "color:#3A9BF0")),
            tags$li(code("AVGRBS1N"), ": Average of the most (2 or )3 recent rectal bleeding diary entry scores",span("*",style = "color:#3A9BF0"))
          )
        )
      ),
      
      span("* Derivation excludes all records on bowel prep date or on endoscopy date and the day after",style = "color:rgb(64,142,186)")
      
    )
    #USUBJID, Date, VISITNUM, VISIT, BwlPrepDt, StlDiaryTot, StlDiaryTotAll, RbDiaryTot, RbDiaryTotAll, DRYCMP1N, StlSum, SFSSCR1N, StlAvg, AVGSFS1N, RbSum, RBSSCR1N, RbAvg, AVGRBS1N)
    
  })
  
  getData <- reactive({
    if(input$dataTabs == "sum"){
      getDiaryData() %>%
        dplyr::arrange(USUBJID, Date)
    } else {
      getRawDiaryData() %>%
        dplyr::select(USUBJID, Date, Time, QSTESTCD, QSTEST, QSSTRESC, QSSTRESN)
    }
  })
  
  getRawData <- reactive({
    data <- NULL
    if(input$datatype == 1) {
      req(input$raw)
      data <- read.csv(input$raw$datapath, header = TRUE)
    } else {
      req(input$raw_dd, input$raw_ds, input$raw_bp)
      
      raw_dd <- read.csv(input$raw_dd$datapath, header = TRUE)
      raw_ds <- read.csv(input$raw_ds$datapath, header = TRUE)
      raw_bp <- read.csv(input$raw_bp$datapath, header = TRUE)
      
      coa <- data.frame(matrix(nrow=0,ncol=13))
      names(coa) <- c("STUDYID", "DOMAIN", "USUBJID", "QSTESTCD", "QSTEST", "QSCAT", "QSSCAT", "QSORRES","QSSTRESC", "QSSTRESN", "VISITNUM", "VISIT", "QSDTC")
      
      VISITNUM=c(20,30,40,50,60,70,90,100,120,130,140,150,160,170,180,190,200,210,220,230,240)
      VISIT = c("WEEK 0", "WEEK 1", "WEEK 2", "WEEK 4", "WEEK 6", "WEEK 8", "WEEK 14","WEEK 22", "4 WEEK SAFETY F/U", "8 WEEK SAFETY F/U","UNSCHEDULED VISIT 1",
                "UNSCHEDULED VISIT 2", "UNSCHEDULED VISIT 3", "UNSCHEDULED VISIT 4", "UNSCHEDULED VISIT 5", "DISEASE EVALUATION 1", "DISEASE EVALUATION 2", "DISEASE EVALUATION 3", "DISEASE EVALUATION 4", "DISEASE EVALUATION 5","EARLY TERMINATION")
      
      visits <- cbind.data.frame(VISITNUM,VISIT) %>%
        dplyr::mutate(VISIT = as.character(VISIT))
      
      # Generate diary score summary portion of data file
      ds <- raw_ds %>%
        dplyr::mutate(STUDYID = "GA39925", DOMAIN = "QS", QSCAT = "Daily Diary Scores", QSSCAT = "Daily Diary Scores") %>%
        dplyr::mutate(DtTm = as.character(Report.Datetime)) %>%
        dplyr::mutate(Dt = substring(DtTm,1,11)) %>%
        dplyr::mutate(Tm = substring(DtTm, 13,17)) %>%
        dplyr::mutate(Dt = as.Date(Dt, format='%d %b %Y')) %>%
        dplyr::mutate(DtTm = paste(Dt,Tm)) %>%
        dplyr::mutate(DtTm = as.POSIXct(DtTm)) %>%
        dplyr::mutate(QSDTC = paste0(Dt,"T",Tm,":00")) %>%
        dplyr::mutate(Visit = toupper(Visit), Visit = ifelse(Visit == "-", NA, Visit)) %>%
        dplyr::rename(VISIT = Visit, USUBJID = Subject.ID) %>%
        dplyr::left_join(visits, by = "VISIT") %>%
        dplyr::rename(SFSSCR1N = Stool.Frequency.Score, RBSSCR1N = Rectal.Bleeding.Score, DRYCMP1N = Diaries.Completed, AVGSFS1N = Average.Stool.Frequency.Score, AVGRBS1N = Average.Rectal.Bleeding.Score, RANDOM1N = Randomization.ID) %>%
        dplyr::mutate(VST1L = VISIT) %>%
        tidyr::gather(key = QSTESTCD, value = QSSTRESC, SFSSCR1N, RBSSCR1N, DRYCMP1N, AVGSFS1N, AVGRBS1N, VST1L, RANDOM1N) %>%
        dplyr::mutate(QSSTRESC = ifelse(QSSTRESC == "-", NA, QSSTRESC), QSORRES = QSSTRESC, QSSTRESN = ifelse(QSTESTCD=="VST1L", as.numeric(VISITNUM), as.numeric(QSSTRESC))) %>%
        dplyr::select(STUDYID, DOMAIN, USUBJID, QSTESTCD, QSCAT, QSSCAT, QSORRES, QSSTRESC, QSSTRESN, VISITNUM, VISIT, QSDTC)
      
      # Generate raw diary
      dd <- raw_dd %>%
        dplyr::mutate(STUDYID = "GA39925", DOMAIN = "QS", QSCAT = "Daily Diary", QSSCAT = "Daily Diary") %>%
        dplyr::mutate(DtTm = as.character(Report.Datetime)) %>%
        dplyr::mutate(Dt = substring(DtTm,1,11)) %>%
        dplyr::mutate(Tm = substring(DtTm, 13,17)) %>%
        dplyr::mutate(Dt = as.Date(Dt, format='%d %b %Y')) %>%
        dplyr::mutate(DtTm = paste(Dt,Tm)) %>%
        dplyr::mutate(DtTm = as.POSIXct(DtTm)) %>%
        dplyr::mutate(QSDTC = paste0(Dt,"T",Tm,":00")) %>%
        dplyr::rename(USUBJID = Subject.ID, NUMSTL1L = How.many.stools, RECBLD1L = Rectal.bleeding) %>%
        tidyr::gather(key = QSTESTCD, value = QSORRES, NUMSTL1L, RECBLD1L) %>%
        dplyr::mutate(QSSTRESC = QSORRES, QSSTRESN = as.numeric(substring(QSSTRESC,1,1)), VISITNUM = NA, VISIT = NA) %>%
        dplyr::select(STUDYID, DOMAIN, USUBJID, QSTESTCD, QSCAT, QSSCAT, QSORRES, QSSTRESC, QSSTRESN, VISITNUM, VISIT, QSDTC)
      
      bwl <- raw_bp %>%
        dplyr::mutate(STUDYID = "GA39925", DOMAIN = "QS", QSCAT = "Bowel Prep Date", QSSCAT = "Bowel Prep Date") %>%
        dplyr::mutate(DtTm = as.character(Report.Datetime)) %>%
        dplyr::mutate(Dt = substring(DtTm,1,11)) %>%
        dplyr::mutate(Tm = substring(DtTm, 13,17)) %>%
        dplyr::mutate(Dt = as.Date(Dt, format='%d %b %Y')) %>%
        dplyr::mutate(DtTm = paste(Dt,Tm)) %>%
        dplyr::mutate(DtTm = as.POSIXct(DtTm)) %>%
        dplyr::mutate(QSDTC = paste0(Dt,"T",Tm,":00")) %>%
        dplyr::rename(USUBJID = Subject.ID, BWLPRP1D = Bowel.prep.date, ENDDAT1D = Endoscopy.date, RANDOM1N = Randomization.ID) %>%
        dplyr::mutate(VISIT = toupper(Visit), VISIT = ifelse(VISIT == "-", NA, VISIT), VST1L = VISIT) %>%
        dplyr::left_join(visits, by = "VISIT") %>%
        tidyr::gather(key = QSTESTCD, value = QSORRES, BWLPRP1D, ENDDAT1D, VST1L, RANDOM1N) %>%
        dplyr::mutate(QSORRES = ifelse(QSORRES == "-", NA, QSORRES),QSSTRESC = QSORRES, QSSTRESN = ifelse(QSTESTCD == "VST1L", as.numeric(VISITNUM), as.numeric(QSSTRESC))) %>%
        dplyr::select(STUDYID, DOMAIN, USUBJID, QSTESTCD, QSCAT, QSSCAT, QSORRES, QSSTRESC, QSSTRESN, VISITNUM, VISIT, QSDTC) %>%
        dplyr::mutate(QSSTRESC = as.Date(QSSTRESC, format ='%d %b %Y'), QSSTRESC = as.character(format(QSSTRESC, '%d-%b-%y'))) %>%
        dplyr::mutate(QSORRES = QSSTRESC)
      
      coa <- rbind(ds,dd)
      
      testcodes <- data.frame(c("SFSSCR1N","RBSSCR1N","DRYCMP1","AVGSFS1N","AVGRBS1N","VST1L","RANDOM1N","NUMSTL1L","RECBLD1L","BWLPRP1D","ENDDAT1D"),
                              c("Stool Frequency Score Sum", "Rectal Bleeding Score Sum", "Completed Diaries", "Average Stool Frequency Score", 
                                "Average Rectal Bleeding Score", "Visit Label", "Randomization ID", "Stool Frequency Diary Entry", "Rectal Bleeding Diary Entry",
                                "Bowel Prep Date", "Endoscopy Date"))
      names(testcodes) <- c("QSTESTCD", "QSTEST")
      
      data <- rbind(coa,bwl) %>%
        dplyr::left_join(testcodes, by = "QSTESTCD")
    }
    
    data <- data %>%
      dplyr::select(USUBJID, QSTESTCD, QSTEST, QSCAT, QSSCAT, QSORRES, QSSTRESC, QSSTRESN, VISITNUM, VISIT, QSDTC) %>%
      dplyr::filter(grepl("Daily Diary|Bowel Prep Date|Assignment",QSCAT)) %>%
      # Create Datetime format variable to sort by
      dplyr::mutate(DtTm = str_replace(as.character(QSDTC), "T", " ")) %>%
      dplyr::mutate(DtTm = as.POSIXct(DtTm)) %>%
      dplyr::mutate(Date = as.Date(substring(QSDTC,1,10))) %>%
      dplyr::mutate(Time = substring(QSDTC,12,19)) %>%
      dplyr::mutate(USUBJID = as.character(USUBJID)) %>%
      dplyr::arrange(USUBJID, DtTm) %>%
      dplyr::select(-QSDTC) %>%
      # Keep only second round test subjects/records
      dplyr::filter(grepl("114789|412048|210004|13790|15987",USUBJID)) %>%
      dplyr::filter(!(USUBJID == "412048" & Date < as.Date("2018-08-01")))
    
    js$collapse("readme")
    return(data)
  })
  
  getBwlData <- reactive({
    bwl <- getRawData() %>%
      dplyr::filter(QSSCAT == "Bowel Prep Date") %>%
      dplyr::filter(QSTESTCD == "BWLPRP1D" & QSSTRESC != "") %>%
      dplyr::select(USUBJID, QSSTRESC, VISITNUM) %>%
      dplyr::rename(BwlPrepDt = QSSTRESC) %>%
      dplyr::mutate(BwlPrepDt = as.Date(BwlPrepDt, "%d-%b-%y"))
    
    return(bwl)
  })
  
  getEndoData <- reactive({
    endo <- getRawData() %>%
      dplyr::filter(QSSCAT == "Bowel Prep Date" & QSTESTCD == "ENDDAT1D" & QSSTRESC != "") %>%
      dplyr::select(USUBJID, QSSTRESC, VISITNUM) %>%
      dplyr::rename(EndoDt = QSSTRESC) %>%
      dplyr::mutate(EndoDt = as.Date(EndoDt, "%d-%b-%y"))
    
    return(endo)
  })
  
  getRawDiaryData <- reactive({
    diary_raw <- getRawData() %>%
      dplyr::filter(QSSCAT == "Daily Diary") %>%
      dplyr::arrange(USUBJID, Date)
    return(diary_raw)
  })
  
  getDiaryData <- reactive({
    withProgress(message="Calculating...", value = 0, {
      n = 4  
      raw <- getRawData()
      incProgress(1/n, detail = paste("Derive visit labels"))
      # Assignment/start date
      # start <- raw %>%
      #   dplyr::filter(QSTEST == "Patient LogPad Start Date and Time") %>%
      #   dplyr::select(USUBJID, QSTEST, QSSCAT, QSSTRESC, Date) %>%
      #   # Use assignment date if pad activation missing
      #   dplyr::mutate(PadActivation = ifelse(QSSTRESC=="",as.character(Date),NA))%>%
      #   dplyr::mutate(PadActivation = as.Date(PadActivation)) %>%
      #   dplyr::mutate(QSSTRESC = substring(QSSTRESC,1,nchar(as.character(QSSTRESC))-5)) %>%
      #   dplyr::mutate(QSSTRESC = as.Date(QSSTRESC,"%m/%d/%y")) %>%
      #   dplyr::mutate(PadActivation = ifelse(is.na(PadActivation), as.character(QSSTRESC), as.character(PadActivation))) %>%
      #   dplyr::rename(Assignment = Date) %>%
      #   dplyr::mutate(Assignment = as.Date(Assignment), PadActivation = as.Date(PadActivation)) %>%
      #   dplyr::select(USUBJID, Assignment, PadActivation)
      
      
      # Visit labels
      visit_info <- raw %>%
        dplyr::filter(QSTESTCD == "VST1L" & grepl("Daily Diary Scores|Bowel Prep Date",QSSCAT)) %>%
        dplyr::select(USUBJID, VISITNUM, Date, Time, QSSCAT) %>%
        dplyr::arrange(USUBJID, VISITNUM, QSSCAT, Date, Time) %>%
        dplyr::group_by(USUBJID, VISITNUM) %>%
        # Preferentially take diary score date, and only use bowel prep date for visit if diary date missing
        dplyr::filter(row_number() == n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(-QSSCAT)
      
      # Table of all possible visit labels and numbers
      VISITNUM=c(20,30,40,50,60,70,90,100,120,130,140,150,160,170,180,190,200,210,220,230,240)
      VISIT = c("WEEK 0", "WEEK 1", "WEEK 2", "WEEK 4", "WEEK 6", "WEEK 8", "WEEK 14","WEEK 22", "4 WEEK SAFETY F/U", "8 WEEK SAFETY F/U","UNSCHEDULED VISIT 1",
                "UNSCHEDULED VISIT 2", "UNSCHEDULED VISIT 3", "UNSCHEDULED VISIT 4", "UNSCHEDULED VISIT 5", "DISEASE EVALUATION 1", "DISEASE EVALUATION 2", "DISEASE EVALUATION 3", "DISEASE EVALUATION 4", "DISEASE EVALUATION 5","EARLY TERMINATION")
      
      incProgress(1/n, detail = paste("Filter out bowel prep"))
      # Raw diary entries, filter out all bowel prep date range from these entries
      diary_raw <- getRawDiaryData() %>%
        # Exclude entries around bowel prep date
        dplyr::group_by(USUBJID, Date, QSTESTCD) %>%
        # Only keep the last record if duplicate dairies are present on same day
        dplyr::filter(row_number() == n()) %>%
        dplyr::mutate(DiaryExclFlg = bwlEndoDay(USUBJID, Date)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(DiaryExclFlg == 0) %>%
        dplyr::select(USUBJID, QSTESTCD, QSTEST, QSSTRESC, QSSTRESN, Date, Time)
      
      incProgress(1/n, detail = paste("Fetch vendor-computed scores"))
      # Derived scores
      diary_scr <- raw %>%
        dplyr::filter(QSSCAT == "Daily Diary Scores") %>%
        dplyr::filter(grepl("AVGSFS1N|AVGRBS1N|DRYCMP1N|SFSSCR1N|RBSSCR1N",QSTESTCD)) %>%
        dplyr::select(USUBJID, QSTESTCD, QSSTRESN, VISITNUM) %>%
        dplyr::filter(!is.na(VISITNUM)) %>%   ###########
      tidyr::spread(key = QSTESTCD, value = QSSTRESN)
      
      # Subdata by test
      stool_d <- diary_raw %>%
        dplyr::filter(QSTESTCD == "NUMSTL1L")
      
      rb_d <- diary_raw %>%
        dplyr::filter(QSTESTCD == "RECBLD1L")
      
      incProgress(1/n, detail = paste("Compute summary scores"))
      # Compute summary table
      diary <- visit_info %>%
        dplyr::left_join(cbind.data.frame(VISITNUM,VISIT), by = c("VISITNUM")) %>%
        dplyr::full_join(getBwlData(), by = c("USUBJID","VISITNUM")) %>%
        dplyr::full_join(getEndoData(), by = c("USUBJID", "VISITNUM")) %>%
        #dplyr::left_join(start, by = "USUBJID") %>%
        dplyr::group_by(USUBJID, VISITNUM) %>%
        dplyr::filter(!is.na(VISITNUM)) %>% ####
        # dplyr::mutate(StlSum = sumDiaryScore(df = stool_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = Assignment, actdate = PadActivation)[1]) %>%
        # dplyr::mutate(StlDiaryTot = sumDiaryScore(df = stool_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = Assignment, actdate = PadActivation)[2]) %>%
        # dplyr::mutate(RbSum = sumDiaryScore(df = rb_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = Assignment, actdate = PadActivation)[1]) %>%
        # dplyr::mutate(RbDiaryTot = sumDiaryScore(df = rb_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = Assignment, actdate = PadActivation)[2]) %>%
        #
        dplyr::mutate(StlSum = sumDiaryScore(df = stool_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = "", actdate = "")[1]) %>%
        dplyr::mutate(StlDiaryTot = sumDiaryScore(df = stool_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = "", actdate = "")[2]) %>%
        dplyr::mutate(RbSum = sumDiaryScore(df = rb_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = "", actdate = "")[1]) %>%
        dplyr::mutate(RbDiaryTot = sumDiaryScore(df = rb_d, subj = USUBJID, visit = VISITNUM, currdate = Date, assigndate = "", actdate = "")[2]) %>%
        dplyr::ungroup() %>%
        # Adjust for unqualified entries
        dplyr::mutate(StlSum = ifelse(VISITNUM == 20 & StlDiaryTot < 3, NA,
                                      ifelse(StlDiaryTot < 2, NA, StlSum))) %>%
        dplyr::mutate(RbSum = ifelse(VISITNUM == 20 & RbDiaryTot < 3, NA,
                                     ifelse(RbDiaryTot < 2, NA, RbSum))) %>%
        dplyr::mutate(ScrnFail = ifelse(((VISITNUM == 20 & StlDiaryTot < 3) | (VISITNUM == 20 & RbDiaryTot < 3)), 1, 0)) %>%
        # Compute Average Scores
        dplyr::mutate(StlAvg = round2(ifelse(StlDiaryTot == 2, StlSum/2, StlSum/3),0)) %>%
        dplyr::mutate(RbAvg = round2(ifelse(RbDiaryTot == 2, RbSum/2, RbSum/3),0)) %>%
        # Merge with Device data
        dplyr::full_join(diary_scr, by = c("USUBJID", "VISITNUM")) %>%
        dplyr::select(USUBJID, Date, Time, VISITNUM, VISIT, ScrnFail, BwlPrepDt, EndoDt, StlDiaryTot, RbDiaryTot, DRYCMP1N, StlSum, SFSSCR1N, StlAvg, AVGSFS1N, RbSum, RBSSCR1N, RbAvg, AVGRBS1N) %>%
        dplyr::arrange(USUBJID, Date)
      
      show("dnBtn")
      return(diary)
    })
  })
  
  getStlData <- reactive({
    diary <- getDiaryData()
    
    stool <- diary %>%
      dplyr::filter(AVGSFS1N != StlAvg) %>%
      dplyr::select(USUBJID, Date, Time, VISITNUM, VISIT, BwlPrepDt, EndoDt, StlDiaryTot, DRYCMP1N, StlSum, SFSSCR1N, StlAvg, AVGSFS1N)
    
    # Dummy data to force success for UI coding check purposes
    dummydt <- stool %>%
      dplyr::mutate(a = 0) %>%
      dplyr::filter(a == 1)
    
    return(stool)
  })
  
  getRbData <- reactive({
    diary <- getDiaryData()
    
    rb <- diary %>%
      dplyr::filter(AVGRBS1N != RbAvg) %>%
      dplyr::select(USUBJID, Date, Time, VISITNUM, VISIT, BwlPrepDt, EndoDt, RbDiaryTot, DRYCMP1N, RbSum, RBSSCR1N, RbAvg, AVGRBS1N)
    
    return(rb)
  })
  
  getScrnFailRndData <- reactive({
    rand <- getRawData() %>%
      dplyr::filter(QSTESTCD == "RANDOM1N" & !is.na(QSSTRESN)) %>%
      dplyr::arrange(USUBJID, DtTm) %>%
      dplyr::group_by(USUBJID) %>%
      dplyr::filter(row_number() == 1)
    
    scrnFail <- getDiaryData() %>%
      dplyr::filter(ScrnFail == 1) %>%
      dplyr::select(USUBJID, Date, Time, VISIT, BwlPrepDt, EndoDt, StlDiaryTot, RbDiaryTot, DRYCMP1N) %>%
      dplyr::left_join(rand %>%
                         dplyr::select(USUBJID, QSSTRESN),
                       by = "USUBJID") %>%
      dplyr::rename(RandID=QSSTRESN) %>%
      dplyr::filter(!is.na(RandID))
    
    return(scrnFail)
  })
  
  getBwlEndoData <- reactive({
    data <- getDiaryData() %>%
      dplyr::select(USUBJID, Date, Time, VISITNUM, VISIT, BwlPrepDt, EndoDt) %>%
      dplyr::filter(!is.na(BwlPrepDt) | !is.na(EndoDt)) %>%
      # Find records where Bowel prep is after endoscopy or is more than 1 day before it,
      # and where there is either a bowel prep or endoscopy but not the other
      dplyr::mutate(BwlEndoFlg = ifelse(is.na(BwlPrepDt) | is.na(EndoDt),1,
                                        ifelse(BwlPrepDt > EndoDt, 1,
                                               ifelse(EndoDt - BwlPrepDt > 1,1,0
                                               )))) %>%
      dplyr::filter(BwlEndoFlg == 1) %>%
      dplyr::select(-BwlEndoFlg)
    return(data)
  })
  
  getDupData <- reactive({
    dups <- getRawDiaryData() %>%
      # Find extra entries
      dplyr::group_by(USUBJID, Date, QSTESTCD) %>%
      dplyr::filter(row_number() > 1) %>%
      # Obtain visit info for dups
      dplyr::select(USUBJID, Date, QSTESTCD) %>%
      dplyr::filter(row_number() == 1) %>%
      # Fetch all entries on that visit
      dplyr::left_join(getRawDiaryData(), by = c("USUBJID", "Date", "QSTESTCD")) %>%
      dplyr::select(USUBJID, Date, Time, QSTESTCD, QSTEST, QSSTRESC, QSSTRESN)
    
    return(dups)
  })
  
}

shinyApp(ui, server)