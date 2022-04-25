#Converting Continuous variables in to Categories or Groups

##############################################################################
# Loading Required Liabitlies
##############################################################################
library(shiny)
library(shinydashboard)    # for Dashboard
library(shinyWidgets)      # for radio button widgets
library(shinyjs)           # to perform common useful JavaScript operations in Shiny apps
library(shinyBS)           # for bsTooltip function
library(shinyalert)        # for alert message very nice format
library(dplyr)             # select functions are covered in the library
library(plyr)              # empty() function is from this package
library(DT)                # for using %>% which works as a pipe in R code
library(ggplot2)



##############################################################################
# Unloading or detaching shinydashboardPlus package
##############################################################################
#This ShinydashboardPlus creates some problem in the box format, 
#so if is already loaded in your Rstudio,this code will unload 
if("package:shinydashboardPlus" %in% search()==TRUE){
  detach("package:shinydashboardPlus", unload = TRUE) 
}


##############################################################################
#Actionbutton style function default 30px and width 100px
##############################################################################
styleButtonBlue<- function(xheight="50px",xwidth="180px",xcolor='#4682B4'){
  paste(
    "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:",xcolor,";",
    "border-color: #ffffff;
                        border-width:2px;
                        height:",xheight,";
                        width:",xwidth,";
                        font-size: 13px;")
}


##############################################################################
#function for "NOT IN" for example column name not in the given list
##############################################################################
'%ni%' <- Negate('%in%')




##############################################################################
#functions to get right string and left string - these are facilitating functions
##############################################################################
fnRightString = function(x,n){
  substring(text = x,first = nchar(x)-n+1)
}
fnLeftString = function(x,n){
  substring(text = x,first = 1,last = n)
}





##############################################################################
#UI - Header
##############################################################################
header<- dashboardHeader(
  title = "Converting Numeric Data to Categories",
  titleWidth = '1200px',
  tags$li(class = "dropdown")
)

##############################################################################
# UI - Sidebar - see the code here which will hide the side bar
# 100% hiding of sidebar not possible if shinydashboardPlus package is loaded
##############################################################################
sidebar <- dashboardSidebar(
  id = 'msidebarid',
  useShinyjs(),
  collapsed = TRUE,
  
  # Remove the sidebar toggle element
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  minified = TRUE  #this option will completely close the side bar and expand the header text
)


##############################################################################
# UI - DashboardBody
##############################################################################
body <- dashboardBody(
  useShinyalert(),
  shinyjs::useShinyjs(),
  #where you got tab width adjustment https://community.rstudio.com/t/shiny-css-how-to-adjust-width-of-tabsetpanel-titles/44658/2
  tags$style(HTML("
    .tabbable > .nav > li > a {background-color: light-blue;  color:black; width: 120vx;font-size: 11px;}
  ")),
  tags$head(
    tags$style(
      HTML(
        ".form-group {
            margin-bottom: 0 !important;
          }
        "
      )
    )
  ),
  
  column(style = "border: 4px double red;height: 500px;overflow-y: auto;",
         id ='mcolumnID1',
         width = 3,
         align="center",
         br(),
         actionButton(inputId = 'mOverviewBtn',label = "Overview & Citations!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
         actionButton(inputId = 'mImportDataBtn',label = "Data Upload / Cleansing!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
         actionButton(inputId = 'mshowtableBtn',label = "Review Dataset!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
         actionButton(inputId = 'mGroupDataBtn',label = "Data Grouping!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
         downloadButton("downloadCSVBtn", "Download Dataset CSV", style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
         downloadButton("downloadRDSBtn", "Download Dataset RDS", style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
         div(style = "margin-top:-15px"),
         tags$hr(style = "border: 2px double red;"),
         div(style = "margin-top:-15px"),
         selectInput(inputId = "mDataGrpID",label = "Select variable to Group",choices = NULL,selected = NULL),
         div(style = "margin-top:-15px"),
         actionButton(inputId = "mGrpReviewBtn",label = "Review",style = styleButtonBlue(xheight = '50px',xwidth = '100px')),
         actionButton(inputId = "mGrpCommitBtn",label = "Commit Grouping",style = styleButtonBlue(xheight = '50px',xwidth = '100px')),
         actionButton(inputId = "mGrpUndo",label = "Undo Changes",style = styleButtonBlue(xheight = '50px',xwidth = '100px')),
         actionButton(inputId = "mConvertFactor",label = "Convert as Factor",style = styleButtonBlue(xheight = '50px',xwidth = '100px')),
         actionButton(inputId = "mShowStrBtn",label = "Data Structure",style = styleButtonBlue(xheight = '50px',xwidth = '100px'))
         
         
  ),#column closure
  column(style = "border: 4px double red;height: 500px;overflow-y: auto;",
         width = 9,
         align ='center',
         tags$div(id = 'placeholder_MultiPurpose')
  )
) # dashboardBody closure

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)



server <- function(input, output, session) {
  
  ##############################################################################
  # These are initial codes to be part of Server
  ##############################################################################
  #to the top of server.R would increase the limit to 30MB.
  options(shiny.maxRequestSize=30*1024^2) 

  # prevents printing scientific notation
  options(scipen=999)
  
  # This insert is relating to placehoder function
  inserted <- c()
  
  # all these Actionbuttons are disabled initally and enabled stage by stage
  disable("mshowtableBtn")
  disable("mGroupDataBtn")
  disable("downloadCSVBtn")
  disable("downloadRDSBtn")
  disable("mDataGrpID")
  disable("mGrpReviewBtn")
  disable("mGrpCommitBtn")
  disable("mGrpUndo")
  disable("mConvertFactor")
  disable("mShowStrBtn")
  
  
##########################################################################
# Since we use place holder, we need a function to remove the existing box
# otherwise you one below the other when you click 
##########################################################################
  removeRightBox <- function(x){
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
      
    )
    inserted <<- inserted[-length(inserted)]
  }
  
  
  ##########################################################################
  # Overview - Conversion of Numeric Data to Category
  ##########################################################################
  observeEvent(input$mOverviewBtn,{
    enable("mOverviewBtn")
    removeRightBox()
    btn <- input$mOverviewBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        column(width = 12,
               offset = 0,
               br(),
               box(
                 width = 12,
                 height = '425px', 
                 title ="Overview & Citations", 
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 autoWidth = FALSE,
                 tags$head(
                   tags$style(
                     paste0("#mybox455{color:black; font-size:10px; font-style:bold;overflow:auto;text-align: justify;margin: 5px 5px 5px 5px;
                                            width: '100%';height: 365px;max-height: 365px; background: #ffffff;}")
                   )
                 ),
                 uiOutput("mybox455")
                ) #box closure
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  urlgrp  <- a("Converting Numeric Data to Categories by Salvatore S. Mangiafico", href="https://rcompanion.org/handbook/E_05.html")

  output$mybox455 <- renderUI({
    tags$div(
      tags$p(
        useShinyjs(),
        HTML(paste('<h5><b>',"Overview:",'</b><h5>',
                   "There are occations when it is useful to convert continuous data into groups or 
                   categories.  For example, while analysing patient data, we get more insight,
                   if we convert age into range, like less 20, 20-40,40-60 and  so on, instead of keeping it as numbers.  
                   Similarly, for certain analysis we may have to convert categorical variable to factors.")),
        HTML(paste('<h5>',
                   "In this Shiny App, we cover categorizing by range of values and factoriation. There are few 
                   other methods, say, Categorizing data by percentiles, Categorizing data with clustering, 
                   to learn more about this, you could refer the link provided here:",urlgrp))   
      )
    )
  })
  
  
  
  
  ##########################################################################
  # Import dataset & Data Cleansing
  ##########################################################################
  vmy <- reactiveValues(mydata=NULL,mbreaks=NULL,mlabels=NULL)
  
  observeEvent(input$mImportDataBtn,{
    enable("mgetfileclick")
    removeRightBox()
    btn <- input$mImportDataBtn
    id <- paste0('txt', btn)
    
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        column(width = 10,
               offset = 1,
               br(),
        box(
          id = "mprocBox1",
          width = 6,
          height = '425px',
          align = "center",
          title = "Uplaod Dataset",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          
          # Input: Select a file ----
          fileInput("file",
                    label = "Select: csv,txt, xls, xlsx, rds ",
                    multiple = FALSE,
                    accept = c("text/csv/txt/Excel",
                               "text/comma-separated-values,text/plain/excel",
                               ".csv",".txt",".xls",".xlsx",".rds")),
          
          # Horizontal line ----
          #tags$hr(),
          column(
            width = 5,
            offset = 1,
            align = "left",
            fluidRow(
              # Input: Checkbox if file has header ----
              checkboxInput("header", "Header", TRUE),
              
              # Input: Select separator ----
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ","),
            )
          ),
          column(
            width = 5,
            offset = 0,
            align = "left",
            fluidRow(
              br(),
              br(),
              # Input: Select quotes ----
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Qot." = '"',
                                       "Single Qot." = "'"),
                           selected = '"')
            )
          ),
          
          tags$hr(),
          column(
            width = 12,
            align="center",
            useShinyjs(),
            br(),br(),br(),br(),
            actionButton(inputId = "mgetfileclick",label = "Get Data!",style = styleButtonBlue(xheight = '35px',xwidth = '200px'))
          )
          
        ),#box for file import close
        box(
          id = "mprocBox2",
          width = 6,
          height = '425px',
          align = "center",
          title = "Data Cleansing",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          tags$head(
            tags$style(
              paste0("#dt{color:black; font-size:11px; font-style:bold;overflow-y:scroll;font-family: 'Lucida Sans Unicode';
                                            width: '100%';height: 300px;max-height: 350px; background: #ffffff;text-align: left;}")
            )
          ),
          DT::dataTableOutput("dt",height = '300px',width = '100%'),
          tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
          useShinyjs(),
          extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
          textOutput("mselectedvariable"),
          actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable!",style = styleButtonBlue(xheight = '35px',xwidth = '200px'))
        )
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  observeEvent(input$mgetfileclick,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse, select and import ...!", type = "error")
      return()
    }
    enable("mshowtableBtn")
    enable("downloadCSVBtn")
    enable("downloadRDSBtn")
    
    #### file import code start
    ext <- tools::file_ext(input$file$name)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    if (ext != "csv" & ext !='rds' & ext != 'xlsx' & ext != 'xlsx'){
      shinyalert("Oops!", "valid files are csv, txt, excel and rds only", type = "error")
      return()
    }
    else if (ext == "rds"){
      vmy$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv" || ext == 'txt'){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop where you got:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        
        vmy$mydata <- as.data.frame(read.csv(input$file$datapath,
                                             header = input$header,
                                             sep = input$sep,
                                             quote = input$quote)
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    #### file import code End
    
    #### delete NA rows code start
    vmy$mydata <- na.omit(vmy$mydata)
    vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
    
    #### create datatype df to use in cleansing phase
    fncreatedftype()
    disable("mgetfileclick")
    fnMakeGrpParamtbl()
  })
  
  
  
  
  
  ##########################################################################
  # Codes to create table with data type to delete columns
  ##########################################################################
  fncreatedftype <- function(){
    vmy$df_types <- data.frame("col_types" = unlist(lapply(vmy$mydata, class)))
    vmy$df_types$Var_name <- rownames(vmy$df_types)
    row.names(vmy$df_types) <- NULL
    vmy$df_types <-vmy$df_types %>% dplyr::select(-col_types, everything())
    vmy$df_types <- subset(vmy$df_types,fnLeftString(vmy$df_types$Var_name,2)!="X_")
  }
  
  
  output$dtvartbl <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "multiple", target = 'row'),
                  #fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#E5E5E5', 'color': '#fff'});",   #where you got: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                                 
                  )
                  
    )
    
  })
  
  output$mselectedvariable <-  renderText({
    if(length(input$dtvartbl_cell_clicked) != 0){
      clicked_list <- input$dtvartbl_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],"Type:",vmy$df_types[clicked_list$row,2]))
      
    }
    else{
      HTML("select variable")
    }
  })
  
  
  ### delete selected column
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    temp <- dplyr::select(vmy$mydata,-vmy$df_types[input$dt_cell_clicked$row,1])
    vmy$mydata <- temp
    
    mNewVariable <- paste0('X_',input$dt_cell_clicked$row,1)
    if (mNewVariable %in% names(vmy$mydata)==TRUE){
      temp <- dplyr::select(vmy$mydata,-c("mNewVariable"))
    }

    removeModal()
    temp2 <- subset(vmy$df_types, Var_name!=vmy$df_types[input$dt_cell_clicked$row,1] )
    vmy$df_types <- temp2
    fnMakeGrpParamtbl()
  })
  
  
  output$dt <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "single", target = 'row'),
                  #fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",   #where you got: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                  ) 
    )
  })
  
  output$mselectedvariable <-  renderText({
    if(length(input$dt_cell_clicked) != 0){
      clicked_list <- input$dt_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],"Type:",vmy$df_types[clicked_list$row,2]))
      
    }
    else{
      HTML("select variable")
    }
  })
  
  
  
  
  ##########################################################################
  # Display Dataset
  ##########################################################################
  observeEvent(input$mshowtableBtn,{
    enable("mGroupDataBtn")
    removeRightBox()
    btn <- input$mshowtableBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        column(width = 12,
               offset = 0,
               box(
                 width = 12,
                 height = '450px', 
                 title ="Dataset", 
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 autoWidth = FALSE,
                 tags$head(
                   tags$style(
                     paste0("#mdatatable{color:black; font-size:11px; font-style:bold;overflow-y:scroll;font-family: 'Lucida Sans Unicode';
                                            width: '100%';height: 375px;max-height: 375px; background: #ffffff;text-align: left;}")
                   )
                 ),
                 DT::dataTableOutput('mdatatable', height = '375px',width = '100%'),
                 tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}'))
               ) #box closure
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  output$mdatatable <- DT::renderDataTable({
    DT::datatable(vmy$mydata %>%dplyr::select(-starts_with("X_")),
                  rownames = FALSE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  #fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(
                    lengthMenu = list(c(10, 25, 50,-1), c('10', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = FALSE,
                    ordering = FALSE,
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      paste0("$(this.api().table().container()).css({'font-size': '", "12px", "'});"),
                      "}")
                  ),
                  
                  class ='cell-border stripe compact white-space: nowrap', #where you got this multiple classes: https://rstudio.github.io/DT/
    )
  })
  
  
  # proxy = dataTableProxy('mdatatable')
  # observeEvent(input$mdatatable_cell_edit, {
  #   if ((dim(vmy$mydata)[1] == 0)){
  #     tt=""
  #   }
  #   else{
  #     info = input$mdatatable_cell_edit
  #     i = info$row
  #     j = info$col+1
  #     v = info$value
  #     vmy$mydata[i, j] <- DT::coerceValue(v, vmy$mydata[i, j])
  #     replaceData(proxy, vmy$mydata, resetPaging = FALSE)  # important
  #   }
  #   
  # })
  
  
  
  ##########################################################################
  # Code to download dataset as csv and rds
  ##########################################################################
  output$downloadCSVBtn<- downloadHandler(
    filename = function() {
      paste("mydatadf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vmy$mydata[input[["mdatatable_rows_all"]], ]), file, row.names = FALSE)
    }
  )
  
  ### can download the table in RDS
  output$downloadRDSBtn <- downloadHandler(
    filename = function() {
      paste("mydatadf", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(data.frame(vmy$mydata[input[["mdatatable_rows_all"]], ]), file)
    }
  )

  
  
  ################################################################################
  ## Data Grouping code set
  ################################################################################

  observeEvent(input$mGroupDataBtn,{
    enable("mDataGrpID")
    enable("mGrpReviewBtn")
    enable("mGrpCommitBtn")
    enable("mGrpUndo")
    enable("mConvertFactor")
    enable("mShowStrBtn")
    disable('c22')
    disable('c33')
    disable('c44')
    zz <- names(vmy$mydata %>%dplyr::select(-starts_with("X_")))
    updateSelectInput(session,inputId = 'mDataGrpID',choices = zz,selected = NULL)
    removeRightBox()
    btn <- input$mGroupDataBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        # HTML(paste('<h4><b>',"Grouping suggested by the system; you have option to edit",'</b><h5>')),
        column(width = 6,
               tabsetPanel( 
                 id = 'tabs',
                 tabPanel(title = 'By Range',
                          HTML(paste('<h5>',"System suggested grouping; with an option to edit",'<h5>')),
                          DT::dataTableOutput(outputId = 'mGRPdatatable',height = '250px',width = '325px'),
                          tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}'))
                          ),
                 tabPanel(
                   title = 'Customized',
                   column(
                     width = 12,
                     align='center',
                     HTML(paste('<p text-align ="center"><h5><i>',
                                paste('<span style="color:#ff0000;"> ',"You have option to create max 10 groups; for 10 groups you have to enter 11 values; if less groups, leave the rest as zero",'</span>'),'</i></p><h5>')),
                     
                     # to align textinput and label in one line where you got: https://stackoverflow.com/questions/20850483/how-to-put-a-box-and-its-label-in-the-same-row-shiny-package
                     tags$head(
                       tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle;font-size:11px;height: 5px;width: 60px; } 
                #inline .form-group { display: table-row;}")
                     ),
                     tags$head(tags$style(HTML("#c1 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#Grp1comment {height: 30px;font-size:13px;font-style:bold;width: 120px;}"))),
                     tags$head(tags$style(HTML("#c2 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c22 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c3 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c33 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c4 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c44 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c5 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c55 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c6 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c66 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c7 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c77 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c8 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c88 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c9 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c99 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c10 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c1010 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     tags$head(tags$style(HTML("#c11 {height: 30px;font-size:11px;font-style:bold;width: 80px;}"))),
                     tags$head(tags$style(HTML("#c1111 {height: 30px;font-size:11px;font-style:bold;width:80px;}"))),
                     column(
                       width = 6,
                       tags$div(id = "inline",numericInput(inputId = "c1",label = "1st value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c2",label = "2nd value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c3",label = "3rd value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c4",label = "4th value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c5",label = "5th value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c6",label = "6th value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c7",label = "7th value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c8",label = "8th value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c9",label = "9th value :",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c10",label = "10th value:",value = 0)),
                       tags$div(id = "inline",numericInput(inputId = "c11",label = "11th value:",value = 0))
                     ),
                     column(
                       width = 6,
                       textOutput(outputId = 'Grp1comment'),
                       tags$div(id = "inline",textInput(inputId = "c22",label = "1st Group :")),
                       tags$div(id = "inline",textInput(inputId = "c33",label = "2nd Group :")),
                       tags$div(id = "inline",textInput(inputId = "c44",label = "3rd Group :")),
                       tags$div(id = "inline",textInput(inputId = "c55",label = "4th Group :")),
                       tags$div(id = "inline",textInput(inputId = "c66",label = "5th Group :")),
                       tags$div(id = "inline",textInput(inputId = "c77",label = "6th Group :")),
                       tags$div(id = "inline",textInput(inputId = "c88",label = "7th Group :")),
                       tags$div(id = "inline",textInput(inputId = "c99",label = "8th Group :")),
                       tags$div(id = "inline",textInput(inputId = "c1010",label = "9th Group:")),
                       tags$div(id = "inline",textInput(inputId = "c1111",label = "10th Group:"))
                     ),
                     column(
                       width = 12,
                       actionButton(inputId = "mCustomGrpBtn",label = "Commit Grouping..!",style = styleButtonBlue(xheight = 50,xwidth = 100)),
                       actionButton(inputId = "mEditGrpLabels",label = "Edit Group Label!",style = styleButtonBlue(xheight = 50,xwidth = 100)),
                       HTML(paste('<p text-align ="center"><h5><i>',
                                  paste('<span style="color:#ff0000;"> ',textOutput(outputId = 'Grp2comment'),'</span>'),'</i></p><h5>'))
                     )

                   ) # column closure
                 
                 ),
                 tabPanel(
                   title = 'Summary Statistics',
                   tags$head(tags$style("#mMydataVarSummary{color:black; font-size:12px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-y:scroll; height: 150px; background: white;}")),
                   column(width = 12,
                          verbatimTextOutput(outputId = 'mMydataVarSummary')
                          )
                   )
                 
                 )
        ),
        column(width = 6,
               tags$head(tags$style("#mSurviceFreq{color:black; font-size:13px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-x: auto;overflow-y: auto; height: 350px; background: #ffffd4;}")),
               tags$head(tags$style("#mSurviceGroupedFreq{color:black; font-size:13px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-x: auto;overflow-y: auto; height: 350px; background: #ffffd4';}")),
               tags$head(tags$style("#mGrpErrorComment{color:black; font-size:13px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-x: auto;overflow-y: auto; height: 75px; background: white;}")),
               
               splitLayout(cellWidths = c("50%","50%"),
                           HTML(paste("<h5><b>","Before Grouping ","</b><h5>")),
                           HTML(paste("<h5><b>",actionLink(inputId = 'mBarPlotBtn',label = "After Grouping-Click me!"),"</b><h5>"))
               ),
               splitLayout(cellWidths = c("50%","50%"),
                           verbatimTextOutput('mSurviceFreq'),
                           verbatimTextOutput('mSurviceGroupedFreq')
               ),
               uiOutput(outputId = 'mGrpErrorComment')
        )
      )
    )
    fnMakeGrpParamtbl()
    inserted <<- c(id, inserted)
  })
  
  

  observeEvent(input$mBarPlotBtn,{
    showModal(
      modalDialog(size = 'm',
                  title = "Frequency Plot",
                  plotOutput('mFreqBarPlot', height = '350px',width = '100%'),
                  easyClose = TRUE)
      
    )
  }) 
  
  
  output$mFreqBarPlot <- renderPlot({
    df<- data.frame(table(vmy$mydata[,input$mDataGrpID]))
    ggplot(data=df, aes(x=Var1, y=Freq)) +
      geom_bar(stat="identity", fill="#87CEFA")+
      xlab(input$mDataGrpID) +
      ylab("Frequency") +
      geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = ifelse(nrow(df)>10,90,360), hjust = 1))
  })
  
  
  output$mMydataVarSummary <- renderPrint({
    summary(vmy$mydata[input$mDataGrpID])
  })
  
  
  
  
  

  ##########################################################################
  # This function creates table with suggested grouping range 
  ##########################################################################
  fnMakeGrpParamtbl <- function(){
    vmy$cutparamtbl <- data.frame(var_name = character(),
                              min_val = numeric(),
                              max_val = numeric(),
                              inter_val = numeric())
    yy<- names(dplyr::select_if(vmy$mydata %>%dplyr::select(-starts_with("X_")),is.numeric))
    
    # yy<- names(dplyr::select_if(vmy$mydata,is.numeric))
    for (u in yy){
      tt <- assign(paste0('vmy$mydata$',u),eval(parse(text=  paste0('vmy$mydata$',u))))
      r<-range(tt,na.rm = TRUE)
      l <- length(table(tt))
      r<-(r[2]-r[1])/10
      c <- ifelse (r<=1,0.25,ifelse(l <=20,3,ifelse(l <=50,5,ifelse(l <=100,10,ifelse(l <=200,20,ifelse(l <=300,30,ifelse(l <=500,50,100)))))))

      library(plyr) #round_any is including in this function
      mMin <- round_any(min(tt), -c, f = ceiling)
      mMax <- round_any(max(tt),  c, f = ceiling)+c

      #https://rpubs.com/dvdunne/agegroups
      labs <- c(paste(seq(mMin, mMax-c, by = c), seq(mMin+c, mMax, by = c),
                      sep = "-"))
      vmy$cutparamtbl <- vmy$cutparamtbl%>%add_row(var_name=u,
                                           min_val = mMin,
                                           max_val = mMax,
                                           inter_val = c)
    }
    
    for (i in 1:nrow(vmy$cutparamtbl)){
      if (substring(vmy$cutparamtbl[i,1],1,2)=="X_"){
        vmy$cutparamtbl[i,1] <- substring(vmy$cutparamtbl[i,1],3,100)      
      }
    }
  }
  
  
  
  output$mGRPdatatable <- DT::renderDataTable({
    DT::datatable( vmy$cutparamtbl,
                   class ='cell-border stripe compact white-space: nowrap', #where you got this multiple classes: https://rstudio.github.io/DT/
                   rownames = FALSE,
                   editable = list(target = 'cell', disable = list(columns = c(0))),
                   selection = list(mode = "single", selected = c(1), target = 'row'),
                   options = list(dom = 't',ordering=F, pageLength = -1,class="compact",
                                  initComplete = 
                                    htmlwidgets::JS(
                                      "function(settings, json) {",
                                      paste0("$(this.api().table().container()).css({'font-size': '", "14px", "'});"),
                                      "}")
                   )
    )
  })
  
  
  proxy = dataTableProxy('mGRPdatatable')
  observeEvent(input$mGRPdatatable_rows_selected,{
    m<- input$mGRPdatatable_rows_selected
    if (length(m)){
      updateSelectInput(session,inputId = 'mDataGrpID',selected =vmy$cutparamtbl[m,1] )      
    }
  })
  
  observeEvent(input$mGRPdatatable_cell_edit, {
    if ((dim(vmy$cutparamtbl)[1] == 0)){
      tt=""
    }
    else{
      info = input$mGRPdatatable_cell_edit
      i = info$row
      j = info$col+1
      v = info$value
      vmy$cutparamtbl[i, j] <- DT::coerceValue(v, vmy$cutparamtbl[i, j])
      replaceData(proxy, vmy$cutparamtbl, resetPaging = FALSE)  # important
    }
    
  })
  

  ##########################################################################
  # observeExvent to Review data to see the frequency as is
  ##########################################################################
  observeEvent(input$mGrpReviewBtn,{
      s <-  input$mDataGrpID
      if (length(s)) {
        mNewVariable <- paste0('X_',s)
        
        output$mSurviceFreq <- renderPrint({
          if (mNewVariable %ni% names(vmy$mydata)==TRUE){
            as.data.frame(table(vmy$mydata[s]))
          }
          else{
            as.data.frame(table(vmy$mydata[mNewVariable]))
          }
        })
        output$mSurviceGroupedFreq <- renderPrint({
          if (mNewVariable %ni% names(vmy$mydata)==FALSE){
            as.data.frame(table(vmy$mydata[s]))
          }
          else{
            HTML("not yet \ngrouped")
          }
        })
      }
  })
  
  
  ##########################################################################
  # Code to Commit Grouping
  ##########################################################################
  observeEvent(input$mGrpCommitBtn,{
    s <-  input$mDataGrpID
    if (length(s)) {
      mrowid <- which(vmy$cutparamtbl$var_name == s)
      if (length(mrowid)==0){
        shinyalert(title = 'Data Type Error',text = "Please check; this must of character; only numeric columns can be grouped")
        return()
      }
      mNewVariable <- paste0('X_',s)
      if (mNewVariable %ni% names(vmy$mydata)==TRUE){
        vmy$mydata[mNewVariable] <- vmy$mydata[s]
      }
      else{
        vmy$mydata[s] <<- vmy$mydata[mNewVariable]
      }
      
      tt <- assign(paste0('vmy$mydata$',s),eval(parse(text=  paste0('vmy$mydata$',s))))
      mMin <- vmy$cutparamtbl[mrowid,2]
      mMax <- vmy$cutparamtbl[mrowid,3]
      c <- vmy$cutparamtbl[mrowid,4]
      #https://rpubs.com/dvdunne/agegroups
      mlabels <- c(paste(seq(mMin, mMax-c, by = c), seq(mMin+c, mMax, by = c),
                      sep = "-"))
      mhightBand <- c(seq(mMin+c, mMax, by = c), Inf)
      mhightBand <- mhightBand[length(mhightBand)-1]
      
      output$mGrpErrorComment <- renderText({
      if ((max(tt) > mhightBand)==TRUE){
          HTML(paste('<h6>',"The highest band of ",mhightBand," is less than the MAX value in this column;(",max(tt),"). You can increase the Max_val in the table and re-run the commit grouping.",
                "Otherwise, values more than highest band will be grouped under the highest band; it will NOT be ignored",'<h5>'))
      }
        else{
          HTML("")
          
        }
        })
      
      ####################################################################
      # These codes will update input boxes in Custom grouping in next tab
      mbreaks <- c(seq(mMin, mMax-c, by = c),Inf)
      n <- ifelse(length(mbreaks)>11,11,length(mbreaks))
      updateNumericInput(session,inputId = "c1",value = 0)
      for (i in 2:11){
        updateNumericInput(session,inputId = paste0("c",i),value = 0)
        updateTextInput(session,inputId = paste0("c",i,i),value = "")
      }
      updateNumericInput(session,inputId = "c1",value = mbreaks[1])
      for (i in 2:n){
        if (i== n){
          updateNumericInput(session,inputId = paste0("c",i),value = as.numeric(sub(".*-", "", mlabels[i-1])))
        }else{
          updateNumericInput(session,inputId = paste0("c",i),value = mbreaks[i])
        }
        updateTextInput(session,inputId = paste0("c",i,i),value = mlabels[i-1])
      }
      ####################################################################
      
      fnHandlingNewVar()
      vmy$mydata[[s]] <- cut(vmy$mydata[[s]], breaks = c(seq(mMin, mMax-c, by = c), Inf), labels = mlabels, right = FALSE) #better to keep as FALSE; EXPLANATION: when right is false, if value 20 is considered in Group 20-30; if TRUE, 20 is consideredin Group 10-20
      fnDisplayGrpSummary()
    }
    fncreatedftype()
  })
  
  observeEvent(input$mConvertFactor,{
    output$mGrpErrorComment <- renderText({
      HTML("")
    })
    s <-  input$mDataGrpID
    if (length(s)) {
      mNewVariable <- paste0('X_',s)
      if (mNewVariable %ni% names(vmy$mydata)==TRUE){
        vmy$mydata[mNewVariable] <- vmy$mydata[s]
      }
      fnHandlingNewVar()
      vmy$mydata[[s]] <- as.factor(vmy$mydata[[s]])
      fnDisplayGrpSummary()
    }
    fncreatedftype()
  })
  
  
  ##########################################################################
  # Codes to un-do the grouping which you did earlier
  ##########################################################################
  observeEvent(input$mGrpUndo,{
    output$mGrpErrorComment <- renderText({
        HTML("")
    })
    s <-  input$mDataGrpID
    if (length(s)) {
      mNewVariable <- paste0('X_',s)

      if (mNewVariable %in% names(vmy$mydata)==TRUE){
        vmy$mydata[s] <- vmy$mydata[mNewVariable] 
      }
      else{
        shinyalert(title = "Error..!",text = "You have not done any changes to this column please check...!")
      }

      fnHandlingNewVar()
      fnDisplayGrpSummary()

    }
    fnDisplayGrpSummary()
    fncreatedftype()
    })
 
  
  ##########################################################################
  # Codes to show the structure of the dataset for all columns variables
  ##########################################################################
  observeEvent(input$mShowStrBtn,{
    showModal(
        modalDialog(
          title = "Info",
          verbatimTextOutput(outputId = 'mShowStrTxt'),
          easyClose = TRUE)
      
    )
  }) 
  
  output$mShowStrTxt <- renderPrint({
    str(vmy$mydata)
  })

  
  
  ##########################################################################
  # Grouping related Actionbuttons are disabled when you click these buttons
  ##########################################################################
  observeEvent(c(input$mImportDataBtn,
                 input$mshowtableBtn,
                 input$ downloadCSVBtn,
                 input$downloadRDSBtn),{
                   disable("mDataGrpID")
                   disable("mGrpReviewBtn")
                   disable("mGrpCommitBtn")
                   disable("mGrpUndo")
                   disable("mConvertFactor")
                   disable("mShowStrBtn")
                   
                 })
  

  ##########################################################################
  # Customized Grouping code starts here
  ##########################################################################
  observeEvent(input$tabs, {
    if(input$tabs=="Customized"){
      shinyjs::disable('c22')
      shinyjs::disable('c33')
      shinyjs::disable('c44')
      shinyjs::disable('c55')
      shinyjs::disable('c66')
      shinyjs::disable('c77')
      shinyjs::disable('c88')
      shinyjs::disable('c99')
      shinyjs::disable('c1010')
      shinyjs::disable('c1111')
      mrowid <- which(vmy$cutparamtbl$var_name == input$mDataGrpID)
      if (length(mrowid)==0){
        shinyalert(title = 'Data Type Error',text = "Please check; this must of character; only numeric columns can be grouped")
        return()
      }
    }
  })
  

  observeEvent(
    c(input$c1,
      input$c2,
      input$c3,
      input$c4,
      input$c5,
      input$c6,
      input$c7,
      input$c8,
      input$c9,
      input$c10,
      input$c11),
    {validate(need((input$c1+input$c2+input$c3+input$c4+input$c5+input$c6+input$c7+input$c8+input$c9+input$c10+input$c11)>0,"there is an error"))
      
      if (input$c2 !=0){
        assign("c22", paste0("[",input$c1+1,"-",input$c2,"]"))
        updateTextInput(session,inputId = "c22",value = c22)
      }
      if (input$c3 !=0){
        assign("c33", paste0("[",input$c2+1,"-",input$c3,"]"))
        updateTextInput(session,inputId = "c33",value = c33)
      }
      if (input$c4 !=0){
        assign("c44", paste0("[",input$c3+1,"-",input$c4,"]"))
        updateTextInput(session,inputId = "c44",value = c44)
      }
      if (input$c5 !=0){
        assign("c55", paste0("[",input$c4+1,"-",input$c5,"]"))
        updateTextInput(session,inputId = "c55",value = c55)
      }
      if (input$c6 !=0){
        assign("c66", paste0("[",input$c5+1,"-",input$c6,"]"))
        updateTextInput(session,inputId = "c66",value = c66)
      }
      if (input$c7 !=0){
        assign("c77", paste0("[",input$c6+1,"-",input$c7,"]"))
        updateTextInput(session,inputId = "c77",value = c77)
      }
      if (input$c8 !=0){
        assign("c88", paste0("[",input$c7+1,"-",input$c8,"]"))
        updateTextInput(session,inputId = "c88",value = c88)
      }
      if (input$c9 !=0){
        assign("c99", paste0("[",input$c8+1,"-",input$c9,"]"))
        updateTextInput(session,inputId = "c99",value = c99)
      }
      if (input$c10 !=0){
        assign("c1010", paste0("[",input$c9+1,"-",input$c10,"]"))
        updateTextInput(session,inputId = "c1010",value = c1010)
      }
      if (input$c11 !=0){
        assign("c1111", paste0("[",input$c10+1,"-",input$c11,"]"))
        updateTextInput(session,inputId = "c1111",value = c1111)
      }
    })
  
  
  observeEvent(input$mCustomGrpBtn,{
    mrowid <- which(vmy$cutparamtbl$var_name == input$mDataGrpID)
    if (length(mrowid)==0){
      shinyalert(title = 'Data Type Error',text = "Please check; this must of character; only numeric columns can be grouped")
      return()
    }
    mbreaks <-c(input$c1)
    mlabels = c()
    for (i in 2:11){
      if (eval(parse(text =paste0("input$c",i))) != 0){
        mbreaks <- c(mbreaks,eval(parse(text =paste0("input$c",i))))
        mlabels <- c(mlabels,eval(parse(text =paste0("input$c",i,i))))
      }
    }
    vmy$mbreaks <- mbreaks
    vmy$mlabels <- mlabels
    
    fnHandlingNewVar()
    if (isTRUE(max(vmy$mbreaks) < max(vmy$mydata[[input$mDataGrpID]]))){
      alert(paste("max value of the variable is ",max(vmy$mydata[[input$mDataGrpID]]),"; you have not grouped upto that"))
      return()
    } 
    else{
      s<- input$mDataGrpID
      fnHandlingNewVar()
      vmy$mydata[[s]] <- cut(x = vmy$mydata[[s]],breaks = vmy$mbreaks,labels = vmy$mlabels)
      fnDisplayGrpSummary()
      # shinyjs::disable("mCustomGrpBtn")
      fnDisplayGrpSummary()
      # click(id = 'mGroupDataBtn')
      fncreatedftype()
    }
  })
  
  
  observeEvent(input$mEditGrpLabels,{
    if ((input$mEditGrpLabels%%2)==0){
      shinyjs::enable('c22')
      shinyjs::enable('c33')
      shinyjs::enable('c44')
      shinyjs::enable('c55')
      shinyjs::enable('c66')
      shinyjs::enable('c77')
      shinyjs::enable('c88')
      shinyjs::enable('c99')
      shinyjs::enable('c1010')
      shinyjs::enable('c1111')
    }else{
      shinyjs::disable('c22')
      shinyjs::disable('c33')
      shinyjs::disable('c44')
      shinyjs::disable('c55')
      shinyjs::disable('c66')
      shinyjs::disable('c77')
      shinyjs::disable('c88')
      shinyjs::disable('c99')
      shinyjs::disable('c1010')
      shinyjs::disable('c1111')
    }
    
  })
  
  output$Grp1comment <- renderText({
    HTML(paste("<- should be less than",min(vmy$mydata[[input$mDataGrpID]])))
  })
  
  output$Grp2comment <- renderText({
    paste(input$mDataGrpID, "in Dataset has MIN value of",min(vmy$mydata[[input$mDataGrpID]])," and MAX value of",max(vmy$mydata[[input$mDataGrpID]]))
  })
  
  
  observeEvent(input$c1,{
    validate(
      need(input$c1 != '', 'cannot make it blank...!')
    )
    if (exists("vmy$mydata")==TRUE){
      if (input$c1 >= min(vmy$mydata[[input$mDataGrpID]])){
        alert(paste("1st value should be less than",min(vmy$mydata[[input$mDataGrpID]]),"; otherwise observations having value less than that will be deleted from dataset"))
      }
    }
  })
  
  
  fnHandlingNewVar <- function(){
    s <-  input$mDataGrpID
    if (length(s)) {
      mNewVariable <- paste0('X_',s)
      if (mNewVariable %ni% names(vmy$mydata)==TRUE){
        vmy$mydata[mNewVariable] <- vmy$mydata[s]
      }
      else{
        vmy$mydata[s] <<- vmy$mydata[mNewVariable]
      }
      output$Grp1comment <- renderText({
        HTML(paste("<- should be less than",min(vmy$mydata[[mNewVariable]])))
      })
      output$Grp2comment <- renderText({
        paste(s, "in Dataset has MIN value of",min(vmy$mydata[[mNewVariable]])," and MAX value of",max(vmy$mydata[[mNewVariable]]))
      })
    }
    
  }
  
  
  fnDisplayGrpSummary <- function(){
    s <-  input$mDataGrpID
    if (length(s)) {
      mNewVariable <- paste0('X_',s)
      tryCatch({ 
        output$mSurviceFreq <- renderPrint({
          if (mNewVariable %ni% names(vmy$mydata)==TRUE){
            as.data.frame(table(vmy$mydata[s]))
          }
          else{
            as.data.frame(table(vmy$mydata[mNewVariable]))
          }
        })

        temp <- as.data.frame(table(vmy$mydata[s]))
        temp <- subset(temp,temp$Freq!=0)
        temp
        output$mSurviceGroupedFreq <- renderPrint({
          if (mNewVariable %ni% names(vmy$mydata)==FALSE){
            temp
          }
          else{
            "not yet grouped"
          }
        })
        
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    fncreatedftype()
  }
  

  
} #server closure
shinyApp(ui, server)



