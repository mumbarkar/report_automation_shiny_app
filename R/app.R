#### load required libraries ####
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(dplyr) # to manimpulate the dataframes
library(gtsummary) # to generate stats results
library(flextable)  # to save gtsummary object into word docs
library(writexl)# to save gtsummary object into excel
library(plotrix) # has a std.error function
library(gt) # to save gtsummary object into pdf
library(shinyBS) # display an info text when hovering over a R shiny selectInput
library(readxl) # read the excel file
library(huxtable) # to convert gtsummary table into huxtable and then extract into excel file.

#### maximize the fileinput size upto 30 MB ####
options(shiny.maxRequestSize=30*1024^2)

#### set working directory ####
# setwd("~/anlitiks/stats-report-automation/descriptive_shiny_app")

#### source the files for required functions ####
source(here::here("R","stats_fun.R"))

#### inputs ####
ci = c("TRUE", "FALSE")
cont_test = c("kruskal.test", "t.test","wilcox.test")
cat_test = c("fisher.test", "chisq.test")
export_data = c("Word","PDF","Excel")
font_name = c("Times New Roman","Arial","Verdana","Lora","News Cycle","Calibri","Cambria","Cambria Math")
font_name = sort(font_name,decreasing = FALSE)
font_size = paste(seq(1:50), "px",sep = "")

#### add function for mandatory fields ####
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

appCSS <-
    ".mandatory_star { color: red; }"

#### to make the tooltip wider  ####
options = list(container = 'body')
#### Define UI for application ####
ui <- dashboardPage(
    header = dashboardHeader(title="Report Automation", titleWidth = 230),
    skin = "blue",
    dashboardSidebar(
        sidebarMenu(id = "menu1",
                    tags$style(HTML(".main-sidebar { font-size: 14px!important; }
                   .treeview-menu>li>a { font-size: 14px!important; }")),
                    # sidebarSearchForm(textId = "searchbar", buttonId = "searchbtn", label = "Search..."),
                    menuItem("Import Data", tabName = "data_var", icon = icon("table")),
                    # menuSubItem("Read Data",tabName = "data_var",icon = icon("fas fa-map-signs"))),
                    menuItem("Data Analysis", tabName = "ana_var1", icon = icon("bar-chart-o"),
                             menuSubItem("Exploratory Data Analysis",tabName = "ana_var",icon = icon("fas fa-map-signs"))
                             # menuSubItem("Regression Analysis",tabName = "reg_var",icon = icon("fas fa-map-signs"))
                    )
        )
    ),
    dashboardBody(
        fluidPage(
            # shinytheme(theme = "spacelab"),
            tags$style(type='text/css',
                       ".selectize-dropdown-content{
                 height: 1000px;
                 background-color: #92a8d1;
                }"),
            tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                                }
                                "))),
            tags$head(tags$style(
                HTML('
             #sidebar {
             background-color: #dec2cb; 
             }
             
             body, label, input, button, select { 
             font-family: "Arial";
             }')
            )),
            shinyjs::useShinyjs(),
            shinyjs::inlineCSS(appCSS),
            
            ##### reading data ####
            tabItems(
                tabItem(tabName = "data_var",
                        mainPanel(width = 12,
                                  splitLayout(
                                      sidebarPanel(id = 'sidebar_panel',
                                                   # uploading a csv file in app
                                                   fileInput(inputId = "csv_var", label = labelMandatory("Upload file"),multiple = FALSE, accept = c(".csv"), width = NULL, buttonLabel = "Choose a csv file..."),
                                                   
                                                   checkboxInput("header_var", "Header", TRUE),
                                                   
                                                   radioButtons('sep_var', labelMandatory('Separator'),c(Comma=',', Semicolon=';', Tab='\t'),','),
                                                   bsTooltip(id = "sep_var",title = "Select the separator.", trigger = "hover",placement = "left"),
                                                   
                                                   actionButton(inputId = "go_csv", label = "Submit"),
                                                   bsTooltip(id = "go_csv",title = "Submit the data for further processing.", trigger = "hover",placement = "bottom"),
                                                   
                                                   actionButton(inputId = "reset_var", label = "Reset"),
                                                   bsTooltip(id = "reset_var",title = "Reset all input file(s).", trigger = "hover",placement = "bottom"),
                                                   width = 12
                                      )
                                  ),
                                  box(
                                      title = "Table",width = NULL,status = "primary",collapsible = TRUE,solidHeader = T,
                                      withSpinner(dataTableOutput("csv_df"),8)
                                  )
                        )
                ),
                
                ##### function input ####
                tabItem(tabName = "ana_var",
                        mainPanel(width = 12,
                                  splitLayout(
                                      sidebarPanel(id = "sidebar_panel_input1",
                                                   # select continuous variable(s)
                                                   selectInput(inputId = "continuous_var", label = labelMandatory("Continuous Variable(s)"), "Name", multiple = TRUE),
                                                   bsTooltip(id = "continuous_var",title = "Add continuous type variables", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input2",
                                                   # select factor variable(s)
                                                   selectInput(inputId = "factor_var", label = labelMandatory("Factor Variable(s)"), NULL, multiple = TRUE),
                                                   bsTooltip(id = "factor_var",title = "Add categorical type variables", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input3",
                                                   # select strata variable
                                                   selectInput(inputId = "strata_var", label = labelMandatory("Strata Variable"), "Name", multiple = FALSE),
                                                   bsTooltip(id = "strata_var",title = "Add a strata type variable", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input4",
                                                   # select Confidence Interval
                                                   selectInput(inputId = "ci_var", label = labelMandatory("Confidence Interval"), NULL, multiple = FALSE),
                                                   bsTooltip(id = "ci_var",title = "Logical type.", trigger = "hover",placement = "bottom"),
                                                   width = 12
                                      )
                                  ),
                                  splitLayout(
                                      sidebarPanel(id = "sidebar_panel_input5",
                                                   # select continuous var tests type
                                                   selectInput(inputId = "cont_test_var", label = labelMandatory("Stat Test - Continuous"), NULL, multiple = FALSE),
                                                   bsTooltip(id = "cont_test_var",title = "Select statistical test for continuous variables.", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input6",
                                                   # select categorical var tests type
                                                   selectInput(inputId = "cat_test_var", label = labelMandatory("Stat Test - Categorical"), NULL, multiple = FALSE),
                                                   bsTooltip(id = "cat_test_var",title = "Select statistical test for categorical variables.", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input7",
                                                   # add font name
                                                   selectInput(inputId = "font_var", label = "Font", NULL, multiple = FALSE),
                                                   bsTooltip(id = "font_var",title = "Add a font name.", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input8",
                                                   # add font size
                                                   selectInput(inputId = "fontsize_var", label = "Font Size", NULL, multiple = FALSE), 
                                                   bsTooltip(id = "fontsize_var",title = "Add a font size.", placement = "bottom"),
                                                   width = 12
                                      )
                                  ),
                                  splitLayout(
                                      sidebarPanel(id = "sidebar_panel_input9",
                                                   # add table title
                                                   textInput(inputId = "title_var",label = "Title",value = "Descriptive Results",placeholder = "Add text for title of the result."),
                                                   bsTooltip(id = "title_var",title = "Add text for the title of the result.", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input10",
                                                   # whether to export results into word, excel and pdf file
                                                   selectInput(inputId = "export_var", label = "Export Data", "Name", multiple = FALSE), 
                                                   bsTooltip(id = "export_var",title = "Select type to export the results in CSV, PDF or Word format.", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input11",
                                                   # add action button
                                                   actionButton(inputId = "go", label = "Submit"),
                                                   bsTooltip(id = "go",title = "Submit all the inputs to create a result table.", placement = "bottom"),
                                                   width = 12
                                      ),
                                      sidebarPanel(id = "sidebar_panel_input11",
                                                   # add action button
                                                   actionButton(inputId = "reset_input_var", label = "Reset"),
                                                   bsTooltip(id = "reset_input_var",title = "Reset all input fields.", placement = "bottom"),
                                                   width = 12
                                      )
                                  ),
                                  hr(),
                                  splitLayout(
                                      box(
                                          title = "Results",width = 12,status = "primary",collapsible = TRUE,solidHeader = T,
                                          withSpinner(tableOutput("Desc_output"),8)
                                          
                                      )
                                  ),
                                  br(),
                                  splitLayout(
                                      downloadButton(outputId = "downld_var",label = "Download", class = "fa fa-download"),
                                      bsTooltip(id = "downld_var",title = "Download the result in the form of selected type in Export Data.", placement = "right")
                                  )
                        )
                )
                
            )
        )
    )
)


#### Define server logic ####
server <- function(input, output, session) {
    
    #### insert csv logic ####
    external_file = eventReactive(input$go_csv,{
        input_file = input$csv_var
        # checking file null or not
        if (is.null(input_file)) {
            return(NULL)
        } else {
            input_file_df = read.csv(file = input_file$datapath, header = input$header_var, sep = input$sep_var)
            return(input_file_df)
        }
    })
    
    #### reset input (Data tab) ####
    observeEvent(input$reset_var, {
        shinyjs::reset("sidebar_panel")
    })
    
    #### reset all input (Analysis tab) ####
    observeEvent(input$reset_input_var, {
        # shinyjs::reset("sidebar_panel_input1")
        eval(parse(text = paste0("shinyjs::reset('sidebar_panel_input",seq(1:11),"')")))
    })
    
    #### subset the main dataset ####
    mydata = eventReactive(input$go,{
        df = external_file()
        req(input$continuous_var)
        req(input$factor_var)
        req(input$strata_var)
        
        continuous_vars = c(input$continuous_var)
        strata_vars = c(input$strata_var)
        factor_vars = c(input$factor_var)
        my_data = df %>% select(continuous_vars,strata_vars,factor_vars)
        return(my_data)
    })
    
    #### update selectInput dynamically ####
    observe({
        data = external_file()
        updateSelectInput(session, inputId = "continuous_var", choices = colnames(data))
    })
    
    observe({
        data = external_file()
        updateSelectInput(session, inputId = "factor_var", choices = colnames(data))
    })
    
    observe({
        data = external_file()
        updateSelectInput(session, inputId = "strata_var", choices = colnames(data))
    })
    
    observe({
        updateSelectInput(session, inputId = "cont_test_var", choices = cont_test)
    })
    
    observe({
        updateSelectInput(session, inputId = "cat_test_var", choices = cat_test)
    })
    
    observe({
        updateSelectInput(session, inputId = "ci_var", choices = ci)
    })
    
    observe({
        updateSelectInput(session, inputId = "export_var", choices = export_data)
    })
    
    observe({
        updateSelectInput(session, inputId = "font_var", choices = font_name)
    })
    
    observe({
        updateSelectInput(session, inputId = "fontsize_var", choices = font_size, selected = font_size[10])
    })
    
    #### some reactive expressions ####
    
    cont_test_var1 = eventReactive(input$go,{
        req(input$cont_test_var)
        cont_test_var = input$cont_test_var
        return(cont_test_var)
    })
    
    cat_test_var1 = eventReactive(input$go,{
        req(input$cat_test_var)
        cat_test_var = input$cat_test_var
        return(cat_test_var)
    })
    
    ci_val1 = eventReactive(input$go,{
        req(input$ci_var)
        ci_var = input$ci_var
        return(ci_var)
    })
    
    font_name1 = eventReactive(input$go,{
        req(input$font_var)
        font_var = input$font_var
        return(font_var)
    })
    
    font_size1 = eventReactive(input$go,{
        req(input$fontsize_var)
        font_size = input$fontsize_var
        return(font_size)
    })
    
    title_name1 = eventReactive(input$go,{
        req(input$title_var)
        title_nm = input$title_var
        return(title_nm)
    })
    
    export_var1 = eventReactive(input$go,{
        req(input$export_var)
        export_var = c(input$export_var)
        return(export_var)
    })
    
    #### print logic for given dataset ####
    output$csv_df = renderDataTable({
        df = external_file()
        print(head(df))
        DT::datatable(df,options = list(scrollY = TRUE,scrollX = TRUE,autoWidth = TRUE,lengthMenu = c(10, 20, 50,100))) %>% formatStyle(0, target = "row", backgroundColor = "white")
    }, )
    
    #### print logic for descriptive stats and its calculation ####
    output$Desc_output = render_gt({
        req(input$continuous_var)
        req(input$factor_var)
        req(input$strata_var)
        data = mydata()
        # ckeck whether data is null or not
        if(!is.null(data) == TRUE){
            print(head(data))
            continuous_vars1 = c(input$continuous_var)
            strata_vars1 = c(input$strata_var)
            Factor_vars1 = c(input$factor_var)
            cont_test = cont_test_var1()
            cat_test = cat_test_var1()
            ci_val = noquote(ci_val1())
            font_name = font_name1()
            font_size = font_size1()
            tab_title = title_name1()
            export_var = export_var1()
            
            summary_table = DescStatFun1(continuous_vars1, strata_vars1, Factor_vars1, data,cont_test,cat_test,ci_val,font_name, font_size, tab_title, export_var)
            
            return(summary_table)
        } else {return("data is not available...")}
    })
    
    #### add code chunks for download results ####
    output$downld_var =  downloadHandler(
        # specify the file name
        filename = function() {
            switch(export_var1(),
                   "Excel" = {
                       paste("output_",Sys.time(),".csv", sep = "")
                   },
                   "PDF" = {
                       paste("output_",Sys.time(),".pdf", sep = "")
                   },
                   "Word" = {
                       paste("output_",Sys.time(),".docx", sep = "")
                   }
            )
        },
        # save to the local machine
        content = function(file) {
            
            # write switch statement here to specify the filename and result to be download
            switch(export_var1(),
                   "Excel" = {
                       results_excel <- read_excel("output/results_excel.xlsx")
                       write.table(x = results_excel, file, sep = ",", row.names = FALSE)
                   },
                   "PDF" = {
                       file.copy("./output/results.pdf", file)           
                   },
                   "Word" = {
                       file.copy("./output/result_word.docx", file)           
                   }
            )
            
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
