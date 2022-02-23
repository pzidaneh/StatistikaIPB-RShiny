library(shiny)
library(shinydashboard)
library(stringr)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "DASHBOARD"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Regression", tabName = "menu1", icon = icon("th-large"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem("menu1",
                                fluidPage(
                                  box(title = "Inputasi Data", 
                                      status = "primary",
                                      height = "302px",
                                      solidHeader = T,
                                      
                                      fileInput(inputId = "file", label = "Masukkan File", multiple = FALSE,
                                                accept = c("text/csv", ".csv", 
                                                           "text/comma-separated-values,text/plain",
                                                           ".xlsx",".xls"), 
                                                width = NULL, buttonLabel = "Cari File...", 
                                                placeholder = "Tidak ada file yang dipilih"),
                                      
                                      checkboxInput(inputId = "header", label = "Baris pertama nama kolom", 
                                                    value = T, width = NULL),
                                      
                                      selectInput(inputId = "pemisah",
                                                  label = "Pilih Jenis Pemisah",
                                                  choices = c("Semicolon (;)" = ";",
                                                              "Comma (,)" =",",
                                                              "Tab" = "\t",
                                                              "Pipe (|)" = "|",
                                                              "Spasi" = " "
                                                  ),
                                                  selected = ";")),
                                  
                                  box(title = "Variabel Y", status = "primary", solidHeader = T,
                                      selectInput(inputId = "varY",
                                                  label = "Pilih Variabel Y:",
                                                  choices = NULL)),
                                  
                                  box(title = "Variabel X", status = "primary", solidHeader = T,
                                      selectInput("varX",
                                                  label = "Pilih Variabel X:",
                                                  choices = NULL,
                                                  multiple = TRUE,
                                                  selected = NULL))
                                ),
                                fluidPage(
                                  tabBox(
                                    id = "tabset1",
                                    height = "1000px",
                                    width = 12,
                                    
                                    tabPanel("Data",
                                             dataTableOutput(outputId = "tabel")),
                                    
                                    tabPanel(
                                      "Data Summary",
                                      verbatimTextOutput(outputId = "summary")),
                                    
                                    tabPanel(
                                      "Plots",
                                      box(title = "Plot Korelasi antar Variabel",
                                          collapsible = TRUE,
                                          plotOutput(outputId = "corr")),
                                      box(title = "Plot Residu",
                                          collapsible = T,
                                          plotOutput(outputId = "resid"))),
                                    
                                    tabPanel(
                                      "Model and Regression Summary",
                                      verbatimTextOutput(outputId = "model"),
                                      verbatimTextOutput(outputId = "regsum")),
                                    
                                    tabPanel(
                                      "Uji Asumsi",
                                      
                                      box(title = "Uji Asumsi Normalitas",
                                          selectInput(inputId = "sel.norm",
                                                      label = "Pilih Jenis Uji",
                                                      choices = c("Shapiro-Wilk"="shapiro",
                                                                  "Kolmogorov-Smirnov"= "ks",
                                                                  "Anderson-Darling" = "anderson",
                                                                  "Chi-Square" = "chisq",
                                                                  "Lilliefors" = "lili"),
                                                      selected = "shapiro"),
                                          verbatimTextOutput(outputId = "norm")),
                                      
                                      box(title = "Uji Asumsi Heteroskedastisitas",
                                          selectInput(inputId = "sel.hetero",
                                                      label = "Pilih Jenis Uji",
                                                      choices = c("Breusch-Pagan" = "bp",
                                                                  "Glejser" = "glesjer",
                                                                  "Bartlett" = "bart",
                                                                  "Park"= "park",
                                                                  "White"= "white"),
                                                      selected = "bp"),
                                          verbatimTextOutput(outputId = "hetero")),
                                      
                                      box(title = "Uji Asumsi Autokorelasi",
                                          selectInput(inputId = "sel.auto",
                                                      label = "Pilih Jenis Uji",
                                                      choices = c("Durbin-Watson" = "dw",
                                                                  "Breusch-Godfrey" = "bg",
                                                                  "Newey-West" = "nw"),
                                                      selected = "dw"),
                                          verbatimTextOutput(outputId = "auto")),
                                      
                                      box(title = "Multikolinearitas",
                                          verbatimTextOutput(outputId = "multikol"))
                                    )
                                  )
                                ))
                      )
                    )
)

server <- function(input, output, session){
  inData <- reactive({file <- input$file
  ext <- tools::file_ext(file$datapath)
  req(file)
  
  dataIn <- read.table(file$datapath, sep = input$pemisah, header = input$header)
  
  if(input$header == T) {
    main <- colnames(dataIn)
    
  } else {
    main <- NULL
  }
  
  return(dataIn)
  
  })
  
  observe(
    updateSelectInput(session = session, inputId = "varY", 
                      label = "Variabel", choices = colnames(inData())[sapply(inData(), is.numeric)])
  )
  
  observe(
    updateSelectInput(session = session, inputId = "varX", 
                      label = "Variabel", choices = colnames(inData())[sapply(inData(), is.numeric)])
  )
  
  output$tabel <- renderDataTable(inData(), options = list(pageLength = 10))
  
  output$summary <- renderPrint(
    stargazer::stargazer(
      inData(),
      type = "text",
      title = "Descriptive statistics",
      digits = 1,
      out = "table1.txt")
  )
  
  dtNumerik <- reactive({
    req(ncol(inData())!=1)
    Filter(is.numeric,inData())
  })
  
  korelasi <- reactive({
    cor(dtNumerik())
  })
  
  output$corr <-
    renderPlot(corrplot::corrplot(
      korelasi(),
      type = "lower",
      method = "number"
    ))
  
  lm_reg <- reactive({
    if(is.null(input$varX)){
      return(NULL)
    }
    else{
      return(lm(as.formula(paste(input$varY," ~ ",paste(input$varX,collapse="+"))),data=inData()))
    }
  })
  
  output$resid <- renderPlot({
    req(lm_reg())
    par(mfrow= c(2,2))
    plot(lm_reg())
  })
  
  
  output$model <- renderPrint(lm_reg())
  
  output$regsum <- renderPrint(summary(lm_reg()))
  
  output$norm <- renderPrint({
    req(lm_reg())
    print(stats::shapiro.test(lm_reg()$residuals))
  })
  
  output$hetero <- renderPrint({
    req(lm_reg())
    lmtest::bptest(lm_reg())
  })
  
  output$auto <- renderPrint({
    req(lm_reg())
    car::durbinWatsonTest(lm_reg())
    
  })
  
  mulcol <- reactive({
    req(input$varX)
    req(lm_reg())
    if(length(paste(input$varX)) == 1){
      return(NULL)
    }
    else{
      return(car::vif(lm_reg()))
    }
  })
  
  output$multikol <- renderPrint(mulcol())
  
}

shinyApp(ui, server)
