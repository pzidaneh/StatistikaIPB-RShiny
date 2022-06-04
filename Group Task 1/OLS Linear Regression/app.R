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
                                      
                                      conditionalPanel(
                                        condition = "output.ekstensi",
                                        selectInput(inputId = "pemisah",
                                                    label = "Pilih Jenis Pemisah",
                                                    choices = c("Semicolon (;)" = ";",
                                                                "Comma (,)" =",",
                                                                "Tab" = "\t",
                                                                "Pipe (|)" = "|",
                                                                "Spasi" = " "),
                                                    selected = ";"))),
                                  
                                  conditionalPanel(
                                    condition = "output.fileUploaded",
                                    box(title = "Variabel Y", status = "primary", solidHeader = T,
                                        selectInput(inputId = "varY",
                                                    label = "Pilih Variabel Y:",
                                                    choices = NULL))),
                                  
                                  conditionalPanel(
                                    condition = "output.fileUploaded",
                                    box(title = "Variabel X", status = "primary", solidHeader = T,
                                        selectInput("varX",
                                                    label = "Pilih Variabel X:",
                                                    choices = NULL,
                                                    multiple = TRUE,
                                                    selected = NULL))
                                  )),
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
                                      conditionalPanel(
                                        condition = "output.fileUploaded",
                                        box(title = "Plot Korelasi antar Variabel",
                                            collapsible = TRUE,
                                            plotOutput(outputId = "corr"))),
                                      conditionalPanel(
                                        condition = "output.regression",
                                        box(title = "Plot Residual",
                                            collapsible = T,
                                            plotOutput(outputId = "resid"))),
                                      box(title = "Plot Tebaran",
                                          collapsible = T,
                                          plotOutput(outputId = "scatter")
                                      )),
                                    
                                    tabPanel(
                                      "Model and Regression Summary",
                                      conditionalPanel(
                                        condition = "output.regression",
                                        verbatimTextOutput(outputId = "model"),
                                        verbatimTextOutput(outputId = "regsum"))),
                                    
                                    tabPanel(
                                      "Uji Asumsi",
                                      conditionalPanel(
                                        condition = "output.regression",
                                        box(title = "Uji Asumsi Normalitas",
                                            height = "280px",
                                            selectInput(inputId = "sel.norm",
                                                        label = "Pilih Jenis Uji",
                                                        choices = c("Shapiro-Wilk"="Shapiro-Wilk",
                                                                    "Kolmogorov-Smirnov"= "Kolmogorov-Smirnov",
                                                                    "Anderson-Darling" = "Anderson-Darling"
                                                        ),
                                                        selected = "Shapiro-Wilk"),
                                            verbatimTextOutput(outputId = "norm"),
                                            verbatimTextOutput(outputId = "norm.result")),
                                        
                                        box(title = "Uji Asumsi Heteroskedastisitas",
                                            height = "280px",
                                            selectInput(inputId = "sel.hetero",
                                                        label = "Pilih Jenis Uji",
                                                        choices = c("Breusch-Pagan" = "Breusch-Pagan",
                                                                    "Glesjer" = "Glesjer"),
                                                        selected = "Breusch-Pagan"),
                                            verbatimTextOutput(outputId = "hetero"),
                                            verbatimTextOutput(outputId = "hetero.result")),
                                        
                                        box(title = "Uji Asumsi Autokorelasi",
                                            height = "280px",
                                            selectInput(inputId = "sel.auto",
                                                        label = "Pilih Jenis Uji",
                                                        choices = c("Durbin-Watson" = "Durbin-Watson",
                                                                    "Breusch-Godfrey" = "Breusch-Godfrey"),
                                                        selected = "Durbin-Watson"),
                                            verbatimTextOutput(outputId = "auto"),
                                            verbatimTextOutput(outputId = "auto.result")),
                                        
                                        box(title = "Ringkasan",
                                            height = "280px",
                                            tableOutput(outputId = "ringkas_uji"))
                                      )
                                    ),
                                    
                                    tabPanel(
                                      "Diagnostik",
                                      box(title = "Pencilan dan Amatan Berpengaruh",
                                          height = "280px",
                                          plotOutput(outputId = "diag")),
                                      
                                      conditionalPanel(
                                        condition = "output.multi",
                                        box(title = "Multikolinearitas",
                                            height = "280px",
                                            textOutput("note_vif"),
                                            verbatimTextOutput(outputId = "multikol")))
                                      
                                      
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
  
  if(ext == "txt" | ext == "csv"){
    
    dataIn <- read.table(file$datapath, sep = input$pemisah, header = input$header)
    
    if(input$header == T) {
      main <- colnames(dataIn)
      
    } else {
      main <- NULL
    }
    
    return(dataIn)
  }
  
  else{
    dataIn <- readxl::read_excel(file$datapath, col_names = input$header)
  }
  
  })
  
  output$ekstensi <- reactive({file <- input$file
  eks <- tools::file_ext(file$datapath)
  req(file)
  return(eks != "xlsx")
  })
  outputOptions(output, 'ekstensi', suspendWhenHidden=FALSE)
  
  output$fileUploaded <- reactive({
    return(!is.null(inData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$regression <- reactive({
    return(!is.null(lm_reg()))
  })
  outputOptions(output, 'regression', suspendWhenHidden=FALSE)
  
  output$multi <- reactive({
    return(length(paste(input$varX)) != 1)
  })
  outputOptions(output, 'multi', suspendWhenHidden=FALSE)
  
  observe(
    updateSelectInput(session = session, inputId = "varY", 
                      label = "Variabel", choices = colnames(inData())[sapply(inData(), is.numeric)])
  )
  
  # When the Choose button is clicked, update the selector
  observeEvent(input$varY,{
    updateSelectInput(session = session, inputId = "varX",label = "Variabel",
                      choices = colnames(inData())[sapply(inData(), is.numeric)][!(colnames(inData())[sapply(inData(), is.numeric)] %in% input$varY)])})
  
  output$tabel <- renderDataTable(inData(), options = list(pageLength = 10))
  
  output$summary <- renderPrint(
    stargazer::stargazer(
      inData(),
      type = "text",
      title = "Descriptive statistics",
      digits = 1,
      out = "table1.text")
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
  
  final <- reactive({
    if(length(paste(input$varX)) == 1){
      
      x <- as.numeric(inData()[[input$varX]])
      y <- as.numeric(inData()[[input$varY]])
      
      plot(x, y, pch = 16, col = "blue", xlab = paste(input$varX), 
           ylab = paste(input$varY)) + abline(lm_reg(), col = "red")}
    else{
      return(NULL)
    }
  })
  
  output$scatter <- renderPlot({
    req(lm_reg())
    final()
  })
  
  output$model <- renderPrint(lm_reg())
  
  output$regsum <- renderPrint(summary(lm_reg()))
  
  
  
  asumsi.norm <- reactive({
    req(lm_reg())
    req(input$sel.norm)
    if(input$sel.norm == "Shapiro-Wilk"){
      return(stats::shapiro.test(lm_reg()$residuals))
    }
    else if(input$sel.norm == "Kolmogorov-Smirnov"){
      return(stats::ks.test(lm_reg()$residuals, y = pnorm))
    }
    else if(input$sel.norm == "Anderson-Darling"){
      return(nortest::ad.test(lm_reg()$residuals))
    }
  })
  
  norm.result <- reactive({
    req(asumsi.norm())
    if(asumsi.norm()$p.value > 0.05) {
      return("Menyebar normal")
    }
    else{
      return("Tidak menyebar normal")
    }
  })
  
  output$norm <- renderPrint({
    req(lm_reg())
    print(asumsi.norm())
  })
  
  output$norm.result <- renderPrint({
    req(norm.result())
    print(norm.result())
  })
  
  asumsi.hetero <- reactive({
    req(lm_reg())
    req(input$sel.hetero)
    if(input$sel.hetero == "Breusch-Pagan"){
      return(lmtest::bptest(lm_reg()))
    }
    else if(input$sel.hetero == "glejser"){
      return(skedastic::glesjer(lm_reg()))
    }
  })
  
  hetero.result <- reactive({
    req(asumsi.hetero())
    if(asumsi.hetero()$p.value > 0.05){
      return("Sisaan tidak heterogen")
    }  
    else{
      return("Sisaan heterogen")
    }
  })
  
  
  output$hetero <- renderPrint({
    req(lm_reg())
    print(asumsi.hetero())
  })
  
  output$hetero.result <- renderPrint({
    req(hetero.result())
    print(hetero.result())
  })
  
  asumsi.auto <- reactive({
    req(lm_reg())
    if(input$sel.auto == "Durbin-Watson"){
      return(lmtest::dwtest(lm_reg()))  
    }
    else if(input$sel.auto == "Breusch-Godfrey"){
      return(lmtest::bgtest(lm_reg()))
    }
    
  })
  
  auto.result <- reactive({
    req(asumsi.auto())
    if(asumsi.auto()$p.value > 0.05){
      return("Tidak ada autokorelasi")
    }
    else{
      return("Ada Autokorelasi")
    }
  })
  
  output$auto <- renderPrint({
    req(lm_reg())
    print(asumsi.auto())
    
  })
  
  output$auto.result <- renderPrint({
    req(auto.result())
    print(auto.result())
  })
  
  p.norm <- reactive(asumsi.norm()$p.value)
  
  p.hetero <- reactive(asumsi.hetero()$p.value)
  
  p.auto <- reactive(asumsi.auto()$p.value)
  
  output$ringkas_uji <- renderTable({
    req(norm.result())
    req(hetero.result())
    req(auto.result())
    data.frame(
      "Jenis Asumsi" = c("Normalitas", "Heterokedastisitas", "Autokorelasi"),
      "Jenis Uji" = c(input$sel.norm, input$sel.hetero, input$sel.auto),
      "P-value" = c(p.norm(), p.hetero(), p.auto()),
      "Keputusan" = c(paste(norm.result()), paste(hetero.result()), paste(auto.result()))
    )}
  )
  
  output$diag <- renderPlot({
    req(lm_reg())
    olsrr::ols_plot_resid_lev(lm_reg())
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
  
  output$note_vif <- renderText(paste("Note: VIF > 10 mengindikasikan multikolinearitas"))
  
}

shinyApp(ui, server)
