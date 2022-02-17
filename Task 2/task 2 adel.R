library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tugas 1", tabName = "tugas1", icon = icon("th")),
      menuItem("Tugas 2", tabName = "tugas2", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName ="tugas1",
        fluidPage(
          titlePanel("Tugas 1"),
          sidebarLayout(
            sidebarPanel(
              fileInput(inputId = "inputFile1", label = "Masukkan File", 
                        multiple = FALSE, accept = c(".csv",".txt"), 
                        buttonLabel = "Browse", placeholder = "No file selected"),
              checkboxInput(inputId = "header1", 
                            label = "Baris pertama merupakan nama kolom", 
                            value = TRUE),
              radioButtons(inputId = "stat", label = "Nilai Statistik",
                           choices = c("Frekuensi" = "freq",
                                       "Proporsi" = "prop")),
              textInput("text", label = "Masukkan warna barchart", 
                        placeholder = "Contoh : light blue", 
                        value = "light blue"),
              checkboxInput(inputId = "order", 
                            label = "Urutkan kategori berdasarkan frekuensi")
            ),
            mainPanel(
              tableOutput(outputId = "tableFreq"),
              plotOutput(outputId = "barPlot")
            )
          )
          
        )
      ),
      tabItem(
        tabName ="tugas2",
        fluidPage(
          titlePanel("Tugas 2"),
          sidebarLayout(
            sidebarPanel(
              fileInput(inputId = "inputFile", label = "Masukkan File", 
                        multiple = FALSE, accept = c(".csv",".txt",".xlsx"), 
                        buttonLabel = "Browse", placeholder = "No file selected"),
              checkboxInput(inputId = "header", 
                            label = "Baris pertama merupakan nama kolom", 
                            value = TRUE),
              radioButtons(inputId = "delimiter", label = "Delimiter",
                           choices = c(";",",")),
              selectInput(inputId = "variabel", label = "Pilih Variabel", 
                          choices = NULL),
              img(src="task2_logo.png", width='100px')
            ),
            mainPanel(
              dataTableOutput(outputId = "tabel"),
              plotOutput(outputId = "histo"),
              plotOutput(outputId = "boxplot")
            )
          )
          
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  file1 <- reactive({
    req(input$inputFile1)
    file1 <- read.csv(input$inputFile1$datapath, 
                      header = input$header1)
    if(input$header1==F){
      main <<- NULL
    } else if(input$header1){
      main <<- colnames(file1)
    }
    
    return(file1)
  })
  
  cek <- reactive({
    tabFreq <- table(file1())
    
    if(input$order==T){
      tabFreq <- sort(tabFreq)
    }
    
    if(input$stat=="freq"){
      ylab <<- "Frekuensi"
      return(tabFreq)
    }
    
    if(input$stat=="prop"){
      ylab <<- "Proporsi"
      return(tabFreq/sum(tabFreq)*100)
    }
    
    return(tabFreq)
  })
  
  output$tableFreq <- renderTable({
    return(cek())
    
  })
  
  output$barPlot <- renderPlot({
    return(barplot(cek(), ylab=ylab, main=main, xpd=F, col=input$text))
  })
  
  file <- reactive({
    req(input$inputFile)
    ext <- tools::file_ext(input$inputFile$datapath)
    validate(need(ext == c("txt","csv"), "Harap unggah file .txt atau .csv"))
    file <- read.csv(input$inputFile$datapath, 
                     header = input$header,
                     sep = input$delimiter)
    
    if(input$header==F){
      main <<- NULL
    } else if(input$header){
      main <<- colnames(file)
    }
    
    return(file)
  })
  
  observe(updateSelectInput(session = session, inputId = "variabel",
                            choices = colnames(file())))
  
  var <- reactive({
    data <- file()[[input$variabel]]
    data <- as.numeric(data)
    
  })
  
  output$tabel <- renderDataTable(file(), options = list(pageLength = 6))
  
  output$histo <- renderPlot(hist(var(), xlab = input$variabel, main = paste("Histogram of", input$variabel)))
  
  output$boxplot <- renderPlot(boxplot(var(), xlab = input$variabel, main = paste("Boxplot of", input$variabel)))
}

shinyApp(ui, server)