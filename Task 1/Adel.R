library(shiny)

ui <- fluidPage(
  titlePanel("Adelia Putri Pangestika"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "inputFile", label = "Masukkan File", 
                multiple = FALSE, accept = c(".csv",".txt"), 
                buttonLabel = "Browse", placeholder = "No file selected"),
      checkboxInput(inputId = "header", 
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

server <- function(input, output, session){
  
  file <- reactive({
    req(input$inputFile)
    file <- read.csv(input$inputFile$datapath, 
                     header = input$header)
    if(input$header==F){
      main <<- NULL
    } else if(input$header){
      main <<- colnames(file)
    }
    
    return(file)
  })
  
  cek <- reactive({
    tabFreq <- table(file())
    
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
}

shinyApp(ui, server)



