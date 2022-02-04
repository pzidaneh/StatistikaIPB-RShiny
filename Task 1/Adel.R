library(shiny)

ui <- fluidPage(
  titlePanel("Adelia Putri Pangestika"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "inputFile", label = "Masukkan File (.txt)", multiple = FALSE, accept = ".txt", buttonLabel = "Browse", placeholder = "No file selected"),
      checkboxInput(inputId = "inputCheckbox", label = "Baris pertama merupakan nama kolom", value = TRUE),
      textInput("text", label = "Masukkan warna barchart", placeholder = "Contoh : light blue"),
      checkboxInput(inputId = "order", label = "Urutkan kategori berdasarkan frekuensi")
    ),
    mainPanel(
      tableOutput(outputId = "tableFreq"),
      plotOutput(outputId = "barPlot")
    )
  )
)

server <- function(input, output, session){
  output$tableFreq <- renderTable({
    file <- input$inputFile
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "txt", "Please upload a txt file"))
    
    var <- read.table(file$datapath, header = input$inputCheckbox)
    tabFreq <- table(var)
    
    if (input$order == T) {
      tabFreq <- sort(tabFreq)
    }
    return(tabFreq)
    
  })
  
  output$barPlot <- renderPlot({
    file <- input$inputFile
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "txt", "Please upload a txt file"))
    
    req(input$text)
    
    var <- read.table(file$datapath, header = input$inputCheckbox)
    tabFreq <- table(var)
    
    if (input$order == T) {
      tabFreq <- sort(tabFreq)
    }
    
    df <- as.data.frame(tabFreq)
    barplot(height=df$Freq, names=df$var, xpd=F, col=input$text)
  })
}

shinyApp(ui, server)

