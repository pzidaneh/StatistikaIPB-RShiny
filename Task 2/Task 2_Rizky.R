library(shiny)

ui <- fluidPage(
  titlePanel("EKSPLORASI DATA"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file", label = "Masukkan File", multiple = FALSE,
                accept = c("text/csv", ".csv", 
                           "text/comma-separated-values,text/plain"), 
                width = NULL, buttonLabel = "Cari File...", 
                placeholder = "Tidak ada file yang dipilih"),
      checkboxInput(inputId = "header", label = "Baris pertama nama kolom", 
                    value = T, width = NULL),
      radioButtons(inputId = "pemisah", label = "Jenis pemisah:", 
                   c(";",","," ")),
      selectInput(inputId = "variabel", label = "Variabel", choices = NULL),
      br(),
      br(),
      br(),
      img(src="task2_logo.png", width = "30%", align = "bottom")
    ),
    mainPanel(
      tableOutput("headData"),
      plotOutput("histogram"),
      plotOutput("boxplot")
    )
  )
)

server <- function(input, output, session){
  inData <- reactive({file <- input$file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "txt", "Harap unggah file .txt"))
  
    dataIn <- read.table(file$datapath, sep = input$pemisah, header = input$header)
  
    if(input$header == T) {
      main <- colnames(dataIn)
    
    } else {
      main <- NULL
    }
  
    return(dataIn)
    
  })
  
  observe(
    updateSelectInput(session = session, inputId = "variabel", 
                      label = "Variabel", choices = colnames(inData())[sapply(inData(), is.numeric)])
  )
  
  pilihan <- reactive({
    dat <- inData()[[input$variabel]]
    dat <- as.numeric(dat)
  })
  
  output$headData <- renderTable(
    return(head(inData()))
  )
  
  output$histogram <- renderPlot(
    return(hist(pilihan(), xlab=input$variabel, main = paste("Histogram of", 
                input$variabel)))
  )
  
  output$boxplot <- renderPlot(
    return(boxplot(pilihan(), horizontal = T))
  )
}

shinyApp(ui, server)