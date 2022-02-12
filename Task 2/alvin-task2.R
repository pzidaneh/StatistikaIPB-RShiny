library(shiny)

ui <- fluidPage(
  titlePanel("Eksplorasi Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("txtFile", "Please input the data",
                buttonLabel = "Browse...", 
                placeholder = "No file selected"),
      checkboxInput("withColumn", label="First row is header"),
      radioButtons("sep", "Select separator:",
                   c("Semicolon (;)"=";",
                     "Dot (.)"=".",
                     "Comma (,)"=",",
                     "Space ( )"=" ")),
      selectInput("variable","Select Variable",choices=""),
      imageOutput("logo")
    ),
    mainPanel(
      tableOutput("head"),
      plotOutput("histplot"),
      plotOutput("boxplot")
    )
  )
)

server <- function(input, output, session) {
  output$logo <- renderImage({
    return(list(
      src = "task2_logo.png",
      contentType = "image/png",
      width="30%", 
      alt = "Logo"))
  },deleteFile = FALSE)
  
  readTxtFile <- reactive({
    if (is.null(input$txtFile))
      return(NULL)
    
    df <- read.csv(input$txtFile$datapath, sep = input$sep, 
                   header=input$withColumn)
    return(df)
  })
  
  observe({
  updateSelectInput(session=session, 
                    inputId="variable", 
                    label="Select variable: ",
                    choices = colnames(readTxtFile()[,-1]))
  })
  
  output$head <- renderTable({
    if (is.null(input$txtFile))
      return(NULL)
    
    data <- readTxtFile()
    head(data)
  })
  
  output$histplot <- renderPlot({
    if (is.null(input$txtFile))
      return(NULL)
    
    data      <- readTxtFile()
    column    <- input$variable
    x         <- as.numeric(data[,column])
    hist(x=x, col="gold", border="black",
         main=paste("Histogram of",input$variable),
         xlab=input$variable)
  })
  
  output$boxplot <- renderPlot({
    if (is.null(input$txtFile))
      return(NULL)
    
    data    <- readTxtFile()
    column  <- input$variable
    x       <- as.numeric(data[,column])
    boxplot(x=x, 
            main=paste("Boxplot of",input$variable),
            xlab=input$variable)
  })
}

shinyApp(ui = ui, server = server)
