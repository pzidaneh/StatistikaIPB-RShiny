library(shiny)

ui <- fluidPage(
    titlePanel("Eksplorasi Data 2"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1",  "Input Your Data Here"),
      checkboxInput("header", "First row as header", value = T),
      radioButtons("separator", label = "pemisah",
                   choices=c("semicolon (;)"=";",
                             "comma (,)"=",",
                             "pipe (|)"="|",
                             "space ( )"=" ")),
      selectInput("variable", "Choose a variable",
                  choices = NULL
      ),
      imageOutput("logo")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("tablefile1")),
        tabPanel("Histogram", plotOutput("Hist")),
        tabPanel("Box Plot", plotOutput("BoxPlot"))
      )
      
    )
  )
)

server <- function(input, output, session){
  data1 <- reactive({
    req(input$file1)
    data1 <- read.csv(input$file1$datapath, header = input$header, sep = input$separator)
    
    return(data1)
  })
  observe({
    updateSelectInput(session=session,
                      inputId="variable", 
                      label="Select variable: ",
                      choices = colnames(data1())[sapply(data1(), is.numeric)])
  })
  output$tablefile1 <- renderTable({
    if(is.null(input$variable)){
      return(NULL)
    } else{
      return(head(data1()[input$variable]))
    }
  })
  output$logo <- renderImage({
    return(list(
      src = "task2_logo.png",
      contentType = "image/png",
      width="30%", 
      alt = "Logo"))
  },deleteFile = FALSE)
  output$Hist <- renderPlot({
    hist(as.numeric(data1()[[input$variable]]), main = paste("Histogram of", input$variable), col ="495371", xlab = input$variable)
  })
  output$BoxPlot <- renderPlot(
    boxplot(as.numeric(data1()[[input$variable]]), main = paste("Box Plot of", input$variable), col ="#FDEFF4", xlab = input$variable, horizontal=T)
  )
}

shinyApp(ui, server)