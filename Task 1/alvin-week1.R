library(shiny)

ui <- fluidPage(
    titlePanel("Created by Alvin ^^"),
    sidebarLayout(
        sidebarPanel(
          fileInput("txtFile", "Please input the data",
                    buttonLabel = "Browse...", 
                    placeholder = "No file selected"),
          checkboxInput(inputId = "withColumn", label="First row is header")
        ),
        mainPanel(
           tableOutput("freqtab"),
           plotOutput("plots")
        )
    )
)

server <- function(input, output, session) {
  readTxtFile <- function(){
    req(input$txtFile)
    df <- read.csv(input$txtFile$datapath, header=input$withColumn)
    return(df)
  }
  output$freqtab <- renderTable({
    data <- readTxtFile()
    table(data)
  })
  output$plots <- renderPlot({
    data <- readTxtFile()
    barplot(table(data))
  })
}

shinyApp(ui = ui, server = server)
