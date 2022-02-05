library(shiny)

ui <- fluidPage(
  titlePanel("Kayla Fakhriyya Jasmine"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", 
                label = "Insert File",
                multiple = FALSE,
                accept = c("text/csv", ".csv",
                           "text/comma-separated-values,text/plain"),
                buttonLabel = "Browse...",
                placeholder = "No File Selected"
      ),
      checkboxInput("header", "Baris pertama nama kolom")
      
    ),
  
    mainPanel(
      tableOutput(outputId = "tablefile1"),
      plotOutput(outputId = "plotfile1")
    )
  )
)

server <- function(input, output, session){
   filecontent <- reactive({
    req(input$file1)
     filecontent <- read.csv(input$file1$datapath, 
                     input$header)
    
    if(input$header == F){
      main <<- NULL
    } else if(input$header == T){
      main <<- colnames(filecontent)
    }
    
    return(filecontent)
  })
  
  table1 <- reactive({
    tab <- table(filecontent())
    xlab <<- "Status Kepemilikan Rumah"
    ylab <<- "Frek."
    return(tab)
  })
  
  output$tablefile1 <- renderTable({
    return(table1())
  })
  
  output$plotfile1 <- renderPlot({
    return(barplot(table1(), xlab = xlab, 
                   ylab = ylab, main = main))
  })
}

shinyApp(ui, server)
