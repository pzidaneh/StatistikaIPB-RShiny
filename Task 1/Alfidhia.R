library(shiny)

ui <- fluidPage(
  titlePanel("Alfidhia Rahman Nasa Juhanda"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "berkas", label = "Input Your Data Here :"),
      
      checkboxInput(inputId = "head",
                    label = "First Row as Column"),
      
      radioButtons(inputId = "stat", label = "Nilai Statistik:",
                   choices = c("Frequency" = "freq",
                               "Proportion" = "prop")),
      
      br(),
      
      checkboxInput(inputId = "valueOrder",
                    label = "Sort by Value"),
      
      checkboxInput(inputId = "rev",
                    label = "Reverse")
    ),
    
    mainPanel(
      tableOutput(outputId = "freqTab"),
      plotOutput(outputId = "freqBar") 
    )
  )
)

server <- function(input, output, session) {
  
  Word <- reactive({
    req(input$berkas)
    Word <- read.csv(input$berkas$datapath,
                     header = input$head)
    
    if (input$head == F) {
      main <<- NULL
      
    } else if (input$head == T) {
      main <<- colnames(Word)
      
    }
    
    return(Word)
  })
  
  Tab1 <- reactive({
    tab <- table(Word())
    
    if (input$valueOrder == T) {
      tab <- sort(tab)
    }
    
    if (input$rev == T) {
      tab <- rev(tab)
    }
    
    if (input$stat == "freq") {
      ylab <<- "Frequency"
      return(tab)
      
    } else if (input$stat == "prop") {
      ylab <<- "Proportion (%)"
      return(tab/sum(tab)*100)
      
    }
    
    return(tab)
  })
  
  output$freqTab <- renderTable({
    return(Tab1())
  })
  
  output$freqBar <- renderPlot({
    return(barplot(Tab1(), ylab = ylab, main = main))
  })
}

shinyApp(ui, server)