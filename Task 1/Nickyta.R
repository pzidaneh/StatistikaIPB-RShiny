library(shiny)

ui<- fluidPage(
  titlePanel("Nickyta Shavira Maharani"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileID", "Upload the .txt File",
                multiple = FALSE,
                accept = ".txt",
                buttonLabel = "Browse...",
                placeholder = "No File Selected"
      ),
      checkboxInput("checkID", "Baris pertama adalah nama kolom", FALSE),
      
      radioButtons(inputId = "stat", label = "Nilai Statistik:",
                   choices = c("Frekuensi" = "freq",
                               "Proporsi" = "prop")),
      
      br(),
      
      checkboxInput(inputId = "order.by.val",
                    label = "Urutkan kategori berdasarkan nilai"),
      
      checkboxInput(inputId = "reverse",
                    label = "Putar balik urutan")
      
    ),
     mainPanel(
      verbatimTextOutput(outputId = "textOutput"),
      tableOutput("tabel1out"),
      plotOutput("plot1out")
    )
  )
)

  server <- function(input, output, session) {
    
    project <- reactive({
      req(input$fileID)
      project <- read.csv(input$fileID$datapath,
                       header = input$checkID)
      
      if (input$checkID == FALSE) {
        main <<- NULL
        
      } else if (input$checkID == TRUE) {
        main <<- colnames(project)
        
      }
      
      return(project)
    })
    
    Tab1 <- reactive({
      tab <- table(project())
      
      if (input$order.by.val == T) {
        tab <- sort(tab)
      }
      
      if (input$reverse == T) {
        tab <- rev(tab)
      }
      
      if (input$stat == "freq") {
        ylab <<- "Frekuensi Kategori"
        return(tab)
        
      } else if (input$stat == "prop") {
        ylab <<- "Proporsi Kategori (%)"
        return(tab/sum(tab)*100)
        
      }
      
      return(tab)
    })
    
    output$textOutput <- renderPrint({
      print(head(project()))
    })
    
    output$tabel1out <- renderTable({
      return(Tab1())
    })
    
    output$plot1out <- renderPlot({
      return(barplot(Tab1(), ylab = ylab, main = main))
    })
  }

shinyApp(ui, server)