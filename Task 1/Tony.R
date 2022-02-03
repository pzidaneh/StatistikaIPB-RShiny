library(shiny)

help("renderTable")
ui <- fluidPage(
  titlePanel(h3("Tony Alfan")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file",
                label = "Input Data",
                placeholder = "Masukkan Data"),
      checkboxInput(inputId = "chk",
                    label = "Baris Pertama Kolom",
                    value = F)
    ),
    
    mainPanel(
      tableOutput("table"),
      
      plotOutput("Bar.1")
    )
  )
)


server <- function(input,output, session) {
  output$table <- renderTable(input$file)
}
