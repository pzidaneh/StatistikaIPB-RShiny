library(shiny)

ui <- fluidPage(
  titlePanel("Tony Alfan", windowTitle = "Task 1 Tony"),
  
  sidebarLayout(
    sidebarPanel(
      
      #Buat input berupa file dengan id "file"
      fileInput(inputId = "file",
                label = "Input Data",
                placeholder = "Masukkan Data"),
      
      #Buat Input warna
      textInput(inputId = "colour",
                label = "Masukkan kode Hex warna",
                placeholder = "Contoh : #30D5C8"),
      
      
      #Buat Checkbox untuk memilih apakah akan menggunakan 
      checkboxInput(inputId = "chk",
                    label = "Baris Pertama Nama Kolom",
                    value = F),
      
      #Buat Checkbox untuk order val
      checkboxInput(inputId = "order",
                    label = "Urutkan Kategori"),
      
      #Buat Checkbox untuk reverse
      checkboxInput(inputId = "rev",
                    label = "Balikkan Urutan Data")
      
    ),
    
    mainPanel(
      tableOutput("table"),
      
      plotOutput("Bar.1")
    )
  )
)


server <- function(input,output, session) {
  data <- reactive({
    req(input$file)
    file1 <- input$file
    if(is.null(file1)){return()}
    read.csv(file = file1$datapath, header = input$chk)
  })
  
  tab <-  reactive({
    tab <- table(data())
    
    if(input$order == T){
      tab <- sort(tab)
    }
    
    if(input$rev == T){
      tab <- rev(tab)
    }
    return(tab)
  })
    
    
  output$table <- renderTable({
    
    return(tab())
    })
  
  output$Bar.1 <- renderPlot({
    req(input$colour)
    barplot(tab(), main = "Data", col = input$colour)
  })
}

shinyApp(ui,server)
