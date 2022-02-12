library(shiny)
library(colourpicker)

ui <- fluidPage(
  responsive = T,
  titlePanel("Visualisasi", windowTitle = "Stats"),
  
  sidebarLayout(
    sidebarPanel(
      #Buat input berupa file
      fileInput(inputId = "file", 
                label = "Masukkan Data",
                placeholder = "data.csv"),
      
      #Buat Checkbox untuk memilih apakah akan menggunakan 
      checkboxInput(inputId = "chk",
                    label = "Baris Pertama Nama Kolom",
                    value = F),
      
      #Buat Radio Button buat Separator
      selectInput(inputId = "sep",
                  label = "Pilih Pemisah pada dataset",
                  choices = c("Comma (,)" = ",",
                               "Semicolon (;)" =";",
                               "Tab" = "\t",
                               "Pipe" = "|",
                               "Spasi" = " "),
                  selected = ","),
      br(),
      
      #Buat pilihan checkbox group output
      radioButtons(inputId = "c.his",
                   label = NULL,
                   choices = c("Histogram" = T),
                   selected = F),
      br(),
      
      radioButtons(inputId = "c.box",
                   label = NULL,
                   choices = c("Boxplot" = T),
                   selected = F),
      br(),
      
      radioButtons(inputId = "c.bar",
                   label = NULL,
                   choices = c("Barplot" = T),
                   selected = F),
      br(),
      
      radioButtons(inputId = "c.pie",
                   label = NULL,
                   choices = c("Pie Chart" = T),
                   selected  = F),
      br(),
      
      selectInput(inputId = "select",
                  label = "Pilih Variabel",
                  choices = NULL),
      
      
      tags$img(src = "task2_logo.png", width = "30%")
    
  ),
  
    mainPanel(
      verbatimTextOutput(outputId = "txt"),
      fluidRow(
        column(width = 5,
               plotOutput(outputId = "plot.his"),
               plotOutput(outputId = "plot.bar")
               ),
        column(width = 5,
               plotOutput(outputId = "plot.box"),
               plotOutput(outputId = "plot.pie")
               )
      )
    )
  )
)

server <- function(input, output, session){
  data <- reactive({
    req(input$file)
    file1 <- input$file
    
    if(is.null(file1)){return()}
    df <- as.data.frame(read.csv2(file = file1$datapath, header = input$chk, sep = input$sep))
    
    return(df)
  })
  
  observe({
    updateSelectInput(session,"select",
                      label = "Variabel",
                      choices = colnames(data()))
  })
  
  output$txt <- renderPrint({
    print(head(data()))
  })
  
  output$plot.his <- renderPlot({
    req(data())
    req(input$select)
    req(input$c.his)
    
    if (input$c.his == F){return()}
    else {
      hist(as.numeric(data()[[input$select]]), 
           main = paste("Histogram of", input$select),
           xlab = paste(input$select))
    }
  })
    
  output$plot.box <- renderPlot({
    req(data())
    req(input$select)
    req(input$c.box)
    
    if (input$c.box == F){return()}
    else {
      boxplot(as.numeric(unlist(data()[[input$select]])),
              main = paste("Boxplot of", input$select))
    }
  })
    
  output$plot.pie <- renderPlot({
    req(data())
    req(input$select)
    req(input$c.pie)
    
    if (input$c.pie == F){return()}
    else{
      pie(table(data()[[input$select]]),
          main = paste("Pie Chart of", input$select))
    }
  })
  
  output$plot.bar <- renderPlot({
    req(data())
    req(input$select)
    req(input$c.bar)
    
    if (input$c.bar == F){return()}
    else {
      barplot(table(data()[[input$select]]),
              main = paste("Barplot of", input$select))
    }
    
  })
}

shinyApp(ui,server)
