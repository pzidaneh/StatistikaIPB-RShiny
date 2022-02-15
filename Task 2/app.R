library(shiny)
library(shinydashboard)

ui <- fluidPage(
  responsive = T,
  titlePanel("Regresi Linear", windowTitle = "Regresi"),
  
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
      
      #Buat Select input buat Separator
      selectInput(inputId = "sep",
                  label = "Pilih Pemisah pada dataset",
                  choices = c("Comma (,)" = ",",
                              "Semicolon (;)" =";",
                              "Tab" = "\t",
                              "Pipe" = "|",
                              "Spasi" = " "),
                  selected = ","),
      #Buat Pilihan Variabel X
      selectInput(inputId = "x",
                  label = "Pilih Variable X",
                  choices = NULL),
      
      #Buat Pilihan Variabel Y
      selectInput(inputId = "y",
                  label = "Pilih Variable Y",
                  choices = NULL)
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",verbatimTextOutput(outputId = "txt.sum")),
        tabPanel("Plot Regresi", plotOutput(outputId = "regline")),
        tabPanel("Residuals",plotOutput(outputId = "resid")),
        tabPanel("Assumption",
                 
                 h3("Uji Asumsi Normalitas"),
                 verbatimTextOutput(outputId = "norm"),
                 
                 h3("Uji Asumsi Heteroskedastisitas"),
                 verbatimTextOutput(outputId = "hetero"),
                 
                 h3("Uji Asumsi Autokorelasi"),
                 verbatimTextOutput(outputId = "auto"))
      
        )
      )
  )
)


server <- function(input,output,session) {
  data <- reactive({
    req(input$file)
    file1 <- input$file
    
    if(is.null(file1)){return()}
    df <- as.data.frame(read.csv2(file = file1$datapath, header = input$chk, sep = input$sep))
    
    return(df)
  })
  
  observe({
    updateSelectInput(session,"x",
                      label = "Variabel X",
                      choices = colnames(data()))
  })
  
  observe({
    updateSelectInput(session,"y",
                      label = "Variabel Y",
                      choices = colnames(data()))
  })
  
  output$txt.data <- renderPrint({
    print(head(data()))
  })
  
  
  reg <- reactive({
    req(input$x)
    req(input$y)
    
    x <- data()[[input$x]]
    y <- data()[[input$y]]
    reg <- lm(y ~ x)
    return(reg)
  })
  
  final <- reactive({
    req(data())
    req(input$x)
    req(input$y)
    
    x <- as.numeric(data()[[input$x]])
    y <- as.numeric(data()[[input$y]])
    
    plot(x, y, pch = 16, col = "blue", xlab = paste(input$x), 
                  ylab = paste(input$y)) + abline(reg(), col = "red")
  })
  
  output$regline <- renderPlot(final())
  
  output$txt.sum <- renderPrint({
    print(summary(reg()))
  })
  
  output$resid <- renderPlot({
    par(mfrow= c(2,2))
    plot(reg())
  })
  
  output$norm <- renderPrint({
    print(stats::shapiro.test(reg()$residuals))
    
  })
  
  output$hetero <- renderPrint({
    print(lmtest::bptest(reg()))
  })
  
  output$auto <- renderPrint({
    print(car::durbinWatsonTest(reg()))
  })
}

shinyApp(ui,server)
