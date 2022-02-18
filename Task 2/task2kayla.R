library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "black",
                   dashboardHeader(title = "Dashboard"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Task 1", tabName = "task1", icon = icon("file")),
                       menuItem("Task 2", tabName = "task2", icon = icon("file"))
                     )
                   ), 
  
  dashboardBody(
    tabItems(
      tabItem("task1",
              fluidPage(
                titlePanel("Kayla Fakhriyya Jasmine"),
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput(inputId = "file", 
                              label = "Insert File",
                              multiple = FALSE,
                              accept = c("text/csv", ".csv",
                                         "text/comma-separated-values,text/plain"),
                              buttonLabel = "Browse...",
                              placeholder = "No File Selected"
                    ),
                    checkboxInput("header1", "Baris pertama nama kolom")
                    
                  ),
                  
                  mainPanel(
                    tableOutput(outputId = "tablefile"),
                    plotOutput(outputId = "plot")
                  )
                )
              )
      ),
      tabItem("task2",
        fluidPage(
          titlePanel("Eksplorasi Data"),
          
          sidebarLayout(
            sidebarPanel(
              fileInput("file1", "Choose a File"),
              checkboxInput("header", "Baris pertama kolom", T),
              radioButtons("separator", "Pemisah",
                           c(comma = ',',
                             semicolon = ';',
                             tab = '\t',
                             space = ' '),
                           selected = ";"),
              selectInput("variable", "Choose a variable",
                          choices=NULL
                          ),
              imageOutput("image"),
            ),
            
            mainPanel(
              tableOutput("tablefile1"),
              tabsetPanel(
                tabPanel("Histogram", plotOutput("plotfile1")),
                tabPanel("Boxplot", plotOutput("plotfile2"))
                )
              )
            )
          )
        )
      )
    )
  )

server <- function(input, output, session){
  
  filecontent <- reactive({
    req(input$file)
    filecontent <- read.csv(input$file$datapath, 
                            input$header1)
    
    if(input$header1 == F){
      main <<- NULL
    } else if(input$header1 == T){
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
  
  output$tablefile <- renderTable({
    return(table1())
  })
  
  output$plot <- renderPlot({
    return(barplot(table1(), xlab = xlab, 
                   ylab = ylab, main = main))
  })
  
  data1 <- reactive({
    req(input$file1)
    data1 <- read.csv(input$file1$datapath, header = input$header, sep = input$separator)
    if(input$header == F){
      main <<- NULL
    } else if(input$header == T){
      main <<- colnames(data1)
    }
    return(data1)
  })
  
  observe({
    updateSelectInput(session=session, 
                      inputId="variable", 
                      label="Choose a Variable: ",
                      choices = colnames(data1())[sapply(data1(), is.numeric)]
                      )
  })
  
  output$image <- renderImage({
    return(list(
      src = "task2_logo.png",
      contentType = "image/png",
      width="30%", 
      alt = "Logo"))
  },deleteFile = FALSE)
  
  output$tablefile1 <- renderTable(
    return(head(data1()[input$variable]))
  )
  
  output$plotfile1 <- renderPlot(
    hist(as.numeric(data1()[[input$variable]]), 
         main = paste("Histogram of", input$variable),
         col = "bisque",
         xlab = input$variable)
  )
  
  output$plotfile2 <- renderPlot(
    boxplot(as.numeric(data1()[[input$variable]]), 
            horizontal = T,
            col ="bisque",
            xlab = input$variable)
  )
}

shinyApp(ui, server)