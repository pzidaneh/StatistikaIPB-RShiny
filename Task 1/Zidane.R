library(shiny)

ui <- fluidPage(
    titlePanel("Perisai Zidane Hanapi"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "data.file", label = "Input Data",
                      accept = c("text/csv", ".csv",
                                 "text/comma-separated-values,text/plain")),
            
            checkboxInput(inputId = "first.row.header",
                          label = "Baris pertama sebagai kolom"),
            
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
            tableOutput(outputId = "freq.tab"),
            plotOutput(outputId = "freq.bar")  # add check box to sort by value
        )
    )
)

server <- function(input, output, session) {
    
    Var1 <- reactive({
        req(input$data.file)
        Var1 <- read.csv(input$data.file$datapath,
                         header = input$first.row.header)
        
        if (input$first.row.header == F) {
            main <<- NULL
            
        } else if (input$first.row.header == T) {
            main <<- colnames(Var1)
            
        }
        
        return(Var1)
    })
    
    Tab1 <- reactive({
        tab <- table(Var1())
        
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
        print(head(Var1()))
    })
    
    output$freq.tab <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        return(Tab1())
    })
    
    output$freq.bar <- renderPlot({
        return(barplot(Tab1(), ylab = ylab, main = main))
    })
}

shinyApp(ui, server)
