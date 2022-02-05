# Membuat sebuah aplikasi shiny untuk  membaca data kepemilikan rumah


library(shiny)

ui <- fluidPage(
    titlePanel("Faridatun Nisa"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "file", label = h3("Choose Text File"), 
                      accept = ".txt"),
            
            checkboxInput(inputId = "header", 
                          label = "Baris pertama nama kolom"),
            
            checkboxInput(inputId = "sort",
                          label = "Urutkan kategori berdasarkan nilai"),
            
            br(),
            
            textInput(inputId = "color", label = "Masukkan Warna / Kode Warna",
                      placeholder = "black"),
            
            br(),
            
            radioButtons(inputId = "statistik", label = "Statistik:",
                         choices = c("Frekuensi" = "freq",
                                     "Persentase" = "persentase"))
        ),
        
        mainPanel(
            verbatimTextOutput(outputId = "textOutput"),
            tableOutput(outputId = "tabFreq"),
            plotOutput(outputId = "barChart")
        )
    )
)


server <- function(input, output, session){
    
    var <- reactive({
        ext <- tools::file_ext(input$file$datapath)
        req(input$file)
        validate(need(ext == "txt", "Please upload a txt file"))
        var <- read.table(input$file$datapath, header = input$header)
        if (input$header == T) {
            main <<- NULL
        } 
        
        else if(input$header == F) {
            main <<- colnames(var)
        }
        
        return(var)
        
    })
    
    tab <- reactive({
        tab <- table(var())
        
        if (input$sort == T) {
            tab <- sort(tab, T)
        }
        
        if (input$statistik == "freq") {
            
            ylab <<- "Frekuensi"
            main <<- "Frekuensi Kategori"
            
            if (input$sort == T) {
                Frekuensi <- sort(table(var()), T)
                return (Frekuensi)
            }
            
            else if (input$sort == F) {
                Frekuensi <- table(var())
                return (Frekuensi)
            }
            
            
           return(tab)
        }
        
        else if (input$statistik == "persentase") {
            
            ylab <<- "Persentase"
            main <<- "Persentase Kategori"
            
            if (input$sort == T) {
                Persentase <- sort(table(var())/sum(table(var()))*100, T)
                return (Persentase)
            }
            
            else if (input$sort == F) {
                Persentase <- table(var())/sum(table(var()))*100
                return (Persentase)
            }
            
            
            return(tab/sum(tab)*100)
        }
        
        
        return(tab)
    })
    
    
    output$tabFreq <- renderTable({
        return(tab())
    })
    
    output$barChart <- renderPlot({
        return(barplot(tab(), ylab = ylab, main = main, col=input$color))
    })
    
}
shinyApp(ui, server)
