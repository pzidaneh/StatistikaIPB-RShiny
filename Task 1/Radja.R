#Membuat aplikasi untuk tugas 1
library(shiny)

#Membuat fluid ui
ui <- fluidPage(
    
    #Beri judul untuk aplikasi
    titlePanel("Aplikasi untuk tugas 1"),
    
    #Buat layout untuk input pada sidebar
    sidebarLayout(
        sidebarPanel(
            
            #Buat panel untuk input file .txt
            fileInput(inputId = "fileInput", label = "Masukkan file .txt pada menu dibawah:"),
            br(),
            checkboxInput(inputId = "confHeader", label = "File ini memiliki header")
        ),
        
        #Buat main panel untuk output
        mainPanel(
            tableOutput(outputId = "freqTable"),
            plotOutput(outputId = "freqPlot")
        )
    )
)


#Membuat backend untuk pemrosesan file
server <- function(input, output, session){
    
    #Buat tabel
    output$freqTable <- renderTable({
        
        #Pastiin ada file yang diupload
        req(input$fileInput)
        
        #Masukkan data file txt ke dalam data frame
        locFile <- input$fileInput
        df <- read.table(locFile[, 4])
        df <- as.data.frame(df)
        
        #Cek apakah cekbox header terpilih
        if (input$confHeader == T) {
            
            #Ambil data baris pertama dan jadikan nama kolom 
            colnames(df) <- df[1,]
            df <- df[2:nrow(df), ]
        
        #Apabila tidak dicek maka
        } else {
           df <- df 
        }
        
        #Coba cek df dulu
        tabelFreq <- table(df)
    })
    
    #Buat barplot dari df
    output$freqPlot <- renderPlot({
        
        #Cek dulu udah ada file atau belum
        req(input$fileInput)
        
        #Masukkan data file txt ke dalam data frame
        locFile <- input$fileInput
        df <- read.table(locFile[, 4])
        df <- as.data.frame(df)
        
        #Cek apakah cekbox header terpilih
        if (input$confHeader == T) {
            
            #Ambil data baris pertama dan jadikan nama kolom 
            colnames(df) <- df[1,]
            df <- df[2:nrow(df), ]
            
            #Apabila tidak dicek maka
        } else {
            df <- df 
        }
        
        #Buat tabel dulu
        tabelFreq <- table(df)
        
        #Buat plot
        barplot(tabelFreq)
    })
}

shinyApp(ui, server)
