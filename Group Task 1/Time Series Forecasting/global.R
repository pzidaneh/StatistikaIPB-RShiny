library(shiny)
library(shinydashboard)
library(dashboardthemes)

library(forecast)
library(TTR)

df <- read.csv("C:/Users/LENOVO/Documents/00 IPB/Project R Shiny/1c task2/task2/task2_data.txt",
               header = T, sep = ";")

myDMA <- function (data, m, h) {
    result <- list()
    smooth.sma <- SMA(data, n = m)  # SMA
    smooth.dma <- SMA(smooth.sma, n = m)  # DMA = SMA of SMA
    
    # DMA forecast formula
    At <- 2*smooth.sma - smooth.dma
    Bt <- 2/(m - 1)*(smooth.sma - smooth.dma)
    
    data.dma <- At + Bt  
    forecast.dma <- c(NA, smooth.dma)
    
    bind <- cbind(data = c(data, NA),
                  smooth.dma = c(smooth.dma, NA),
                  forecast = forecast.dma)
    
    f <- At[length(At)] + Bt[length(Bt)]*(1:h)
    
    future <- data.frame(data = rep(NA, h),
                         smooth.dma = rep(NA, h),
                         forecast = f)
    
    return(ts(future$forecast, start = start(ts.data.test),
              end = end(ts.data.test), frequency = frequency(ts.data.test)))
}