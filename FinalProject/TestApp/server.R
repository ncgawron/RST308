#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    
    
    greekdata <- reactive({
        value <- c(input$alpha ,input$beta)
        rBinomial <- rbinom(n=value[2], size = input$bins, prob = value[1])
        list(rand = rBinomial, alpha = value[1], beta = value[2])
    })
    
    
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

 
        
          })
    
    output$numBin <- renderText({
        paste0("The slide value is ", input$bins)
    
    })
    
    output$myTable <- renderTable({
        info <-greekdata()
        data.frame(Binomial = info$rand)
    })
    
   data2 <- eventReactive(input$runPlot, )
})
