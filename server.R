library(shiny)
source("arkham.R")

shinyServer(function(input, output) {
    
    output$out <- renderPrint({
        if (input$random == "none"){
            toprint <- cat("Select Investigators or Great Old One.")
        }
        if (input$random == "hero"){
            toprint <- heroes(input$players, input$chars, input$dunwich)
        }
        if (input$random == "goo"){
            toprint <- GOO(input$goos, input$dunwich)
        }
        cat(toprint)
    }
    )

})