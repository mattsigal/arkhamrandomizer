library(shiny)
source("arkham.R")

shinyServer(function(input, output) {
    
    output$out <- renderPrint({
        if (input$random == "none"){
            toprint <- cat("Select the component you would like to randomize from the drop down menu.")
        }
        if (input$random == "hero"){
            toprint <- heroes(input$players, input$chars, input$baseC, input$dunwichC, input$kingsportC, input$innsmouthC, input$calvinC)
        }
        if (input$random == "goo"){
            toprint <- GOO(input$goos, input$baseA, input$dunwichA, input$kingsportA, input$innsmouthA, input$daolothA)
        }
        if (input$random == "herald"){
            toprint <- herald(input$hers, input$dph, input$dhh, input$kiyh, input$kingh, input$bgwh, input$innh, input$lth)
        }
        if (input$random == "guardian"){
            toprint <- guardians(input$guards, input$king)
        }
        if (input$random == "institution"){
            toprint <- institutions(input$insts, input$misk)
        }
        cat(toprint)
    }
    )

})