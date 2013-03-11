library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

    headerPanel("Arkham Horror Randomizer"),
    
    sidebarPanel(
        selectInput("random", "What to randomize?",
                    list("Heroes or GOOs?" = "none", "Heroes" = "hero", "Great Old Ones" = "goo"),
                    selected="none"),
        checkboxInput("dunwich", "Use Dunwich Horror?", FALSE),
        
        conditionalPanel(
            condition = "input.random == 'hero'",
            sliderInput("players", "Number of Investigators", min=1, max=8, value=4, step=1),
            sliderInput("chars", "Number of Investigators per Player?", min=1, max=4, value=1, step=1)
        ),
        conditionalPanel(
            condition = "input.random == 'goo'",
            sliderInput("goos", "Number of Ancient Ones to choose from?", min=1, max=5, value=1, step=1)
        )
    ),
    
    mainPanel(
        verbatimTextOutput("out")
    )
    
))