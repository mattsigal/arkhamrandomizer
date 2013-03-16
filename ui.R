library(shiny)

shinyUI(pageWithSidebar(

    headerPanel("Arkham Horror Randomizer"),
    
    sidebarPanel(
        selectInput("random", "What to randomize?",
                    list("Shuffle..." = "none", 
                         "Investigators" = "hero", 
                         "Great Old Ones" = "goo",
                         "Heralds" = "herald",
                         "Guardians" = "guardian",
                         "Institutions" = "institution",
                         "View Summary" = "summ"),
                    selected="none"),
        
        conditionalPanel(
            condition = "input.random == 'hero'",
            checkboxInput("baseC", "Use investigators from the base set?", TRUE),
            checkboxInput("dunwichC", "Use Dunwich Horror?", FALSE),
            checkboxInput("kingsportC", "Use Kingsport Horror?", FALSE),
            checkboxInput("innsmouthC", "Use Innsmouth Horror?", FALSE),
            checkboxInput("calvinC", "Add Calvin Wright?", FALSE),
            sliderInput("players", "Number of Investigators", min=1, max=8, value=4, step=1),
            sliderInput("chars", "Number of Investigators per Player?", min=1, max=4, value=1, step=1)
        ),
        conditionalPanel(
            condition = "input.random == 'goo'",
            checkboxInput("baseA", "Use GOOs from the base set?", TRUE),
            checkboxInput("dunwichA", "Use Dunwich Horror?", FALSE),
            checkboxInput("kingsportA", "Use Kingsport Horror?", FALSE),
            checkboxInput("innsmouthA", "Use Innsmouth Horror?", FALSE),
            checkboxInput("daolothA", "Add Daoloth?", FALSE),
            sliderInput("goos", "Number of Ancient Ones to choose from?", min=1, max=5, value=1, step=1)
        ),
        conditionalPanel(
            condition = "input.random == 'herald'",
            checkboxInput("dph", "Include the Dark Pharaoh?", FALSE),
            checkboxInput("dhh", "Include the Dunwich Horror Herald?", FALSE),
            checkboxInput("kiyh", "Include the King in Yellow?", FALSE),
            checkboxInput("kingh", "Include the Kingsport Horror Heralds?", FALSE),
            checkboxInput("bgwh", "Include the Black Goat of the Woods?", FALSE),
            checkboxInput("innh", "Include the Innsmouth Horror Heralds?", FALSE),
            checkboxInput("lth", "Include the Lurker at the Threshold?", FALSE),
            sliderInput("hers", "Number of Heralds to Choose?", min=1, max=5, value=1, step=1)
        ),
        conditionalPanel(
            condition = "input.random == 'guardian'",
            checkboxInput("king", "Include Guardians from the Kingsport Horror?", FALSE),
            sliderInput("guards", "Number of Guardians to Choose?", min=1, max=3, value=1, step=1)
        ),
        conditionalPanel(
            condition = "input.random == 'institution'",
            checkboxInput("misk", "Include Institutions from Miskatonic U.?", FALSE),
            sliderInput("insts", "Number of Institutions to Choose?", min=1, max=3, value=1, step=1)
        )
    ),
    
    mainPanel(
        verbatimTextOutput("out")
    )
    
))