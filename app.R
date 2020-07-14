#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://jrobertsds.shinyapps.io/DerbyStats/

library (shiny)
library (lubridate)
library (dplyr)
library (ggplot2)

## Load Data
bankedRawData <- read.csv ("DBRefResumeBanked.csv", stringsAsFactors = FALSE)
flatRawData <- read.csv ("DBRefResumeFlat.csv", stringsAsFactors = FALSE)
# names (bankedRawData) [1] "Date"      "Year"      "HR"        "Position"  "Bout"      "With.Whom" "Notes"
# names (flatRawData) [1] "Bout.Date"  "Year"       "HR"         "Position"   "Bout"       "Notes"      "More.Notes" "X"
with (bankedRawData, bankedGameData <<- data.frame (Date = as.POSIXct(Date, format="%m/%d/%Y"), 
            Game = "Banked", HR = HR, Position = Position, Bout = Bout, Notes = With.Whom, Add.Notes = Notes))
# The 13th flat row is not a game, so 86 it.
with (flatRawData[-13, ], flatGameData <<- data.frame (Date = as.POSIXct(Bout.Date, format="%m/%d/%Y"), 
            Game = "Flat", HR = HR, Position = Position, Bout = Bout, Notes = Notes, Add.Notes = paste (More.Notes, X)))

gameData <- rbind (bankedGameData, flatGameData)
HRs <- count (gameData, HR) %>% arrange (desc (n))
Positions <- count (gameData, Position) %>% arrange (desc (n))

ui <- fluidPage(
    titlePanel("Derby Stats"),
    sidebarLayout(
        sidebarPanel(
            h5 ("Choose the number of bars to view:"),
            sliderInput("topHits", "Number of Bars:",
                        min = 4, max = max (length (HRs$n), length (Positions$n)), step = 1, value = 10),
            h5 ("Choose the type of chart to view:"),
            radioButtons("whichChart", "Show HRs, or Positions:", 
                        c("HRs" = "HRs", "Positions" = "Positions"))
        ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    #initialize
    topHits = 10
    whichChart = "HRs"
    output$distPlot <- renderPlot({
        if (input$whichChart == "Positions") {
            topHits <- min (input$topHits, length (Positions$n))
            graphTitle = sprintf ("Top %d ", topHits)
            graphTitle <- paste(graphTitle, "Positions")
            gameData[gameData$Position %in% Positions[1:input$topHits, ]$Position, ] %>% 
                ggplot(.) + geom_bar(aes(x = reorder (Position, Position, function (x) - length(x)), fill = Game), position = "stack") + theme(axis.text.x = element_text(angle = 90)) + xlab ("Position") + ylab ("Count") + ggtitle (graphTitle)
            
        } else {
            topHits <- min (input$topHits, length (HRs$n))
            graphTitle = sprintf ("Top %d ", topHits)
            graphTitle <- paste(graphTitle, "Head Referees")
            gameData[gameData$HR %in% HRs[1:input$topHits, ]$HR, ] %>% 
                ggplot(.) + geom_bar(aes(x = reorder (HR, HR, function (x) - length(x)), fill = Game), position = "stack") + theme(axis.text.x = element_text(angle = 90)) + xlab ("Head Referee") + ylab ("Count") + ggtitle (graphTitle)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
