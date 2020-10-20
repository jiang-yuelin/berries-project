#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(conflicted)
filter <- dplyr::filter

# import cleaned dataframe
sberry <- read.csv("sberry_cleaned.csv")


# prepare dataframe for production
d_total <- subset(sberry, domain1 == "TOTAL")
usd <- subset(d_total, unit == "$")
test <- usd

usd$Value <- as.numeric(gsub(pattern = ",", replacement = "",usd$Value))
usd[is.na(usd)] <- 0

prod <- subset(usd, what == " PRODUCTION" )

# prepare dataframe for yield
yield <- subset(d_total, what == " YIELD")
yield <- yield %>% select(Year, State, Value, unit)
yield$Value <- as.numeric(gsub(pattern = ",", replacement = "",yield$Value))
yield[is.na(yield)]<- 0

# Build Shiny UI
ui <- fluidPage(
    title = "Examples of Data Tables",
    sidebarLayout(
        tabsetPanel(
            conditionalPanel(
                'input.dataset === "sberry"'),
            
            conditionalPanel(
                'input.dataset === "sberry"',
            )
        ),
        mainPanel(
            
            tabsetPanel(
                id = 'dataset',
                tabPanel("Strawberry Production",
                         
                         # Create a new Row in the UI for selectInputs
                         fluidRow(
                             
                             column(4,
                                    selectInput("year",
                                                "Year:",
                                                c("All",
                                                  unique(prod$Year)))
                             )
                             ,
                             column(4,
                                    selectInput("state",
                                                "State:",
                                                c("All",
                                                  unique(prod$State)))
                             ),
                         ),
                         # Create plots for production of each state
                         plotOutput("prodplot"),
                         tableOutput("prodtable"),
                         br(),br()
                         # plotOutput("yieldplot"),
                         # br(), 
                         # plotOutput("harvest")
                         )
                ,

                tabPanel("Strawberry Yield",

                         # Create a new Row in the UI for selectInputs
                         fluidRow(
                             column(4,
                                    selectInput("yearY",
                                                "Year:",
                                                c("All",
                                                  unique(yield$Year))
                                    ),
                             ),
                             column(4,
                                    selectInput("stateY",
                                                "State:",
                                                c("All", unique(yield$State))

                                    )
                             ),
                             # Plot rain of the year
                             plotOutput("yieldPlot"),
                             br(), br(),
                             br(),
                             # Create summary table for rain of the year
                             tableOutput("yieldSum"),
                             br(),

                             # Create summary table for selected fips(county)
                             # tableOutput("fipssum"), br(),
                             # Plot storm& rain for selected fips
                             #plotOutput("fertPlot"),
                             br()

                         )
                )
                 ,

                tabPanel("Chemicals",

                         fluidRow(
                             column(4,
                                 selectInput(
                                     "chemitype",
                                     "Chemical type:",
                                     c("All", unique(as.character(sberry$dc2) ))
                                 ),
                             ),
                             column(4,
                                    selectInput(
                                        "state",
                                        "State:",
                                        c("All", unique(as.character(sberry$State) ))
                                    )

                             ),
                             plotOutput("chemi plot")
                         )

                )
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$prodtable <- renderTable({
            if(input$state =="All" & input$year == "All"){
                filtered <- prod
            } else if (input$year == "All"){
                filtered <- prod %>% filter(State == input$state)
            } else if (input$state == "All"){
                filtered <- prod %>% filter(Year == input$year)
            }
              else {
                filtered <- prod %>% filter(State == input$state & Year ==input$year )
            }
            filtered <- filtered %>% select(Year, State, Value)
            names(filtered) = c("Year", "State", "Production value in $")
            filtered
        })
    
    output$prodplot <- renderPlot({
        if(input$state =="All"){
            filtered <- prod
            p <- ggplot(data = filtered)+
                geom_point(aes(x=Year, y= Value/1000000, color = factor(State)), alpha=0.6, size =5)
        } else {
            filtered <- prod %>% filter(State == input$state)
            p <- ggplot(data = filtered)+
                geom_point(aes(x=Year, y= Value/1000000), color = "skyblue", size = 5)
        }
         
        p
    })
    
    output$yieldPlot <- renderPlot({
        if(input$stateY =="All" & input$yearY == "All"){
            filtered <- yield
            p <- ggplot(data = filtered)+
                geom_point(aes(x=Year, y= Value, color = factor(State)), alpha=0.6, size = 5)+
                ylim(c(0, 800))+
                xlim(c(2015, 2019))
        } else if (input$yearY == "All"){
            filtered <- yield %>% filter(State == input$stateY)
            p <- ggplot(data = filtered)+
                geom_col(aes(x=Year, y= Value, fill = State),  width = 0.4)+
                ylim(c(0, 800))+
                xlim(c(2014, 2020))
        } else if (input$stateY == "All"){
            filtered <- yield %>% filter(Year == input$yearY)
            p <- ggplot(data = filtered)+
                geom_point(aes(x=Year, y= Value, color = factor(State)), alpha=0.6, size = 5)+
                ylim(c(0, 800))+
                xlim(c(2014, 2020))
        }
        else {
            filtered <- yield %>% filter(State == input$stateY)
            p <- ggplot(data = filtered)+
                geom_col(aes(x=Year, y= Value, fill = State),  width = 0.4)+
                ylim(c(0, 800))+
                xlim(c(2014, 2020))
        }
        
        p
    })
    
    output$yieldSum <- renderTable({
        
        if(input$stateY =="All" & input$yearY == "All"){
            filtered <- yield
        } else if (input$yearY == "All"){
            filtered <- yield %>% filter(State == input$stateY)
        } else if (input$stateY == "All"){
            filtered <- yield %>% filter(Year == input$yearY)
        }
        else {
            filtered <- yield %>% filter(State == input$stateY & Year ==input$yearY )
        }
        
        filtered <- filtered %>% select (Year, State, Value, unit)
        names(filtered) = c("Year", "State", "Yield", "Unit")
        filtered
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
