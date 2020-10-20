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

# import cleaned dataframe
sberry <- read.csv("sberry_cleaned.csv")
d_total <- subset(sberry, domain1 == "TOTAL")

# Subset where unit is in $
usd <- subset(d_total, unit == "$")
test <- usd

# Set value to numeric
usd$Value <- as.numeric(gsub(pattern = ",", replacement = "",usd$Value))
usd[is.na(usd)] <- 0

# Prepare dataset of production for plots
prod <- subset(usd, what == " PRODUCTION" )

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
                         br(),br(),
                         # plotOutput("yieldplot"),
                         # br(), 
                         # plotOutput("harvest"))
                # ,
                # 
                # tabPanel("Strawberry Fertilizer and Yield",
                #          
                #          # Create a new Row in the UI for selectInputs
                #          fluidRow(
                #              column(4,
                #                     selectInput("year2",
                #                                 "Year:",
                #                                 c("All",
                #                                   unique(data_rain$year))
                #                     ),
                #              ),
                #              column(4,
                #                     selectInput("state",
                #                                 "State:",
                #                                 c("All", unique(fert$State))
                #                         
                #                     )  
                #              ),
                #              # Plot rain of the year
                #              plotOutput("rainplot"),
                #              br(), br(),
                #              br(), 
                #              # Create summary table for rain of the year
                #              tableOutput("rainsum"),
                #              br(),
                #              
                #              # Create summary table for selected fips(county)
                #              # tableOutput("fipssum"), br(),
                #              # Plot storm& rain for selected fips
                #              # plotOutput("fipsplot"),
                #              br() 
                #              
                #          )
                # )
                #  ,
                # 
                # tabPanel("Chemicals",
                #          
                #          fluidRow(
                #              column(4,
                #                  selectInput(
                #                      "chemitype",
                #                      "Chemical type:",
                #                      c("All", unique(as.character(sberry$dc2) ))
                #                  ),
                #              ),
                #              column(4,
                #                     selectInput(
                #                         "state",
                #                         "State:",
                #                         c("All", unique(as.character(chemi$State) ))
                #                     )
                #                  
                #              ),
                #              plotOutput("chemi plot")
                #          )
                #          
                # )
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
            filtered %<>% select(Year, State, Value)
            names(filtered) = c("Year", "State", "Production value in $" )
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
}

# Run the application 
shinyApp(ui = ui, server = server)
