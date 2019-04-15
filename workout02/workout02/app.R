#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gtools)
library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Peijie's Workout02"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
            sliderInput("initial", "Initial Amount", min = 1, max = 100000, step = 500, value = 1000),
            sliderInput("contribution", "Annual Contribution", min = 0, max = 50000, step = 500, value = 2000)
            ),
     column(4,
            sliderInput("rate", "Return Rate (in %)", min = 0, max = 20, step = 0.1, value = 5),
            sliderInput("growth", "Growth Rate (in %)", min = 0, max = 20, step = 0.1, value = 2)
     ),
     column(3,
            sliderInput("year", "Years", min = 0, max = 50, step= 1, value = 10),
            selectInput("facet", "Facet?", c("Yes" = "yes", "No" = "no"))
     )
   ),
   # Show a plot of the generated distributionmain
   mainPanel("Timelines"),
   mainPanel(
     plotOutput("timelines")
    ),
   mainPanel("Balances"),
   mainPanel(
     dataTableOutput("balances")
   )
)

# code
future_value <- function(amount, rate, years) {
  return(amount*((1+rate)**years))
}
annuity <- function(contrib, rate, years) {
  return(contrib*(((1 + rate) ** years - 1) / rate))
}
growing_annuity <- function(contrib, rate, growth, years) {
  diff <- (1+rate)**years - (1+growth)**years
  return(contrib*diff/(rate-growth))
}
create_modalities <- function(rate, initial, contrib, growthrate, period) {
  rate <- rate / 100
  growthrate <- growthrate / 100
  years <- 0:period
  mode1 <- as.vector(c(initial))
  mode2 <- as.vector(c(initial))
  mode3 <- as.vector(c(initial))
  for (i in 1:length(years)-1) {
    mode1[i+1] <- future_value(amount = initial, rate = rate, years = i)
    mode2[i+1] <- mode1[i+1] + annuity(contrib = contrib, rate = rate, years = i)
    mode3[i+1] <- mode1[i+1] + growing_annuity(contrib = contrib, rate = rate, growth = growthrate, years = i)
  }
  modalities <- data.frame(year = years, no_contrib = mode1, fixed_contrib = mode2, growing_contrib = mode3)
  return(modalities)
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$timelines <- renderPlot({
    modalities <- create_modalities(
      rate = input$rate, initial = input$initial, contrib = input$contribution, 
      growthrate = input$growth, period = input$year)
    modalitiesMelted <- melt(modalities, id.var='year', variable.name = "Mode")
    if (input$facet == "no") {
      ggplot(modalitiesMelted, aes(x=year, y=value, col=Mode)) + geom_line() + geom_point() + 
        xlab("Years") + ylab("Balance ($)") + 
        scale_x_continuous("Years", seq(0, input$year, input$year/8)) + 
        ggtitle("Timeline Graph for different saving modes for 10-year period")
    } else {
      modalities <- melt(modalities, id.var='year', variable.name = "Mode")
      ggplot(data=modalities, aes(x = year, y = value, col = Mode)) + 
        geom_line() + geom_point() + 
        facet_wrap(~Mode) +
        aes(fill = Mode) + geom_area(alpha = 0.4) + 
        ggtitle("Three Modes of Investing")
    }
  })
  output$balances <- renderDataTable(create_modalities(
    rate = input$rate, initial = input$initial, contrib = input$contribution, 
    growthrate = input$growth, period = input$year))
}

# Run the application 
shinyApp(ui = ui, server = server)

