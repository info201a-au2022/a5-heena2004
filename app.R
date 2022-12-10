library(dplyr)
library(tidyverse)
library(shiny)
library(plotly)
library(ggplot2)


data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
View(data)


Introduction <- tabPanel(
  "Introduction",
  titlePanel("Climate Change and the Annual growth of production-based emissions of carbon dioxide(CO₂)"),
  p("Climate change is happening all around us and is an important issue we must all acknowledge.
  The impacts climate change makes, affects our life and overall health. With this data, I was able to find the country
  with the highest annual percentage growth in total production-based emissions of carbon dioxide across all the countries, which was,
  `r country_with_the_highest_annual_percentage_growth_in_total_production()`, the country with the highest annual growth in
  total production-based emissions of carbon dioxide, `r country_with_the_highest_annual_growth_in_total_production()`, lastly, the popluation number with the 
  the highest annual percentage growth in total production-based emissions of
  carbon dioxide accrued, `r the_population_with_the_highest_annual_percentage_growth()`.",
  ),
)

Visual <- tabPanel("Data Bar Chat",
sidebarLayout(
  sidebarPanel(
    selectInput(inputId = "choose_country", "Select a Country:", choices = unique(data$country))
  ),
  numericInput(inputId = "date_input",
    label = "Add Year from 1750-2021:",
    min = 1750, value = 0, max = 2021
  ),
),
  mainPanel(
    plotlyOutput("chart")
  )
)


ui <- navbarPage(
  "CO2 and Greenhouse Gas Emissions From Our World",
  Introduction,
  Visual,
)



imageOutput("myImage")
server <- function(input,output){
  con <- function(countrys, years){
    con2 <- select(data, country, year, co2_growth_abs) %>%
      group_by(country) %>%
      filter(country %in% countrys) %>%
      filter(year >= years)
    return(con2)
    
    # Value 1
    country_with_the_highest_annual_percentage_growth_in_total_production <- data %>%
      filter(co2_growth_prct == max(co2_growth_prct, na.rm = TRUE)) %>%
      pull(country, co2_growth_prct)
    
    #Value 2
    country_with_the_highest_annual_growth_in_total_production <- data %>%
      filter(co2_growth_abs == max(co2_growth_abs, na.rm = TRUE)) %>%
      pull(country, co2_growth_abs)
    
    #Value 3
    the_population_with_the_highest_annual_percentage_growth_ <- data %>%
      filter(co2_growth_prct == max(co2_growth_prct, na.rm = TRUE)) %>%
      pull(population, co2_growth_prct)
  }
  
  chart <- function()  {
    data <- data %>%
    ggplot(data, mapping = aes(x= year, y= co2_growth_abs))+
      geom_col()+
      xlab("Year")+
      ylab("Annual growth in total production-based emissions of carbon dioxide")+
      ggtitle("Annual Growth in Total Production-Based Emissions of CO2")
  } 

}
shinyApp(ui = ui, server = server)