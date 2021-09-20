#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#if (!dir.exists("data")) {
#    dir.create('data')
#}
#download.file("https://www.kaggle.com/aman1py/gapminder?select=gapminder_tidy.csv",
 #             destfile = "data/gapminder_data.csv")
gapminder <- read.csv("data/gapminder-data-2020-06-12.csv")
View(gapminder)

library(shiny)
library(ggplot2)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Drop-down selection box generated from Gapminder dataset
    output$choose_country <- renderUI({
        selectInput("country_from_gapminder", "Country", as.list(levels(gapminder$country)))
    })
    one_country_data  <- reactive({
        if(is.null(input$country_from_gapminder)) {
            return(NULL)
        }
        subset(gapminder, country == input$country_from_gapminder & year >= input$year_range[1] & year <= input$year_range[2] )
    })
    output$gapminder_table <- renderTable({ 
        one_country_data()
    })
    output$output_country <- renderText({
        if (is.null(input$country_from_gapminder)){
            return(NULL)
        }
        paste("Country selected", input$country_from_gapminder)
    })
    output$ggplot_gdppc_vs_country <- renderPlot({
        p <-  ggplot(one_country_data(), aes(x = year, y = gdpPercap))
        p + geom_point() 
    })
})