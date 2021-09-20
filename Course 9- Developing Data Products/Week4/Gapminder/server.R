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



library(shiny)
library(ggplot2)
library(gapminder)
library(DT)

#Load Gapminder Data
data <- gapminder

#Readable Population
data$pop <- data$pop/1000000
data$lifExp <- round(data$lifeExp, 2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Drop-down selection box generated from Gapminder dataset
    output$choose_country <- renderUI({
        selectInput("country_from_gapminder", 
                    h5("Country"),
                    as.list(levels(data$country), 
                    selected="Afghanistan"))
    })
    one_country_data  <- reactive({
        if(is.null(input$country_from_gapminder)) {
            return(NULL)
        }
        subset(data, country == input$country_from_gapminder & year >= input$year_range[1] & year <= input$year_range[2] )
    })
    output$gapminder_table <- renderDT({ 
        one_country_data()
    })
    output$output_country <- renderText({
        if (is.null(input$country_from_gapminder)){
            return(NULL)
        }
        paste("Country selected", input$country_from_gapminder)
    })
    
    # Render ggplot plot based on variable input from radioButtons
    output$ggplot_gdppc_vs_country <- renderPlot({
        
        if(is.null(one_country_data()$year)){
            return(NULL)
        }
        
        if(input$variable_from_gapminder == "pop") y_axis_label <- "Population (millions)"
        if(input$variable_from_gapminder == "lifeExp") y_axis_label <- "Life Expectancy (years)"
        if(input$variable_from_gapminder == "gdpPercap") y_axis_label <- "GDP Per Capita, PPP (fixed 2005 international $)"
        
        # Add aes_string argument for input from radioButtons, see:
        # https://groups.google.com/forum/#!topic/shiny-discuss/Ds2CKVfC4-Q
           ggplot(one_country_data(), aes_string(x = "year",
                                              y = input$variable_from_gapminder,
                                              colour = "country")) +
            geom_point() +
            geom_smooth(aes(fill= country), method="lm", formula=y~x) +
            xlab("Year") +
            ylab(y_axis_label)
      
    })
    
    data1 <- one_country_data()
    
    
    output$summary <- renderPrint({
        summary(one_county_data())
       })
    
})