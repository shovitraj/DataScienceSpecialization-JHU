#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
    titlePanel("GDP of countries"),
    
    
    sidebarLayout(
        sidebarPanel("User inputs will be here",
        h2("Select Country and Year"),
                     uiOutput("choose_country"),
 #                    selectInput("select_country", 
 #                                label = "Country",
  #                               choices = gapminder$country)
   #                                        
    #                                   
                      sliderInput("year_range", 
                                 label = "Range of years:",
                                 min = 1952, max = 2007, 
                                 value = c(1955, 2005),
                                 format = "####")
        ),
        mainPanel(h3(textOutput("output_country")),
                  
                  plotOutput("ggplot_gdppc_vs_country"),
                  tableOutput("gapminder_table")
                  
        )
    )
))