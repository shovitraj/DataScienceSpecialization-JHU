#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(gapminder)
data <- gapminder
year <- levels(factor(data$year))
continent <- levels(data$continent)


ui <- dashboardPage(
         dashboardHeader(title="Gap Minder"),
              dashboardSidebar(
                  sidebarMenu(id="menu1",
                              menuItem("Life Expectancy Over Time", tabName="life"),
                              menuItem("Population Over Time", tabName="pop"),
                              menuItem("GDP Per Capita Over Time", tabName="gdp")),
                  conditionalPanel(condition = "input.menu1" == "life",
                                   
                  selectInput("select", label="Select Continent",
                              choices = data$continent,
                              selected ="Asia")),
                  
         dashboardBody(
#             tabItems(
                 tabItem(tabName="life",
                         fluidRow(
                             box(title="Gapminder:Life Expectacy Over Time", plotOutput("plot1"), width=8)
                             )),
                 tabItems(tabName ="pop",
                          fluidRow(
                              box(plotOutput("plot2"), width=8)
                          ))))
             
            
         

