#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rhandsontable)
library(shiny)
editTable <- function(DF, outdir=getwd(), outfilename="table"){
    ui <- shinyUI(fluidPage(
        
        titlePanel("Edit and save a table"),
        sidebarLayout(
            sidebarPanel(
                helpText("Shiny app based on an example given in the rhandsontable package.", 
                         "Right-click on the table to delete/insert rows.", 
                         "Double-click on a cell to edit"),
                
                wellPanel(
                    h3("Table options"),
                    radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
                ),
                br(), 
                
                wellPanel(
                    h3("Save"), 
                    actionButton("save", "Save table")
                )        
                
            ),
            
            mainPanel(
                
                rHandsontableOutput("hot")
                
            )
        )
    ))
}