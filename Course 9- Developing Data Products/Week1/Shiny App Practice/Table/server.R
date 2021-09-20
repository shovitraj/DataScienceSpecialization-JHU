#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rhandsontable)

library(shiny)
server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
    ## Handsontable
    observe({
        if (!is.null(input$hot)) {
            DF = hot_to_r(input$hot)
        } else {
            if (is.null(values[["DF"]]))
                DF <- DF
            else
                DF <- values[["DF"]]
        }
        values[["DF"]] <- DF
    })
    
    output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF))
            rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
    })
    
    ## Save 
    observeEvent(input$save, {
        finalDF <- isolate(values[["DF"]])
        saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
    })
    
})



