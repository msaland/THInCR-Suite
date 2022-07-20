## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE-----------------------------------------------------------
library(shiny)
library(shinyjqui)

## ---- eval=FALSE--------------------------------------------------------------
#  server <- function(input, output) {
#    output$order <- renderPrint({input$foo})
#  }
#  
#  ui <- fluidPage(
#    orderInput(inputId = 'foo', label = 'A simple example', items = c('A', 'B', 'C')),
#    verbatimTextOutput('order')
#  )
#  
#  shinyApp(ui, server)

## ---- eval=FALSE--------------------------------------------------------------
#  # items in A can be dragged to B
#  orderInput('A', 'A', items = 1:3, connect = 'B')
#  # items in B can be dragged to A
#  orderInput('B', 'B', items = 4:6, connect = 'A')

## ---- eval=FALSE--------------------------------------------------------------
#  # In source mode, items dragged to B are copied
#  orderInput('A', 'A', items = 1:3, connect = 'B', as_source = TRUE)
#  orderInput('B', 'B', items = 4:6)

## ---- eval=FALSE--------------------------------------------------------------
#  # Anything dropped into a "source" orderInput will be deleted
#  orderInput('A', 'A', items = 1:3, as_source = TRUE),
#  orderInput('B', 'B', items = 4:6)

## ---- eval=FALSE--------------------------------------------------------------
#  ui <- fluidPage(
#    orderInput('A', 'A', items = 1:3, as_source = TRUE, connect = c("B", "C")),
#    orderInput('B', 'B', items = 4:6, connect = "C"),
#    orderInput('C', 'C', items = 7:9, connect = "B"),
#    hr(),
#    actionButton("save", "Save"),
#    actionButton("load", "Load")
#  )
#  server <- function(input, output, session) {
#    observeEvent(input$save, jqui_sortable("#B,#C", "save"))
#    observeEvent(input$load, jqui_sortable("#B,#C", "load"))
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  orderInput('A', 'A', items = 1:3, connect = 'B')
#  orderInput('B', 'B', items = NULL, placeholder = 'Drag item here...')

## ---- eval=FALSE--------------------------------------------------------------
#  orderInput('default', 'default', items = 1:3, item_class = 'default')
#  orderInput('primary', 'primary', items = 1:3, item_class = 'primary')
#  orderInput('success', 'success', items = 1:3, item_class = 'success')
#  orderInput('info', 'info', items = 1:3, item_class = 'info')
#  orderInput('warning', 'warning', items = 1:3, item_class = 'warning')
#  orderInput('danger', 'danger', items = 1:3, item_class = 'danger')

