#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(htmlwidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # add a theme
  
  theme = shinytheme("flatly"),
  
  # Application title
  
  h1("Next Word Prediction", align = "center"),
  
  # grid layout
  
  fluidRow(column(
    6, offset = 3,
    align = "center",
    wellPanel(
      textInput("predtext", "Enter text here:", width = "90%"),
      actionButton("predButton", "Predict next word!")
    ),
    
    column(10,offset = 1,
    splitLayout(
      conditionalPanel("input.predButton",
                       wellPanel(
                         h4("All Words", align = "center"),
                         tableOutput("prediction")
                       )),
      conditionalPanel("input.predButton",
                       wellPanel(
                         h4("No Stopwords", align = "center"),
                         tableOutput("prediction_nosw")
                       ))
    ))
  ))
))
