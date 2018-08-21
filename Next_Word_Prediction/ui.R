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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # add a theme
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Next Word Prediction"),
  
  # grid layout
  
  fluidRow(
    column(12,
           textInput("caption", "Enter text here:"),
           fluidRow(
             column(6,
                    "Fluid 6",
                    fluidRow(
                      column(6, 
                             "Fluid 6"),
                      column(6,
                             "Fluid 6")
                    )
             ),
             column(width = 6,
                    "Fluid 6")
           )
    )
  )
))
