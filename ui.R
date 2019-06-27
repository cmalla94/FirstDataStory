#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # Navbar
    navbarPage(
      "CMalla",
      tabPanel(
        "About Me",
        mainPanel(
          tags$h1("Chad Malla"),
          tags$p("I am a computing science major with a focus in AI and Data Science. 
                  Also have an interest in theoretical computing science. I have worked as a full-stack
                  developer at Plexia and going to work as a Data Analyst starting in May. I took
                 this assignment as an opportunity to explore animations and more advanced
                 data visualizations with Plotly and GGPlot2. Through the process I got more
                 experience using dplyr library for data wrangling."))
        ),
      #---------------------------------------------------------------------
      tabPanel(
        "Your Analysis",
        sidebarPanel(
          selectInput(inputId = "file",
                      label = "Select File for Distribution Plot",
                      choices = c("NYCarCrashes",
                                  "C02Worldwide", 
                                  "CanadianAvgSnow",
                                  "CanadianMeanTemp"))
        ),
        mainPanel(
          conditionalPanel(
            condition = "input.file == 'CanadianAvgSnow'",
            plotOutput("snowRidge")
          ), 
          conditionalPanel(
            condition = "input.file == 'CanadianMeanTemp'",
            plotOutput("tempRidge")
          ),
          conditionalPanel(
            condition = "input.file == 'C02Worldwide'",
            plotlyOutput("co2Anim"),
            plotlyOutput("co2Acc")
          ),
          conditionalPanel(
            condition = "input.file == 'NYCarCrashes'",
            plotlyOutput("carAgeFreq")
          ))
      ),
      tabPanel(
        "Airline Crash Story",
        mainPanel(
          tags$h1("The Airline Crash Story"),
          tags$br(),
          plotlyOutput("animation"),
          tags$p("The data points are green if the survival rate for that year is greater than 
                 40% else red."),
          tags$br(),
          tags$h3("Top 60 Operators involved in crashes"),
          plotOutput("cirPlot"),
          tags$br(),
          plotOutput("opTop"),
          tags$br(),
          verbatimTextOutput("air_selec_sum"),
          tags$br(),
          verbatimTextOutput("LargestAFCrash"),
          tags$br(),
          tags$h3("Story behind largest Air France Crash"),
          tags$p("The largest Air France crash in terms of the number of fatalities
                 happened on June 1, 2009 where all 228 that boarded the plane had
                 been killed. According to Wikipedia only parts of the plane and 
                 51 bodies have been found in the days following the crash. The probable 
                 cause was reported, by the BEA, as the aircraft's pilot tubes icing over leading 
                 the autopilot to disconnect and handling full control to pilots. Pilots 
                 were confused by the all warnings, pulled up the nose of the aircraft to
                 the point that the aircraft stalled. By the time the pilots realized 
                 the aircraft stalled, it was too late."),
          tags$h3("But the biggest crash involved two planes"),
          tags$p("The Pan American World Airways plane crashed with an KLM on the runway
                 as one was still on the runway after landing and the other taking off."),
          verbatimTextOutput("LargestCrash")
        )
        )
      )
    )
  )
 
