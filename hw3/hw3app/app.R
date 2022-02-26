#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)

icu_cohort <- readRDS("~/203b-hw/hw3/mimic_icu_cohort.rds")

#---- User interface ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("MIMIC-IV Data Summary"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("MIMIC-IV is a relational database containing real hospital
               stays for patients admitted to a tertiary academic medical
               center in Boston, MA, USA."),
      
      selectInput("popvar", 
                  label = "Demographics",
                  choices = c("Thirty Day Mort", "Gender", "Ethnicity", "Language", 
                              "Insurance", "Marital Status"),
                  selected = "Thirty Day Mort"),
      helpText("Barplot"),
      selectInput("labvar", 
                  label = "Lab Results",
                  choices = c("Bicarbonate", "Calcium", "Chloride", 
                              "Creatinine", "Glucose", 
                              "Magnesium", "Potassium", 
                              "Sodium", "Hematocrit", "Wbc"),
                  selected = "Bicarbonate"),
      helpText("Histogram"),
      selectInput("vitalvar",
                  label = "Vital stauts",
                  choices = c("Heart Rate", 
                              "Noninvasive blood pressure systolic",
                              "Noninvasive blood pressure mean",
                              "Respiratory Rate",
                              "Temperature Fahrenheit"),
                  selected = "Heart Rate"),
      helpText("Histogram"),
   # mainPanel(plotOutput("demoplot"), 
    #          plotOutput("vitalplot"),
     #plotOutput("catplot"))
   ),
   mainPanel(plotOutput("plot"),
             plotOutput("plot1"),
             plotOutput("plot2"))
  )
)


# Server logic ----
# server function is run once each time a user visits the app
# observations and NA count
server <- function(input, output) {

  
  # renderPlot is run once each time a user changes a widget that output$map depends on
  
  output$plot <- renderPlot({
    #---- **Histogram/barplot ----
    Demographics <- switch(input$popvar, 
                   "Thirty Day Mort" = icu_cohort$thirty_day_mort,
                   "Gender" = icu_cohort$gender,
                   "Ethnicity" = icu_cohort$ethnicity,
                   "Language" = icu_cohort$language,
                   "Insurance" = icu_cohort$insurance,
                   "Marital Status" = icu_cohort$marital_status)
    barplot(table(Demographics),
            main="Demographics",
            border="white",
            col="black"
    )
  })
  
  # renderPlot is run once each time a user changes a widget that output$map depends on
  
  output$plot1 <- renderPlot({
    #---- **Histogram/barplot ----
    Lab_Results <- switch(input$labvar,
                          "Bicarbonate" = icu_cohort$le50882, 
                          "Calcium" = icu_cohort$le50893, 
                          "Chloride" = icu_cohort$le50902, 
                          "Creatinine" = icu_cohort$le50912, 
                          "Glucose" = icu_cohort$le50931, 
                          "Magnesium" = icu_cohort$le50960, 
                          "Potassium" = icu_cohort$le50971, 
                          "Sodium" = icu_cohort$le50983, 
                          "Hematocrit" = icu_cohort$le51221, 
                          "Wbc" = icu_cohort$le51301)
    hist(Lab_Results,
         main = "Lab Results")
  })
  
  # renderPlot is run once each time a user changes a widget that output$map depends on
  
  output$plot2 <- renderPlot({
    #---- **Histogram/barplot ----
    Vital_stauts <- switch(input$vitalvar,
                    "Heart Rate" =  icu_cohort$le220045,
                    "Noninvasive blood pressure systolic" = 
                      icu_cohort$le220179,
                    "Noninvasive blood pressure mean" = icu_cohort$le220181,
                    "Respiratory Rate" = icu_cohort$le220210,
                    "Temperature Fahrenheit" = icu_cohort$le223761)
    hist(Vital_stauts,
         main = "Vital Stauts")
  })
}

# Run app ----
    shinyApp(ui = ui, server = server)

