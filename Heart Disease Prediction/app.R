library(shiny)
require(shiny)
library(caret)
library(shiny)
library(readr)
library(ggplot2)

#UI 
ui<- fluidPage(headerPanel("Heart disease prediction"),
               
               sidebarPanel(
                 
                 numericInput(inputId = "age", 
                              label = "Age", 
                              value = NULL, 
                              min = 20, 
                              max = 100),
                 
                 
                 selectInput("sex", "Gender:",
                             c(" " = "NULL",
                               "Male" = 1,
                               "Female" = 0)),
                 
                 
                 selectInput("cp", "Chest pain type:",
                             c(" " = "NULL",
                               "None" = 4,
                               "Mild" = 3,
                               "Moderate" = 2,
                               "Severe" = 1)),
                 
                 
                 numericInput(inputId = "trestbps", 
                              label = "Resting systolic blood pressure", 
                              value = NULL, 
                              min = 40, 
                              max = 220), 
                 
                 numericInput(inputId = "chol", 
                              label = "Cholesterol", 
                              value = NULL, 
                              min = 80, 
                              max = 650),  
                 
                 
                 selectInput("restecg", "Rest ECG:",
                             c(" " = "NULL",
                               "Normal" = 0,
                               "Slight abnormality" =1,
                               "Left ventricular hypertrophy" = 2
                             )),
                 
                 numericInput(inputId = "thalach", 
                              label = "Maximum heart rate achieved", 
                              value = NULL, 
                              min = 50, 
                              max = 220),
                 
                 selectInput("exang", "Exercise induced angina:",
                             c(" " = "NULL",
                               "Yes" = 1,
                               "No" = 0
                             )),
                 
                 numericInput(inputId = "oldpeak", 
                              label = "ST depression induced by exercise relative to rest", 
                              value = NULL,
                              min = -5, 
                              max = 10),  
                 
                 selectInput("slope", "Slope of the peak exercise ST segment:",
                             c(" " = "NULL",
                               "Upsloping" = 1,
                               "Flat" = 2,
                               "Downsloping" =3
                             )),
                 
                 selectInput("ca", "Number of major vessels (0-3) colored by flourosopy:",
                             c(" " = "NULL",
                               "Low" = 0,
                               "Mild" = 1,
                               "Moderate" = 2,
                               "High" = 3
                             )),
                 
                 selectInput("thal", "Presence of defect:",
                             c(" " = "NULL",
                               "Normal" = 3,
                               "Fixed defect" = 6,
                               "Reversible defect" = 7
                             )),
                 
                 actionButton("submit", "Submit")
               ), #close side bar


# Show result
mainPanel(
  textOutput("text1"),
  textOutput("text2"))
         )

# Load saved model
load("logreg.rda")    


#SERVER

server <- function(input, output) {
  
  options(shiny.maxRequestSize = 800*1024^2)
  
  call_me <- eventReactive(input$submit, {
    
    age = (input$age)
    sex = (input$sex)
    cp = (input$cp)
    trestbps = (input$trestbps)
    chol = (input$chol)
    restecg = (input$restecg)
    thalach = (input$thalach)
    exang = (input$exang)
    oldpeak = (input$oldpeak)
    slope = (input$slope)
    ca = (input$ca)
    thal = (input$thal)
  
    data_input <- data.frame(age, sex, cp, trestbps, chol, restecg, thalach, exang, oldpeak, slope, ca, thal)
  
    
    #PREPROCESS INPUT DATA
    ##CONVERT CONTINUOUS TO CATEGORICAL 
    breaks <- c(0,30, 35, 40, 50, 60, 70, 80, 90, 100, Inf)
    age_transformed <- cut(data_input$age,breaks = breaks, right=FALSE, labels=c(1:10))
    breaks1 <- c(0, 110, 120, 130, 140, 150, 160, Inf)
    trestbps_transformed <-cut(data_input$trestbps,breaks = breaks1, right=FALSE, labels=c(1:7))
    breaks2 <- c(0, 180, 200, 220, 240, 260, 280, 300, Inf)
    chol_transformed <-cut(data_input$chol,breaks = breaks2, right=FALSE, labels=c(1:8))
    breaks3 <- c(0, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, Inf)
    thalach_transformed <-cut(data_input$thalach,breaks = breaks3, right=FALSE, labels=c(1:13))
    breaks4 <- c(-10, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, Inf)
    oldpeak_transformed <- cut(data_input$oldpeak,breaks = breaks4, right=FALSE, labels=c(1:14))
    
    ##REPLACE DATA WITH NEW TRANSFORMED VALUES
    for(i in 1:ncol(data_input))
    {
      data_input$age <- age_transformed
      data_input$trestbps <- trestbps_transformed
      data_input$chol <- chol_transformed
      data_input$thalach <- thalach_transformed
      data_input$oldpeak <- oldpeak_transformed
    }
    
    pred <- glm_fit %>% predict(data_input)
    pred
  })
  
  output$text1 <- renderText({"The heart disease prediction is   "})
  output$text2 <- renderText(paste(call_me()))
  
}

shinyApp(ui = ui, server = server)
