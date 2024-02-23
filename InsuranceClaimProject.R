install.packages('rsconnect')
# Install and load necessary packages
library(rsconnect)
library(shiny)
library(dplyr)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Health Insurance Claim Prediction - Dharmik Joshi"),
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML(".js-irs-0 {color: #337ab7;}")), # Change color of slider labels
      numericInput("age", "Age:", value = 30),
      selectInput("sex", "Sex:", choices = c("Female" = 0, "Male" = 1)),
      numericInput("bmi", "BMI:", value = 25),
      numericInput("steps", "Number of steps daily:", value = 5000),
      numericInput("children", "Number of children:", value = 0),
      selectInput("smoker", "Smoker:", choices = c("Non-Smoker" = 0, "Smoker" = 1)),
      selectInput("region", "Region in US:", choices = c("Northeast" = 0, "Northwest" = 1, "Southeast" = 2, "Southwest" = 3)),
      numericInput("charges", "Past medical charges in USD:", value = 10000),
      actionButton("predictButton", "Predict", class = "btn-primary btn-lg btn-block")
    ),
    mainPanel(
      h4("Prediction Result:"),
      textOutput("predictionText"),
      h4("Model Description:"),
      verbatimTextOutput("modelSummary")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load dataset
  data <- read.csv("C:/Users/dharm/SEM 4/R Inssurance project/insurance.csv")
  
  # Convert factors
  data$sex <- factor(data$sex)
  data$smoker <- factor(data$smoker)
  data$region <- factor(data$region)
  data$insuranceclaim <- factor(data$insuranceclaim)
  
  # Partition data
  set.seed(1234)
  data1 <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[data1 == 1, ]
  test <- data[data1 == 2, ]
  
  # Build logistic regression model
  mymodel <- glm(insuranceclaim ~ age + sex + bmi + steps + children + smoker + region + charges,
                 data = train,
                 family = 'binomial')
  
  # Prediction function
  predict_claim <- function(age, sex, bmi, steps, children, smoker, region, charges) {
    new_data <- data.frame(age = age,
                           sex = as.factor(sex),
                           bmi = bmi,
                           steps = steps,
                           children = children,
                           smoker = as.factor(smoker),
                           region = as.factor(region),
                           charges = charges)
    prediction <- predict(mymodel, new_data, type = 'response')
    ifelse(prediction > 0.5, "WILL CLAIM : The policyholder is likely to claim health insurance according to the logistic regression model.", "WILL NOT CLAIM : The policyholder is not likely to claim health insurance according to the logistic regression model.")
  }
  
  # Event handler for predict button
  observeEvent(input$predictButton, {
    prediction <- predict_claim(input$age, input$sex, input$bmi, input$steps,
                                input$children, input$smoker, input$region, input$charges)
    output$predictionText <- renderText(prediction)
    output$modelSummary <- renderPrint(summary(mymodel))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

#deploy
rsconnect::setAccountInfo(name='dharmikjoshi', token='EDF0D9159A89455AF5520DA308E89C29', secret='+CaA5mW3iZ/hhmtvnVdhgiSvFfcepyHucOvKZYy0')
