#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

VacancyData <- read.csv("merged_parcel.csv")

library(shiny)

EnsurePackage <- function(x) {
  x <- as.character(x)
  
  if(!require(x,character.only = T)){
    install.packages(pkgs = x,repos = "http://cran.r-project.org")
    require(x,character.only = T)}
  }
EnsurePackage("ggplot2")
EnsurePackage("e1071")

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Prediction Models"),
   
   # Sidebar with a select input for adding categories 
   sidebarLayout(
      sidebarPanel(
        
      selectInput(inputId = "naivebayes",label = "Select Variables",multiple = TRUE,choices = colnames(VacancyData)
                  ),
      actionButton("button", "Predict")),
      
      # Show a table of predicted vs observed
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("NaivesBayes", tableOutput("table")),
                    tabPanel("SVM", tableOutput("SVMtable")))
      )
   )
)

# Define server logic
server <- function(input, output) {

model <- eventReactive(input$button, {
  VacList<- c("VacantBuil",unlist(input$naivebayes))
  VacSubset <- VacancyData[,VacList]
  set.seed(101) # Set Seed so that same sample can be reproduced in future also
  # Now Selecting 75% of data as sample from total 'n' rows of the data  
  sample <- sample.int(n = nrow(VacSubset), size = floor(.9*nrow(VacSubset)), replace = F)
  train <<- VacSubset[sample, ]
  test  <<- VacSubset[-sample, ]
  naiveBayesModel <- naiveBayes(VacantBuil ~.,data = train)
  
  prediction <- predict(naiveBayesModel,test)
  
  return(table(prediction,test$VacantBuil))
  
  })

svmmodel <- eventReactive(input$button,{
  Svm_model <- svm(formula = VacantBuil ~ .,data = train) 
  Svmprediction <- predict(Svm_model,test)
  
  return(table(Svmprediction,test$VacantBuil))
})
  



output$table <- renderTable(model())
output$SVMtable <- renderTable(svmmodel())
   
}
  
# Run the application 
shinyApp(ui = ui, server = server)

