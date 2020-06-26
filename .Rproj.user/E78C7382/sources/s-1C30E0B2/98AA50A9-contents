library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)      #sort ggplot by amount
library(cowplot)      #Themes and tools for ggplot
library(ggthemes)     #themes for ggplot
library(rpart)        #Decision Tree
library(rpart.plot)   #Decision Tree - Plots
library(caret)        #Split data
library(e1071)
library(corrplot)     #CorrelationMatrix
library(DT)           #Display Table
library(pROC)         #pROC
library(randomForest) #RandomForest

#------------------------------------------------------------
#Data
#------------------------------------------------------------
original_df <- read.csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
df <- original_df
colnames(df)[1] = "Age"
df$StandardHours<-NULL
df$PerformanceRating<-NULL
df$Over18<-NULL
df$EmployeeCount<-NULL
df$JobLevel<-NULL
df$DailyRate<-NULL
df$HourlyRate<-NULL
df$DailyRate<-NULL
df$MonthlyRate<-NULL
df$PercentSalaryHike<-NULL
df$EmployeeNumber<-NULL
df$MaritalStatus<-NULL
df$EducationField<-NULL
df$RelationshipSatisfaction<-NULL
df$WorkLifeBalance<-NULL
df$YearsSinceLastPromotion<-NULL
df$JobInvolvement<-NULL

#Convert df_num for the correlation matrix
df_num <- df
df_num$Attrition= as.integer(as.factor(df_num$Attrition))-1
df_num$BusinessTravel= as.integer(as.factor(df_num$BusinessTravel))
df_num$Department= as.integer(as.factor(df_num$Department))
df_num$Gender= as.integer(as.factor(df_num$Gender))
df_num$JobRole= as.integer(as.factor(df_num$JobRole))
df_num$OverTime= as.integer(as.factor(df_num$OverTime))

set.seed(666)

# Split data
trainIndex <- createDataPartition(df$Attrition, p=0.8, list=FALSE, times=1)
train <- df[trainIndex,]
test <- df[-trainIndex,]

#------------------------------------------------------------
#Model Decision tree
#------------------------------------------------------------
model_dt <- rpart(Attrition ~ ., data=train)
model_dt <- rpart(Attrition ~ ., data=train,control=rpart.control(minsplit=10,cp=0.001))
model_dt
# plot(model_dt, uniform=TRUE, branch=0.6, margin=0.05)
# text(model_dt, all=TRUE, use.n=TRUE)
# 
# rpart.plot(model_dt, extra=0, type=2)

# Confusion Matrix DT
conf_mat_dt = predict(model_dt, newdata = test, type = "class") 
# conf_mat_info_dt <- confusionMatrix(conf_mat_dt ,test$Attrition)
# conf_mat_info_dt

# ROC Matrix DT
predict_dt = predict(model_dt, newdata = test, type = "prob")[,1]
par(pty="s")
# roc_dt <-  roc(response = test$Attrition, predictor = predict_dt,plot=TRUE, legacy.axes = TRUE, percent = TRUE, xlab= "False Positive Perc.", ylab= "True Positive Perc.",
#                col="#377EB8",lwd=4, print.auc=TRUE)

#Variable Importance DT
var_imp <- data.frame(model_dt$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$model_dt.variable.importance, 2)
var_imp$model_dt.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))
feature_importance <- var_imp 

# ggplot(feature_importance, aes(x=reorder(features, importance), y=importance, fill=features)) + 
#   geom_bar(stat='identity') + 
#   coord_flip() + 
#   geom_label(aes(label=paste0(importance, "%")), colour = "white", fontface = "italic", hjust=0.6) + 
#   theme_minimal() +
#   theme(legend.position="none")

#------------------------------------------------------------
#Model randomForest
#------------------------------------------------------------
model_rf <- randomForest::randomForest(Attrition ~ .,data = train)
model_rf <- randomForest::randomForest(Attrition ~ .,data = train,ntree=100, mtry=2)
model_rf

#Confusion Matrix RF
conf_mat_rf = predict(model_rf, newdata = test, type = "class") 
# conf_mat_info_rf <- confusionMatrix(conf_mat_rf ,test$Attrition)
# conf_mat_info_rf

#ROC RF
predict_rf = predict(model_rf, newdata = test, type = "prob")[,1]
# par(pty="s")
# roc(response = test$Attrition, predictor = predict_rf,plot=TRUE, legacy.axes = TRUE, percent = TRUE, xlab= "False Positive Perc.", ylab= "True Positive Perc.",
#     col="#377EB8",lwd=4, print.auc=TRUE)

#Variable Importance RF
# randomForest::varImpPlot(model_rf)

#------------------------------------------------------------
# EDA - Correlation
#------------------------------------------------------------
box_eda_corr <- tabPanel("Correlation", 
                         fluidRow(                         
                             shinydashboard::box(
                                 title = "Correlation Matrix", width = 8, status = "primary", solidHeader = TRUE,
                                 plotOutput("eda_corr_plot", click = "eda_corr_plot_click", height = "600px"),
                                 
                             ),
                             shinydashboard::box(
                                 title = "Correlation Detail", width = 4, status = "primary", solidHeader = TRUE,
                                 "Select a cell from the Correlation Matrix", br(), #, "More text",
                                 #verbatimTextOutput("corr_info"),
                                 conditionalPanel("output.corr_clicked",
                                                  plotOutput("eda_corr_click_info", height="320px")
                                 )
                             )
                         )
)
#------------------------------------------------------------
# EDA - Attrition
#------------------------------------------------------------
box_eda_attrition <- tabPanel("Attrition", 
                              fluidRow(                         
                                  shinydashboard::box(
                                      title = "Attrition", width = 4, status = "primary", solidHeader = TRUE,
                                      plotOutput("eda_attr_attr", height = "320px"),
                                      
                                  ),
                                  shinydashboard::box(
                                      title = "Attrition by Educational Level", width = 4, status = "primary", solidHeader = TRUE,
                                      plotOutput("eda_attr_edu_level", height = "320px"),
                                      
                                  ),
                                  shinydashboard::box(
                                      title = "Attrition by Job", width = 4, status = "primary", solidHeader = TRUE,
                                      plotOutput("eda_attr_job", height = "320px"),
                                      
                                  )
                              ),
                              fluidRow(                         
                                  shinydashboard::box(
                                      title = "Attrition by Age", width = 3, status = "primary", solidHeader = TRUE,
                                      plotOutput("eda_attr_age", height = "320px"),
                                      
                                  ),
                                  shinydashboard::box(
                                      title = "Attrition by Monthly Income", width = 3, status = "primary", solidHeader = TRUE,
                                      plotOutput("eda_attr_income", height = "320px"),
                                      
                                  ),
                                  shinydashboard::box(
                                      title = "Attrition by Distance from Home", width = 3, status = "primary", solidHeader = TRUE,
                                      plotOutput("eda_attr_distance", height = "320px"),
                                      
                                  ),
                                  shinydashboard::box(
                                      title = "Attrition by Years Current Manager", width = 3, status = "primary", solidHeader = TRUE,
                                      plotOutput("eda_attr_manager", height = "320px"),
                                      
                                  )
                              )                        
)

#------------------------------------------------------------
# EDA - Monthly Income
#------------------------------------------------------------
# box_eda_monthly <- tabPanel("Monthly Income", 
#                               fluidRow(                         
#                                 box(
#                                   title = "Monthy Income by Gender", width = 3, status = "primary", solidHeader = TRUE,
#                                   plotOutput("eda_monthly_gender", height = "320px"),
#                                   
#                                 ),
#                                 box(
#                                   title = "Monthy Income (mean) by Job Satisfaction", width = 6, status = "primary", solidHeader = TRUE,
#                                   plotOutput("eda_monthly_job", height = "320px"),
# 
#                                 )
#                                 # box(
#                                 #   title = "Gender", width = 3, status = "primary", solidHeader = TRUE,
#                                 #   plotOutput("eda_attr_gender", height = "320px"),
#                                 #   
#                                 # )
#                               )
#                               # fluidRow(                         
#                               #   box(
#                               #     title = "Attrition by Age", width = 3, status = "primary", solidHeader = TRUE,
#                               #     plotOutput("eda_attr_age", height = "320px"),
#                               #     
#                               #   ),
#                               #   box(
#                               #     title = "Attrition by Monthly Income", width = 3, status = "primary", solidHeader = TRUE,
#                               #     plotOutput("eda_attr_income", height = "320px"),
#                               #     
#                               #   ),
#                               #   box(
#                               #     title = "Attrition by Distance from Home", width = 3, status = "primary", solidHeader = TRUE,
#                               #     plotOutput("eda_attr_distance", height = "320px"),
#                               #     
#                               #   ),
#                               #   box(
#                               #     title = "Attrition by Years Current Manager", width = 3, status = "primary", solidHeader = TRUE,
#                               #     plotOutput("eda_attr_manager", height = "320px"),
#                               #     
#                               #   )
#                               # )                        
# )
#------------------------------------------------------------
# Intro
#------------------------------------------------------------
tab_intro <- shinydashboard::tabItem(tabName = "intro",
                                     shinydashboard::box(
                                       title = "Introduction", width = 12, status = "primary", solidHeader = TRUE,
                                       h2("Problem"),
                                       p("The consulting world suffers from high rotation and this represent a big problem to the companies"),
                                       tags$ul(
                                         tags$li("The cost of hiring a new person from the market is usually higher than the average of the current resources"), 
                                         tags$li("The time that it's spent in training and onboarding of new employee represent also a cost" ),
                                         tags$li("When several people leaves an area the others starts to wonder and why a lot of people are leaving"),
                                       ),
                                       h2("Benefits of the project"),
                                       p("Help the companies to detect possible unwanted attrition and resigns from his employees and also detect which variables are the most relevant"),
                                       h2("Data"),
                                       p("The well know IBM HR Attrition, which have a fictional database with info related on how \"Happy\" the employee is. The original dataset can be found here "),
                                       a(href="https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset", "IBM HR Analytics Employee Attrition & Performance"),                  
                                       p(""),
                                       p("In this app the Dataset was already been cleaned discarting fields because there was very simimlar fields or unrelated to the study like:"), 
                                       tags$ul(
                                         tags$li("Job Involment"),
                                         tags$li("Daily Rate"),
                                         tags$li("Hourly Rate"),
                                         tags$li("Relationship Satisfaction"),
                                         tags$li("Over18"),
                                       )                                       
                                     )
)

#------------------------------------------------------------
# EDA
#------------------------------------------------------------
tab_eda <- shinydashboard::tabItem(tabName = "eda",
                                   shinydashboard::box(
                                     title = "EDA - Exploratory Data Analysis", width = 12, status = "primary", solidHeader = TRUE,
                                   shinydashboard::tabBox(
                                       title = "",
                                       id = "box_eda", height = "100%", width = "100%",
                                       box_eda_corr,
                                       box_eda_attrition
                                       # box_eda_monthly,
                                       # tabPanel("Tab2", DT::dataTableOutput("original_datos") )
                                   )
                                   )
)

#------------------------------------------------------------
# Model
#------------------------------------------------------------
tab_model <- tabItem(tabName = "model",
                     shinydashboard::box(
                       title = "Models", width = 12, status = "primary", solidHeader = TRUE,
                       p("In each of the sub-section you can find the following information regarding the different models"),
                       tags$ul(
                         tags$li("ROC Curve"), 
                         tags$li("Confusion Matrix & General Stadistics" ),
                         tags$li("Importance of the variable within each model")
                       ),
                       p(""),
                       p("Even though the Random Forest have a better overall performance. The model choosed was Decision Tree because it's really important to interpret and explain the reasons behind the \"Why\" of the prediction besides the dataset also have lots of Categorical variables which this type of model can deal very well"),
                    )
)
#------------------------------------------------------------
# Model - Decision Tree
#------------------------------------------------------------
tab_model_dt <- shinydashboard::tabItem(tabName = "model_dt",
                                        shinydashboard::box(
                                            title = "Decision Tree Model", width = 12, background = "navy",
                                            ""
                                        ),                        
                                        shinydashboard::box(
                                            title = "ROC", width = 4,  status = "info", solidHeader = TRUE,
                                            plotOutput("roc_dt")#, height = "320px")
                                        ),
                                        shinydashboard::box(
                                            title = "Confusion Matrix & Stadistics", width = 4, status = "info", solidHeader = TRUE,
                                            verbatimTextOutput("conf_mat_info_dt")
                                        ),
                                        shinydashboard::box(
                                            title = "Importance Variables", width = 4, status = "info", solidHeader = TRUE,
                                            plotOutput("imp_var_dt")#, height = "320px")
                                        )                      
)
#------------------------------------------------------------
# Model - random forest
#------------------------------------------------------------
tab_model_rf <- shinydashboard::tabItem(tabName = "model_rf",
                                        shinydashboard::box(
                                            title = "Random Forest Model", width = 12, background = "olive",
                                            ""
                                        ),                          
                                        shinydashboard::box(
                                            title = "ROC", width = 4, status = "success", solidHeader = TRUE,
                                            plotOutput("roc_rf")#, height = "320px")
                                        ),
                                        shinydashboard::box(
                                            title = "Confusion Matrix & Stadistics", width = 4, status = "success", solidHeader = TRUE,
                                            verbatimTextOutput("conf_mat_info_rf")
                                        ),
                                        shinydashboard::box(
                                            title = "Importance Variables", width = 4, status = "success", solidHeader = TRUE,
                                            plotOutput("imp_var_rf")#, height = "320px")
                                        )                      
)
#------------------------------------------------------------
# Predict
#------------------------------------------------------------
tab_predict <- shinydashboard::tabItem(tabName = "predict",
                                       fluidRow(
                                           shinydashboard::box( 
                                               title = "Relevant Parameters", width = 3, status = "primary", solidHeader = TRUE,
                                               #age
                                               sliderInput("i_age", "Age?", min = 14, max = 100, value = 38),                         
                                               #travel
                                               selectInput("i_travel", "Travel Frequency", 
                                                           choices = as.list(levels(train$BusinessTravel)),
                                                           # choices = list("Non-Travel" = "Non-Travel", 
                                                           #                "Rarely" = "Travel_Rarely", 
                                                           #                "Frequently" = "Travel_Frequently"), 
                                                           selected = 1),
                                               #monthlyIncome
                                               sliderInput("i_monthly_income", "Monthly Income", min = 1000, max = 25000, value = 15000),
                                               #JobRole
                                               selectInput("i_role", "Job Role", 
                                                           choices = list("Sales Executive" = "Sales Executive", 
                                                                          "Research Scientist" = "Research Scientist", 
                                                                          "Laboratory Technician" = "Laboratory Technician",
                                                                          "Manufacturing Director" = "Manufacturing Director",
                                                                          "Healthcare Representative" = "Healthcare Representative",
                                                                          "Manager" = "Manager",
                                                                          "Sales Representative" = "Sales Representative",
                                                                          "Research Director" = "Research Director",
                                                                          "Human Resources" = "Human Resources"
                                                           ), 
                                                           selected = 3),     
                                               #JobSatisfaction
                                               selectInput("i_job_satisfaction", "Job Satisfaction", 
                                                           choices = list("Low" = 1, 
                                                                          "Medium"  = 2, 
                                                                          "High" = 3,
                                                                          "Very High"   = 4), 
                                                           selected = 2),  
                                               #Overtime
                                               selectInput("i_overtime", "Overtime", 
                                                           choices = list("No" = "No", 
                                                                          "Yes"  = "Yes"), 
                                                           selected = 2),                           
                                               #StockOptionLvl
                                               sliderInput("i_stock", "Stock Option level", min = 0, max = 3, value = 1),
                                               #YearsCompany
                                               sliderInput("i_years_company", "Years at Company", min = 0, max = 40, value = 6),                                                                         
                                               #Years Current Role
                                               sliderInput("i_years_role", "Years at Current Role", min = 0, max = 40, value = 2),                                                                         
                                               #Years Same Manager
                                               sliderInput("i_years_manager", "Years with Current Manager", min = 0, max = 40, value = 2),                                                                         
                                           ),
                                           column(width = 3,                       
                                                  shinydashboard::box(
                                                      title = "Less Relevant Parameters", width = NULL, status = "primary", solidHeader = TRUE,
                                                      #department
                                                      selectInput("i_department", "Department", 
                                                                  choices = list("Human Resources" = "Human Resources", 
                                                                                 "Research & Development" = "Research & Development", 
                                                                                 "Sales" = "Sales"), 
                                                                  selected = 3),
                                                      #Distance from home
                                                      sliderInput("i_distance", "Distance from home", min = 1, max = 30, value = 15),
                                                      #Education
                                                      selectInput("i_education", "Education", 
                                                                  choices = list("Below College" = 1, 
                                                                                 "College"  = 2, 
                                                                                 "Bachelor" = 3,
                                                                                 "Master"   = 4,
                                                                                 "Doctor"   = 5), 
                                                                  selected = 3),
                                                      #EnviromentSatisfaction
                                                      selectInput("i_env_satisfaction", "Enviroment Satisfaction", 
                                                                  choices = list("Low" = 1, 
                                                                                 "Medium"  = 2, 
                                                                                 "High" = 3,
                                                                                 "Very High"   = 4), 
                                                                  selected = 2),
                                                      #Gender
                                                      selectInput("i_gender", "Gender", 
                                                                  choices = list("Female" = "Female", 
                                                                                 "Male"  = "Male"), 
                                                                  selected = 2),  
                                                      #NumCompanyWorked
                                                      sliderInput("i_companyworked", "No Company Worked", min = 1, max = 10, value = 2),
                                                      #TotalWorkingYears
                                                      sliderInput("i_totalyworked", "Total Working Years", min = 0, max = 40, value = 18),                                                
                                                      #Training
                                                      sliderInput("i_training", "Training Times Last Year", min = 0, max = 10, value = 2),                                                
                                                  ),
                                                  shinydashboard::box(width = NULL,align="center",
                                                                      actionButton("predict", label = "Predict"),                           
                                                  )
                                           ),
                                           column(width = 3,
                                                  verbatimTextOutput("case")
                                                  # verbatimTextOutput("model")
                                           ),
                                           shinydashboard::valueBoxOutput("PredictBox", width = 3),                              
                                       )
)

#------------------------------------------------------------
# Datos
#------------------------------------------------------------
tab_datos <- shinydashboard::tabItem(tabName = "datos",
                                     fluidRow(
                                         # column(
                                         shinydashboard::box(
                                             title = "Data", width = 12, status = "primary", solidHeader = TRUE,
                                             DT::DTOutput("original_datos2")
                                         )
                                         # ,width=12)
                                     ),
                                     p("Education"),
                                     tags$ul(
                                       tags$li("1 'Below College'"),
                                       tags$li("2 'College'"),
                                       tags$li("3 'Bachelor'"),
                                       tags$li("4 'Master'"),
                                       tags$li("5 'Doctor'"),
                                     ),
                                     p(" "),
                                     p("EnvironmentSatisfaction"),
                                     tags$ul(
                                       tags$li("1 'Low'"),
                                       tags$li("2 'Medium'"),
                                       tags$li("3 'High'"),
                                       tags$li("4 'Very High'"),
                                     ),
                                     p(" "),
                                     p("JobSatisfaction"),
                                     tags$ul(
                                       tags$li("1 'Low'"),
                                       tags$li("2 'Medium'"),
                                       tags$li("3 'High'"),
                                       tags$li("4 'Very High'"),
                                     )                                                                              
)



sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Introduction", tabName = "intro",icon=icon("file-alt")),
        shinydashboard::menuItem("EDA", tabName = "eda",icon = icon("chart-bar")),
        shinydashboard::menuItem("Models", tabName = "model",icon = icon("tree")),
        shinydashboard::menuSubItem("Decision Tree", tabName = "model_dt",icon = icon("none")),
        shinydashboard::menuSubItem("Random Forest", tabName = "model_rf",icon = icon("none")),
        shinydashboard::menuItem("Predict", tabName = "predict",icon = icon('dashboard')),
        shinydashboard::menuItem("DataSet", tabName = "datos",icon=icon("database"))
    )
)

#------------------------------------------------------------
#Body
#------------------------------------------------------------
body <- shinydashboard::dashboardBody(
    tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
      color:#fff;
      background:#00699B
                        }
    
    .box.box-solid.box-primary{
    border-bottom-color:#00699B;
    border-left-color:#00699B;
    border-right-color:#00699B;
    border-top-color:#00699B;
    }

    .box.box-solid.box-success>.box-header {
      color:#fff;
      background:#3D9970
                        }
    
    .box.box-solid.box-success{
    border-bottom-color:#3D9970;
    border-left-color:#3D9970;
    border-right-color:#3D9970;
    border-top-color:#3D9970;
    }    
    
    .box.box-solid.box-info>.box-header {
      color:#fff;
      background:#001F3F
                        }
    
    .box.box-solid.box-info{
    border-bottom-color:#001F3F;
    border-left-color:#001F3F;
    border-right-color:#001F3F;
    border-top-color:#001F3F;
    }        
    ")),  
    shinydashboard::tabItems(
        tab_intro,
        tab_eda,
        tab_model,
        tab_model_dt,
        tab_model_rf,
        tab_predict,
        tab_datos
    )
)

#------------------------------------------------------------
# Shiny - UI
#------------------------------------------------------------
ui <- shinydashboard::dashboardPage(skin = c("blue"),  
                                    shinydashboard::dashboardHeader(title = "Attrition Analysis"),
                                    sidebar,
                                    body
)


server <- shinyServer(function(input, output) {
    output$box_eda_selected <- renderText({
        input$box_eda
    })
    
    #------------------------------------------------------------
    # General
    #------------------------------------------------------------    
    data_table <- reactive({
        df_num
    })
    df_table <- reactive({
        df
    })
    eda_attr_attr <- reactive({
        eda_attr_attr
    })    
    #------------------------------------------------------------
    # EDA - Correlation
    #------------------------------------------------------------    
    output$eda_corr_plot <- renderPlot({
        corrplot::corrplot(cor(data_table()), method="circle", tl.col="#3982B7",type="full")
    }, height = 600 #, width =510
    )    
    
    output$corr_clicked <- reactive({
        result <- ifelse(is.null(input$eda_corr_plot_click$x), FALSE, TRUE)
        return(result)
    })
    
    output$eda_corr_click_info <- renderPlot({
        x_column <- ifelse(is.null(input$eda_corr_plot_click$x), ncol(data_table()), round(input$eda_corr_plot_click$x))
        y_column <- ifelse(is.null(input$eda_corr_plot_click$y), 1, round(input$eda_corr_plot_click$y))
        n_columns <- ncol(data_table())+1
        df_sub <- data_table()
        
        if( x_column <= ncol(data_table() ) & y_column <= ncol(data_table()) & x_column > 0 & y_column > 0 ){
            if(colnames(df_sub)[n_columns - y_column] == "Attrition"){
                ggplot2::ggplot(df_sub, ggplot2::aes_string(colnames(df_sub)[x_column])) +
                    ggplot2::geom_density(alpha = 0.25, ggplot2::aes(fill = factor(Attrition))) +
                    ggplot2::guides(fill=ggplot2::guide_legend(title="Attrition:")) + 
                    ggplot2::theme_bw() +
                    ggplot2::theme(legend.position = "bottom") +
                    ggplot2::scale_fill_manual(values=c("#eb5e28", "#006494"))
            } else if(colnames(df_sub)[x_column] == "Attrition"){
                ggplot2::ggplot(df_sub, ggplot2::aes_string(colnames(df_sub)[n_columns-y_column])) +
                    ggplot2::geom_density(alpha = 0.25, ggplot2::aes(fill = factor(Attrition))) +
                    ggplot2::guides(fill=ggplot2::guide_legend(title="Attrition:")) + 
                    ggplot2::theme_bw() +
                    ggplot2::theme(legend.position = "bottom") +
                    ggplot2::scale_fill_manual(values=c("#eb5e28", "#006494"))
            } else if(colnames(df_sub)[x_column] != colnames(df_sub)[n_columns-y_column]) {
                x_name = colnames(df_sub)[x_column]
                y_name = colnames(df_sub)[n_columns-y_column]
                ggplot2::ggplot(df_sub, ggplot2::aes_string(x_name, y_name) ) + ggplot2::stat_bin2d() + ggplot2::theme_bw()
            } else {
                ggplot2::ggplot(df_sub, ggplot2::aes_string("factor(Attrition)", colnames(df_sub)[x_column])) +
                    ggplot2::geom_violin(alpha = 0.2, ggplot2::aes(fill = factor(Attrition))) + ggplot2::theme_bw() +
                    ggplot2::scale_fill_manual(values=c("#eb5e28", "#006494")) +
                    ggplot2::guides(fill=FALSE) + ggplot2::xlab("Attrition") +
                    ggplot2::theme(legend.position = "bottom")
            }
        }
    }, height = 320)
    
    #------------------------------------------------------------
    # EDA - Attrition
    #------------------------------------------------------------
    # Attrition Bars
    output$eda_attr_attr <- renderPlot({
        df_table() %>% group_by(Attrition) %>% summarise(Count=n()) %>%
            ggplot2::ggplot(ggplot2::aes(x=Attrition, y=Count)) + 
            ggplot2::geom_bar(stat="identity", fill="#7fc8f8") +
            ggplot2::coord_flip() + 
            
            ggplot2::scale_fill_manual(values=c("#B5EAD7", "#FF9AA2")) + 
            ggplot2::scale_color_manual(values=c("#B5EAD7","#FF9AA2")) + 
            ggplot2::geom_label(ggplot2::aes(label=Count, fill = Attrition), colour = "Black", fontface = "bold") + 
            
            ggplot2::labs(x="Employee Attrition",y="Amount") + 
            ggthemes::theme_few() +
            ggplot2::theme(legend.position="none")
        
    })
    
    # Attrition - Education Level    
    output$eda_attr_edu_level <- renderPlot({
        df_tmp <- df_table()
        
        # Give names for the different education levels.
        df_tmp$Educational_Levels <-  ifelse(df$Education == 1, "Without College D.",
                                             ifelse(df$Education == 2 , "College D.",
                                                    ifelse(df$Education == 3, "Bachelors D.",
                                                           ifelse(df$Education == 4, "Masters D.", "Phd D."))))
        
        df_tmp <- df_tmp %>% select(Educational_Levels, Attrition) %>% group_by(Educational_Levels, Attrition) %>%
            summarize(n=n())# %>%
        
        ggplot2::ggplot(df_tmp, ggplot2::aes(x=forcats::fct_reorder(Educational_Levels,n), y=n, fill=Attrition, color=Attrition)) + ggplot2::geom_bar(stat="identity") + 
            ggplot2::facet_wrap(~Attrition) + 
            ggplot2::coord_flip() + 
            ggplot2::scale_fill_manual(values=c("#B5EAD7", "#FF9AA2")) + 
            ggplot2::scale_color_manual(values=c("#B5EAD7","#FF9AA2")) + 
            ggplot2::geom_label(ggplot2::aes(label=n, fill = Attrition), colour = "Black", fontface = "bold") + 
            ggplot2::labs(x="", y="Number of Employees") + 
            ggthemes::theme_few() +
            ggplot2::theme(legend.position="none") 
    })
    
    # Attrition - Job Satisfaction    
    output$eda_attr_job <- renderPlot({
        df_tmp <- df_table()
        
        df_tmp$JobSatisfaction <-  ifelse(df$JobSatisfaction == 1, "Low",
                                          ifelse(df$JobSatisfaction == 2 , "Medium",
                                                 ifelse(df$JobSatisfaction == 3, "High",
                                                        ifelse(df$JobSatisfaction == 4, "Very High",""))))
        
        
        df_tmp %>% select(JobSatisfaction, Attrition) %>% group_by(JobSatisfaction, Attrition) %>% 
            summarize(n=n()) %>% 
            
            ggplot2::ggplot( ggplot2::aes(x=forcats::fct_reorder(JobSatisfaction, n, .desc = FALSE ), y=n, fill=Attrition, color=Attrition)) + ggplot2::geom_bar(stat="identity") + 
            ggplot2::facet_wrap(~Attrition) + 
            ggplot2::coord_flip() + 
            ggplot2::scale_fill_manual(values=c("#B5EAD7", "#FF9AA2")) + 
            ggplot2::scale_color_manual(values=c("#B5EAD7","#FF9AA2")) + 
            ggplot2::geom_label(ggplot2::aes(label=n, fill = Attrition), colour = "Black", fontface = "bold") + 
            ggplot2::labs(x="", y="Number of Employees") + 
            ggthemes::theme_few() +
            ggplot2::theme(legend.position="none") 
    })
    
    # Attrition Age
    output$eda_attr_age <- renderPlot({
        ggplot2::ggplot(df_table(), ggplot2::aes(x=Attrition, y=Age, color=Attrition, fill=Attrition)) + 
            ggplot2::geom_boxplot() + 
            ggplot2::scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            ggplot2::scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            ggthemes::theme_few() +
            ggplot2::theme(legend.position="none")
    })    
    
    # Attrition Monthly Income
    output$eda_attr_income <- renderPlot({
        ggplot2::ggplot(df_table(), ggplot2::aes(x=Attrition, y=MonthlyIncome, color=Attrition, fill=Attrition)) + 
            ggplot2::geom_boxplot() + 
            ggplot2::scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            ggplot2::scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            #coord_flip() + 
            #labs(title="Are there any Gender Disparities in Income?") +       
            ggthemes::theme_few() +
            ggplot2::theme(legend.position="none")
    })
    
    # Attrition Distance from Home
    output$eda_attr_distance <- renderPlot({
        ggplot2::ggplot(df_table(), ggplot2::aes(x=Attrition, y=DistanceFromHome, color=Attrition, fill=Attrition)) + 
            ggplot2::geom_boxplot() + 
            ggplot2::scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            ggplot2::scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            ggthemes::theme_few() +
            ggplot2::theme(legend.position="none")
    })
    
    # Attrition Years same Manager
    output$eda_attr_manager <- renderPlot({
        ggplot2::ggplot(df_table(), ggplot2::aes(x=Attrition, y=YearsWithCurrManager, color=Attrition, fill=Attrition)) + 
            ggplot2::geom_boxplot() + 
            ggplot2::scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            ggplot2::scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            ggthemes::theme_few() +
            ggplot2::theme(legend.position="none")
    })
    
    # # Monthly Gender
    # output$eda_monthly_gender <- renderPlot({
    #     ggplot(df_table(), aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + 
    #         geom_boxplot() + 
    #         scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
    #         scale_color_manual(values=c("#f7aef8", "#386fa4")) +
    #         theme_few() +
    #         theme(legend.position="none")
    # })
    # 
    # # Monthly Job satisfaction
    # output$eda_monthly_job <- renderPlot({
    #     df_tmp <- df_table()
    #     
    #     df_tmp$JobSatisfaction <-  ifelse(df$JobSatisfaction == 1, "Low",
    #                                       ifelse(df$JobSatisfaction == 2 , "Medium",
    #                                              ifelse(df$JobSatisfaction == 3, "High",
    #                                                     ifelse(df$JobSatisfaction == 4, "Very High",""))))
    #     
    #     df_tmp <- df_tmp %>% select(JobSatisfaction, MonthlyIncome, Attrition) %>% group_by(JobSatisfaction, Attrition) %>%
    #         summarize(med=median(MonthlyIncome))
    #     
    #     ggplot(df_tmp, aes(x=fct_reorder(JobSatisfaction, -med), y=med, color=Attrition)) + 
    #         geom_point(size=5) +
    #         geom_line(size=3) +
    #         geom_segment(aes(x=JobSatisfaction,
    #                          xend=JobSatisfaction,
    #                          y=0,
    #                          yend=med)) +
    #         facet_wrap(~Attrition) +
    #         labs(y="Median Income", x="Level of Job Satisfaction") +
    #         coord_flip() +
    #         scale_color_manual(values=c("#58FA58", "#FA5858")) +
    #         geom_text(aes(x=JobSatisfaction, y=0.01, label= paste0("$ ", round(med,2))),
    #                   hjust=-0.5, vjust=-0.5, size=4,
    #                   colour="black", fontface="italic",
    #                   angle=360) +
    #         theme_few()
    # })
    
    #------------------------------------------------------------
    # Model
    #------------------------------------------------------------    
    output$conf_mat_info <- renderPrint({
        caret::confusionMatrix(conf_mat,test$Attrition)
    })
    
    output$roc_dt <- renderPlot({
        pROC::roc(response = test$Attrition, predictor = predict_dt,plot=TRUE, legacy.axes = TRUE, percent = TRUE, xlab= "False Positive Perc.", ylab= "True Positive Perc.",
                  col="#377EB8",lwd=4, print.auc=TRUE)
    })
    output$conf_mat_info_dt <- renderPrint({
        caret::confusionMatrix(conf_mat_dt,test$Attrition)
    })
    output$imp_var_dt <- renderPlot({
        ggplot2::ggplot(feature_importance, ggplot2::aes(x=reorder(features, importance), y=importance, fill=features)) + 
            ggplot2::geom_bar(stat='identity') + 
            ggplot2::coord_flip() + 
            ggplot2::geom_label(ggplot2::aes(label=paste0(importance, "%")), colour = "white", fontface = "italic", hjust=0.6) + 
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position="none")
    })
    
    output$roc_rf <- renderPlot({
        pROC::roc(response = test$Attrition, predictor = predict_rf,plot=TRUE, legacy.axes = TRUE, percent = TRUE, xlab= "False Positive Perc.", ylab= "True Positive Perc.",
                  col="#377EB8",lwd=4, print.auc=TRUE)
    })    
    output$conf_mat_info_rf <- renderPrint({
        caret::confusionMatrix(conf_mat_rf,test$Attrition)
    })
    output$imp_var_rf <- renderPlot({
        randomForest::varImpPlot(model_rf)
    })
    
    #------------------------------------------------------------
    # Predict
    #------------------------------------------------------------
    pred_case <- reactive({
        Age                   = as.integer(input$i_age)
        BusinessTravel        = as.factor(input$i_travel)
        Department            = as.factor(input$i_department)
        DistanceFromHome      = as.integer(input$i_distance)
        Education             = as.integer(input$i_education)
        EnvironmentSatisfaction = as.integer(input$i_env_satisfaction)
        Gender                = as.factor(input$i_gender)
        JobRole               = as.factor(input$i_role)
        JobSatisfaction       = as.integer(input$i_job_satisfaction)
        MonthlyIncome         = as.integer(input$i_monthly_income)
        NumCompaniesWorked    = as.integer(input$i_companyworked)
        OverTime              = as.factor(input$i_overtime)
        StockOptionLevel      = as.integer(input$i_stock)
        TotalWorkingYears     = as.integer(input$i_totalyworked)
        TrainingTimesLastYear = as.integer(input$i_training)
        YearsAtCompany        = as.integer(input$i_years_company)
        YearsInCurrentRole    = as.integer(input$i_years_role)
        YearsWithCurrManager  = as.integer(input$i_years_manager)
        test <- data.frame(Age,               
                           BusinessTravel,
                           Department,            
                           DistanceFromHome,
                           Education,     
                           EnvironmentSatisfaction,
                           Gender,
                           JobRole,   
                           JobSatisfaction,
                           MonthlyIncome,
                           NumCompaniesWorked,
                           OverTime,     
                           StockOptionLevel,
                           TotalWorkingYears,     
                           TrainingTimesLastYear,
                           YearsAtCompany,
                           YearsInCurrentRole,
                           YearsWithCurrManager)
        test
    })
    
    pred_result_prob <-  eventReactive(input$predict, {
        pred <- predict(model_dt, newdata = pred_case(), type = "prob")
    })
    
    pred_result_class <-  eventReactive(input$predict, {
        pred <- predict(model_dt, newdata = pred_case(), type = "class")
    })
    
    output$case <- renderText({
        text <- paste(" Age:", pred_case()[1,1], "\n",
                      "Business Travel:", pred_case()[1,2],"\n",
                      "Department:", pred_case()[1,3],"\n",
                      "Distance From Home:", pred_case()[1,4],"\n",
                      "Education:", pred_case()[1,5],"\n",
                      "Enviroment Satisfaction:", pred_case()[1,6],"\n",
                      "Gender:", pred_case()[1,7],"\n",
                      "Job Role:", pred_case()[1,8],"\n",
                      "Job Satisfaction", pred_case()[1,9],"\n",
                      "Monthly Income:", pred_case()[1,10],"\n",
                      "No Company Worked:", pred_case()[1,11],"\n",
                      "Overtime:", pred_case()[1,12],"\n",
                      "Stock Option Level:", pred_case()[1,13],"\n",
                      "Total Working Years:", pred_case()[1,14],"\n",
                      "Training in Last Year:", pred_case()[1,15],"\n",
                      "Years at Company:", pred_case()[1,16],"\n",
                      "Years in Current Role:", pred_case()[1,17],"\n",
                      "Years with Current Manager:", pred_case()[1,18],"\n"
        )
    })    
    
    output$PredictBox <- shinydashboard::renderValueBox({
        if(pred_result_class()=="No"){
            shinydashboard::valueBox(
                pred_result_class(), pred_result_prob()[pred_result_class()], icon = icon("thumbs-up", lib = "glyphicon"),color = "green"
            )
        }
        else{
            shinydashboard::valueBox(
                pred_result_class(), pred_result_prob()[pred_result_class()], icon = icon("thumbs-down", lib = "glyphicon"),color = "red"
            )
        }
    })
    #------------------------------------------------------------
    # Data
    #------------------------------------------------------------
    output$original_datos = DT::renderDataTable({
        df_num
    })
    
    output$original_datos2 = DT::renderDT(
        data_table(), options = list(autoWidth = TRUE,scrollX=TRUE)
    )
    
    outputOptions(output, "corr_clicked", suspendWhenHidden = FALSE)    
}
)

# Run the application 
shinyApp(ui = ui, server = server)
