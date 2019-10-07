require(shiny)
require(caret)
require(ROCR)
require(glmnet)
require(rowr)

## Lines from Processing-Cleaning-Learning.R have the line number at the end.

## Prepare environment
prepare_data_frame <- function(data_source) {
    HDMA_select_imputed <- read.csv(data_source, header = TRUE, na.strings = c("NA","","#NA"))
    HDMA_select_imputed <- HDMA_select_imputed[2:17] # Omit the first column (X)
    HDMA_select_imputed$TARGET_VALUE =  as.factor(HDMA_select_imputed$TARGET_VALUE) #495
    return(HDMA_select_imputed)
}

## Generate the model ## wrong model!
# generate_cv_model <- function(data_source, model_filename='glmnet_fit.model) {
#     data_frame <- prepare_data_frame(data_source)
#     classes <- data_frame[, "TARGET_VALUE"] #511
#     train_set <- createDataPartition(classes, p = 0.75, list = FALSE) #517
#     data_frame_train <- data_frame[train_set, ] #533
#     glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), #536
#                                 lambda = seq(.01, .2, length = 20))
#     glmnet_ctrl <- trainControl(method = "cv", number = 10) #538
#     #######
#     #SLOW, give it a few mins: glmnet_fit
#     glmnet_fit <- train(TARGET_VALUE ~ ., data = data_frame_train, #539
#                         method = "glmnet",
#                         preProcess = c("center", "scale"),
#                         tuneGrid = glmnet_grid,
#                         trControl = glmnet_ctrl)
#     saveRDS(glmnet_fit, model_filename)
# }

generate_model <- function(data_source, model_filename='logit.model') {
    data_frame <- prepare_data_frame(data_source)
    set.seed(102)
    train_set <- data_frame$TARGET_VALUE %>%
        createDataPartition(p = 0.75, list = FALSE)
    data_frame_train  <- data_frame[train_set ,]
    #######
    #SLOW, give it a few mins: logit
    logit <- glm(formula =TARGET_VALUE ~tract_to_msamd_income +
                     population +
                     minority_population +
                     number_of_1_to_4_family_units +
                     loan_amount_000s +
                     hud_median_family_income +
                     applicant_income_000s +
                     loan_type_name +
                     loan_purpose_name,
                 family=binomial(link='logit'),
                 data=data_frame_train,
                 maxit = 100)
    saveRDS(logit, model_filename)
}

## Generate the values for the UI non-numeric inputs
generate_select_values <- function(data_source, values_filename) {
    data_frame <- prepare_data_frame(data_source)
    values <- cbind.fill(fill=NA,
        data.frame('property_type_name' = unique(data_frame$property_type_name)),
        data.frame('loan_type_name' = unique(data_frame$loan_type_name)),
        data.frame('loan_purpose_name' = unique(data_frame$loan_purpose_name)),
        data.frame('hoepa_status_name' = unique(data_frame$hoepa_status_name)),
        data.frame('applicant_sex_name' = unique(data_frame$applicant_sex_name)),
        data.frame('applicant_race_name_1' = unique(data_frame$applicant_race_name_1)),
        data.frame('applicant_ethnicity_name' = unique(data_frame$applicant_ethnicity_name))
        )
    saveRDS(values, values_filename)
}


## Define UI for application
## Generate/Load values for each of the non-numeric inputs
# generate_select_values('../data/Working/HDMA_select_imputed.csv', 'select_values.csv')
select_values <- readRDS('select_values.csv') # load a pre-generated values
ui <- fluidPage(
    titlePanel("CSML1000(6) - Government Assistance Application"),
    fluidRow(
        column("Numeric Input", width=4,
            numericInput("tract_to_msamd_income",
                         "Percentage of median family for the census tract:",
                         min = 1,
                         max = 10000,
                         step = 1,
                         value = 0
            ),
            numericInput("population",
                         "Percentage of minority population for the census tract:",
                         min = 1,
                         max = 100000,
                         step = 1,
                         value = 0
            ),
            numericInput("minority_population",
                         "Percentage of minority population for the census tract:",
                         min = 1,
                         max = 10000,
                         step = 1,
                         value = 0
            ),
            numericInput("number_of_owner_occupied_units",
                         "Number of dwellings in the census tract:",
                         min = 1,
                         max = 10000,
                         step = 1,
                         value = 0
            ),
            numericInput("number_of_1_to_4_family_units",
                         "Number of dwellings in the census tract for families of 5 or less:",
                         min = 1,
                         max = 10000,
                         step = 1,
                         value = 0
            ),
            numericInput("loan_amount_000s",
                         "Amount of the loan applied for, in thousands:",
                         min = 1,
                         max = 10000,
                         step = 1,
                         value = 0
            ),
            numericInput("hud_median_family_income",
                         "Median family income in dollars for the Metropolitan Division:",
                         min = 1,
                         max = 10000,
                         step = 1,
                         value = 0
            ),
            numericInput("applicant_income_000s",
                         "Gross anual income of the applicant:",
                         min = 1,
                         max = 10000,
                         step = 1,
                         value = 0
            )
        ),
        column("Categorical input", width = 4,
            selectInput("property_type_name",
                        "Type of the property:",
                        na.omit(select_values['property_type_name'])
            ),
            selectInput("loan_type_name",
                        "Type of the property:",
                        na.omit(select_values['loan_type_name'])
            ),
            selectInput("loan_purpose_name",
                        "The purpose of the loan:",
                        na.omit(select_values["loan_purpose_name"])
            ),
            selectInput("hoepa_status_name",
                        "HOEPA status of the application:",
                        na.omit(select_values["hoepa_status_name"])
            ),
            selectInput("applicant_sex_name",
                        "Sex of the primary applicant:",
                        na.omit(select_values["applicant_sex_name"])
            ),
            selectInput("applicant_race_name_1",
                        "Race for the primary applicant:",
                        na.omit(select_values["applicant_race_name_1"])
            ),
            selectInput("applicant_ethnicity_name",
                        "The ethnicity of the primary applicant:",
                        na.omit(select_values["applicant_ethnicity_name"])
            )
            
        ),
    column(width = 4,
           mainPanel(
               h4("Result:"),
               textOutput("is_candidate")
               )
           )
    )
)

## Define server logic required to determine if the applicant is a candidate
## Generate/Load the model
#generate_model('../data/Working/HDMA_select_imputed.csv', 'logit.model')
logit <- readRDS('logit.model') # load a pre-generated model
server <- function(input, output) {
    output$is_candidate <- renderText({
        # Build a new data frame with data from the form
        this_data <- data.frame('tract_to_msamd_income' = input$tract_to_msamd_income,
                                'population' = input$population,
                                'minority_population' = input$minority_population,
                                'number_of_owner_occupied_units' = input$number_of_owner_occupied_units,
                                'number_of_1_to_4_family_units' = input$number_of_1_to_4_family_units,
                                'loan_amount_000s' = input$loan_amount_000s,
                                'hud_median_family_income' = input$hud_median_family_income,
                                'applicant_income_000s' = input$applicant_income_000s,
                                'property_type_name' = input$property_type_name,
                                'loan_type_name' = input$loan_type_name,
                                'loan_purpose_name' = input$loan_purpose_name,
                                'hoepa_status_name' = input$hoepa_status_name,
                                'applicant_sex_name' = input$applicant_sex_name,
                                'applicant_race_name_1' = input$applicant_race_name_1,
                                'applicant_ethnicity_name' = input$applicant_ethnicity_name)
        # Use the new data frame for the prediction
        pred_probs <- predict(logit, newdata = this_data, type = "response") #554
        result <- "Not Candidate" # Default result
        if (pred_probs>0.5) {
            result <- "Candidate"
        }
        return(result)
    })
}

## Run the application 
shinyApp(ui = ui, server = server)

## Deploy App
#rsconnect::deployApp('.')


# TEST input
# this_data <- data.frame('tract_to_msamd_income' = 90.48,
#                         'population' = 4046,
#                         'minority_population' = 5.61,
#                         'number_of_owner_occupied_units' = 1318,
#                         'number_of_1_to_4_family_units' = 1526,
#                         'loan_amount_000s' = 174,
#                         'hud_median_family_income' = 61800,
#                         'applicant_income_000s' = 166,
#                         'property_type_name' = 'One-to-four family dwelling (other than manufactured housing)',
#                         'loan_type_name' = 'Conventional',
#                         'loan_purpose_name' = 'Refinancing',
#                         'hoepa_status_name' = 'Not a HOEPA loan',
#                         'applicant_sex_name' = 'Male',
#                         'applicant_race_name_1' = 'White',
#                         'applicant_ethnicity_name' = 'Not Hispanic or Latino')
# # TEST computation
# pred_probs <- predict(logit, newdata = this_data, type = "response") #554
# print(pred_probs) # check if prediction is correct

#python helper
#print(',\n'.join(list('\'{0}\' = input${0}'.format(colname) for line in ((colname[colname.find(']')+2:]).replace('"','').split() for colname in colnames.split('\n')) for colname in line)[:-1]))