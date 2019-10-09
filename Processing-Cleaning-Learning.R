library(readr)
library(mice)
library(dplyr)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(caret)
library(glmnet)
library(MASS)
library(car)
library(rpart)
library(bestglm)
library(VIM)
library(scales)
library(RColorBrewer)
##################################################################################
###########################   DATA PROCESSING #####################################
# Adding the variables “TARGET_VALUE”(numerical) and “TARGET_LABEL”(string) to the
# HDMA dataset
##################################################################################
#setwd()

HDMA_dataset <- read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))

attach(HDMA_dataset)
summary(HDMA_dataset)

HDMA_dataset = cbind(HDMA_dataset, TARGET_LABEL= ifelse(
    HDMA_dataset$action_taken_name == "Application denied by financial institution" |
    HDMA_dataset$action_taken_name == "Preapproval request denied by financial institution",
    "CANDIDATE", "NOT_CANDIDATE" ))
summary(HDMA_dataset$TARGET_LABEL)
HDMA_dataset = cbind(HDMA_dataset, TARGET_VALUE= ifelse(
    HDMA_dataset$TARGET_LABEL == "CANDIDATE" , 1, 0 ))
str(HDMA_dataset$TARGET_VALUE)

summary(HDMA_dataset$TARGET_LABEL)

#write.csv(HDMA_dataset, "HDMA_Processed.csv")

HDMA_Processed <- HDMA_dataset

str(HDMA_Processed)
################################################################################
###########################   Data  Exploration  1 #############################
################################################################################




#############  Loan Application Status in Washinton   #####################
require(dplyr)
detach("package:Hmisc", unload = TRUE)

Loan_status <- HDMA_Processed %>%
  group_by(action_taken_name) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
head(Loan_status)
#Pie plot
require(ggplot2)
ggplot(Loan_status, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(action_taken_name,count))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "white")+
  ggtitle("Loan Application Status in Washinton")+
  #scale_fill_brewer("Loan Status") + theme_void()
  scale_fill_grey(start = 0.8, end = 0.2,"Loan Status") + theme_void()

############# Applicant  Status in Washinton   #####################
Canditate_status <- HDMA_Processed %>%
  group_by(TARGET_LABEL) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
head(Canditate_status)

#Pie plot
ggplot(Canditate_status, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(TARGET_LABEL,count))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
  ggtitle(" Status of Loan Applicant in Washinton")+
  scale_fill_grey(start = 0.8, end = 0.2,"Applicant Status") + theme_void()

################## Loan Purpose  Vs TARGET_LABEL ########################
# Create contigency table
#Loan_purpose = table(HDMA_Processed$loan_purpose_name,HDMA_Processed$TARGET_LABEL)
#head(Loan_purpose)
#freq_tbl = table(HDMA_Processed$loan_purpose_name)
#round(prop.table(freq_tbl),2)

# Creating crosstabs for categorical variable
Loan_purpose_xtab = xtabs(~HDMA_Processed$loan_purpose_name+HDMA_Processed$TARGET_LABEL)
head(Loan_purpose_xtab)
round(prop.table(Loan_purpose_xtab),2)
#display.brewer.all()
barplot(prop.table(Loan_purpose_xtab,2),
        legend = rownames(Loan_purpose_xtab), beside = T,
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal(3, name = "Dark2"),
        main = "Difference in Target Variable for different Loan purpose ")
require(RColorBrewer)
barplot(prop.table(Loan_purpose_xtab,2),
        legend = rownames(Loan_purpose_xtab),
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal(3, name = "Dark2"),
        main = "Difference in Target Variable for different Loan purpose ")


################## Loan Type  Vs TARGET_LABEL (Applicant Status) ########################
# Creating crosstabs for categorical variable
Loan_type_xtab = xtabs(~HDMA_Processed$loan_type_name+HDMA_Processed$TARGET_LABEL)
head(Loan_type_xtab)
round(prop.table(Loan_type_xtab),2)
#display.brewer.all()
barplot(prop.table(Loan_type_xtab,2),
        legend = rownames(Loan_type_xtab),
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal( 4, name = "Dark2"),
        main = "Difference in Target Variable for different Loan Type ")


################## Applicant Gender  Vs TARGET_LABEL ########################
# Pie Chart for the total population of applicant gender (with percentages) against the entire dataset
applicant_sex_population <- table(HDMA_Processed$applicant_sex_name)
lbls <- paste(names(applicant_sex_population), "\n", slides, sep="")
pct <- round(applicant_sex_population/sum(applicant_sex_population)*100)
lbls <- paste(lbls, "|")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(applicant_sex_population,labels=lbls, radius=0.75, main="Pie Chart of Applicant Gender ")



# Creating crosstabs for categorical variable
Applicant_sex_xtab = xtabs(~HDMA_Processed$applicant_sex_name+HDMA_Processed$TARGET_LABEL)
head(Applicant_sex_xtab)
round(prop.table(Applicant_sex_xtab),2)
#display.brewer.all()
barplot(prop.table(Applicant_sex_xtab,2),
        legend = c("Female", "not provided", "Male", "Not applicable" ) ,
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal( 4, name = "Dark2"),
        main = "Difference in Target Variable for Applicant Sex ")


################## Applicant Gender  Vs TARGET_LABEL ########################
# Creating crosstabs for categorical variable

Applicant_sex_xtab = xtabs(~HDMA_Processed$applicant_sex_name+HDMA_Processed$TARGET_LABEL)
head(Applicant_sex_xtab)
round(prop.table(Applicant_sex_xtab),2)
#display.brewer.all()
barplot(prop.table(Applicant_sex_xtab,2),
        legend = c("Female", "Not provided", "Male", "Not applicable" ) ,
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal( 4, name = "Dark2"),
        main = "Difference in Target Variable for Applicant Sex ")


################## Applicant Ethnicity  Vs TARGET_LABEL ########################
# Creating crosstabs for categorical variable

Applicant_ethni_xtab = xtabs(~HDMA_Processed$applicant_ethnicity_name+HDMA_Processed$TARGET_LABEL)
head(Applicant_ethni_xtab)
round(prop.table(Applicant_ethni_xtab),2)
#display.brewer.all()
barplot(prop.table(Applicant_sex_xtab,2),
        legend = c("Hispanic or Latino", "Not provided", "Not applicable", "Not Hispanic or Latino" ) ,
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal( 4, name = "Dark2"),
        main = "Difference in Target Variable for Applicant Ethni ")



################## Applicant Race  Vs TARGET_LABEL ########################
# Creating crosstabs for categorical variable

Applicant_race_xtab = xtabs(~HDMA_Processed$applicant_race_name_1+HDMA_Processed$TARGET_LABEL)
head(Applicant_race_xtab)
round(prop.table(Applicant_race_xtab),2)
#display.brewer.all()
barplot(prop.table(Applicant_race_xtab,2),
        legend = c("American Indian or Alaska Native ", "Asian", "Black or African American"," Not provided", "Native Hawaiian or Other Pacific Islander","Not applicable", "White" ) ,
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal( 7, name = "Dark2"),
        main = "Difference in Target Variable for Applicant Race ")

################## Lien Status  Vs TARGET_LABEL ########################
# Creating crosstabs for categorical variable

Lien_status_xtab = xtabs(~HDMA_Processed$lien_status_name+HDMA_Processed$TARGET_LABEL)
head(Lien_status_xtab)
round(prop.table(Lien_status_xtab),2)
#display.brewer.all()
barplot(prop.table(Lien_status_xtab,2),
        legend = rownames(Lien_status_xtab) ,
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal( 4, name = "Dark2"),
        main = "Difference in Target Variable for Lien Status ")



################## HOEPA Status  Vs TARGET_LABEL ########################
# Creating crosstabs for categorical variable

HOEPA_status_xtab = xtabs(~HDMA_Processed$hoepa_status_name+HDMA_Processed$TARGET_LABEL)
head(HOEPA_status_xtab)
round(prop.table(HOEPA_status_xtab),2)
#display.brewer.all()
barplot(prop.table(HOEPA_status_xtab,2),
        legend = rownames(HOEPA_status_xtab) ,
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal( 2, name = "Dark2"),
        main = "Difference in Target Variable for Lien Status ")





################## Applicant CANDIDATE from different county in Washington   ########################
#County_name <- data.frame(HDMA_Processed$county_name,HDMA_Processed$TARGET_LABEL)
#head(County_name)


 require(dplyr)
County_name_of_Candidate <- HDMA_Processed %>%
  filter(TARGET_LABEL == "CANDIDATE")%>%
  group_by(county_name) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


ggplot(County_name_of_Candidate, aes(x=reorder(county_name,count), y=count )) +
  geom_bar(stat="identity")+
  #ggtitle("") +
  #xlab("") + ylab("")
  labs(title = "County of Applicants CANDIDATE", x ="County name" , y =  "Nber of CANDIDATE")+
  coord_flip()+  scale_fill_brewer(palette="Greys")+
  theme_minimal()





#require(dplyr))
County_name_of_NotCandidate <- HDMA_Processed %>%
  filter(TARGET_LABEL == "NOT_CANDIDATE")%>%
  group_by(county_name) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

head(County_name_of_NotCandidate)

County <- County_name_of_Candidate %>%
  mutate(NOT_CANDIDATE=County_name_of_NotCandidate$count)%>%
  rename(CANDIDATE=count)
head(County)



################## Correlation Matrix   ########################
# find out if application income and loan amount played a role in target
#1 extract column usung ibrary(dplyr)
#newdata1  <- HDMA_Processed %>%
#  group_by(TARGET_LABEL) %>%
 # select(applicant_income_000s, loan_amount_000s,action_taken_name)
#describeBy(newdata1, TARGET_LABEL)#library(psych)
#describe(HDMA_Processed$applicant_income_000s,HDMA_Processed$loan_amount_000s) #library(Hmisc)





##########################################
nrow(HDMA_Processed)
data_for_lr <- subset(HDMA_Processed,select=c(8,9,48))

#### CLEAN DATA should be performed to assist with model training
#train <- data_for_lr[1:349925,]
#test <- data_for_lr[349926:466566,]

train <- data_for_lr[300000:350000,]
test <- data_for_lr[350001:375000,]
#test <- data_for_lr[300000:350000,]


target_population_train <- table(train$TARGET_LABEL)
lbls <- paste(names(target_population_train), "\n", target_population_train, sep="")
pie(target_population_train,labels=lbls, radius=0.75, main="Target labels within Train Data")

target_population_test <- table(test$TARGET_LABEL)
lbls <- paste(names(target_population_test), "\n", target_population_test, sep="")
pie(target_population_test,labels=lbls, radius=0.75, main="Target labels within Test Data")

model <- glm(TARGET_LABEL ~., family="binomial", data=train)
summary(model)

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$TARGET_VALUE)
print(paste('Accuracy',1-misClasificError))
table(fitted.results)
fitted.results





################################################################################
######################    MISSING VALUES     ##############################
################################################################################

HDMA_Processed <- read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))
str(HDMA_Processed)

#Check the data for missing values.
sapply(HDMA_Processed, function(x) sum(is.na(x)))



# Select Interesting variables
NumColumn  <- c("tract_to_msamd_income", "population", "minority_population",
                 "number_of_owner_occupied_units", "number_of_1_to_4_family_units",
                 "loan_amount_000s","hud_median_family_income","applicant_income_000s")
#
CategoColum = c("property_type_name","loan_type_name","loan_purpose_name","hoepa_status_name",
                "applicant_sex_name","applicant_race_name_1","applicant_ethnicity_name")

WorkingColumn= c(NumColumn,CategoColum,"TARGET_VALUE","TARGET_LABEL")
#data <- subset(data, select = -c(TARGET_VALUE))
HDMA_select_data = subset(HDMA_Processed,select=WorkingColumn )

head(HDMA_select_data)
#Check the data for missing values.
sapply(HDMA_select_data, function(x) sum(is.na(x)))

#=============================
# set Categorical varibla as factor
require(dplyr)
HDMA_select_data <- HDMA_select_data %>%
  mutate(
    TARGET_VALUE  = as.factor(TARGET_VALUE),
    TARGET_LABEL  = as.factor(TARGET_LABEL),
    property_type_name = as.factor(property_type_name),
    loan_type_name = as.factor(loan_type_name),
    loan_purpose_name =  as.factor(loan_purpose_name),
    hoepa_status_name =  as.factor(hoepa_status_name),
    applicant_sex_name =  as.factor(applicant_sex_name),
    applicant_race_name_1 =  as.factor(applicant_race_name_1),
    applicant_ethnicity_name = as.factor( applicant_ethnicity_name)
  )

#Look the dataset structure.
str(HDMA_select_data)




#============ Visualize missing Data =======================================
#Calculates every unique combination of missing data & shows of times that happens
HDMA_select_num_subset  <- subset(HDMA_select_imputed, select = c(NumColumn) )
md.pattern(HDMA_select_num_subset)
md.pattern(HDMA_select_num_subset,rotate.names = TRUE)


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#==============================================================================
require(mice)
init = mice(HDMA_select_data, maxit=0)
meth = init$method
predM = init$predictorMatrix

##remove the variable as a predictor but still will be imputed. Just for illustration purposes,
##I select the "TARGET_VALUE" variable to not be included as predictor during imputation.
predM[, c(CategoColum)]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""
meth[c("TARGET_VALUE")]=""
meth[c("property_type_name")]=""
meth[c("loan_type_name")]=""
meth[c("loan_purpose_name")]=""
meth[c("hoepa_status_name")]=""
meth[c("applicant_sex_name")]=""
meth[c("applicant_race_name_1")]=""
meth[c("applicant_ethnicity_name")]=""
#++++++++++++++++++++++++++++++++++

##Now let specify the methods for imputing the missing values.
## we impute only the Numerical Variable
meth[c("tract_to_msamd_income")]="pmm"
meth[c("population")]="pmm"
meth[c("minority_population")]="pmm"
meth[c("number_of_owner_occupied_units")]="pmm"
meth[c("number_of_1_to_4_family_units")]="pmm"
meth[c("loan_amount_000s")]="pmm"
meth[c("hud_median_family_income")]="pmm"
meth[c("applicant_income_000s")]="pmm"
#meth[c("property_type_name")]="norm"
#meth[c("loan_type_name")]="logreg"
#meth[c("loan_purpose_name")]="polyreg"

set.seed(103)
 imputed = mice(HDMA_select_data, method=meth, predictorMatrix=predM, m=5)

#Create a dataset after imputation.
HDMA_select_imputed<- complete(imputed)

#Check for missings in the imputed dataset.
sapply(HDMA_select_imputed, function(x) sum(is.na(x)))

HDMA_Processed_imputed <-  HDMA_Processed%>%
  mutate(
      tract_to_msamd_incomem = HDMA_select_imputed$tract_to_msamd_incomem,
      population = HDMA_select_imputed$population,
      minority_population = HDMA_select_imputed$minority_population,
      number_of_owner_occupied_units = HDMA_select_imputed$number_of_owner_occupied_units,
      number_of_1_to_4_family_units = HDMA_select_imputed$number_of_1_to_4_family_units,
      loan_amount_000s = HDMA_select_imputed$loan_amount_000s,
      hud_median_family_income = HDMA_select_imputed$hud_median_family_income,
      applicant_income_000s = HDMA_select_imputed$applicant_income_000s,
  )


write.csv(HDMA_Processed_imputed , "HDMA_Processed_imputed.csv")


#==============================Accuracy=======================================
#### Var1
#actual <- original$Var1[is.na(dat$Var1)] #
#predicted <- imputed$Var1l[is.na(dat$Var1)]
##### Var2
#actual <- original$Var2[is.na(dat$Var2)]
#predicted <- imputed$Var2[is.na(dat$Var2)]

#table(actuals)
#table(predicted)
#mean(actual)
#mean(predicted)
#================================================================================


##################################################################################
###########################   Data  Exploration  2 ###############################
###########################  Correlation Matrix    ###############################
##################################################################################
#HDMA_select_imputed  <- read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))



HDMA_select_imputed <- HDMA_select_imputed%>%
  mutate(TARGET_LABEL = HDMA_Processed$TARGET_LABEL)


str(HDMA_select_imputed)
#================= Select numerical variables ==================================

NumColumn  <- c("tract_to_msamd_income", "population", "minority_population",
                 "number_of_owner_occupied_units", "number_of_1_to_4_family_units",
                 "loan_amount_000s","hud_median_family_income","applicant_income_000s")



HDMA_select_num_subset  <- subset(HDMA_select_imputed, select = c(NumColumn, "TARGET_LABEL", "TARGET_VALUE") )

#Correlation Matrix - default one in R
cor(subset(HDMA_select_num_subset,select = -c(TARGET_LABEL,TARGET_VALUE)))
#correlation matrix with statistical significance
require(Hmisc)
cor_result=rcorr(as.matrix(subset(HDMA_select_num_subset,select = -c(TARGET_LABEL,TARGET_VALUE))))

require(corrplot)
#The function corrplot() takes the correlation matrix as the first argument.
#The second argument (type="upper")
#is used to display only the upper triangular of the correlation matrix.
corrplot(cor_result$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)

#======================== Observed Correlations=================================
#population and number_of_owner_occupied_units
#population and number_of_1_to_4_family_units
#number_of_owner_occupied_units and number_of_1_to_4_family_units
#===============================================================================




################################################################################
#################################### LOGIT REGRESSION I ########################
################################################################################
#HDMA_select_imputed  <- read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))
#HDMA_select_num_subset  <- subset(HDMA_select_imputed, select = c(NumColumn,"TARGET_VALUE") )




HDMA_select_num_subset$TARGET_VALUE =  as.factor(HDMA_select_num_subset$TARGET_VALUE)
HDMA_select_num_subset$TARGET_LABEL =  as.factor(HDMA_select_num_subset$TARGET_LABEL)

str(HDMA_select_num_subset)

#============================ I - (Numeric Variable) ===========================

#HDMA_select_num_subset  <- subset(HDMA_select_imputed, select = c(NumColumn) )

##========================    Data Partition  ==================================
require(caret)
set.seed(101)
require(dplyr)
training1 <- HDMA_select_num_subset$TARGET_VALUE %>%
  createDataPartition(p = 0.75, list = FALSE)
train1  <- HDMA_select_num_subset[training1 ,]
test1 <- HDMA_select_num_subset[-training1, ]


##========================     Estimating logit model    ================================
#mylogit <- glm(TARGET_VALUE ~ loan_amount_000s + hud_median_family_income + applicant_income_000s, data = train, family = "binomial")

#We will use function glm() (part of base R) to estimate a logit model. 
#GLM stands for generalized linear model. Its first argument is a formula in the 
#Y ~ X1 + X2 format where Y is the variable we try to predict (the dependent variable) 
#and the X1, X1 etc are the predictor (independent) variables. 
#The second argument specifies the data and in the third argument we specify we want 
#a “binomial” model which tells gml() to fit a logistic function to the data.
logit1 <- glm(formula =TARGET_VALUE ~tract_to_msamd_income +
           population                        +
           minority_population               +
           number_of_owner_occupied_units    +
           number_of_1_to_4_family_units     +
           loan_amount_000s                  +
           hud_median_family_income          +
           applicant_income_000s,family=binomial(link='logit'),data=train1, maxit = 100)

target_population_train1 <- table(train1$TARGET_LABEL)
lbls1 <- paste(names(target_population_train1), "\n", target_population_train1, sep="")
pie(target_population_train1,labels=lbls1, radius=0.75, main="Target labels within Train Data")



logit2<-glm(TARGET_VALUE~tract_to_msamd_income +
           population                        +
           number_of_owner_occupied_units    +
           number_of_1_to_4_family_units     +
           loan_amount_000s                  +
           hud_median_family_income          +
           applicant_income_000s, data=train1,
          family=binomial(link='logit'))

#step funciotn in R .. gives the same model
#step(out2, k=log(nrow(train1)))

require(bestglm)
bestglm(train1, IC="BIC", family=binomial(link='logit'))
outbest <- bestglm(train1, IC="BICq", t=0.25, family=binomial(link='logit'))


summary(logit1)
summary(logit2)

#The output includes summary of deviance residuals 
#and  Akaike information criterion (AIC) which are both measures of fit.
#it also includes coefficients associated with each independent variable and the intercept.
#It tells us that a one point increase in fico score raises the log of odds ratio by 0.01. 
#Log of odds is rather hard to interpret, therefore, we often take the exponential of the coefficients.


round(exp(coef(logit1)),3)
round(exp(coef(logit2)),3)

#The exponential of the slope coefficient tells us the factor by which 
#odds increase if the independent variable increases by one. 
# So, if population  increases by 1 point while the rest remain constant, 
#the odds ratio  being candidate  increases by factor of 0.999 which means odds increase 0.999 percent. 

#It is important to keep in mind that with more than one independent variable,
#the interpretation of the coefficient on an independent variables is the effect of that independent variable holding 
#all other independent variables constant. 
#For example, holding all the variable constant except loan_amount, 
#the effect of raising this later  score by 1 point raises the odds of been cadidate by 1.00 percent.

ans1<-vif(logit1)
barplot(ans1, col = "blue", ylab = "VIF", ylim =c(0,20),las=2, space=1,
        main = "VIF  logit1 ")
abline(h=10, col="red", lwd=2)



names.arg=c("3 Gears", "4 Gears", "5 Gears"))

ans2<-vif(logit2)
barplot(ans2, col = "blue", ylab = "VIF", ylim =c(0,20),las=2, space=1,
        main = "VIF  logit2 ")
abline(h=10, col="red", lwd=2)

#We can use the confint function to obtain confidence intervals for the coefficient estimates.
selectedModel1 = logit1
selectedModel2 = logit2

## CIs using profiled log-likelihood
#confint(selectedModel)
## CIs using standard errors
confint.default(selectedModel1)
confint.default(selectedModel2)

##========================     TESTING DATA     ================================


target_population_test1 <- table(test1$TARGET_LABEL)
lbls1 <- paste(names(target_population_test1), "\n", target_population_test1, sep="")
pie(target_population_test1,labels=lbls1, radius=0.75, main="Target labels within Test Data")



#=============================   logit1   ======================================
fitted1 <- predict(logit1,test1,type='response')
#Our decision boundary will be 0.5. If P(y=1|X) > 0.5 then y = 1 otherwise y=0.
fitted1 <- ifelse(fitted1 > 0.5,1,0)

fitted11 = as.factor(fitted1)
str(fitted1)
str(test1$TARGET_VALUE)

misClasificError1 <- mean(fitted1 != test1$TARGET_VALUE)
Accuracy_logit1 = 1-misClasificError1
print(paste('Accuracy', round(Accuracy_logit1,2)))
predicted_1_factor <- factor(fitted1, levels=levels(test1$TARGET_VALUE))
str(fitted1)
confusionMatrix(predicted_1_factor,test1$TARGET_VALUE)







#The 0.86 accuracy on the test set is quite a good result.
#However, keep in mind   we chose only one random data split
# For a for a more precise score we should  run some kind of cross validation
#such as k-fold cross validation.

#====Receiver Operating Characteristic (ROC) curve and the Area under the curve (AUC)
#The ROC curve compares the rank of prediction and answer.
#We set the cut off for classifying a person as candidat at 0.50 probability. 
#This gave us sensitivity (probability of detecting NOT_CANDIDAT among NOT_CANDIDAT) of 1.00, 
#and specificity (probability of detecting CANDIDAT among CANDIDAT) of 0.00.

require(ROCR)
#p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
p1 <- predict(logit1, test1, type="response")
pr1 <- prediction(p1, test1$TARGET_VALUE)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1,main = "ROC Curve logit1 ")
#plot(prf1,ylab = "Sensitivity", xlab = "Specificity",
#main = "ROC Curve logit1 ")

# area under the curve (AUC)which are typical performance measurements
# for a binary classifier. AUC closer to 1 (1 is ideal) than to 0.5.
auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
print(paste('AUC logit1', round(auc1,2)))


#=============================   logit2   ======================================
fitted2 <- predict(logit2,test1,type='response')
fitted2 <- ifelse(fitted2 > 0.5,1,0)

misClasificError2 <- mean(fitted2 != test1$TARGET_VALUE)
Accuracy_logit2 = 1-misClasificError2
print(paste('Accuracy', round(Accuracy_logit2,2)))

predicted_2_factor <- factor(fitted2, levels=levels(test1$TARGET_VALUE))

confusionMatrix(predicted_2_factor,test1$TARGET_VALUE)


require(ROCR)

p2 <- predict(logit2, test1, type="response")
pr2 <- prediction(p2, test1$TARGET_VALUE)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")

plot(prf2,  main = "ROC Curve logit2")

#plot(prf2,ylab = "Sensitivity", xlab = "Specificity",
 #    main = "ROC Curve logit2")

auc2 <- performance(pr2, measure = "auc")
auc2 <- auc2@y.values[[1]]
print(paste('AUC logit2', round(auc2,2)))

#=============================   CROSS VALIDATION I   ==========================


#Check for missings in the imputed dataset.
sapply(HDMA_select_num_subset, function(x) sum(is.na(x)))
HDMA_select_num_subset$TARGET_VALUE =  as.factor(HDMA_select_num_subset$TARGET_VALUE)

seed <- 1500
classes1 <- HDMA_select_num_subset[, "TARGET_VALUE"]
str(classes1)
#Exclude "minority_population" as predictor == logit2
predictors1 <- HDMA_select_num_subset[, -match(c("TARGET_LABEL","TARGET_VALUE","minority_population",), colnames(HDMA_select_num_subset))]
predictors1
require(caret)
train_set1 <- createDataPartition(classes1, p = 0.75, list = FALSE)
str(train_set1)

train_predictors1 <- predictors1[train_set1, ]
train_classes1 <- classes1[train_set1]
test_predictors1 <- predictors1[-train_set1, ]
test_classes1 <- classes1[-train_set1]
str(train_predictors1)
set.seed(seed)
cv_splits1 <- createFolds(classes1, k = 10, returnTrain = TRUE)
str(cv_splits1)

require(glmnet)

set.seed(seed)

HDMA_num_subset_train <- HDMA_select_num_subset[train_set1, ]
HDMA_num_subset_test <- HDMA_select_num_subset[-train_set1, ]

glmnet_grid1 <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_ctrl1 <- trainControl(method = "cv", number = 10)
#================ Training
glmnet_fit1 <- train(TARGET_VALUE ~ tract_to_msamd_income +
           population                        +
           number_of_owner_occupied_units    +
           number_of_1_to_4_family_units     +
           loan_amount_000s                  +
           hud_median_family_income          +
           applicant_income_000s,  data = HDMA_num_subset_train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid1,
                    trControl = glmnet_ctrl1)

glmnet_fit1
trellis.par.set(caretTheme())
plot(glmnet_fit1, scales = list(x = list(log = 2)) ,
     main = "CrossValidation glmnet_fit1")
#post(glmnet_fit1, file = "CrossValidation1.ps",
     #title = "Cross Validation for  HDMA Numeric Varible")

#The previous plot shows the “accuracy”, that is the percentage of correctly classified observations,
#for the penalized logistic regression model with each combination of the two tuning parameters α and λ.
#The optimal tuning parameter values are α=0 and λ= 0.01.
#Then, it is possible to predict new samples with the identified optimal model using the predict method:
#================ TESTING
pred_classes1 <- predict(glmnet_fit1, newdata = HDMA_num_subset_test)
table(pred_classes1)


pred_probs1 <- predict(glmnet_fit1, newdata = HDMA_num_subset_test, type = "prob")
head(pred_probs1)




#===============================================================================
#======================= II - (Numeric + Categorical Variables) ================
#===============================================================================

#HDMA_select_imputed  <- read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))
#HDMA_select_imputed <- HDMA_select_imputed%>%
 # mutate(TARGET_LABEL = HDMA_Processed$TARGET_LABEL)

NumColumn  <- c("tract_to_msamd_income", "population", "minority_population",
                "number_of_owner_occupied_units", "number_of_1_to_4_family_units",
                "loan_amount_000s","hud_median_family_income","applicant_income_000s")
#
CategoColum = c("property_type_name","loan_type_name","loan_purpose_name","hoepa_status_name",
                "applicant_sex_name","applicant_race_name_1","applicant_ethnicity_name")



HDMA_select_imputed = subset(HDMA_select_imputed, select = c(NumColumn,CategoColum,"TARGET_VALUE") )

HDMA_select_imputed$TARGET_VALUE =  as.factor(HDMA_select_imputed$TARGET_VALUE)
HDMA_select_imputed$TARGET_LABEL =  as.factor(HDMA_select_imputed$TARGET_LABEL)


str(HDMA_select_imputed)

#Check for missings in the imputed dataset.
sapply(HDMA_select_imputed, function(x) sum(is.na(x)))

##============================= Data Partition  ===============================
require(caret)
set.seed(102)
require(dplyr)
training3 <- HDMA_select_imputed$TARGET_VALUE %>%
  createDataPartition(p = 0.75, list = FALSE)
train3  <- HDMA_select_imputed[training3 ,]
test3 <- HDMA_select_imputed[-training3, ]



##==============================   Training  ==================================
# + two Categorical Variable  "loan_type_name " and  "loan_purpose_name"
logit3 <- glm(formula =TARGET_VALUE ~tract_to_msamd_income +
                            population +
                            minority_population +
                            number_of_1_to_4_family_units +
                            loan_amount_000s +
                            hud_median_family_income +
                            applicant_income_000s +
                            loan_type_name +
                            loan_purpose_name,
                            family=binomial(link='logit'),data=train3, maxit = 100)

summary(logit3)
require(MASS)
ans3<-vif(logit3)
barplot(ans3, col = "blue", ylab = "VIF", ylim =c(0,20),las=2, space=1)
abline(h=10, col="red", lwd=2)


#==============================   TESTING DATA =================================
fitted3 <- predict(logit3,test3,type='response')
fitted3 <- ifelse(fitted3 > 0.5,1,0)

misClasificError3 <- mean(fitted3 != test3$TARGET_VALUE)

Accuracy_logit3 = 1-misClasificError3
print(paste('Accuracy', round(Accuracy_logit3,2)))

predicted_3_factor <- factor(fitted3, levels=levels(test1$TARGET_VALUE))

confusionMatrix(predicted_3_factor,test1$TARGET_VALUE)

require(ROCR)
#p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
p3 <- predict(logit3, test3, type="response")
pr3 <- prediction(p3, test3$TARGET_VALUE)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")

plot(prf3, main = "ROC Curve logit3")
#plot(prf3,ylab = "Sensitivity", xlab = "Specificity",
 #    main = "ROC Curve logit3")

auc3 <- performance(pr3, measure = "auc")
auc3 <- auc3@y.values[[1]]
print(paste('AUC logit3', round(auc3,2)))

#=============================   CROSS VALIDATION II   ==========================
HDMA_select_imputed$TARGET_VALUE =  as.factor(HDMA_select_imputed$TARGET_VALUE)

#Check the data for missing values.
sapply(HDMA_select_imputed, function(x) sum(is.na(x)))

#drop a column with missing data

str(HDMA_select_imputed)


not_features <- c("number_of_owner_occupied_units",
                    "property_type_name","hoepa_status_name",
                    "applicant_sex_name","applicant_race_name_1",
                    "applicant_ethnicity_name")



seed <- 250
classes2 <- HDMA_select_imputed[, "TARGET_VALUE"]
str(classes2)
predictors2 <- HDMA_select_imputed[, -match(c("TARGET_LABEL", "TARGET_VALUE", not_features),
                colnames(HDMA_select_imputed))]
predictors2
require(caret)
train_set2 <- createDataPartition(classes2, p = 0.75, list = FALSE)
str(train_set2)

train_predictors2 <- predictors2[train_set2, ]
train_classes2 <- classes2[train_set2]
test_predictors2 <- predictors2[-train_set2, ]
test_classes2 <- classes2[-train_set2]

set.seed(seed)
cv_splits2 <- createFolds(classes2, k = 10, returnTrain = TRUE)
str(cv_splits2)

require(glmnet)

set.seed(seed)

HDMA_imputed_train <- HDMA_select_imputed[train_set2, ]
HDMA_imputed_test <- HDMA_select_imputed[-train_set2, ]

glmnet_grid2 <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_ctrl2 <- trainControl(method = "cv", number = 10)
glmnet_fit2 <- train(TARGET_VALUE ~ ., data = HDMA_imputed_train,
                    method = "glmnet",
                    preProcess = c("center", "scale"),
                    tuneGrid = glmnet_grid2,
                    trControl = glmnet_ctrl2)

glmnet_fit2


trellis.par.set(caretTheme())
plot(glmnet_fit2, scales = list(x = list(log = 2)) ,
     main = "CrossValidation glmnet_fit3")


pred_classes2 <- predict(glmnet_fit2, newdata = HDMA_imputed_test)
table(pred_classes2)

pred_probs2 <- predict(glmnet_fit2, newdata = HDMA_imputed_test, type = "prob")
head(pred_probs2)


save.image("~/Documents/ML-UYork/ML1000/assignment1/Data_meaning.RData")



################################################################################
#################################### DECISION TREE #############################
################################################################################
