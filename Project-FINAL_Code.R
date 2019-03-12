---------------------------------------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%% PREDITCING EMPLOYEE ATTRITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
##Loading the libraries
library(caTools)
library(ggplot2)
library(ROCR)
library(caret)
library(psych)
library(grid)
library(gridExtra)
library(ROSE)
library(corrplot)
library(rpart)
library(dplyr)
library(magrittr)
library(xgboost)


#Import csv data
attr_data <- read.csv("Attrition.csv")
str(attr_data)

#Columns dropped after analysing and domain knowledge EmployeeCount, EmployeeNumber, Over18, StandardHours
attr_data<-attr_data[-c(9,10,22,27)]

####################################################################################
###########################CHECKING FOR CLASS BIAS #################################
####################################################################################

nrow(attr_data)  #total 1470 records
table(attr_data$Attrition)#Only 237 are YES and remaining 1233 are NO
prop.table(table(attr_data$Attrition))


####################################################################################
###########################DATA SPLIT AS TRAIN & TEST DATA SET ##############################
####################################################################################

set.seed(123)
#Convert the dependent variable as factor
attr_data$Attrition <- as.factor(attr_data$Attrition)

#Sampling train & test as 80, 20
ind <- sample(2,nrow(attr_data),replace=TRUE,prob=c(0.8,0.2))
train_attr <- attr_data[ind==1,]
test_attr <- attr_data[ind==2,]

table(train_attr$Attrition)
prop.table(table(train_attr$Attrition))

table(test_attr$Attrition)


####################################################################################
########################### BIAS CORRECTION ########################################
####################################################################################

#over sampling
dim(train_attr)
prop.table(table(train_attr$Attrition))
data_balanced_over <- ovun.sample(Attrition ~ ., data = train_attr, method = "over",N = 1990)$data

#under sampling
data_balanced_under <- ovun.sample(Attrition ~ ., data = train_attr, method = "under",N = 382)$data

#both sampling
data_balanced_both <- ovun.sample(Attrition ~ ., data = train_attr, method = "both",p=0.5)$data

#Rose sampling
data.rose <- ROSE(Attrition ~ ., data = train_attr, seed = 1)$data

#Imbalanced data check after sampling techniques
table(data_balanced_over$Attrition)
prop.table(table(data_balanced_over$Attrition))

table(data_balanced_under$Attrition)
prop.table(table(data_balanced_under$Attrition))

table(data_balanced_both$Attrition)
prop.table(table(data_balanced_both$Attrition))

table(data.rose$Attrition)
prop.table(table(data.rose$Attrition))


######################### ESTIMATING SAMPLING ACCURACY ####################################

dim(test_attr)
prop.table(table(test_attr$Attrition))

#build decision tree models
tree.under <- rpart(Attrition ~.,data=data_balanced_under)
tree.over <- rpart(Attrition ~.,data=data_balanced_over)
tree.rose <- rpart(Attrition ~.,data=data.rose)
tree.both <- rpart(Attrition~.,data=data_balanced_both)
#make predictions on unseen data
pred.tree.under <- predict(tree.under,newdata=test_attr)
pred.tree.over <- predict(tree.over,newdata=test_attr)
pred.tree.rose <- predict(tree.rose,newdata=test_attr)
pred.tree.both <- predict(tree.both,newdata=test_attr)



#Evaluate accuracy of respective predictions
roc.curve(test_attr$Attrition,pred.tree.rose[,2])
##Area under the curve (AUC): 0.704

roc.curve(test_attr$Attrition,pred.tree.over[,2])
#Area under the curve (AUC): 0.695

roc.curve(test_attr$Attrition,pred.tree.under[,2])
#Area under the curve (AUC): 0.727

roc.curve(test_attr$Attrition,pred.tree.both[,2])
#Area under the curve (AUC): 0.709


#######EXPLORATORY DATA ANALYSIS#################################
trData <- data_balanced_under
tsData <- test_attr

summary(trData)

# Combine both datasets. Call it FullData
FullData <- rbind(trData,tsData)

View(FullData)
dim(FullData)

#Check for NA's
colSums(is.na(FullData))  ###We dont have any null values

#Check for any blank values
summary(FullData)

############# NO MISSING VALUES #############################

# 1. Indep vars outlier detection and correction

str(FullData)

###NUmeric variables/Non-Categorical variables
#Age, DailyRate, DistanceFromHome, Education, EmployeeCount, EmployeeNumber, EnvironmentSatisfaction , HourlyRate, JobInvolvement, JobLevel,JobSatisfaction, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike, PerformanceRating, RelationshipSatisfaction, StandardHours, StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager
##26 variables

#####################################################################################################################
####################### OUTLIERS DETECTION - BIVARIATE ANALYSIS of CONTINOUS VARIABLES [BOXPLOT]#############################
#####################################################################################################################
boxplot(Age ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "DailyRate", 
        main = "Daily Rate",col=
          c("Pink","lightblue"))        


boxplot(DailyRate ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "DailyRate", 
        main = "Daily Rate",col=
          c("Pink","lightblue"))        

boxplot(DistanceFromHome ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "DistanceFromHome", 
        main = "Distance From Home",col=
          c("Pink","lightblue"))

boxplot(Education ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "Education", 
        main = "Education",col=
          c("Pink","lightblue"))    

boxplot(EnvironmentSatisfaction ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "EnvironmentSatisfaction", 
        main = "Employee Satisfaction",col=
          c("Pink","lightblue")) 

boxplot(HourlyRate ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "HourlyRate", 
        main = "Hourly Rate",col=
          c("Pink","lightblue")) 

boxplot(JobInvolvement ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "JobInvolvement", 
        main = "Job Involvement",col=
          c("Pink","lightblue")) 

boxplot(JobLevel ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "JobLevel", 
        main = "Job Level",col=
          c("Pink","lightblue"))

boxplot(JobSatisfaction ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "JobSatisfaction", 
        main = "Job Satisfaction",col=
          c("Pink","lightblue"))

boxplot(MonthlyIncome ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "MonthlyIncome", 
        main = "Monthly Income",col=
          c("Pink","lightblue"))

boxplot(MonthlyRate ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "MonthlyRate", 
        main = "Monthly Rate",col=
          c("Pink","lightblue"))

boxplot(NumCompaniesWorked ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "NumCompaniesWorked", 
        main = "Companies Worked",col=
          c("Pink","lightblue"))

boxplot(PercentSalaryHike ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "PercentSalaryHike", 
        main = "Percent Salary Hike",col=
          c("Pink","lightblue")) 

boxplot(PerformanceRating ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "PerformanceRating", 
        main = "Perf Rating",col=
          c("Pink","lightblue"))

boxplot(RelationshipSatisfaction ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "RelationshipSatisfaction", 
        main = "Relationship Satisfaction",col=
          c("Pink","lightblue")) 

boxplot(StockOptionLevel ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "StockOptionLevel", 
        main = "Stock Option Level",col=
          c("Pink","lightblue"))

boxplot(TotalWorkingYears ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "TotalWorkingYears", 
        main = "Total Working Years",col=
          c("Pink","lightblue"))    #Yes Yes Yes

boxplot(TrainingTimesLastYear ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "TrainingTimesLastYear", 
        main = "Training Times Last",col=
          c("Pink","lightblue"))  ##Yes

boxplot(WorkLifeBalance ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "WorkLifeBalance", 
        main = "Work Life Balance",col=
          c("Pink","lightblue")) 

boxplot(YearsAtCompany ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "YearsAtCompany", 
        main = "YEars at Company",col=
          c("Pink","lightblue"))

boxplot(YearsInCurrentRole ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "YearsInCurrentRole", 
        main = "Years in Current Role",col=
          c("Pink","lightblue"))

boxplot(YearsSinceLastPromotion ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "YearsSinceLastPromotion", 
        main = "Years since Last Promotion",col=
          c("Pink","lightblue"))

boxplot(YearsWithCurrManager ~ Attrition, data = FullData, 
        xlab = "Attrition",ylab = "YearsWithCurrManager", 
        main = "Years with Current Manager",col=
          c("Pink","lightblue"))

######################################################################################################
######################### OUTLIERS TREATMENT USING IQR FUNCTION ######################################
######################################################################################################

###NUMERIC VARIABLES WITH OUTLIERS - Age, DistanceFromHome, JobLevel, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, PerformanceRating, StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager


treat_outlier = function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

# Outlier treatment - Age
FullData$Age <- treat_outlier(FullData$Age)
boxplot(FullData$Age)

# Outlier treatment - DistanceFromHome
FullData$DistanceFromHome <- treat_outlier(FullData$DistanceFromHome)
boxplot(FullData$DistanceFromHome)

# Outlier treatment - JobLevel
FullData$JobLevel <- treat_outlier(FullData$JobLevel)
boxplot(FullData$JobLevel)

# Outlier treatment - MonthlyIncome
FullData$MonthlyIncome <- treat_outlier(FullData$MonthlyIncome)
boxplot(FullData$MonthlyIncome)

# Outlier treatment - NumCompaniesWorked
FullData$NumCompaniesWorked <- treat_outlier(FullData$NumCompaniesWorked)
boxplot(FullData$NumCompaniesWorked)

# Outlier treatment - PercentSalaryHike
FullData$PercentSalaryHike <- treat_outlier(FullData$PercentSalaryHike)
boxplot(FullData$PercentSalaryHike)

# Outlier treatment - PerformanceRating
FullData$PerformanceRating <- treat_outlier(FullData$PerformanceRating)
boxplot(FullData$PerformanceRating)

# Outlier treatment - StockOptionLevel
FullData$StockOptionLevel <- treat_outlier(FullData$StockOptionLevel)
boxplot(FullData$StockOptionLevel)

# Outlier treatment - TotalWorkingYears
FullData$TotalWorkingYears <- treat_outlier(FullData$TotalWorkingYears)
boxplot(FullData$TotalWorkingYears)

# Outlier treatment - TrainingTimesLastYear
FullData$TrainingTimesLastYear <- treat_outlier(FullData$TrainingTimesLastYear)
boxplot(FullData$TrainingTimesLastYear)

# Outlier treatment - YearsAtCompany
FullData$YearsAtCompany <- treat_outlier(FullData$YearsAtCompany)
boxplot(FullData$YearsAtCompany)

# Outlier treatment - YearsInCurrentRole
FullData$YearsInCurrentRole <- treat_outlier(FullData$YearsInCurrentRole)
boxplot(FullData$YearsInCurrentRole)

# Outlier treatment - YearsSinceLastPromotion
FullData$YearsSinceLastPromotion <- treat_outlier(FullData$YearsSinceLastPromotion)
boxplot(FullData$YearsSinceLastPromotion)

# Outlier treatment - YearsWithCurrManager
FullData$YearsWithCurrManager <- treat_outlier(FullData$YearsWithCurrManager)
boxplot(FullData$YearsWithCurrManager)



##########OUTLIER DETECTION for CATEGORICAL VARIABLES ##################################
####PAIRPLOT

pairs(~FullData$Gender + FullData$MaritalStatus +FullData$BusinessTravel +FullData$JobRole+FullData$Department
      +FullData$EducationField,
      FullData$Attrition,data=FullData,main="Attrition DATA for Categorical Variables")


########### gender ##########################

table(FullData$Gender)
table(FullData$MaritalStatus)

FullData %>%
  ggplot(aes(x = Gender, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",  vjust = -.5) +
  labs(y = "Percentage", fill= "Gender") + facet_grid(~Attrition) + ggtitle("Attrition")


########################## BusinessTravel ########################################
FullData%>%group_by(BusinessTravel)%>%tally() %>% ggplot(aes(x=BusinessTravel,y=n,fill=BusinessTravel))+
  geom_bar(stat = "identity") + 
  labs(x="BusinessTravel", y="Business Travel count")+ 
  geom_text(aes(label = n), vjust=-0.5)


FullData%>%group_by(BusinessTravel,Attrition)%>%tally() %>% ggplot(aes(x=BusinessTravel,y=n,fill=Attrition))+
  geom_bar(stat = "identity") + 
  labs(x="BusinessTravel", y="Business Travel count")+ 
  geom_text(aes(label = n), vjust=-0.5 )

#########################Department ###########################################

FullData%>% group_by(Department)%>% tally()%>% ggplot(aes(x=Department,y=n,fill=Department))+
  geom_bar(stat="identity")+labs(x="Department",y="Department count")+geom_text(aes(label=n))

###################### education field ########################################

FullData%>% group_by(EducationField)%>% tally()%>% ggplot(aes(x=EducationField,y=n,fill=EducationField))+
  geom_bar(stat="identity")+labs(x="EducationField",y="EducationField count")+geom_text(aes(label=n))



################### JobRole ################################################################

FullData%>% ggplot(aes(x=JobRole,group =Attrition))+geom_bar(aes(y=..prop..,fill=factor(..x..)),
                                                             stat = "count",alpha=0.7)+geom_text(aes(label=scales::percent(..prop..),y=..prop..),
                                                                                                 stat= "count",  vjust = -.5)+
  labs(y="percentage",fill="JobRole")+facet_grid(~Attrition)+theme_minimal()+ggtitle("Attrition")

################### marital status ############################################

FullData%>% ggplot(aes(x=MaritalStatus,group =Attrition))+geom_bar(aes(y=..prop..,fill=factor(..x..)),
                                                                   stat = "count",alpha=0.7)+geom_text(aes(label=scales::percent(..prop..),y=..prop..), stat= "count",  vjust = -.5)+
  labs(y="percentage",fill="MaritalStatus")+facet_grid(~Attrition)+theme_minimal()+ggtitle("Attrition")


FullData$Education <- as.factor(FullData$Education)
FullData$EnvironmentSatisfaction <- as.factor(FullData$EnvironmentSatisfaction)
FullData$JobInvolvement <- as.factor(FullData$JobInvolvement)
FullData$JobLevel <- as.factor(FullData$JobLevel)
FullData$JobSatisfaction <- as.factor(FullData$JobSatisfaction)
FullData$PerformanceRating <- as.factor(FullData$PerformanceRating)
FullData$RelationshipSatisfaction <- as.factor(FullData$RelationshipSatisfaction)
FullData$StockOptionLevel <- as.factor(FullData$StockOptionLevel)
FullData$WorkLifeBalance <- as.factor(FullData$WorkLifeBalance)
str(FullData)

##############Type casting - categorical variables as factor ########

train_attr$Education <- as.factor(train_attr$Education)
train_attr$EnvironmentSatisfaction <- as.factor(train_attr$EnvironmentSatisfaction)
train_attr$JobInvolvement <- as.factor(train_attr$JobInvolvement)
train_attr$JobLevel <- as.factor(train_attr$JobLevel)
train_attr$JobSatisfaction <- as.factor(train_attr$JobSatisfaction)
train_attr$PerformanceRating <- as.factor(train_attr$PerformanceRating)
train_attr$RelationshipSatisfaction <- as.factor(train_attr$RelationshipSatisfaction)
train_attr$StockOptionLevel <- as.factor(train_attr$StockOptionLevel)
train_attr$WorkLifeBalance <- as.factor(train_attr$WorkLifeBalance)
str(train_attr)

test_attr$Education <- as.factor(test_attr$Education)
test_attr$EnvironmentSatisfaction <- as.factor(test_attr$EnvironmentSatisfaction)
test_attr$JobInvolvement <- as.factor(test_attr$JobInvolvement)
test_attr$JobLevel <- as.factor(test_attr$JobLevel)
test_attr$JobSatisfaction <- as.factor(test_attr$JobSatisfaction)
test_attr$PerformanceRating <- as.factor(test_attr$PerformanceRating)
test_attr$RelationshipSatisfaction <- as.factor(test_attr$RelationshipSatisfaction)
test_attr$StockOptionLevel <- as.factor(test_attr$StockOptionLevel)
test_attr$WorkLifeBalance <- as.factor(test_attr$WorkLifeBalance)
str(test_attr)

####################### Backing up data set's ############
AT <- attr_data
ATrain <- train_attr
ATest <- test_attr


###################################################################################
################# FEATURE ENGINEERING on TRAINING DATA ############################
###################################################################################

ATrain$TenurePerJob <- ifelse(ATrain$NumCompaniesWorked!=0, ATrain$TotalWorkingYears/ATrain$NumCompaniesWorked,0)
ATrain$YearWithoutChange <- ATrain$YearsInCurrentRole - ATrain$YearsSinceLastPromotion
ATrain$YearsWithoutChange2 <- ATrain$TotalWorkingYears - ATrain$YearsSinceLastPromotion

View(ATrain)
tenurePlot <- ggplot(ATrain,aes(TenurePerJob))+geom_density()+facet_grid(~Attrition)
changePlot <- ggplot(ATrain,aes(YearWithoutChange))+geom_density()+facet_grid(~Attrition)
change2Plot <- ggplot(ATrain,aes(YearsWithoutChange2))+geom_density()+facet_grid(~Attrition)
grid.arrange(tenurePlot,changePlot,change2Plot,ncol=2,top = "Fig 8")

Med_HR <- median(ATrain[ATrain$Department == 'Human Resources',]$MonthlyIncome)
Med_RnD <- median(ATrain[ATrain$Department == 'Research & Development',]$MonthlyIncome)
Med_Sales <- median(ATrain[ATrain$Department == 'Sales',]$MonthlyIncome)


Med_LabTech <- median(ATrain[ATrain$JobRole == 'Laboratory Technician',]$MonthlyIncome)

TrainLabTech <- ATrain[ATrain$JobRole == 'Laboratory Technician',]
TrainLabTech$comparole <- TrainLabTech$MonthlyIncome/Med_LabTech

Med_overall <- median(ATrain$MonthlyIncome)

ATrain$CompaRatioDep <- ifelse(ATrain$Department == 'Human Resources',ATrain$MonthlyIncome/Med_HR,ifelse(ATrain$Department=='Research & Development',ATrain$MonthlyIncome/Med_RnD,ATrain$MonthlyIncome/Med_Sales))

ATrain$CompaRatioOverall <- ATrain$MonthlyIncome/Med_overall

ATrain$CompaOverallGroup <- ifelse(ATrain$CompaRatioOverall>4,4,ifelse(ATrain$CompaRatioOverall>3,3,ifelse(ATrain$CompaRatioOverall>2,2,ifelse(ATrain$CompaRatioOverall>1,1,ifelse(ATrain$CompaRatioOverall>0.5,0.5,0)))))

ATrain$CompaDepGroup <- ifelse(ATrain$CompaRatioDep>4,4,ifelse(ATrain$CompaRatioDep>3,3,ifelse(ATrain$CompaRatioDep>2,2,ifelse(ATrain$CompaRatioDep>1,1,ifelse(ATrain$CompaRatioDep>0.5,0.5,0)))))


CompaOverallPlot <- ggplot(ATrain,aes(CompaRatioOverall))+geom_density()+facet_grid(~Attrition)
CompaDepPlot <- ggplot(ATrain,aes(CompaRatioDep))+geom_density()+facet_grid(~Attrition)
grid.arrange(CompaOverallPlot,CompaDepPlot,ncol=2,top = "Fig 9")

###################################################################################
################# FEATURE ENGINEERING on TEST DATA ################################
###################################################################################

ATest$TenurePerJob <- ifelse(ATest$NumCompaniesWorked!=0, ATest$TotalWorkingYears/ATest$NumCompaniesWorked,0)
ATest$YearWithoutChange <- ATest$YearsInCurrentRole - ATest$YearsSinceLastPromotion
ATest$YearsWithoutChange2 <- ATest$TotalWorkingYears - ATest$YearsSinceLastPromotion


ATest$CompaRatioDep <- ifelse(ATest$Department == 'Human Resources',ATest$MonthlyIncome/Med_HR,ifelse(ATest$Department=='Research & Development',ATest$MonthlyIncome/Med_RnD,ATest$MonthlyIncome/Med_Sales))

ATest$CompaRatioOverall <- ATest$MonthlyIncome/Med_overall

ATest$CompaOverallGroup <- ifelse(ATest$CompaRatioOverall>4,4,ifelse(ATest$CompaRatioOverall>3,3,ifelse(ATest$CompaRatioOverall>2,2,ifelse(ATest$CompaRatioOverall>1,1,ifelse(ATest$CompaRatioOverall>0.5,0.5,0)))))

ATest$CompaDepGroup <- ifelse(ATest$CompaRatioDep>4,4,ifelse(ATest$CompaRatioDep>3,3,ifelse(ATest$CompaRatioDep>2,2,ifelse(ATest$CompaRatioDep>1,1,ifelse(ATest$CompaRatioDep>0.5,0.5,0)))))


###################################################################################
################  BINNING OF CONTINOUS VARIABLES ##################################
###################################################################################

ATrain$AgeGroup <- with(ATrain,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) #Creating Age Groups

ATrain$DistanceGroup <- with(ATrain,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1)))))) #Creating Distance Groups

ATrain$YearsWithManagerGroup <- with(ATrain,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1))))) #Creating YearsWithManager Groups


ATrain$TenureGroup <- with(ATrain,ifelse(TenurePerJob>35,9,ifelse(TenurePerJob>30,8,ifelse(TenurePerJob>25,7,ifelse(TenurePerJob>20,6,ifelse(TenurePerJob>15,5,ifelse(TenurePerJob>10,4,ifelse(TenurePerJob>5,3,ifelse(TenurePerJob>2,2,1))))))))) #Creating Tenure Per Job groups

ATrain$Change2Group <- with(ATrain,ifelse(YearsWithoutChange2>10,3,ifelse(YearsWithoutChange2>5,2,1))) #Creating Years Without Change2

ATrain$Change1Group <- with(ATrain,ifelse(YearWithoutChange>2.5,3,ifelse(YearWithoutChange>-2.5,2,1))) #Creating Years Without Change 1

#ATrain$AvgSatisGroup <- with(ATrain,ifelse(AvgSatis<2.5,1,2)) # Create Average Satisfaction Groups

ATrain$WorkYearGroup <- with(ATrain,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

ATrain$NumCompGroup <- with(ATrain,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) #Creating Number of Companies Worked

# For ATest Set

ATest$AgeGroup <- with(ATest,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) #Creating Age Groups

ATest$DistanceGroup <- with(ATest,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1)))))) #Creating Distance Groups

ATest$YearsWithManagerGroup <- with(ATest,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1))))) #Creating YearsWithManager Groups


ATest$TenureGroup <- with(ATest,ifelse(TenurePerJob>35,9,ifelse(TenurePerJob>30,8,ifelse(TenurePerJob>25,7,ifelse(TenurePerJob>20,6,ifelse(TenurePerJob>15,5,ifelse(TenurePerJob>10,4,ifelse(TenurePerJob>5,3,ifelse(TenurePerJob>2,2,1))))))))) #Creating Tenure Per Job groups

ATest$Change2Group <- with(ATest,ifelse(YearsWithoutChange2>10,3,ifelse(YearsWithoutChange2>5,2,1))) #Creating Years Without Change2

ATest$Change1Group <- with(ATest,ifelse(YearWithoutChange>2.5,3,ifelse(YearWithoutChange>-2.5,2,1))) #Creating Years Without Change 1

#ATest$AvgSatisGroup <- with(ATest,ifelse(AvgSatis<2.5,1,2)) # Creating avg satisfaction group

ATest$WorkYearGroup <- with(ATest,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

ATest$NumCompGroup <- with(ATest,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) #Creating Number of Companies Worked

dim(ATest)
dim(ATrain)

###################################################################################
#############################Correlation of Variables##############################
###################################################################################

Training_cor <- ATrain

for(i in 1:ncol(Training_cor)){
  
  Training_cor[,i]<- as.integer(Training_cor[,i])
}

corrplot(cor(Training_cor))

#RE DIRECTING THE CORRELATION RESULT INTO A DATA FRAME for BETTER UNDERSTANDING
t1 <- cor(Training_cor)
View(t1)


#RETAINING only the HIGHLY CORRELATED FACTORs, IDENTIFIED from PREVIOUS STEP RESULT t1

Train <- ATrain[,c(2,3,5,7,8,10,11,12,14,15,16,17,18,20,21,23,24,26,28,29,30,31,41:46)]
dim(Train)
Test <- ATest[]

# Coding the categorical Variables for model building

Train$BusinessTravel <- as.integer(Train$BusinessTravel)
Train$Department <- as.integer(Train$Department)
Train$Gender <- as.integer(Train$Gender)
Train$MaritalStatus <- as.integer(Train$MaritalStatus)
Train$OverTime <- as.integer(Train$OverTime)
Train$JobRole <- as.integer(Train$JobRole)
Train$EducationField <- as.integer(Train$EducationField)
Test$BusinessTravel <- as.integer(Test$BusinessTravel)
Test$Department <- as.integer(Test$Department)
Test$Gender <- as.integer(Test$Gender)
Test$MaritalStatus <- as.integer(Test$MaritalStatus)
Test$OverTime <- as.integer(Test$OverTime)
Test$JobRole <- as.integer(Test$JobRole)
Test$EducationField <- as.integer(Test$EducationField)

########################################################################################################
####################### MODEL BUILDING FOR CLASSIFICATION PROBLEM #######################################
########################################################################################################

#######################   LOGISTIC REGRESSION using CARET TRAIN METHOD ####################################

fit_glm <- train(Attrition~.,Train,method = 'glm',trControl = trainControl(method = 'repeatedcv',number = 3))

#######################   DECISION TREE using CARET TRAIN METHOD      ####################################

fit_rpart <- train(Attrition ~.,Train,method = 'rpart', trControl = trainControl(method = 'cv',number = 3))

#######################   RANDOM FOREST using CARET TRAIN METHOD      ####################################
set.seed(123)
fit_rf <- train(Attrition ~.,Train,method = 'rf', trControl = trainControl(method = 'repeatedcv',number = 3))

#######################   XGBOOST using CARET TRAIN METHOD            ####################################

xgbGrid <- expand.grid(nrounds = 300,
                       max_depth = 1,
                       eta = 0.3,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9)

set.seed(12)
fit_xgb <- train(Attrition ~.,Train,method = 'xgbTree',tuneGrid = xgbGrid,trControl = trainControl(method = 'repeatedcv',number = 3,classProbs = TRUE)) 

####################### SVM (SUPPORT VECTOR MACHINE) using CARET TRAIN METHOD ####################################

fit_svm <- train(Attrition~.,Train,method = 'svmRadial',trControl = trainControl(method = 'repeatedcv',number = 3))

####################### KNN (K NEAREST NEIGHBOUR) using CARET TRAIN METHOD    ####################################

fit_knn <- train(Attrition~.,Train,method = 'knn',trControl = trainControl(method = 'repeatedcv',number = 3))

########################################################################################################
####################### MODEL PREDICTION on TEST SET #################################################
########################################################################################################

Predictions_rpart <- predict(fit_rpart,Test)
Predictions_rf <- predict(fit_rf, Test)
Predictions_xgb <- predict(fit_xgb, Test)
Predictions_glm <- predict(fit_glm, Test)
Predictions_svm <- predict(fit_svm,Test)
Predictions_knn <- predict(fit_knn,Test)

########################################################################################################
####################### MODEL EVALUATION ###########################################################
########################################################################################################

confusionMatrix(Predictions_xgb,ATest$Attrition) #88 kuppa 0.4
confusionMatrix(Predictions_rpart,ATest$Attrition) #83 kuppa 0
confusionMatrix(Predictions_rf,ATest$Attrition) #85 kuppa 0.29
confusionMatrix(Predictions_glm,ATest$Attrition) #86 kuppa 0.366
confusionMatrix(Predictions_svm,ATest$Attrition) #84.5 kuppa 0.07
confusionMatrix(Predictions_knn,ATest$Attrition) #83 kuppa 0.06

########################################################################################################
####################### VARIABLE IMPORTANCE from CHAMPION MODEL #########################################
########################################################################################################
varImp(object=fit_xgb)
#Plotting Varianle importance for GBM
plot(varImp(object=fit_xgb),main="Employee Attrition - Variable Importance")

