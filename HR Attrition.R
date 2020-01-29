attrition<-read.csv('G:/IMARTICUS/R/Attrition.csv')
View(attrition)
str(attrition)
summary(attrition)
colSums(is.na(attrition))
attrition<-attrition[,-c(9,10,14,15,22,27)]
View(attrition)
#Scaling
attrition[,-c(2,3,5,8,10,12,14,18)]<-scale(attrition[,-c(2,3,5,8,10,12,14,18)])
#Boxplot
boxplot(attrition)
boxplot(attrition[,-c(2,3,5,8,10,12,14,18)])
#Removing outliers
summary(attrition)
#Monthly income
mi3<-0.3985
attrition$MonthlyIncome<-ifelse(attrition$MonthlyIncome>mi3+1.5*IQR(attrition$MonthlyIncome),mi3+1.5*IQR(attrition$MonthlyIncome),attrition$MonthlyIncome)
#Num of companies worked
ncw3<-0.5231
attrition$NumCompaniesWorked<-ifelse(attrition$NumCompaniesWorked>ncw3+1.5*IQR(attrition$NumCompaniesWorked),ncw3+1.5*IQR(attrition$NumCompaniesWorked),attrition$NumCompaniesWorked)
#Performance rating
pr3<-(-0.4261)
attrition$PerformanceRating<-ifelse(attrition$PerformanceRating>pr3+1.5*IQR(attrition$PerformanceRating),pr3+1.5*IQR(attrition$PerformanceRating),attrition$PerformanceRating)
#Stock option level
sol3<-0.2419
attrition$StockOptionLevel<-ifelse(attrition$StockOptionLevel>sol3+1.5*IQR(attrition$StockOptionLevel),sol3+1.5*IQR(attrition$StockOptionLevel),attrition$StockOptionLevel)
#Total working years
twy3<-0.4782
attrition$TotalWorkingYears<-ifelse(attrition$TotalWorkingYears>twy3+1.5*IQR(attrition$TotalWorkingYears),twy3+1.5*IQR(attrition$TotalWorkingYears),attrition$TotalWorkingYears)
#Boxplot
boxplot(attrition[,-c(2,3,5,8,10,12,14,18)])
#Training times last year
ttly3<-0.1557
attrition$TrainingTimesLastYear<-ifelse(attrition$TrainingTimesLastYear>ttly3+1.5*IQR(attrition$TrainingTimesLastYear),ttly3+1.5*IQR(attrition$TrainingTimesLastYear),attrition$TrainingTimesLastYear)
ttly1<-(-0.6200)
attrition$TrainingTimesLastYear<-ifelse(attrition$TrainingTimesLastYear<ttly1-1.5*IQR(attrition$TrainingTimesLastYear),ttly1-1.5*IQR(attrition$TrainingTimesLastYear),attrition$TrainingTimesLastYear)
#Years at company
yac3<-0.3251
attrition$YearsAtCompany<-ifelse(attrition$YearsAtCompany>yac3+1.5*IQR(attrition$YearsAtCompany),yac3+1.5*IQR(attrition$YearsAtCompany),attrition$YearsAtCompany)
#Years in current role
yicr3<-0.7647
attrition$YearsInCurrentRole<-ifelse(attrition$YearsInCurrentRole>yicr3+1.5*IQR(attrition$YearsInCurrentRole),yicr3+1.5*IQR(attrition$YearsInCurrentRole),attrition$YearsInCurrentRole)
#Years since last promotion
yslp3<-0.2521
attrition$YearsSinceLastPromotion<-ifelse(attrition$YearsSinceLastPromotion>yslp3+1.5*IQR(attrition$YearsSinceLastPromotion),yslp3+1.5*IQR(attrition$YearsSinceLastPromotion),attrition$YearsSinceLastPromotion)
#Years with current manager
ywcm3<-0.8063
attrition$YearsWithCurrManager<-ifelse(attrition$YearsWithCurrManager>ywcm3+1.5*IQR(attrition$YearsWithCurrManager),ywcm3+1.5*IQR(attrition$YearsWithCurrManager),attrition$YearsWithCurrManager)
boxplot(attrition[,-c(2,3,5,8,10,12,14,18)])
#Feature Engineering
a<-c(attrition$WorkLifeBalance+attrition$PerformanceRating+attrition$OverTime+attrition$Education+attrition$Gender+attrition$Age)
b<-c(attrition$Age+attrition$Gender+attrition$Education+attrition$JobRole+attrition$MonthlyIncome+attrition$NumCompaniesWorked+attrition$YearsInCurrentRole)
telc<-telc[,-c(7:18)]
telc<-data.frame(Mins,Calls,Charges,telc)
#Splitting
split<-sample.split(attrition$Attrition,SplitRatio=0.80)
train<-subset(attrition,split==T)
test<-subset(attrition,split==F)
#Logistic Regression
model1<-glm(Attrition~.,family = binomial(link='logit'),data = attrition)
summary(model1)
