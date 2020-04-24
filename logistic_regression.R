# Reference: https://ocw.mit.edu/courses/sloan-school-of-management/15-071-the-analytics-edge-spring-2017/logistic-regression/
# Written in R

# Healthcare Quality Assessment

# Fields:
# Metadata: health insurance; 131 diabetes patients, age from 35-55, cost $10K -$20K, 2003 Sep - 2005 Sep
# Expert reviewed the claims and wrote descriptive notes: i.e. use of narcotics (sleeping inducing), avandia (insulin sensitizer)
# Expert assessment: good/poor.
# Medical Claims: disgnosis, procedures, doctor/hospital, cost
# Pharmacy Claims: drug, quantity, doctor, medication cost

# Dependent Variable: quality of care (categorical: 0:GoodCare P(y=0), 1: PoorCare P(y=1))
# Independent Variables (x1, x2, .., xk):
## diabetes treatment: narcotics, avandia
## patient demographics
## healthcare utilization: home testing supplies
## providers
## claims
## prescriptions

# MemberID numbers the patients from 1 to 131, and is just an identifying number.

# InpatientDays is the number of inpatient visits, or number of days the person spent in the hospital.
# ERVisits is the number of times the patient visited the emergency room.
# OfficeVisits is the number of times the patient visited any doctor's office.
# Narcotics is the number of prescriptions the patient had for narcotics.

# DaysSinceLastERVisit is the number of days between the patient's last emergency room visit and the end of the study period (set to the length of the study period if they never visited the ER). 
# Pain is the number of visits for which the patient complained about pain.

# TotalVisits is the total number of times the patient visited any healthcare provider.

# ProviderCount is the number of providers that served the patient.

# MedicalClaims is the number of days on which the patient had a medical claim.
# ClaimLines is the total number of medical claims.
# StartedOnCombination is whether or not the patient was started on a combination of drugs to treat their diabetes (TRUE or FALSE).
# AcuteDrugGapSmall is the fraction of acute drugs that were refilled quickly after the prescription ran out.
# PoorCare is the outcome or dependent variable, and is equal to 1 if the patient had poor care, and equal to 0 if the patient had good care.

# Split into Training and Testing Sets
data <- read.csv("~/Desktop/quality.csv")


# plot the relationship of x = office visits and y = narcotics by PoorCare = 0 (green) GOOD! and PoorCare = 1 (red) BAD! 
data$PoorCare = factor(data$PoorCare)
ggplot(data, aes(x=OfficeVisits, y=Narcotics)) + 
  geom_point(aes(colour=PoorCare)) +
  scale_color_manual(values=c("green", "red"))

# split the data -- wrong example!
spl = sample(1:nrow(data), size=0.7 * nrow(data))
train = data[spl,]
test = data[-spl,]
str(data)
table(data$PoorCare)
# 98/131 = 75%
table(train$PoorCare)
# 70/91 = 77%
table(test$PoorCare)
# 70/91 = 70%

library(caTools)
set.seed(88)

# randomly split into 75% and 25% as well as ensure 75% as the accuracy
# this is the correct way of doing it!
split = sample.split(data$PoorCare, SplitRatio = 0.75)
split

quality = data
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

table(qualityTrain$PoorCare)
# 74/99 = 75%
table(qualityTest$PoorCare)
# 24/32 = 75%

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)
# glm(formula = PoorCare ~ OfficeVisits + Narcotics, family = binomial, 
#     data = qualityTrain)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.06303  -0.63155  -0.50503  -0.09689   2.16686  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.64613    0.52357  -5.054 4.33e-07 ***
#   OfficeVisits  0.08212    0.03055   2.688  0.00718 ** 
#   Narcotics     0.07630    0.03205   2.381  0.01728 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 111.888  on 98  degrees of freedom
# Residual deviance:  89.127  on 96  degrees of freedom
# AIC: 95.127
# 
# Number of Fisher Scoring iterations: 4


# both OfficeVisits and Narcotics are positives - the higher of them, the higher prob poor care
# both of them have one star, meaning they are significant
# AIC measure the quality of model : 
# the number of the variables and the # of observations
# by the same dataset. The less the better

# give us the probabilities
predictTrain = predict(QualityLog, type = "response")
# Pr(PoorCare=1) = -2.6 + 0.08212 * OfficeVisits + 0.07630 * Narcotics
# TO ADD: the explanation of intercept? The response?

# summary of the predictions, so all of the numnbers should be [0,1]
summary(predictTrain)

#Min.    1st Qu.  Median    Mean    3rd Qu.   Max. 
#0.06623 0.11912  0.15967  0.25253  0.26765  0.98456 
 
# The min is 0.066 and the max is 0.985

# TO ADD: tapply?
# to give us the actual prediction for the actual outcomes
tapply(predictTrain, qualityTrain$PoorCare, mean)
# 0         1 
# 0.1894512 0.4392246 <- for all of the actual poor quality cases, we predict that the patient has a 44% of chances of receiving a poor healthcare
# vs for all of the actuall good care cases, we predict about 19% that they are receiving a poor quality of healthcare.
# it is a good sign that we the directions of the predictions are aligned with the actuals

QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog)

# Call:
#   glm(formula = PoorCare ~ StartedOnCombination + ProviderCount, 
#       family = binomial, data = qualityTrain)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.61826  -0.72782  -0.64555  -0.08407   1.94662  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -2.00097    0.55097  -3.632 0.000282 ***
#   StartedOnCombinationTRUE  1.95230    1.22342   1.596 0.110541    
# ProviderCount             0.03366    0.01983   1.697 0.089706 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 111.89  on 98  degrees of freedom
# Residual deviance: 104.37  on 96  degrees of freedom
# AIC: 110.37
# 
# Number of Fisher Scoring iterations: 4

# SELECT A THRESHIOLD #
## Because the predicted value of health quality is a probability,
## so we'd like to select a threshold t as a classifier
## and if the Probability of the value is >= t, we predict POOR QUALITY
## if the Probability of the value is < t, we predict GOOD QUALITY

# Revisit of Confusion Matrix:
#             Pred = 0    Pred = 1
#  Act. = 0     TN          FP
#  Act. = 1     FN          TP

## Sensitivity (TPR): the fraction of people they are actually positive and we predict them as positive
### TPR = TP / TP + FN 
## Specificity (TNR): the fraction of people they are actually negative and we predict them as negative 
### TNR = TN / TN + FP

# Should we select 0.5?
table(qualityTrain$PoorCare, predictTrain > 0.5)
#   FALSE TRUE
# 0    70    4
# 1    15   10

# sensitivity
# 10/(10+15) = 10/25 = 0.4

# specificity
# 70/(70+4) = = 70/74 = 0.9459459

table(qualityTrain$PoorCare, predictTrain > 0.7)
#   FALSE TRUE
# 0    73    1
# 1    17    8

# sensitivity
# 8/(8 + 17) = 8/25 = 0.32

# specificity
# 73/(1 + 73) = 73/74 = 0.9864865

table(qualityTrain$PoorCare, predictTrain > 0.2)
#   FALSE TRUE
# 0    54   20
# 1     9   16

# sensitivity
# 16/(16+9) = 16/25 = 0.64

# specificity
# 54/(54+20) = 54/74 = 0.7297297

# by lowering the threshold, we have lower specificity but better sensitivity
# by increasing the thershold, we have worse sensitivity but better specificity
# sensitivity in this case means: we have a better TPR: the higher this rate is, we miss fewer people who are in poor health care
# specificity in this case means: we have a better TNR: the higher this rate is, we are better in identifying people who are receiving good health care. 
# if the specificity is low, it means there are actually more people in good healthcare, but we think they are in poor health care. 

### TO GENERATE ROC Curve ###
# y = TPR and X = 1 - FPR
# the higher the y is, the proportion of the poor care caught (favorable)
# the higher the x is, the higher the false positive rate is, the proportion of good care labeled as poor care

# always starts with (0,0) where the threshold is 1:
# it is very hard to be labeled as poor care, so you will capture all of the good care cases.

# and ends with (1,1) where the threshold is 0
# meaning that it is very easy to be labeled as poor care, so you will capture all of the poor care cases.

table(qualityTrain$PoorCare, predictTrain <= 1)
# Specificity = 1 - FPR = 1 - FP/(FP + TP) 
# y = 0 and x = 0 where TP = 0 and FPR = 0 -> FP = 0

table(qualityTrain$PoorCare, predictTrain <= 0)

#install.packages("ROCR")
library(ROCR)

ROCRpred= prediction(predictTrain, qualityTrain$PoorCare)
ROCperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCperf)

plot(ROCperf, colorize=TRUE)

plot(ROCperf, colorize=TRUE, print.cutoffs.at= seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc_train = as.numeric(performance(ROCRpred, "auc")@y.values)
auc_train
#auc is 0.7745946

### TEST DATASET ###
predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
summary(predictTest)

table(qualityTest$PoorCare, predictTest > 0.3)
# Sensitivity = TP/ Actual Positive = 6/8 = 0.75
# Specificity = TN / Actual Negative = 19/24 = 0.7916667

#ROC curve for testing data
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
ROCRpredTest
auc_test = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc_test
