getwd()
read.csv("College_admission.csv")->college
head(college)
View(college)

#Looking at the Y variable(binary) which is admit we infer that it is a calssification probelm.

#Find the missing values. (if any, perform missing value treatment)
sum(is.na(college))
# there is no missing values in the dataset 

#Find outliers (if any, then perform outlier treatment)
summary(college)
#Find the structure of the data set and if required, transform the numeric data type to
#factor and vice-versa.
str(college)

#Find whether the data is normally distributed or not. Use the plot to determine the same. 
hist(college$gre, breaks = 30, col = "#ded4ef", main = "GRE score",
     xlab = "Gre", ylab="Frequency Density",probability = T)
lines(density(college$gre))

hist(college$gpa, breaks = 30, col = "green", main = "GPA score",
     xlab = "GPA", ylab="Frequency Density",probability = T)
lines(density(college$gpa))

summary(college)

#Normalize the data if not normally distributed.
#normaliztion of data is important as we see that all the colums are categorical and gre and gpa cols are 
#numericals so this can unduly influence the model so we need to scale the Gre and Gpa cols 
#install.packages("caret",dependencies = T)
str(college)
library(dplyr)
college = as_tibble(college)# convert it into tibble format to view the dataset clearly
college%>%select(2:3)%>%mutate(gre_scale = (gre - mean(gre))/sd(gre), 
                                         gpa_scale = scale(gpa))->col
#there are two ways of scalling the value either the way used to scale gre or simply use scale 
hist(col$gre_scale, breaks = 10, col = "blue", main = "GRE score",
     xlab = "Gre", ylab="Frequency Density",probability = T)
lines(density(col$gre_scale))

hist(col$gpa_scale, breaks = 10, col = "red", main = "GPA score",
     xlab = "GPA", ylab="Frequency Density",probability = T)
lines(density(col$gpa_scale))

#Use variable reduction techniques to identify significant variables.
#modal.matrix will not create dummies for numeric or integer column so we will have to convert the numeric colums
#into factor colms
str(college)
college$ses = as.factor(college$ses)
college$Race = as.factor(college$Race)
model.matrix(object = admit~., data = college)[,-1]->college_mat
View(college_mat)
college_mat = as.data.frame(college_mat)
cbind(college$admit, college_mat)->college
names(college)[1] ="admit"
View(college)
str(college)
#install.packages("caTools", dependencies = T)
library(caTools)
college$admit->y
set.seed(10)
sample.split(Y = y, SplitRatio = .7)->v
training = college[v,]
test = college[!v,]
dim(training)
dim(test)
#traning the logistic regression model
glm(admit~rank, family = binomial, data = training)->modal
summary(modal)
# rank of the college is a significant variable in determining addmission the AIC 331.93 , the lower the AIC the better 
#is the model 

#multiple logistic regression model
glm(admit~., family = binomial, data = training)->model
summary(model)
# AIC 327.23 is reduced
#the most significatn variable for the prediction of the admission is Rank of the college ,followed by the 
#gpa score of the student . 

#Now i use the step fnction to take off the insignificant variable from the model 

step(model, direction = "backward")->model_backward
summary(model_backward) 

step(model, direction = "backward")->model_forward
summary(model_forward) 

step(object = model, direction = "both")->model_both
summary(model_both)
# according to the step function , the most significant variable in determining the admission is the rank of the 
#college, gre score followed by the race 2 and finally GPA . the GIC value is 321.47 which is less than the 
#previous AIC value. 

#so the model selected is model_both but we will check if there is any multicolinealiy using
#cross validation.
#install.packages("car",dependencies = T)
library(car)
vif(model_both)

# now we need to predict the best choosed model "model_both" on the test data and see the results 
predict(object = model_both, newdata = test, type = "response")->prob_value
# this gives me the probability values of the model which should be less then .5 
ypred_test = ifelse(prob_value>=.5,1,0)
ypred_test

#checking the classification accuracy by creating the confusion matrix 
table(actual= test$admit , predicted = ypred_test)->confusion_matrix
confusion_matrix
TN = confusion_matrix['0','0'] #correctly predicted as -ve
TP = confusion_matrix[2,2] #correct prediction as +ve
FP = confusion_matrix[1,2] #incorrectly predicted as +ve
FN = confusion_matrix[2,1] #incorrectly predicted as -ve
accuracy = (TP + TN)/(TP+TN + FP + FN)
accuracy

# the accuracy of my logistic regression model is 68% 

# checking the data using other machine learning modal as the score from the logistic regression
#is quite low. for a ML modal to be good the score should be atleast above 80%

#Decision Tree - uses the entropy and ginie index 
# in general decision tree do not require any preprocessing of data but it is a good thing to proprocess and 
#dummy encode the data 
#converting all the 
#install.packages("rpart",dependencies = T)
str(college)
factor_vars = c("admit", "ses2", "ses3", "Gender_Male", "Race2", "Race3", "rank")
college[,factor_vars]=as.data.frame(lapply(college[,factor_vars], as.factor))
str(college)
#train test split 
library(caTools)
college$admit->y
set.seed(10)
sample.split(Y = y, SplitRatio = .7)->v
training = college[v,]
test = college[!v,]
dim(training)
dim(test)
library(rpart)
model = rpart(as.factor(admit)~., training)
library(rpart.plot)
prp(model)
model
# decision trees are prones to overfitting so we prune the tree 
plotcp(model)# plotting the complexity parameter of the model 
mod = prune(tree = model, cp = .022)
prp(mod)

#prediction on the test data to check the accuracu of the above model 
#type= class to get the predictions and type = response to get the probability values 
predict(object = model, newdata = test, type = "class")->ypred_test
table(actual= test$admit , predicted = ypred_test)->confusion_matrix
confusion_matrix
TN = confusion_matrix['0','0'] #correctly predicted as -ve
TP = confusion_matrix[2,2] #correct prediction as +ve
FP = confusion_matrix[1,2] #incorrectly predicted as +ve
FN = confusion_matrix[2,1] #incorrectly predicted as -ve
accuracy = (TP + TN)/(TP+TN + FP + FN)
accuracy

#use the pruned model to chk the accuracy 
predict(object = mod, newdata = test, type = "class")->ypred_test
table(actual= test$admit , predicted = ypred_test)->confusion_matrix
confusion_matrix
TN = confusion_matrix['0','0'] #correctly predicted as -ve
TP = confusion_matrix[2,2] #correct prediction as +ve
FP = confusion_matrix[1,2] #incorrectly predicted as +ve
FN = confusion_matrix[2,1] #incorrectly predicted as -ve
accuracy = (TP + TN)/(TP+TN + FP + FN)
accuracy
#Accurancy is 70%
#using unpruned model the accuracy was 65% and using the pruned model the accuracy improved to 70%. this
#shows that pruning is helping me in handling or reducing the error .

# Random forest
#install.packages("randomForest", dependencies = T)
library(randomForest)
set.seed(100)
sample.split(Y = y, SplitRatio = .7)->v
training = college[v,]
test = college[!v,]
dim(training)
dim(test)
model = randomForest(as.factor(admit)~., data = training, ntree = 500, mtry = 3)
#mtry - as max features - sqrt(n_features) or log(n_features)
predict(object = model, newdata = test, type = "class")->ypred_test
table(actual= test$admit , predicted = ypred_test)->confusion_matrix
confusion_matrix
TN = confusion_matrix['0','0'] #correctly predicted as -ve
TP = confusion_matrix[2,2] #correct prediction as +ve
FP = confusion_matrix[1,2] #incorrectly predicted as +ve
FN = confusion_matrix[2,1] #incorrectly predicted as -ve
accuracy = (TP + TN)/(TP+TN + FP + FN)
accuracy
#accurancy for the model is 69%
# the accuracy for random forest is reduced from the decision tri model 

#I will check the accuracy on the naive bayes model

str(college)
library(dplyr)
college = as_tibble(college)# convert it into tibble format to view the dataset clearly
college%>%select(2:3)%>%mutate(gre_scale = (gre - mean(gre))/sd(gre), 
                               gpa_scale = scale(gpa))->col

cbind(college, col)->col_stand
View(col_stand)
which(colnames(col_stand) == "gre")
col_stand = col_stand[, -10]
which(colnames(col_stand) == "gpa.1")
col_stand = col_stand[, -10]
dim(col_stand)

col_stand$admit->y
set.seed(10)
sample.split(Y = y, SplitRatio = .7)->v
training =col_stand[v,]
test = col_stand[!v,]
dim(training)
dim(test)
str(training)
library(e1071)
model = naiveBayes(x = col_stand[, -1], 
                   y = as.factor(y))
predict(model, newdata = test, type = "class")->ypred_test
table(actual = test$admit, predicted = ypred_test)->confusion_matrix
confusion_matrix
TN = confusion_matrix['0','0'] #correctly predicted as -ve
TP = confusion_matrix[2,2] #correct prediction as +ve
FP = confusion_matrix[1,2] #incorrectly predicted as +ve
FN = confusion_matrix[2,1] #incorrectly predicted as -ve
accuracy = (TP + TN)/(TP+TN + FP + FN)
accuracy
#accuracy score - 65% which is less than the decision tree
# accuracy of the naive bayes is 

# checking the accuracy with SVM model
library(e1071)
model = svm(formula = as.factor(admit)~., data = training,
            kernel = "linear")
predict(model, newdata = test, type = "class")->ypred_test
table(actual = test$admit, predicted = ypred_test)->confusion_matrix
confusion_matrix
TN = confusion_matrix['0','0'] #correctly predicted as -ve
TP = confusion_matrix[2,2] #correct prediction as +ve
FP = confusion_matrix[1,2] #incorrectly predicted as +ve
FN = confusion_matrix[2,1] #incorrectly predicted as -ve
accuracy = (TP + TN)/(TP+TN + FP + FN)
accuracy
#Accuracy of SVM model is 73%
# So we select the SVM model as the accuracy is best for this one. 


