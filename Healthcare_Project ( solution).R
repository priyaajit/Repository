setwd("/Users/priyaajit/Desktop/Simplilearn/R/Healthcare_dataset.xlsx")
getwd()
library(readxl)
read_excel("Healthcare_dataset.xlsx")->health
head(health)
View(health)

n = names(health)[!names(health)=="TOTCHG"]
n
for(col in n)
{
  print(col)
  print(table(health[,col]))
  cat("\n")
}

#To record the patient statistics, the agency wants to find the age category 
#of people who frequent the hospital and has the maximum expenditure.
library(dplyr)
table(health$AGE)
health%>%group_by(AGE)%>%summarize(total_expenditure = sum(TOTCHG))%>%mutate(ranking = rank(desc(total_expenditure)))%>%filter(ranking==1)

hist(health$AGE, col = "orange", main = "Expense", breaks = 10, 
     xlab = "Age", freq = T, labels = T, ylim = c(0,500), xlim = c(0,20))
# Analysis:-
#The graph show that total expenditure for infants are maximum 

#Q2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, 
#the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.

aggregate(health[,c("LOS", "TOTCHG")], by = list(health$APRDRG), mean,na.rm=T)->t
max(t)

# Analysis :- 
#The group with Maximum number of stay will have high total expenditure so looking at the table we infer that 
# Group 640 has the highest no of stay and expense. 

res <- aggregate(LOS ~ APRDRG, data = health, mean)
res
which.max(res$LOS)
res[which.max(res$LOS),]

res <- aggregate(TOTCHG ~ APRDRG, data = health, mean)
res
which.max(res$TOTCHG)
res[which.max(res$TOTCHG),]

#Q3. To make sure that there is no malpractice, the agency needs 
#to analyze if the race of the patient is related to the hospitalization costs.
health$TOTCHG
health$RACE
table(health$RACE)
which(is.na(health$RACE))
class(health$RACE)
health$RACE[is.na(health$RACE)] = median(health$RACE,na.rm = T)
sum(is.na(health$RACE))
#anova model can be a good tool to make a comparison between the categorical varibale(RACE) 
#and the numerical variable (total charge)
aov(TOTCHG~RACE, data = health)->result
summary(result)
#Analysis : - 
#The probability value is very low which means that there is no significant relation
#between the race and the total charge 

#we can also do a LM method to check if race can be  feature for determination of the total charge 
lm(TOTCHG~RACE, data = health)->result
summary(result)
# Analysis :-
#when we do a linear regression, we see that the adjusted R square is very low.
#hence we say that race is not a significant variable in determining the 
#total charge.
#we can also do a quick visualisation . 

tapply(health$TOTCHG, list(health$RACE), mean)->tab
barplot(height = tab, col = c("pink","blue","green","yellow","red","orange"),
        xlab = "Race", ylab = "TotaL Charge")
#Analysis of Visualization : - 
#We see that there is no significant dffernece in the cost paid by different races . 

#Q4. To properly utilize the costs, the agency has to analyze the severity of 
#the hospital costs by age and gender for the proper allocation of resources.

aov(TOTCHG ~ AGE + FEMALE, health)->results
summary(results)
#Analysis :-
#Age and female both as per the Anova model are leading to a signifcant variance in the total charge 
#with age having more significance as compared to females. 

lm(TOTCHG ~ AGE + FEMALE, health)->results
summary(results)
#Analysis : - 
#By checkin  the prediction results through leniar regression model, we see that 
#age has a stong impact in the prediction of total charge 

# Visulaisation for showing a strong relation between age and total charge 
tapply(X = health$TOTCHG,INDEX = list(health$AGE), mean)->tab
class(tab)
barplot((height = tab), xlab = "Age", ylab = "Total Charge")

#Total charge vs Female 
health%>%group_by(FEMALE)%>%summarize(TOTCHG = mean(TOTCHG, na.rm = T))->p
View(p)
as.matrix(p$TOTCHG)->mat
rownames(mat) = p$FEMALE
mat
barplot(height = mat[,1], col = c("pink", "lightblue"), xlab = "female", ylab = "Total Charge")
legend(x = .25, y = 500,legend = c("female","male"), col = c("pink", "lightblue"), fill = c("pink", "lightblue"), adj = c(0, 0.5)) 
#Analysis  - 
#The barplot shows that females have a higher impact on the total charge determination compared to the males 

#Q5. Since the length of stay is the crucial factor for inpatients, the agency 
#wants to find if the length of stay can be predicted from age, gender, and race.
#beacuse it is prediction then we use LM 
model  = lm(LOS~AGE+FEMALE+RACE, health)
summary(model)
#Analysis :- 
#rsquared value is very smallaround 17%. So no these features 
#can't help in prediction

#Q6. To perform a complete analysis, the agency wants to find the variable that 
#mainly affects hospital costs.

model  = aov(TOTCHG~., health)
summary(model)

#Analysis :- we see that age and APRERG has a strong impact on the prediction of hospital cost with a significance 
#of .01% and also the probability for these two variable is the lowest 
