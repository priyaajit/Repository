getwd()
read.csv("Insurance_factor_identification.csv")->insurance
head(insurance)
#View(insurance)
str(insurance)
library(dplyr)
library(plotrix)
#install.packages("scales", dependencies = T)
library(scales)
library(corrplot)

#The committee is interested to know each field of the data collected through descriptive 
#analysis to gain basic insights into the data set and to prepare for further analysis.  
par(mfrow = c(2,2))

insurance$Claims = as.numeric(insurance$Claims)
insurance$Payment = as.numeric(insurance$Payment)
summary(insurance)

#contineous & factor variable - we use the barplot 
tapply(X = insurance$Payment, list(insurance$Bonus), mean)->f
f
barplot(height = f,  col = c("red","blue","green","orange","black", "yellow"))
#Analysis’
#There is a variation between the payment and the claims . the payment differs with the considerably with  claims . So we consider this 
#feature for future analysis 

#factor and factor columns can  be visulaised using a stacked barplot 
table(insurance$Bonus, insurance$Zone)->tab
barplot(tab, beside = T)
#there is no significant variation between Bonus and zone so we reject this feature for future analysis.

tapply(X = insurance$Payment, list(insurance$Make), mean)->t
t
barplot(height = t)
# we can consider this feature as we see that the insurance payment increases with the vehicle type . As 
#per the data the model 9 is the high end vehicle so the payment for it remains high as compared to the other vehicles 
#so we can keep this feature for the future analysis  

tapply(X = insurance$Payment, list(insurance$Zone), mean)->r
r
barplot(height = r)

# we see some variation between paymet and zone as well . the payment for zone 1 to  4 is high as compared to the 
#to the other zones. so we can keep this feature for future analysis . 
tapply(X = insurance$Insured, list(insurance$Claims), mean)->g
g
barplot(height = g)

# the insured amout increases with the increase in the claims so this can be a considerable feature for 
#future analysis. 

tapply(X = insurance$Insured, list(insurance$Bonus), mean)->h
h
barplot(height = h)

dev.off()

##The total value of payment by an insurance company is an important factor to be monitored.
#So the committee has decided to find whether this payment is related to the number of claims 
#and the number of insured policy years.
#They also want to visualize the results for better understanding. 

#Payment relation to the number of claims 
insurance%>%group_by("Payment", "Claim")%>%arrange(desc(Payment))->x
x
plot(x = insurance$Payment, 
     y = insurance$Claims, xlab = "Payment", 
     main = "Scatter Plot", ylab = "Claim", 
     col = c("red", "blue", "yellow","green"))

cor(insurance$Payment, insurance$Claims)

#Does the payment vary with the number of Insured 
insurance%>%group_by("Payment", "Insured")%>%arrange(desc(Payment))->y
y
plot(x = insurance$Payment, 
     y = insurance$Insured, xlab = "Payment", 
     main = "Scatter Plot", ylab = "Insured", 
     col = c("red", "blue", "yellow","green"))
cor(insurance$Payment, insurance$Insured)
#Analysis:-
#We see that the corralation percentage between the payment and the claim is 99% 
# The correlations between the insured and Payment is also vry high (93%) and its visible from the linear bent of the 
#scatter Plot too . 

#Question
#The committee wants to figure out the reasons for insurance payment increase and decrease. 
#So they have decided to find whether distance, location, bonus, make, and insured amount or 
#claims are affecting the payment or all or some of these are affecting it. 

#We use anova test to do multi variate analysis 
aov(Payment~., data = insurance)->result
summary(result)

#Analysis :- We see that all these features are significantly leading to the variance in price 
#The probability value of (KM, zone, bonus, make, Insured & Claims) - 2x10(-16) 
#*** indicates that the prob value is between 0 and .1% . So we can say that 
# Km, Zone, Bonus, Make,Insured, Claims leads to the variance in payment , we can say this with a confidence of
#99.999999%
corrplot(cor(insurance))

#Question
#The insurance company is planning to establish a new branch office, 
#so they are interested to find at what location, kilometre, and bonus level their 
#insured amount, claims, and payment gets increased. (Hint: Aggregate Dataset) 

aggregate(insurance[,c("Insured", "Claims", "Payment")], by = list(insurance$Zone), mean)
par(mfrow = c(1,3))
plot(x = insurance$Zone, 
     y = insurance$Insured, xlab = "Zone", 
     main = "Scatter Plot", ylab = "Insured", 
     col = c("red", "blue", "yellow","green"))

plot(x = insurance$Zone, 
     y = insurance$Claims, xlab = "Zone", 
     main = "Scatter Plot", ylab = "claims", 
     col = c("red", "blue", "yellow","green"))

plot(x = insurance$Zone, 
     y = insurance$Payment, xlab = "Zone", 
     main = "Scatter Plot", ylab = "Payment", 
     col = c("red", "blue", "yellow","green"))

#for zone 2 insured amount is highest ,for zone 4 the claim is highest and Zone 6 has the highest payment 

aggregate(insurance[,c("Insured", "Claims", "Payment")], by = list(insurance$Bonus), mean)

plot(x = insurance$Bonus, 
     y = insurance$Insured, xlab = "Bonus", 
     main = "Scatter Plot", ylab = "Insured", 
     col = c("red", "blue", "yellow","green"))

plot(x = insurance$Bonus, 
     y = insurance$Claims, xlab = "Bonus", 
     main = "Scatter Plot", ylab = "claims", 
     col = c("red", "blue", "yellow","green"))

plot(x = insurance$Bonus, 
     y = insurance$Payment, xlab = "Bonus", 
     main = "Scatter Plot", ylab = "Payment", 
     col = c("red", "blue", "yellow","green"))


##Insured amount, no claim bonus and payment is higest in the 7th year of non claim bonus

aggregate(insurance[,c("Insured", "Claims", "Payment")], by = list(insurance$Kilometres), mean)


plot(x = insurance$Kilometres, 
     y = insurance$Insured, xlab = "Kilometers", 
     main = "Scatter Plot", ylab = "Insured", 
     col = c("red", "blue", "yellow","green"))

plot(x = insurance$Kilometres, 
     y = insurance$Claims, xlab = "Kilometers", 
     main = "Scatter Plot", ylab = "claims", 
     col = c("red", "blue", "yellow","green"))

plot(x = insurance$Kilometers, 
     y = insurance$Payment, xlab = "Kilometers", 
     main = "Scatter Plot", ylab = "Payment", 
     col = c("red", "blue", "yellow","green"))

#iInsured amount is high for the group 1 (1km) ,whereas the claims increase group 2 (2KM) , 
#payment is high for Group 2 (2KM)

#The committee wants to understand what affects their claim rates so as to decide the 
#right premiums for a certain set of situations. Hence, they need to find whether the 
#insured amount, zone, kilometre, bonus, or make affects the claim rates and to what extent. 
aov(Claims~., data = insurance)->results
summary(results)

#Analysis:-
#We tried to see the relation between claim with the other features and we see that the Km , zone,no claim bonus,
#Make of the vehicle , insured amoutn and payment has a significant level of 0 to 01% which means that the confidence 
#level is as high as 99.99% . 

