# to find the web activites from the dataset so the Y variable can be visits which is a contineous column 
#hence it is a regrssion problem. so we can use the ML models like the linear regression and SVM
getwd()
library(readxl)
read_excel(path = "/Users/priyaajit/Desktop/Simplilearn /Dataset/1555058318_internet_dataset.xlsx")->internet
View(internet)
str(internet)
sum(is.na(internet))
summary(internet)

#The team wants to analyze each variable of the data collected through data summarization to 
#get a basic understanding of the dataset and to prepare for further analysis.

#visits from continents
table(internet$Continent)
slices =  c(321, 3171, 6470, 20043, 1356, 748)
pct = round(slices/sum(slices)*100)
pct
lbls= paste(c("AF", "AS", "EU", "N.America", "OC", "SA"), pct, "%", sep = " ")
lbls
pie(slices, labels = lbls, col = c("red","blue","green","yellow","orange","black"), main = "Freequency Distribution")

# the pie chart represents clearly that 62% of the visitors to the website is from North america followed by 
#20% Europeen union whereas the minimum of 1% coming from Africa 

#visits from sources 
table(internet$Sourcegroup)
slices =  c(7532, 92, 11542, 5360, 1354, 616, 2249, 2388, 976)
pct = round(slices/sum(slices)*100)
pct
lbls= paste(c("Direct", "Facebook", "Google", "others", "public.tableau software", "reddit","t.co", "tableausoftware", "visualisingdata"), 
            pct, "%", sep = " ")
lbls
pie(slices, labels = lbls, col = rainbow(7), main = "Freequency Distribution")

# 36% of the visitors find the website through google search, and around 23% are the direct visitors . 
#A very minimal ratio coming from facebook. 

#Unique Views
table(internet$Uniquepageviews)->g
g
barplot(g)
# the unique visitors shows that the maximum times the visit is on the first page of the website 
#and it decreases to the following pages.

# time spent per page 
table(internet$Timeinpage,internet$Uniquepageviews)->d
d
barplot(height = d, main = "Time spent per page", ylab = "Time (sec)", xlab = "Page views", 
        col= c("red","green","yellow","blue","orange","pink"))
# the bargraph shows that the time spent by each visitor on the webpages and we see that the maximum time 
#is spent on the first page of the website and the time spent decreases with the subsequent pages.

#As mentioned earlier, a unique page view represents the number of sessions during which 
#that page was viewed one or more times. A visit counts all instances, no matter how many 
#times the same visitor may have been to your site. So the team needs to know whether 
#the unique page view value depends on visits.

library(dplyr)
unlist(lapply(internet, is.numeric))
#install.packages("quantable",dependencies = T)

internet%>%select_if(is.numeric)%>%mutate(visit =(Visits- min(Visits))/(max(Visits) - min(Visits)),
                                          uniq = (Uniquepageviews- min(Uniquepageviews))/(max(Uniquepageviews) - min(Uniquepageviews)),
                                          bounces = (Bounces- min(Bounces))/(max(Bounces) - min(Bounces)),
                                          exit = (Exits- min(Exits))/(max(Exits) - min(Exits)),
                                          time = (Timeinpage- min(Timeinpage))/(max(Timeinpage) - min(Timeinpage)),
                                          b_new = (BouncesNew- min(BouncesNew))/(max(BouncesNew) - min(BouncesNew)))%>%
  select(-c(Uniquepageviews,Visits,Bounces,Exits,Timeinpage,BouncesNew))->internet_scale
cbind(internet$Sourcegroup, internet$Continent,internet_scale)->internet_scale
names(internet_scale)[1] = "sourcegroup"
names(internet_scale)[2] = "continent"

#removing the redundant columns
internet_scale$`internet$Sourcegroup` = NULL
internet_scale$`internet$Continent` = NULL
View(internet_scale)

#converting the charecter columns into factor columns
internet_scale$continent = as.factor(internet_scale$continent)
internet_scale$sourcegroup = as.factor(internet_scale$sourcegroup)
summary(internet_scale)
str(internet_scale)

## dummy encoding the factor variables, the dataset has only 2 colms as factor columns

model.matrix(object = visit~., data = internet_scale)->internet_mat
View(internet_mat)
internet_mat = as.data.frame(internet_mat)
View(internet_mat)
internet_mat[-1]->internet_mat
cbind(internet_scale$visit, internet_mat)->internet_mat
View(internet_mat)
names(internet_mat)[1] = "visit"


lm(formula = visit~uniq, data = internet_mat)->model
summary(model)

# according to the linear regression model the unique visit depends on the visit and we can 
#say that with  a confidence of 99.999% . the probability value is .01%


#Find out the probable factors from the dataset, which could affect the exits. 
#Exit Page Analysis is usually required to get an idea about why a user leaves the 
#website for a session and moves on to another one. Please keep in mind that exits 
#should not be confused with bounces.

lm(formula = exit~., data = internet_mat)->model1
summary(model1)
# the most probable factors that influence the exits from the page could be sourcegroupgoogle
#unique and bounces, time , sourcegroupvisualdata.com, sourcegrouptableausoftware.com 
#& sourcegrouppublc.tableausoftware.com. 

lm(formula = exit~bounces+uniq+sourcegroupgoogle+sourcegrouppublic.tableausoftware.com+
     sourcegrouptableausoftware.com+sourcegroupvisualisingdata.com+ continentEU+visit+
     sourcegroupfacebook+sourcegroupt.co, 
   data = internet_mat)->model2
summary(model2)

#i choose the factors manually to see if the r quared value increases and we see that 
#these factors lead to better R squared value of 79%. and adding more variable beyond
#this point does not lead to increase in the score. 

#Every site wants to increase the time on page for a visitor. 
#This increases the chances of the visitor understanding the site content better 
#and hence there are more chances of a transaction taking place. 
#Find the variables which possibly have an effect on the time on page.

lm(formula = time~., data = internet_mat)->model3
summary(model3)

# the factors that effect the time on page are visits ,continent SA, unique visit ,
#bounces and exit.

#A high bounce rate is a cause of alarm for websites which depend on visitor 
#engagement. Help the team in determining the factors that are impacting the bounce.
lm(formula = bounces~., data = internet_mat)->model4
summary(model4)

# this leads to a adjustd r square value of 100% so we will choose the features 
#through step model and multicolinearity

step(object = model4, direction = "both")->mod
summary(mod)

#chking for multi colinearity
library(car)
vif(mod)

#we choose the variable which have a probability value of less than 2.5 and 5 for a better 
#results or r squared value 
#sourcegroupfacebook,sourcegroupgoogle,sourcegroupothers, 
#sourcegrouppublic.tableausoftware.com, sourcegroupreddit.com,sourcegroupt.co,
#sourcegrouptableausoftware.com,sourcegroupvisualisingdata.com,continentOC,time

lm(formula = bounces~sourcegroupfacebook+sourcegroupgoogle+sourcegroupOthers+ 
   sourcegrouppublic.tableausoftware.com+sourcegroupreddit.com+
   sourcegrouptableausoftware.com+sourcegroupvisualisingdata.com+continentOC+time+
    exit+uniq,
   data = internet_mat)->model5
summary(model5)


