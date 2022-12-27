data=read.csv("IT jobs.csv")
if(!require("ggplot2")){
    install.packages("ggplot2")
    library(ggplot2)
}
if(!require("Hmisc")){
  install.packages("Hmisc")
  library(Hmisc)
}
describe(data)
summary(data)
str(data)
attach(data)
if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}

#bar chart to represent Programming language used Vs count 
p <- ggplot(data, aes(Marker_icon))
print(p+geom_bar() + ggtitle("The Frequency of each Marker Icon in Poland")+labs(y = "Frequency", x = "Marker Icon"), col="lightblue")
# pie chart (workplace type : remote, f2f, partially f2f)
if(!require("DescTools")){
  install.packages("DescTools")
  library(DescTools)
}
if(!require("graphics")){
  install.packages("graphics")
  library(graphics)
}
# Workplace type in general 
freq_of_wdType=table(Workplace_type) #frequency table 
pie(freq_of_wdType,col=c("pink","lightgray","lightblue")) 
myLabels=names(freq_of_wdType)
myLabels
pct=prop.table(freq_of_wdType)*100
pct #percentage 
myLabels_round=paste(round(pct),"%",sep="")
myLabelsfinal=paste(myLabels,myLabels_round,sep=" ")
myLabelsfinal
pie(freq_of_wdType,labels=myLabelsfinal)

permanent_job1=data[which(salary_from_permanent>0),]
permanent_job1
summary(permanent_job1)

#Workplace type in permanent job 
freq_of_wdType_permanent=table(permanent_job1$Workplace_type) #frequency table 
pie(freq_of_wdType_permanent,col=c("pink","lightgray","lightblue")) 
pct2=prop.table(freq_of_wdType_permanent)*100
pct2 #percentage 
myLabels2_round=paste(round(pct2),"%",sep="")
myLabelsfinal_permanent=paste(myLabels,myLabels2_round,sep=" ")
myLabelsfinal_permanent
pie(freq_of_wdType_permanent,labels=myLabelsfinal_permanent)


f=Freq(Experience_level)
view(f) #view the complete frequency table of experience level

perm_labels=c("permanent","not permanent")
pie_permanent=table(if_permanent)
pie_permanent
pie_p=prop.table(pie_permanent)*100  #percentage
pie_p_round=paste(round(pie_p),"%",sep="") #rounding the percentage
pie_p_final=paste(perm_labels,pie_p_round,sep=" ")
print(pie(pie_permanent,labels=pie_p_final,col=c("#4682B433", "#4682B4")),title("Is the job permanent?")) #pie chart 


#frequency table of the experience level in the permanent job 
Experience_level_table=Freq(permanent_job1$Experience_level)
View(Experience_level_table)

#categorizing experience level in separate data frames
seniors=subset(permanent_job1,Experience_level=="senior")
mids=subset(permanent_job1,Experience_level=="mid")
juniors=subset(permanent_job1,Experience_level=="junior")

numOflevels=sum(!is.na(permanent_job1$Experience_level)) 

#then calculating the average of each level 

avr_seniors_skillvalues=mean(seniors$skills_value_0+seniors$skills_value_1+seniors$skills_value_2)
avr_seniors_skillvalues
avr_mid_skillvalues=mean(mids$skills_value_0+mids$skills_value_1+mids$skills_value_2)
avr_mid_skillvalues
avr_junior_skillvalues=mean(juniors$skills_value_0+juniors$skills_value_1+juniors$skills_value_2)
avr_junior_skillvalues



#ranges of permanent salary 
senior_permanentrange_to=max(seniors$salary_to_permanent)-min(seniors$salary_to_permanent)
senior_permanentrange_from=max(seniors$salary_from_permanent)-min(seniors$salary_from_permanent)
mid_permanentrange_to=max(mids$salary_to_permanent)-min(mids$salary_to_permanent)
mid_permanentrange_from=max(mids$salary_from_permanent)-min(mids$salary_from_permanent)
junior_permanentrange_to=max(juniors$salary_to_permanent)-min(juniors$salary_to_permanent)
junior_permanentrange_from=max(juniors$salary_from_permanent)-min(juniors$salary_from_permanent)

cat ("Range of seniors' salary from permanent =", senior_permanentrange_from)
cat ("Range of seniors' salary to permanent =", senior_permanentrange_to)
cat ("Range of mids' salary from permanent =", mid_permanentrange_from)
cat ("Range of mids' salary to permanent =", mid_permanentrange_to)
cat ("Range of Juniors' salary from permanent =", junior_permanentrange_from)
cat ("Range of juniors' salary to permanent =", junior_permanentrange_to)


#sum of the skill values 
totalskills=permanent_job1$skills_value_0+permanent_job1$skills_value_1+permanent_job1$skills_value_2
totalskills
senior_totalskills=seniors$skills_value_0+seniors$skills_value_1+seniors$skills_value_2
senior_totalskills
mid_totalskills=mids$skills_value_0+mids$skills_value_1+mids$skills_value_2
mid_totalskills
junior_totalskills=juniors$skills_value_0+juniors$skills_value_1+juniors$skills_value_2
junior_totalskills

#box plot of permanent salary (upper and lower) and finding the outliers 

boxplot(permanent_job1$salary_from_permanent, main= "Salary from permentant")
boxplot(permanent_job1$salary_to_permanent, main=" Salary to permentant")

#trimming the outliers
boxplot(permanent_job1$salary_from_permanent, main= "Salary from permentant",outline=FALSE)
boxplot(permanent_job1$salary_to_permanent, main=" Salary to permentant",outline = FALSE)

p <- ggplot(permanent_job1, aes(totalskills))
p + geom_histogram(bins=16)
print(p + geom_histogram(bins=16)+labs(y = "Frequency", x = "Total skill values")+ggtitle("Total skill values Vs the Frequency"))


#scatter plots between skills and salary
plot(totalskills,permanent_job1$salary_from_permanent,main="Plot the total value of the skills and salary from permanent",
     xlab="Total value of the 3 skills",ylab="Salary from permanent", pch=17)
plot(totalskills,permanent_job1$salary_to_permanent,main="Plot the total value of the skills and salary to permanent",
     xlab="Total value of the 3 skills",ylab="Salary from permanent", pch=17)




#contingency table about Exp Experience level and workplace type for people in permanent job 
table(Workplace_type)
table(Workplace_type,Experience_level)
t <- table(Experience_level,Workplace_type,salary_from_permanent>0)
view(t)
ftable(t)



#numerical summary 

mean(seniors$salary_from_permanent)
mean(seniors$salary_to_permanent)
mean(mids$salary_from_permanent)
mean(mids$salary_to_permanent)
mean(juniors$salary_from_permanent)
mean(juniors$salary_to_permanent)
#---------
var(seniors$salary_from_permanent)
var(seniors$salary_to_permanent)
var(mids$salary_from_permanent)
var(mids$salary_to_permanent)
var(juniors$salary_from_permanent)
var(juniors$salary_to_permanent)
#------

sd(seniors$salary_from_permanent)
sd(seniors$salary_to_permanent)
sd(mids$salary_from_permanent)
sd(mids$salary_to_permanent)
sd(juniors$salary_from_permanent)
sd(juniors$salary_to_permanent)

#---------------------


