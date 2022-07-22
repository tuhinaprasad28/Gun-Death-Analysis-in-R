#title: "IDS 572 Assignment 1"
#author: "Tuhina, UIN: 655777506 And Urjit Amitvikram Kurulkar, UIN:658597085"
#Q1. Libraries
library(ggplot2)
library(kableExtra)
library(lares)
library(plyr)
library(reshape2)
library(digest)
library(party)
library(rpart)
library(e1071)
library(caret)
library(rpart.plot)
library(tidyverse)
library(arules)
library(arulesViz)
library(rattle)
library(tree)

##Getting the Data
gun_death.data = read.csv("gun_deaths.csv")
#Creating a Data Frame
gun_death.data.df <- data.frame(gun_death.data)

#Data Frame for Months
gun_deaths.month.df <- data.frame(gun_death.data$month)
#Table for month deaths
month_deaths <- table(gun_death.data.df$month)

#Creating a Data Frame for Month Deaths
month_deaths.df <- data.frame(month_deaths)
#Using Kable Function for creating an XML
kable(month_deaths.df, "pipe", col.names = c("Month", "Deaths"), align = c("l","c"))

#b)
#Creating a vector of Month Names
month_name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

month_deaths.df$month_name  <- month_name 

#Plotting a Bar Chart
barplot(height = month_deaths.df$Freq, names= month_deaths.df$month_name)

#c) 
#Creating a data frame for Intent of Death
Death_Intent <- data.frame(gun_death.data.df$intent)

intent <- table(Death_Intent)

intent.df <- data.frame(intent)

ggplot(intent.df, aes(x = reorder(Death_Intent, -Freq), Freq)) + geom_bar(stat = "identity")

#d)
#Creating a data frame for Sex
sex_df <- data.frame(gun_death.data.df, sex = sample(c("M","F")))

table(gun_death.data.df$sex, gun_death.data.df$age)

T.male <- subset(gun_death.data, sex == "M")
T.female <- subset(gun_death.data , sex == "F")  

Female.df <- data.frame(T.female)

mean(T.female$age)

ggplot(gun_death.data.df) + geom_boxplot(aes(x=age, y=sex)) + coord_flip() +
  geom_boxplot(aes(x=age, y=sex)) +
  labs(x = "Age", y = "Sex", title = "Age of Gun Death victims by sex")

summary(Female.df$age)

mean(Female.df$age, na.rm = TRUE)

mean(T.male$age, na.rm = TRUE)

#e)
#No. of White Males with at least a high school education which were killed by guns in 2012
sum(gun_death.data.df$year == 2012 & gun_death.data.df$race == "White" & gun_death.data.df$education == "HS/GED" & gun_death.data.df == "M", na.rm = TRUE )


#f)
winter = sum(gun_death.data.df$month == 1 | gun_death.data.df$month == 2 | gun_death.data.df$month == 3, na.rm = TRUE)

spring = sum(gun_death.data.df$month == 4 | gun_death.data.df$month == 5 | gun_death.data.df$month == 6, na.rm = TRUE)

summer = sum(gun_death.data.df$month == 7 | gun_death.data.df$month == 8 | gun_death.data.df$month == 9, na.rm = TRUE)

fall = sum(gun_death.data.df$month == 10 | gun_death.data.df$month == 11 | gun_death.data.df$month == 12, na.rm = TRUE)



season = cut(gun_death.data.df$month, breaks = c(1,2,3,Inf)) 



gun_death.data.df$season[gun_death.data.df$month == 1 | gun_death.data.df$month == 2 | gun_death.data.df$month == 3 ] = 'Winter'
gun_death.data.df$season[gun_death.data.df$month == 4 | gun_death.data.df$month == 5 | gun_death.data.df$month == 6 ] = 'Spring'
gun_death.data.df$season[gun_death.data.df$month == 7 | gun_death.data.df$month == 8 | gun_death.data.df$month == 9 ] = 'Summer'
gun_death.data.df$season[gun_death.data.df$month == 10 | gun_death.data.df$month == 11 | gun_death.data.df$month == 12 ] = 'Fall'



table(gun_death.data.df$season)
#g
Q7 = data.frame(c(gun_death.data.df$race, gun_death.data.df$intent))



White_suicide <- sum(gun_death.data.df$race == "White" & gun_death.data.df$intent == "Suicide" , na.rm = TRUE )
White_homicide <- sum(gun_death.data.df$race == "White" & gun_death.data.df$intent == "Homicide" , na.rm = TRUE )

Black_suicide <- sum(gun_death.data.df$race == "Black" & gun_death.data.df$intent == "Suicide" , na.rm = TRUE )
Black_homicide <- sum(gun_death.data.df$race == "Black" & gun_death.data.df$intent == "Homicide" , na.rm = TRUE )

Hispanics_suicide <- sum(gun_death.data.df$race == "Hispanic" & gun_death.data.df$intent == "Suicide" , na.rm = TRUE )
Hispanics_homicide <- sum(gun_death.data.df$race == "Hispanic" & gun_death.data.df$intent == "Homicide" , na.rm = TRUE )

suicide = c(White_suicide,Black_suicide,Hispanics_suicide)

homicide = c(White_homicide,Black_homicide,Hispanics_homicide)

Intent_SH = data.frame(suicide,homicide)

names(Intent_SH) = c("Suicide", "Homicide")

barplot(height = as.matrix(Intent_SH), beside = TRUE ,col = rainbow(3))

legend("topright", c("White", "Black", "Hispanics"), cex = 1.0, fill = rainbow(3))
ifelse (White_suicide > White_homicide, "whites who are killed by guns are more likely to die because of suicide", "whites who are killed by guns are more likely to die because of homicide")

ifelse (Black_suicide > Black_homicide, "Blacks who are killed by guns are more likely to die because of suicide", "Blacks who are killed by guns are more likely to die because of homicide")

ifelse (Hispanics_suicide > Hispanics_homicide, "Hispanics who are killed by guns are more likely to die because of suicide", "Hispanics who are killed by guns are more likely to die because of homicide")



#h
#Police Involvement
gundeath_police <- sum(gun_death.data.df$police == "1")

#Police Not Involved
gundeath_wpolice <- sum(gun_death.data.df$police == "0")

gundeath_police

gundeath_wpolice

#Simple Pie Chart
slices = c(gundeath_police,gundeath_wpolice)

lbls = c("Gun_Death_Police", "Gun_Death_Wo_Police")

pie_chart = pie(slices,lbls, main = "Police Involvement in Gun Deaths")

gun_death.data.df = na.omit(gun_death.data.df)

corr_var(gun_death.data.df,police,limit = 1, top=20)

#2a
x = read.csv("weightLoss.csv")

df = data.frame(x)

colnames(df)[1] = "id"

colnames(df)[3:8] = c("WeightLoss_month1","WeightLoss_month2","WeightLoss_month3", "SelfEsteem_month1", "SelfEsteem_month2","SelfEsteem_month3")

colnames(df)

#b
wl.data = melt(df, id = c("id","group"),
               variable.name = "WeightLoss_month",
               measure.vars = c("WeightLoss_month1","WeightLoss_month2","WeightLoss_month3"))

#c
names(wl.data)[3:4] <- c("WeightLoss_Month", "WeightLoss")

#d
we.data = melt(df,id = c("id","group"),
               variable.name = "SelfEsteem_month",
               measure.vars = c("SelfEsteem_month1", "SelfEsteem_month2","SelfEsteem_month3"))

names(we.data)[3:4] <- c("SelfEsteem_Month", "SelfEsteem_Score")

#e
data.long <- cbind(wl.data, we.data)[, -5:-6]

str(df)

#f
summary(df)

table(df$group, df$WeightLoss_month1)

#g
dup_plot = ggplot(data.long, aes(x=as.factor(WeightLoss), fill=group))
dup_plot = dup_plot + labs(x = "Weight in pounds", y = "Count", title = "Weight Loss by Group within 3 months")
dup_plot = dup_plot + geom_bar()
dup_plot = dup_plot + facet_grid(WeightLoss_Month ~ group)
dup_plot = dup_plot + geom_line(aes(y = SelfEsteem_Score))
dup_plot = dup_plot + geom_point(aes(y = SelfEsteem_Score, colour = "blue"))
dup_plot = dup_plot + theme(legend.position='bottom')
dup_plot = dup_plot + scale_fill_discrete(guide_legend(title ="Group"))
dup_plot

#h
ggplot(df, aes(x= WeightLoss_month1, y = SelfEsteem_month1, color = group)) +
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 1") +
  geom_point() +
  facet_wrap(~ group)

#i
ggplot(df, aes(x= WeightLoss_month2, y = SelfEsteem_month2, color = group)) +
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 2") +
  geom_point() +
  facet_wrap(~ group)

ggplot(df, aes(x= WeightLoss_month3, y = SelfEsteem_month3, color = group)) +
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 3") +
  geom_point() +
  facet_wrap(~ group)

#j

ggplot(df) + geom_boxplot(aes(x=group, y=WeightLoss_month1),fill = "white") + coord_flip() +
  geom_boxplot(aes(x=group, y=SelfEsteem_month1), fill = "green") +
  labs(x = "Group", y = "Weight Loss      Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 1")



ggplot(df) + geom_boxplot(aes(x=group, y=WeightLoss_month2),fill = "white") + coord_flip() +
  geom_boxplot(aes(x=group, y=SelfEsteem_month2), fill = "green") +
  labs(x = "Group", y = "Weight Loss      Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 2")



ggplot(df) + geom_boxplot(aes(x=group, y=WeightLoss_month3), fill = "white") + coord_flip() +
  geom_boxplot(aes(x=group, y=SelfEsteem_month3), fill = "green") +
  labs(x = "Group", y = "Weight Loss      Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 3")

#k
#-> These plots provide clear pictures of the relationships between weight loss and self-esteem during three 
#months weight loss program. All the three groups have showed different patterns with respect to self-esteem 
#measurements. The more weight loss we have, the higher self-esteem we get. The DietEx group lost more weight
#while the control group lost least weight in the first 2 months and the same pattern can be seen in the 3rd 
#month as well. The longer one stays in the program, the lesser weight loss they have.

#l
data.long$Kilograms = data.long$WeightLoss * 0.453592

View(data.long)

#3
#a
testX = read.csv("testX.csv",header = FALSE)
testY = read.csv("testY.csv",header = FALSE)
trainX = read.csv("trainX.csv",header = FALSE)
trainY = read.csv("trainY.csv",header = FALSE)

#Merging the Test Tables 
testXY = cbind(testX,testY)

#Merging the Train Tables
trainXY = cbind(trainX,trainY)

#Column Names Added
colnames(testXY) = c("R_Mean", "T_Mean","P_Mean", "A_Mean","S_Mean","CP_Mean","CC_Mean", "NC_Mean", "Sy_Mean", "F_Mean","R_SD", "T_SD","P_SD", "A_SD","S_SD","CP_SD","CC_SD", "NC_SD", "Sy_SD", "F_SD","R_LAR", "T_LAR","P_LAR", "A_LAR","S_LAR","CP_LAR","CC_LAR", "NC_LAR", "Sy_LAR", "F_LAR","Tissue")

#Making Tissus as Factor
testXY$Tissue = as.factor(testXY$Tissue)

#Adding Column names
colnames(trainXY)[1:31] = c("R_Mean", "T_Mean","P_Mean", "A_Mean","S_Mean","CP_Mean","CC_Mean", "NC_Mean", "Sy_Mean", "F_Mean","R_SD", "T_SD","P_SD", "A_SD","S_SD","CP_SD","CC_SD", "NC_SD", "Sy_SD", "F_SD","R_LAR", "T_LAR","P_LAR", "A_LAR","S_LAR","CP_LAR","CC_LAR", "NC_LAR", "Sy_LAR", "F_LAR","Tissues")

View(trainXY)

#cleaning the train data
#Outlier Detection

outlier = function(value)
{
  iqr = IQR(value)
  q1 = as.numeric(quantile(value,0.25))
  q3 = as.numeric(quantile(value,0.75))
  higher = q3 + 1.5 * iqr
  lower = q1 - 1.5 *iqr
  
  ifelse(value < higher & value >lower, value , NA)   
  
}



train_final$Tissues = as.factor(train_final$Tissues)

summary(train_final)

str(train_final)


#classification and regression tree
C_Rtree=ctree(Tissues~.,data=train_final)
plot(C_Rtree)

#Create a simple decision tree using rpart using train data
#Decision Tree
C_R = rpart(Tissues~., data = train_final)
rpart.plot(C_R)

#Creating a full depth Decision tree
C_R_full = rpart(Tissues~., data = train_final, parms = list(split = "information"), control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
rpart.plot(C_R_full)
print(C_R_full)

summary(C_R_full)

#Calculating Leaf nodes
printcp(C_R_full)

#As we can see, there are 11 terminal nodes. We made the full depth decision tree with minimum split 
#being zero and minimum bucket being zero and cp value = -1. 


#b
summary(C_R_full)

#The major predictors for the Train data as can be seen from the full depth decision tree are:
#  - The root node: Largest Perimeter. 
#- As we can see from the summary that the important variables of our diagnosis are Largest Perimeter,
#Largest Area and Largest Radius etc. which will be our major predictors for the diagnosis as well.

#c
rules  = apriori(data = train_final, parameter = list(supp = 0.1, conf = 0.8))

inspect(rules[1:10])


#Using the above output, we can make analysis such as

#1) 98.6% of people having malignant cancerous tissues have largest value for Perimeter in the range of [102,166]

#2) 97.22% of people having malignant cancerous tissues have largest value for Radius in the range of 
#[15.5,24.6]

#2) 91.66% of people having malignant cancerous tissues have mean for Number of Concave portions of Contour in the range of 
#[0.0374,0.126]

#d
#prediction for train data
train_predicted = predict(C_R_full,train_final,type = "class")
print(train_predicted)

mean(train_final$Tissues == train_predicted)
#Confusion Matrix
confusionMatrix(train_predicted,train_final$Tissues)

#Prediction for test data
test_predicted_class = predict(C_R_full,testXY, type = "class")
print(test_predicted_class)

#Confusion Matrix
confusionMatrix(test_predicted_class, testXY$Tissue)

#e
#prune a test data
printcp(C_R_full)
pruned = prune(C_R_full,cp = 0.65)
fancyRpartPlot(pruned)


pruned.pred = predict(pruned, testXY, type = 'class')

confusionMatrix(testXY$Tissue, pruned.pred)

print(pruned.pred)
view(pruned.pred)

#We can see the accuracy has increased from 82.4% to 87% with the help of pruning

#f
C_R_full_gini = rpart(Tissues~., data = train_final, parms = list(split = "gini"), control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))

summary(C_R_full_gini)

print(C_R_full_gini)

rpart.plot(C_R_full_gini)

train_predicted_gini = predict(C_R_full_gini,train_final,type = "class")

print(train_predicted_gini)

confusionMatrix(train_predicted_gini,train_final$Tissues)

#Since our major predictors of diagnosis are Largest Perimeter, Largest Area, Largest Radius, Perimeter Mean, 
#Area Mean, Radius Mean. We will construct a model based on these to predict our Y labels.

model_tree_train = tree(Tissues~ P_LAR  +
                          A_LAR +
                          R_LAR +
                          P_Mean +
                          A_Mean +
                          R_Mean, data = train_final)
summary(model_tree_train)


plot(model_tree_train, type = "uniform")

text(model_tree_train, cex = 0.8)

#g
#Gini Index Decision Tree
rpart.plot(C_R_full_gini)

#Decision tree with Information 
rpart.plot(C_R_full)

#Tree based on major Predictors
plot(model_tree_train, type = "uniform")

text(model_tree_train, cex = 0.8)
