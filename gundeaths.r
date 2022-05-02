FiveThirtyEight, a data journalism site devoted to politics, sports, science, economics, and culture, recently published a series of articles on gun deaths in America. Gun violence in the United States is a significant political issue, and while reducing gun deaths is a noble goal, we must first understand the causes and patterns in gun violence in order to craft appropriate policies. As part of the project, FiveThirtyEight collected data from the Centers for Disease Control and Prevention, as well as other governmental agencies and non-profits, on all gun deaths in the United States from 2012-2014.You can find this dataset, called ”gun deaths.csv”, on blackboard.

Generate a data frame that summarizes the number of gun deaths per month. Print the data frame as a formatted kable() table.
#Data Frame for Months
gun_deaths.month.df <- data.frame(gun_death.data$month)
#Table for month deaths
month_deaths <- table(gun_death.data.df$month)

#Creating a Data Frame for Month Deaths
month_deaths.df <- data.frame(month_deaths)
#Using Kable Function for creating an XML
kable(month_deaths.df, "pipe", col.names = c("Month", "Deaths"), align = c("l","c"))
Month	Deaths
1	8273
2	7093
3	8289
4	8455
5	8669
6	8677
7	8989
8	8783
9	8508
10	8406
11	8243
12	8413
Generate a bar chart with labels on the x-axis. That is, each month should be labeled “Jan”, “Feb”, “Mar” and etc.
#Creating a vector of Month Names
month_name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

month_deaths.df$month_name  <- month_name 

#Plotting a Bar Chart
barplot(height = month_deaths.df$Freq, names= month_deaths.df$month_name)


Note that the echo = FALSE parameter was added to the code chunk to prevent printing of the R code that generated the plot.

Generate a bar chart that identifies the number of gun deaths associated with each type of intent cause of death. The bars should be sorted from highest to lowest values.
#Creating a data frame for Intent of Death
Death_Intent <- data.frame(gun_death.data.df$intent)

intent <- table(Death_Intent)

intent.df <- data.frame(intent)

ggplot(intent.df, aes(x = reorder(Death_Intent, -Freq), Freq)) + geom_bar(stat = "identity")


Generate a boxplot visualizing the age of gun death victims, by sex. Print the average age of female gun death victims.
#Creating a data frame for Sex
sex_df <- data.frame(gun_death.data.df, sex = sample(c("M","F")))

table(gun_death.data.df$sex, gun_death.data.df$age)
##    
##        0    1    2    3    4    5    6    7    8    9   10   11   12   13   14
##   F   14   21   16   18   20   24   31   12   13   13   19   21   31   52   65
##   M   19   17   34   48   34   19   19   31   19   34   34   40   86  177  299
##    
##       15   16   17   18   19   20   21   22   23   24   25   26   27   28   29
##   F   78  114  128  173  201  233  273  292  240  306  250  248  249  260  254
##   M  483  750 1057 1580 1864 1986 2231 2420 2232 2131 1980 1983 1821 1726 1701
##    
##       30   31   32   33   34   35   36   37   38   39   40   41   42   43   44
##   F  265  262  256  235  246  250  206  212  231  223  260  278  251  281  270
##   M 1604 1571 1568 1465 1453 1381 1306 1288 1260 1166 1154 1207 1241 1246 1179
##    
##       45   46   47   48   49   50   51   52   53   54   55   56   57   58   59
##   F  256  271  297  340  328  325  333  303  324  289  286  290  260  238  232
##   M 1116 1166 1235 1281 1341 1349 1422 1412 1384 1395 1310 1335 1212 1272 1198
##    
##       60   61   62   63   64   65   66   67   68   69   70   71   72   73   74
##   F  187  172  170  141  192  158  151  116  120  132   93  109   80   73   59
##   M 1174 1134  929  900  934  881  847  749  748  747  790  682  656  664  612
##    
##       75   76   77   78   79   80   81   82   83   84   85   86   87   88   89
##   F   66   61   50   62   50   59   55   44   53   38   36   27   27   19   24
##   M  610  521  525  536  523  487  508  476  499  452  404  380  285  321  221
##    
##       90   91   92   93   94   95   96   97   98   99  100  101  102  107
##   F   12   11    8    9    3    3    4    2    2    0    0    1    0    0
##   M  196  165  120   94   61   52   36   18   14    6    1    1    2    1
T.male <- subset(gun_death.data, sex == "M")
T.female <- subset(gun_death.data , sex == "F")  

Female.df <- data.frame(T.female)

mean(T.female$age)
## [1] NA
ggplot(gun_death.data.df) + geom_boxplot(aes(x=age, y=sex)) + coord_flip() +
  geom_boxplot(aes(x=age, y=sex)) +
  labs(x = "Age", y = "Sex", title = "Age of Gun Death victims by sex")
## Warning: Removed 18 rows containing non-finite values (stat_boxplot).

## Warning: Removed 18 rows containing non-finite values (stat_boxplot).


summary(Female.df$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0    29.0    44.0    43.7    56.0   101.0       3
mean(Female.df$age, na.rm = TRUE)
## [1] 43.69507
mean(T.male$age, na.rm = TRUE)
## [1] 43.8848
How many white males with at least a high school education were killed by guns in 2012?
#No. of White Males with at least a high school education which were killed by guns in 2012
sum(gun_death.data.df$year == 2012 & gun_death.data.df$race == "White" & gun_death.data.df$education == "HS/GED" & gun_death.data.df == "M", na.rm = TRUE )
## [1] 7912
Which season of the year has the most gun deaths? Assume that – Winter = January - March – Spring = April - June – Summer = July - September – Fall = October - December – Hint: You need to convert a continuous variable into a categorical variable.
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
## 
##   Fall Spring Summer Winter 
##  25062  25801  26280  23655
As we can see from the above table, the season with highest gun deaths is Summer.

(g)Are whites who are killed by guns more likely to die because of suicide or homicide? How does this compare to blacks and Hispanics?

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
## [1] "whites who are killed by guns are more likely to die because of suicide"
ifelse (Black_suicide > Black_homicide, "Blacks who are killed by guns are more likely to die because of suicide", "Blacks who are killed by guns are more likely to die because of homicide")
## [1] "Blacks who are killed by guns are more likely to die because of homicide"
ifelse (Hispanics_suicide > Hispanics_homicide, "Hispanics who are killed by guns are more likely to die because of suicide", "Hispanics who are killed by guns are more likely to die because of homicide")
## [1] "Hispanics who are killed by guns are more likely to die because of homicide"
As we can from the graph, Whites who are killed by guns are far more likely to die because of Suicide than Blacks or Hispanics. In the Homicide part, Blacks are more likely to be killed by guns than whites or Hispanics.

Are police-involved gun deaths significantly different from other gun deaths? Assess the relationship between police involvement and other variables.
#Police Involvement
gundeath_police <- sum(gun_death.data.df$police == "1")

#Police Not Involved
gundeath_wpolice <- sum(gun_death.data.df$police == "0")

gundeath_police
## [1] 1402
gundeath_wpolice
## [1] 99396
#Simple Pie Chart
slices = c(gundeath_police,gundeath_wpolice)

lbls = c("Gun_Death_Police", "Gun_Death_Wo_Police")

pie_chart = pie(slices,lbls, main = "Police Involvement in Gun Deaths")


gun_death.data.df = na.omit(gun_death.data.df)

corr_var(gun_death.data.df,police,limit = 1, top=20)
## Warning in .font_global(font, quiet = FALSE): Font 'Arial Narrow' is not
## installed, has other name, or can't be found


Problem 2. Read the “weightLoss.csv” file into R. You can find this dataset on Blackboard.

Rename the 1st column to “id”; the 3rd to the 5th columns respectively to “WeightLoss month1”, “WeightLoss month2”, and “WeightLoss month3”; and the 6th to the 8th columns to “SelfEsteem month1”, “SelfEsteem month2”, “SelfEsteem month3”, respectively.
x = read.csv("weightLoss.csv")

df = data.frame(x)

colnames(df)[1] = "id"

colnames(df)[3:8] = c("WeightLoss_month1","WeightLoss_month2","WeightLoss_month3", "SelfEsteem_month1", "SelfEsteem_month2","SelfEsteem_month3")

colnames(df)
## [1] "id"                "group"             "WeightLoss_month1"
## [4] "WeightLoss_month2" "WeightLoss_month3" "SelfEsteem_month1"
## [7] "SelfEsteem_month2" "SelfEsteem_month3"
Use the melt() function from the “Reshape2” package to reshape the dataset based on the WeightLoss month variables into a single column. Use “id” and “group” as the “id variables”. Let’s call this reshaped data frame “wl.data”.
wl.data = melt(df, id = c("id","group"),
               variable.name = "WeightLoss_month",
               measure.vars = c("WeightLoss_month1","WeightLoss_month2","WeightLoss_month3"))
Rename the 3rd and 4th columns of wl.data to “WeightLoss Month” and “WeightLoss”.
names(wl.data)[3:4] <- c("WeightLoss_Month", "WeightLoss")
This time use the melt() function to reshape the data based on “SelfEsteem month” variables. Call this dataset “we.data”. Rename the 3rd and 4th columns of we.data to “SelfEsteem Month” and “SelfEsteem Score”.
we.data = melt(df,id = c("id","group"),
               variable.name = "SelfEsteem_month",
               measure.vars = c("SelfEsteem_month1", "SelfEsteem_month2","SelfEsteem_month3"))

names(we.data)[3:4] <- c("SelfEsteem_Month", "SelfEsteem_Score")
Combine the two datasets “wl.data” and “we.data” and call the new dataset as “data.long”. Make sure each column is repeated once.
data.long <- cbind(wl.data, we.data)[, -5:-6]

str(df)
## 'data.frame':    34 obs. of  8 variables:
##  $ id               : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ group            : chr  "Control" "Control" "Control" "Control" ...
##  $ WeightLoss_month1: int  4 4 4 3 5 6 6 5 5 3 ...
##  $ WeightLoss_month2: int  3 4 3 2 3 5 5 4 4 3 ...
##  $ WeightLoss_month3: int  3 3 1 1 2 4 4 1 1 2 ...
##  $ SelfEsteem_month1: int  14 13 17 11 16 17 17 13 14 14 ...
##  $ SelfEsteem_month2: int  13 14 12 11 15 18 16 15 14 15 ...
##  $ SelfEsteem_month3: int  15 17 16 12 14 18 19 15 15 13 ...
Use the Weight Loss (pounds) as a categorical data and get the weight loss frequencies by groups.
summary(df)
##        id           group           WeightLoss_month1 WeightLoss_month2
##  Min.   : 1.00   Length:34          Min.   :3.000     Min.   :2.000    
##  1st Qu.: 9.25   Class :character   1st Qu.:4.000     1st Qu.:3.000    
##  Median :17.50   Mode  :character   Median :5.000     Median :4.000    
##  Mean   :17.50                      Mean   :5.294     Mean   :4.353    
##  3rd Qu.:25.75                      3rd Qu.:6.000     3rd Qu.:5.000    
##  Max.   :34.00                      Max.   :9.000     Max.   :9.000    
##  WeightLoss_month3 SelfEsteem_month1 SelfEsteem_month2 SelfEsteem_month3
##  Min.   :1.000     Min.   :11.00     Min.   :11.00     Min.   :11.00    
##  1st Qu.:1.000     1st Qu.:13.00     1st Qu.:12.00     1st Qu.:15.00    
##  Median :2.000     Median :15.00     Median :14.00     Median :17.00    
##  Mean   :2.176     Mean   :14.91     Mean   :13.82     Mean   :16.21    
##  3rd Qu.:3.000     3rd Qu.:16.00     3rd Qu.:15.00     3rd Qu.:18.00    
##  Max.   :4.000     Max.   :19.00     Max.   :19.00     Max.   :19.00
table(df$group, df$WeightLoss_month1)
##          
##           3 4 5 6 7 8 9
##   Control 2 4 4 2 0 0 0
##   Diet    1 3 2 3 3 0 0
##   DietEx  2 1 0 2 2 1 2
Use the dataset “data.long” and the ggplot() function from “ggplot2” package to create the following histogram. i.e. write the R code that reproduces exactly the histogram below.
dup_plot = ggplot(data.long, aes(x=as.factor(WeightLoss), fill=group))
dup_plot = dup_plot + labs(x = "Weight in pounds", y = "Count", title = "Weight Loss by Group within 3 months")
dup_plot = dup_plot + geom_bar()
dup_plot = dup_plot + facet_grid(WeightLoss_Month ~ group)
dup_plot = dup_plot + geom_line(aes(y = SelfEsteem_Score))
dup_plot = dup_plot + geom_point(aes(y = SelfEsteem_Score, colour = "blue"))
dup_plot = dup_plot + theme(legend.position='bottom')
dup_plot = dup_plot + scale_fill_discrete(guide_legend(title ="Group"))
dup_plot


Use the “weightLoss.data” and the ggplot() function to reproduce the exact same scatter plot below.
ggplot(df, aes(x= WeightLoss_month1, y = SelfEsteem_month1, color = group)) +
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 1") +
  geom_point() +
  facet_wrap(~ group)


Generate similar scatter plots for “Weight Loss vs Self-Esteem - Month 2” and “Weight Loss vs Self-Esteem - Month 3”.
ggplot(df, aes(x= WeightLoss_month2, y = SelfEsteem_month2, color = group)) +
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 2") +
  geom_point() +
  facet_wrap(~ group)


ggplot(df, aes(x= WeightLoss_month3, y = SelfEsteem_month3, color = group)) +
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 3") +
  geom_point() +
  facet_wrap(~ group)


Write the R code that generates the following boxplot (Use ggplot() function)
ggplot(df) + geom_boxplot(aes(x=group, y=WeightLoss_month1),fill = "white") + coord_flip() +
  geom_boxplot(aes(x=group, y=SelfEsteem_month1), fill = "green") +
  labs(x = "Group", y = "Weight Loss      Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 1")


ggplot(df) + geom_boxplot(aes(x=group, y=WeightLoss_month2),fill = "white") + coord_flip() +
  geom_boxplot(aes(x=group, y=SelfEsteem_month2), fill = "green") +
  labs(x = "Group", y = "Weight Loss      Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 2")


ggplot(df) + geom_boxplot(aes(x=group, y=WeightLoss_month3), fill = "white") + coord_flip() +
  geom_boxplot(aes(x=group, y=SelfEsteem_month3), fill = "green") +
  labs(x = "Group", y = "Weight Loss      Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 3")


How do you interpret the results provided in the graphs that you have generated. Please briefly explain your findings from these graphs.
-> These plots provide clear pictures of the relationships between weight loss and self-esteem during three months weight loss program. All the three groups have showed different patterns with respect to self-esteem measurements. The more weight loss we have, the higher self-esteem we get. The DietEx group lost more weight while the control group lost least weight in the first 2 months and the same pattern can be seen in the 3rd month as well. The longer one stays in the program, the lesser weight loss they have.

Add a new variable to the long.data, with the subjects’s weight in kilograms (kg) (1 kg = 2.204 pounds).
data.long$Kilograms = data.long$WeightLoss * 0.453592

View(data.long)
Problem 3. (Decision tree in R) One very interesting application area of machine learning is in making medical diagnoses. In this problem you will train and test a decision tree to detect breast cancer using real world data.

We will use the Wisconsin Diagnostic Breast Cancer (WDBC) dataset. The dataset consists of 569 samples of biopsied tissue. The tissue for each sample is imaged and 10 characteristics of the nuclei of cells presenting each image are characterized. These characteristics are (1) Radius (2) Texture (3) Perimeter (4) Area (5) Smoothness (6) Compactness (7) Concavity (8) Number of concave portions of contour (9) Symmetry (10) Fractal dimension

Each of the 569 samples used in the dataset consists of a feature vector of length 30. The first 10 entries in this feature vector are the mean of the characteristics listed above for each image. The second 10 are the standard deviation and last 10 are the largest value of each of these characteristics present in each image. Each sample is also associated with a label. A label of value 1 indicates the sample was for malignant (cancerous) tissue. A label of value 0 indicates the sample was for benign tissue. This dataset has already been broken up into training and test sets for you and is available on blackboard. The names of the files are “trainX.csv”, “trainY.csv”, “testX.csv” and “testY.csv.” The file names ending in “X.csv” contain feature vectors and those ending in “Y.csv” contain labels. Each file is in comma separated value format where each row represents a sample.

Clean your data (if required) before running any model. Create a C&R decision tree to its full depth. How many leaves are in tree? How do you get this information?
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


trainXY_outlier = sapply(trainXY[,1:30],outlier)
View(trainXY_outlier)
train_clean = data.frame(trainXY_outlier,trainXY[31])



train_final = na.omit(train_clean)

train_final$Tissues = as.factor(train_final$Tissues)

summary(train_final)
##      R_Mean           T_Mean          P_Mean           A_Mean      
##  Min.   : 6.981   Min.   : 9.71   Min.   : 43.79   Min.   : 143.5  
##  1st Qu.:11.633   1st Qu.:15.80   1st Qu.: 74.69   1st Qu.: 413.3  
##  Median :12.920   Median :18.16   Median : 83.16   Median : 513.9  
##  Mean   :13.242   Mean   :18.60   Mean   : 85.52   Mean   : 558.2  
##  3rd Qu.:14.588   3rd Qu.:21.00   3rd Qu.: 94.43   3rd Qu.: 656.8  
##  Max.   :20.590   Max.   :29.81   Max.   :137.80   Max.   :1320.0  
##      S_Mean           CP_Mean           CC_Mean           NC_Mean       
##  Min.   :0.06251   Min.   :0.01938   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.08456   1st Qu.:0.05981   1st Qu.:0.02555   1st Qu.:0.01782  
##  Median :0.09254   Median :0.07921   Median :0.04541   Median :0.02775  
##  Mean   :0.09313   Mean   :0.08616   Mean   :0.05936   Mean   :0.03478  
##  3rd Qu.:0.10160   3rd Qu.:0.10723   3rd Qu.:0.08150   3rd Qu.:0.04709  
##  Max.   :0.12430   Max.   :0.20220   Max.   :0.24170   Max.   :0.12590  
##     Sy_Mean           F_Mean             R_SD             T_SD       
##  Min.   :0.1167   Min.   :0.04996   Min.   :0.1144   Min.   :0.3621  
##  1st Qu.:0.1586   1st Qu.:0.05756   1st Qu.:0.2135   1st Qu.:0.7811  
##  Median :0.1724   Median :0.06102   Median :0.2720   Median :1.0240  
##  Mean   :0.1735   Mean   :0.06146   Mean   :0.2975   Mean   :1.0983  
##  3rd Qu.:0.1886   3rd Qu.:0.06475   3rd Qu.:0.3581   3rd Qu.:1.3522  
##  Max.   :0.2275   Max.   :0.07818   Max.   :0.7474   Max.   :2.4260  
##       P_SD            A_SD             S_SD              CP_SD         
##  Min.   :0.757   Min.   : 6.802   Min.   :0.002838   Min.   :0.002252  
##  1st Qu.:1.480   1st Qu.:16.093   1st Qu.:0.005033   1st Qu.:0.011843  
##  Median :1.970   Median :20.695   Median :0.006059   Median :0.017660  
##  Mean   :2.105   Mean   :25.323   Mean   :0.006532   Mean   :0.019877  
##  3rd Qu.:2.559   3rd Qu.:29.740   3rd Qu.:0.007803   3rd Qu.:0.025795  
##  Max.   :5.216   Max.   :81.230   Max.   :0.012910   Max.   :0.055920  
##      CC_SD             NC_SD              Sy_SD               F_SD          
##  Min.   :0.00000   Min.   :0.000000   Min.   :0.009539   Min.   :0.0008948  
##  1st Qu.:0.01336   1st Qu.:0.006808   1st Qu.:0.014778   1st Qu.:0.0020890  
##  Median :0.02043   Median :0.009245   Median :0.018435   Median :0.0027180  
##  Mean   :0.02400   Mean   :0.009776   Mean   :0.018872   Mean   :0.0030428  
##  3rd Qu.:0.03172   3rd Qu.:0.012195   3rd Qu.:0.021785   3rd Qu.:0.0037360  
##  Max.   :0.08158   Max.   :0.022950   Max.   :0.034640   Max.   :0.0077310  
##      R_LAR           T_LAR           P_LAR            A_LAR       
##  Min.   : 7.93   Min.   :12.02   Min.   : 50.41   Min.   : 185.2  
##  1st Qu.:12.98   1st Qu.:20.51   1st Qu.: 83.70   1st Qu.: 512.9  
##  Median :14.24   Median :24.59   Median : 92.44   Median : 623.3  
##  Mean   :14.90   Mean   :24.79   Mean   : 97.49   Mean   : 709.6  
##  3rd Qu.:16.38   3rd Qu.:28.61   3rd Qu.:108.60   3rd Qu.: 819.5  
##  Max.   :24.56   Max.   :39.34   Max.   :166.40   Max.   :1872.0  
##      S_LAR             CP_LAR            CC_LAR           NC_LAR       
##  Min.   :0.08125   Min.   :0.03432   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.:0.11440   1st Qu.:0.13533   1st Qu.:0.1047   1st Qu.:0.05942  
##  Median :0.12885   Median :0.19260   Median :0.1811   Median :0.08337  
##  Mean   :0.12902   Mean   :0.21291   Mean   :0.2118   Mean   :0.09465  
##  3rd Qu.:0.14235   3rd Qu.:0.26645   3rd Qu.:0.3070   3rd Qu.:0.12565  
##  Max.   :0.17940   Max.   :0.61100   Max.   :0.7727   Max.   :0.25430  
##      Sy_LAR           F_LAR         Tissues
##  Min.   :0.1783   Min.   :0.05521   0:242  
##  1st Qu.:0.2487   1st Qu.:0.07074   1: 72  
##  Median :0.2778   Median :0.07827          
##  Mean   :0.2797   Mean   :0.08002          
##  3rd Qu.:0.3067   3rd Qu.:0.08761          
##  Max.   :0.4128   Max.   :0.12050
str(train_final)
## 'data.frame':    314 obs. of  31 variables:
##  $ R_Mean : num  12.9 17.9 19.2 13.7 14.4 ...
##  $ T_Mean : num  13.3 24.5 26.6 15.2 16.5 ...
##  $ P_Mean : num  82.8 115.2 126.2 88.3 94.2 ...
##  $ A_Mean : num  505 999 1138 581 641 ...
##  $ S_Mean : num  0.1134 0.0886 0.102 0.0827 0.0975 ...
##  $ CP_Mean: num  0.0883 0.0703 0.1453 0.0755 0.1139 ...
##  $ CC_Mean: num  0.038 0.057 0.1921 0.0425 0.0801 ...
##  $ NC_Mean: num  0.034 0.0474 0.0966 0.0247 0.0422 ...
##  $ Sy_Mean: num  0.154 0.154 0.19 0.179 0.191 ...
##  $ F_Mean : num  0.0648 0.0551 0.0622 0.059 0.0641 ...
##  $ R_SD   : num  0.221 0.421 0.636 0.14 0.349 ...
##  $ T_SD   : num  1.042 1.433 1.001 0.542 0.771 ...
##  $ P_SD   : num  1.61 2.77 4.32 1.1 2.68 ...
##  $ A_SD   : num  16.6 45.8 69.7 11.3 32.1 ...
##  $ S_SD   : num  0.00591 0.00544 0.00739 0.00521 0.00458 ...
##  $ CP_SD  : num  0.0202 0.0117 0.0245 0.0298 0.0305 ...
##  $ CC_SD  : num  0.019 0.0162 0.0399 0.0244 0.0384 ...
##  $ NC_SD  : num  0.01011 0.00852 0.01293 0.00836 0.01243 ...
##  $ Sy_SD  : num  0.012 0.0142 0.0143 0.0182 0.0187 ...
##  $ F_SD   : num  0.00311 0.00275 0.00345 0.00487 0.00337 ...
##  $ R_LAR  : num  14 20.9 23.7 14.5 16.7 ...
##  $ T_LAR  : num  21.1 34.7 35.9 19.6 21.5 ...
##  $ P_LAR  : num  92.8 135.1 159.8 98 111.4 ...
##  $ A_LAR  : num  600 1320 1724 657 862 ...
##  $ S_LAR  : num  0.155 0.132 0.178 0.128 0.129 ...
##  $ CP_LAR : num  0.223 0.181 0.384 0.31 0.337 ...
##  $ CC_LAR : num  0.179 0.208 0.575 0.257 0.376 ...
##  $ NC_LAR : num  0.116 0.114 0.187 0.105 0.141 ...
##  $ Sy_LAR : num  0.238 0.25 0.326 0.339 0.305 ...
##  $ F_LAR  : num  0.0855 0.0795 0.0972 0.0964 0.0876 ...
##  $ Tissues: Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 1 1 1 ...
##  - attr(*, "na.action")= 'omit' Named int [1:141] 1 6 9 13 20 23 29 32 33 35 ...
##   ..- attr(*, "names")= chr [1:141] "1" "6" "9" "13" ...
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
## n= 314 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 314 72 0 (0.770700637 0.229299363)  
##    2) P_LAR< 105.95 227  5 0 (0.977973568 0.022026432)  
##      4) CC_LAR< 0.40725 218  1 0 (0.995412844 0.004587156)  
##        8) T_LAR< 33.27 208  0 0 (1.000000000 0.000000000) *
##        9) T_LAR>=33.27 10  1 0 (0.900000000 0.100000000)  
##         18) R_LAR< 14.42 9  0 0 (1.000000000 0.000000000) *
##         19) R_LAR>=14.42 1  0 1 (0.000000000 1.000000000) *
##      5) CC_LAR>=0.40725 9  4 0 (0.555555556 0.444444444)  
##       10) T_LAR< 26.9 5  0 0 (1.000000000 0.000000000) *
##       11) T_LAR>=26.9 4  0 1 (0.000000000 1.000000000) *
##    3) P_LAR>=105.95 87 20 1 (0.229885057 0.770114943)  
##      6) T_LAR< 20.045 13  0 0 (1.000000000 0.000000000) *
##      7) T_LAR>=20.045 74  7 1 (0.094594595 0.905405405)  
##       14) NC_Mean< 0.048785 15  6 1 (0.400000000 0.600000000)  
##         28) CP_SD>=0.02017 5  0 0 (1.000000000 0.000000000) *
##         29) CP_SD< 0.02017 10  1 1 (0.100000000 0.900000000)  
##           58) Sy_Mean< 0.14685 1  0 0 (1.000000000 0.000000000) *
##           59) Sy_Mean>=0.14685 9  0 1 (0.000000000 1.000000000) *
##       15) NC_Mean>=0.048785 59  1 1 (0.016949153 0.983050847)  
##         30) Sy_SD< 0.01062 1  0 0 (1.000000000 0.000000000) *
##         31) Sy_SD>=0.01062 58  0 1 (0.000000000 1.000000000) *
summary(C_R_full)
## Call:
## rpart(formula = Tissues ~ ., data = train_final, parms = list(split = "information"), 
##     control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
##   n= 314 
## 
##             CP nsplit  rel error    xerror       xstd
## 1  0.652777778      0 1.00000000 1.0000000 0.10346099
## 2  0.180555556      1 0.34722222 0.4444444 0.07445643
## 3  0.034722222      2 0.16666667 0.2500000 0.05721169
## 4  0.027777778      4 0.09722222 0.2500000 0.05721169
## 5  0.013888889      6 0.04166667 0.2916667 0.06148174
## 6  0.006944444      8 0.01388889 0.3055556 0.06282109
## 7 -1.000000000     10 0.00000000 0.3055556 0.06282109
## 
## Variable importance
##   P_LAR   A_LAR   R_LAR  P_Mean  A_Mean  R_Mean   T_LAR  T_Mean   CC_SD NC_Mean 
##      14      14      13      11      11      11       5       3       2       2 
## Sy_Mean  CC_LAR CC_Mean  NC_LAR   Sy_SD   NC_SD   CP_SD CP_Mean  CP_LAR    T_SD 
##       2       2       1       1       1       1       1       1       1       1 
##    A_SD  S_Mean   F_LAR    P_SD    R_SD 
##       1       1       1       1       1 
## 
## Node number 1: 314 observations,    complexity param=0.6527778
##   predicted class=0  expected loss=0.2292994  P(node) =1
##     class counts:   242    72
##    probabilities: 0.771 0.229 
##   left son=2 (227 obs) right son=3 (87 obs)
##   Primary splits:
##       P_LAR   < 105.95    to the left,  improve=98.13947, (0 missing)
##       A_LAR   < 727.1     to the left,  improve=92.26757, (0 missing)
##       R_LAR   < 15.515    to the left,  improve=90.94375, (0 missing)
##       NC_LAR  < 0.11135   to the left,  improve=87.62984, (0 missing)
##       NC_Mean < 0.048785  to the left,  improve=86.75491, (0 missing)
##   Surrogate splits:
##       A_LAR  < 784.15    to the left,  agree=0.978, adj=0.920, (0 split)
##       R_LAR  < 16.205    to the left,  agree=0.971, adj=0.897, (0 split)
##       P_Mean < 91.405    to the left,  agree=0.939, adj=0.782, (0 split)
##       R_Mean < 14.315    to the left,  agree=0.933, adj=0.759, (0 split)
##       A_Mean < 632.8     to the left,  agree=0.933, adj=0.759, (0 split)
## 
## Node number 2: 227 observations,    complexity param=0.02777778
##   predicted class=0  expected loss=0.02202643  P(node) =0.7229299
##     class counts:   222     5
##    probabilities: 0.978 0.022 
##   left son=4 (218 obs) right son=5 (9 obs)
##   Primary splits:
##       CC_LAR  < 0.40725   to the left,  improve=11.457230, (0 missing)
##       NC_LAR  < 0.13485   to the left,  improve=11.457230, (0 missing)
##       CC_Mean < 0.09752   to the left,  improve=10.015610, (0 missing)
##       NC_Mean < 0.045465  to the left,  improve= 9.287370, (0 missing)
##       CP_LAR  < 0.4124    to the left,  improve= 8.462249, (0 missing)
##   Surrogate splits:
##       NC_LAR  < 0.1443    to the left,  agree=0.987, adj=0.667, (0 split)
##       CP_LAR  < 0.399     to the left,  agree=0.982, adj=0.556, (0 split)
##       CC_Mean < 0.1075    to the left,  agree=0.978, adj=0.444, (0 split)
##       F_LAR   < 0.1148    to the left,  agree=0.974, adj=0.333, (0 split)
##       S_LAR   < 0.17765   to the left,  agree=0.969, adj=0.222, (0 split)
## 
## Node number 3: 87 observations,    complexity param=0.1805556
##   predicted class=1  expected loss=0.2298851  P(node) =0.2770701
##     class counts:    20    67
##    probabilities: 0.230 0.770 
##   left son=6 (13 obs) right son=7 (74 obs)
##   Primary splits:
##       T_LAR   < 20.045    to the left,  improve=23.73991, (0 missing)
##       T_Mean  < 16.95     to the left,  improve=21.74517, (0 missing)
##       NC_Mean < 0.06381   to the left,  improve=14.85026, (0 missing)
##       S_LAR   < 0.13595   to the left,  improve=14.38966, (0 missing)
##       CC_Mean < 0.08556   to the left,  improve=13.86440, (0 missing)
##   Surrogate splits:
##       T_Mean  < 15.745    to the left,  agree=0.977, adj=0.846, (0 split)
##       T_SD    < 0.47315   to the left,  agree=0.885, adj=0.231, (0 split)
##       Sy_Mean < 0.1384    to the left,  agree=0.874, adj=0.154, (0 split)
##       R_SD    < 0.18995   to the left,  agree=0.874, adj=0.154, (0 split)
##       P_SD    < 1.245     to the left,  agree=0.874, adj=0.154, (0 split)
## 
## Node number 4: 218 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.004587156  P(node) =0.6942675
##     class counts:   217     1
##    probabilities: 0.995 0.005 
##   left son=8 (208 obs) right son=9 (10 obs)
##   Primary splits:
##       T_LAR  < 33.27     to the left,  improve=3.131368, (0 missing)
##       T_Mean < 22.455    to the left,  improve=2.183594, (0 missing)
##       S_LAR  < 0.14185   to the left,  improve=1.705924, (0 missing)
##       CC_LAR < 0.21395   to the left,  improve=1.632717, (0 missing)
##       NC_LAR < 0.093205  to the left,  improve=1.542765, (0 missing)
##   Surrogate splits:
##       T_Mean < 25.175    to the left,  agree=0.977, adj=0.5, (0 split)
##       T_SD   < 2.2195    to the left,  agree=0.959, adj=0.1, (0 split)
## 
## Node number 5: 9 observations,    complexity param=0.02777778
##   predicted class=0  expected loss=0.4444444  P(node) =0.02866242
##     class counts:     5     4
##    probabilities: 0.556 0.444 
##   left son=10 (5 obs) right son=11 (4 obs)
##   Primary splits:
##       T_LAR   < 26.9      to the left,  improve=6.182654, (0 missing)
##       A_LAR   < 708.7     to the left,  improve=6.182654, (0 missing)
##       S_Mean  < 0.095875  to the left,  improve=3.680642, (0 missing)
##       NC_Mean < 0.04271   to the left,  improve=3.680642, (0 missing)
##       A_SD    < 18.345    to the left,  improve=3.680642, (0 missing)
##   Surrogate splits:
##       A_LAR   < 708.7     to the left,  agree=1.000, adj=1.00, (0 split)
##       S_Mean  < 0.095875  to the left,  agree=0.889, adj=0.75, (0 split)
##       NC_Mean < 0.04271   to the left,  agree=0.889, adj=0.75, (0 split)
##       A_SD    < 18.345    to the left,  agree=0.889, adj=0.75, (0 split)
##       CC_SD   < 0.039445  to the right, agree=0.889, adj=0.75, (0 split)
## 
## Node number 6: 13 observations
##   predicted class=0  expected loss=0  P(node) =0.04140127
##     class counts:    13     0
##    probabilities: 1.000 0.000 
## 
## Node number 7: 74 observations,    complexity param=0.03472222
##   predicted class=1  expected loss=0.09459459  P(node) =0.2356688
##     class counts:     7    67
##    probabilities: 0.095 0.905 
##   left son=14 (15 obs) right son=15 (59 obs)
##   Primary splits:
##       NC_Mean < 0.048785  to the left,  improve=8.000851, (0 missing)
##       R_LAR   < 16.8      to the left,  improve=7.528256, (0 missing)
##       S_LAR   < 0.13525   to the left,  improve=7.419656, (0 missing)
##       CC_Mean < 0.080135  to the left,  improve=7.093591, (0 missing)
##       A_LAR   < 868.2     to the left,  improve=7.093591, (0 missing)
##   Surrogate splits:
##       CC_Mean < 0.076355  to the left,  agree=0.932, adj=0.667, (0 split)
##       CP_Mean < 0.072635  to the left,  agree=0.865, adj=0.333, (0 split)
##       CC_SD   < 0.01873   to the left,  agree=0.865, adj=0.333, (0 split)
##       NC_LAR  < 0.12105   to the left,  agree=0.865, adj=0.333, (0 split)
##       NC_SD   < 0.0085795 to the left,  agree=0.851, adj=0.267, (0 split)
## 
## Node number 8: 208 observations
##   predicted class=0  expected loss=0  P(node) =0.6624204
##     class counts:   208     0
##    probabilities: 1.000 0.000 
## 
## Node number 9: 10 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.1  P(node) =0.03184713
##     class counts:     9     1
##    probabilities: 0.900 0.100 
##   left son=18 (9 obs) right son=19 (1 obs)
##   Primary splits:
##       R_LAR  < 14.42     to the left,  improve=3.25083, (0 missing)
##       T_LAR  < 33.56     to the right, improve=3.25083, (0 missing)
##       P_LAR  < 91.665    to the left,  improve=3.25083, (0 missing)
##       A_LAR  < 643.25    to the left,  improve=3.25083, (0 missing)
##       NC_LAR < 0.09229   to the left,  improve=3.25083, (0 missing)
## 
## Node number 10: 5 observations
##   predicted class=0  expected loss=0  P(node) =0.01592357
##     class counts:     5     0
##    probabilities: 1.000 0.000 
## 
## Node number 11: 4 observations
##   predicted class=1  expected loss=0  P(node) =0.01273885
##     class counts:     0     4
##    probabilities: 0.000 1.000 
## 
## Node number 14: 15 observations,    complexity param=0.03472222
##   predicted class=1  expected loss=0.4  P(node) =0.0477707
##     class counts:     6     9
##    probabilities: 0.400 0.600 
##   left son=28 (5 obs) right son=29 (10 obs)
##   Primary splits:
##       CP_SD   < 0.02017   to the right, improve=6.844345, (0 missing)
##       NC_SD   < 0.0105105 to the right, improve=6.844345, (0 missing)
##       R_LAR   < 16.825    to the left,  improve=6.844345, (0 missing)
##       Sy_Mean < 0.1862    to the right, improve=4.879643, (0 missing)
##       CC_SD   < 0.030115  to the right, improve=4.879643, (0 missing)
##   Surrogate splits:
##       NC_SD   < 0.0105105 to the right, agree=1.000, adj=1.0, (0 split)
##       Sy_Mean < 0.18025   to the right, agree=0.933, adj=0.8, (0 split)
##       CC_SD   < 0.030115  to the right, agree=0.933, adj=0.8, (0 split)
##       CP_Mean < 0.097055  to the right, agree=0.867, adj=0.6, (0 split)
##       Sy_SD   < 0.014925  to the right, agree=0.867, adj=0.6, (0 split)
## 
## Node number 15: 59 observations,    complexity param=0.01388889
##   predicted class=1  expected loss=0.01694915  P(node) =0.1878981
##     class counts:     1    58
##    probabilities: 0.017 0.983 
##   left son=30 (1 obs) right son=31 (58 obs)
##   Primary splits:
##       Sy_SD  < 0.01062   to the left,  improve=5.069015, (0 missing)
##       T_LAR  < 20.645    to the left,  improve=5.069015, (0 missing)
##       CC_LAR < 0.1894    to the left,  improve=5.069015, (0 missing)
##       NC_LAR < 0.1057    to the left,  improve=5.069015, (0 missing)
##       Sy_LAR < 0.21975   to the left,  improve=5.069015, (0 missing)
## 
## Node number 18: 9 observations
##   predicted class=0  expected loss=0  P(node) =0.02866242
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 19: 1 observations
##   predicted class=1  expected loss=0  P(node) =0.003184713
##     class counts:     0     1
##    probabilities: 0.000 1.000 
## 
## Node number 28: 5 observations
##   predicted class=0  expected loss=0  P(node) =0.01592357
##     class counts:     5     0
##    probabilities: 1.000 0.000 
## 
## Node number 29: 10 observations,    complexity param=0.01388889
##   predicted class=1  expected loss=0.1  P(node) =0.03184713
##     class counts:     1     9
##    probabilities: 0.100 0.900 
##   left son=58 (1 obs) right son=59 (9 obs)
##   Primary splits:
##       Sy_Mean < 0.14685   to the left,  improve=3.25083, (0 missing)
##       F_SD    < 0.001342  to the left,  improve=3.25083, (0 missing)
##       R_LAR   < 16.675    to the left,  improve=3.25083, (0 missing)
##       P_LAR   < 107.3     to the left,  improve=3.25083, (0 missing)
##       A_LAR   < 839.85    to the left,  improve=3.25083, (0 missing)
## 
## Node number 30: 1 observations
##   predicted class=0  expected loss=0  P(node) =0.003184713
##     class counts:     1     0
##    probabilities: 1.000 0.000 
## 
## Node number 31: 58 observations
##   predicted class=1  expected loss=0  P(node) =0.1847134
##     class counts:     0    58
##    probabilities: 0.000 1.000 
## 
## Node number 58: 1 observations
##   predicted class=0  expected loss=0  P(node) =0.003184713
##     class counts:     1     0
##    probabilities: 1.000 0.000 
## 
## Node number 59: 9 observations
##   predicted class=1  expected loss=0  P(node) =0.02866242
##     class counts:     0     9
##    probabilities: 0.000 1.000
#Calculating Leaf nodes
printcp(C_R_full)
## 
## Classification tree:
## rpart(formula = Tissues ~ ., data = train_final, parms = list(split = "information"), 
##     control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
## 
## Variables actually used in tree construction:
## [1] CC_LAR  CP_SD   NC_Mean P_LAR   R_LAR   Sy_Mean Sy_SD   T_LAR  
## 
## Root node error: 72/314 = 0.2293
## 
## n= 314 
## 
##           CP nsplit rel error  xerror     xstd
## 1  0.6527778      0  1.000000 1.00000 0.103461
## 2  0.1805556      1  0.347222 0.44444 0.074456
## 3  0.0347222      2  0.166667 0.25000 0.057212
## 4  0.0277778      4  0.097222 0.25000 0.057212
## 5  0.0138889      6  0.041667 0.29167 0.061482
## 6  0.0069444      8  0.013889 0.30556 0.062821
## 7 -1.0000000     10  0.000000 0.30556 0.062821
As we can see, there are 11 terminal nodes. We made the full depth decision tree with minimum split being zero and minimum bucket being zero and cp value = -1.

What are the major predictors of Diagnosis? Please justify your reasoning.
summary(C_R_full)
## Call:
## rpart(formula = Tissues ~ ., data = train_final, parms = list(split = "information"), 
##     control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
##   n= 314 
## 
##             CP nsplit  rel error    xerror       xstd
## 1  0.652777778      0 1.00000000 1.0000000 0.10346099
## 2  0.180555556      1 0.34722222 0.4444444 0.07445643
## 3  0.034722222      2 0.16666667 0.2500000 0.05721169
## 4  0.027777778      4 0.09722222 0.2500000 0.05721169
## 5  0.013888889      6 0.04166667 0.2916667 0.06148174
## 6  0.006944444      8 0.01388889 0.3055556 0.06282109
## 7 -1.000000000     10 0.00000000 0.3055556 0.06282109
## 
## Variable importance
##   P_LAR   A_LAR   R_LAR  P_Mean  A_Mean  R_Mean   T_LAR  T_Mean   CC_SD NC_Mean 
##      14      14      13      11      11      11       5       3       2       2 
## Sy_Mean  CC_LAR CC_Mean  NC_LAR   Sy_SD   NC_SD   CP_SD CP_Mean  CP_LAR    T_SD 
##       2       2       1       1       1       1       1       1       1       1 
##    A_SD  S_Mean   F_LAR    P_SD    R_SD 
##       1       1       1       1       1 
## 
## Node number 1: 314 observations,    complexity param=0.6527778
##   predicted class=0  expected loss=0.2292994  P(node) =1
##     class counts:   242    72
##    probabilities: 0.771 0.229 
##   left son=2 (227 obs) right son=3 (87 obs)
##   Primary splits:
##       P_LAR   < 105.95    to the left,  improve=98.13947, (0 missing)
##       A_LAR   < 727.1     to the left,  improve=92.26757, (0 missing)
##       R_LAR   < 15.515    to the left,  improve=90.94375, (0 missing)
##       NC_LAR  < 0.11135   to the left,  improve=87.62984, (0 missing)
##       NC_Mean < 0.048785  to the left,  improve=86.75491, (0 missing)
##   Surrogate splits:
##       A_LAR  < 784.15    to the left,  agree=0.978, adj=0.920, (0 split)
##       R_LAR  < 16.205    to the left,  agree=0.971, adj=0.897, (0 split)
##       P_Mean < 91.405    to the left,  agree=0.939, adj=0.782, (0 split)
##       R_Mean < 14.315    to the left,  agree=0.933, adj=0.759, (0 split)
##       A_Mean < 632.8     to the left,  agree=0.933, adj=0.759, (0 split)
## 
## Node number 2: 227 observations,    complexity param=0.02777778
##   predicted class=0  expected loss=0.02202643  P(node) =0.7229299
##     class counts:   222     5
##    probabilities: 0.978 0.022 
##   left son=4 (218 obs) right son=5 (9 obs)
##   Primary splits:
##       CC_LAR  < 0.40725   to the left,  improve=11.457230, (0 missing)
##       NC_LAR  < 0.13485   to the left,  improve=11.457230, (0 missing)
##       CC_Mean < 0.09752   to the left,  improve=10.015610, (0 missing)
##       NC_Mean < 0.045465  to the left,  improve= 9.287370, (0 missing)
##       CP_LAR  < 0.4124    to the left,  improve= 8.462249, (0 missing)
##   Surrogate splits:
##       NC_LAR  < 0.1443    to the left,  agree=0.987, adj=0.667, (0 split)
##       CP_LAR  < 0.399     to the left,  agree=0.982, adj=0.556, (0 split)
##       CC_Mean < 0.1075    to the left,  agree=0.978, adj=0.444, (0 split)
##       F_LAR   < 0.1148    to the left,  agree=0.974, adj=0.333, (0 split)
##       S_LAR   < 0.17765   to the left,  agree=0.969, adj=0.222, (0 split)
## 
## Node number 3: 87 observations,    complexity param=0.1805556
##   predicted class=1  expected loss=0.2298851  P(node) =0.2770701
##     class counts:    20    67
##    probabilities: 0.230 0.770 
##   left son=6 (13 obs) right son=7 (74 obs)
##   Primary splits:
##       T_LAR   < 20.045    to the left,  improve=23.73991, (0 missing)
##       T_Mean  < 16.95     to the left,  improve=21.74517, (0 missing)
##       NC_Mean < 0.06381   to the left,  improve=14.85026, (0 missing)
##       S_LAR   < 0.13595   to the left,  improve=14.38966, (0 missing)
##       CC_Mean < 0.08556   to the left,  improve=13.86440, (0 missing)
##   Surrogate splits:
##       T_Mean  < 15.745    to the left,  agree=0.977, adj=0.846, (0 split)
##       T_SD    < 0.47315   to the left,  agree=0.885, adj=0.231, (0 split)
##       Sy_Mean < 0.1384    to the left,  agree=0.874, adj=0.154, (0 split)
##       R_SD    < 0.18995   to the left,  agree=0.874, adj=0.154, (0 split)
##       P_SD    < 1.245     to the left,  agree=0.874, adj=0.154, (0 split)
## 
## Node number 4: 218 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.004587156  P(node) =0.6942675
##     class counts:   217     1
##    probabilities: 0.995 0.005 
##   left son=8 (208 obs) right son=9 (10 obs)
##   Primary splits:
##       T_LAR  < 33.27     to the left,  improve=3.131368, (0 missing)
##       T_Mean < 22.455    to the left,  improve=2.183594, (0 missing)
##       S_LAR  < 0.14185   to the left,  improve=1.705924, (0 missing)
##       CC_LAR < 0.21395   to the left,  improve=1.632717, (0 missing)
##       NC_LAR < 0.093205  to the left,  improve=1.542765, (0 missing)
##   Surrogate splits:
##       T_Mean < 25.175    to the left,  agree=0.977, adj=0.5, (0 split)
##       T_SD   < 2.2195    to the left,  agree=0.959, adj=0.1, (0 split)
## 
## Node number 5: 9 observations,    complexity param=0.02777778
##   predicted class=0  expected loss=0.4444444  P(node) =0.02866242
##     class counts:     5     4
##    probabilities: 0.556 0.444 
##   left son=10 (5 obs) right son=11 (4 obs)
##   Primary splits:
##       T_LAR   < 26.9      to the left,  improve=6.182654, (0 missing)
##       A_LAR   < 708.7     to the left,  improve=6.182654, (0 missing)
##       S_Mean  < 0.095875  to the left,  improve=3.680642, (0 missing)
##       NC_Mean < 0.04271   to the left,  improve=3.680642, (0 missing)
##       A_SD    < 18.345    to the left,  improve=3.680642, (0 missing)
##   Surrogate splits:
##       A_LAR   < 708.7     to the left,  agree=1.000, adj=1.00, (0 split)
##       S_Mean  < 0.095875  to the left,  agree=0.889, adj=0.75, (0 split)
##       NC_Mean < 0.04271   to the left,  agree=0.889, adj=0.75, (0 split)
##       A_SD    < 18.345    to the left,  agree=0.889, adj=0.75, (0 split)
##       CC_SD   < 0.039445  to the right, agree=0.889, adj=0.75, (0 split)
## 
## Node number 6: 13 observations
##   predicted class=0  expected loss=0  P(node) =0.04140127
##     class counts:    13     0
##    probabilities: 1.000 0.000 
## 
## Node number 7: 74 observations,    complexity param=0.03472222
##   predicted class=1  expected loss=0.09459459  P(node) =0.2356688
##     class counts:     7    67
##    probabilities: 0.095 0.905 
##   left son=14 (15 obs) right son=15 (59 obs)
##   Primary splits:
##       NC_Mean < 0.048785  to the left,  improve=8.000851, (0 missing)
##       R_LAR   < 16.8      to the left,  improve=7.528256, (0 missing)
##       S_LAR   < 0.13525   to the left,  improve=7.419656, (0 missing)
##       CC_Mean < 0.080135  to the left,  improve=7.093591, (0 missing)
##       A_LAR   < 868.2     to the left,  improve=7.093591, (0 missing)
##   Surrogate splits:
##       CC_Mean < 0.076355  to the left,  agree=0.932, adj=0.667, (0 split)
##       CP_Mean < 0.072635  to the left,  agree=0.865, adj=0.333, (0 split)
##       CC_SD   < 0.01873   to the left,  agree=0.865, adj=0.333, (0 split)
##       NC_LAR  < 0.12105   to the left,  agree=0.865, adj=0.333, (0 split)
##       NC_SD   < 0.0085795 to the left,  agree=0.851, adj=0.267, (0 split)
## 
## Node number 8: 208 observations
##   predicted class=0  expected loss=0  P(node) =0.6624204
##     class counts:   208     0
##    probabilities: 1.000 0.000 
## 
## Node number 9: 10 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.1  P(node) =0.03184713
##     class counts:     9     1
##    probabilities: 0.900 0.100 
##   left son=18 (9 obs) right son=19 (1 obs)
##   Primary splits:
##       R_LAR  < 14.42     to the left,  improve=3.25083, (0 missing)
##       T_LAR  < 33.56     to the right, improve=3.25083, (0 missing)
##       P_LAR  < 91.665    to the left,  improve=3.25083, (0 missing)
##       A_LAR  < 643.25    to the left,  improve=3.25083, (0 missing)
##       NC_LAR < 0.09229   to the left,  improve=3.25083, (0 missing)
## 
## Node number 10: 5 observations
##   predicted class=0  expected loss=0  P(node) =0.01592357
##     class counts:     5     0
##    probabilities: 1.000 0.000 
## 
## Node number 11: 4 observations
##   predicted class=1  expected loss=0  P(node) =0.01273885
##     class counts:     0     4
##    probabilities: 0.000 1.000 
## 
## Node number 14: 15 observations,    complexity param=0.03472222
##   predicted class=1  expected loss=0.4  P(node) =0.0477707
##     class counts:     6     9
##    probabilities: 0.400 0.600 
##   left son=28 (5 obs) right son=29 (10 obs)
##   Primary splits:
##       CP_SD   < 0.02017   to the right, improve=6.844345, (0 missing)
##       NC_SD   < 0.0105105 to the right, improve=6.844345, (0 missing)
##       R_LAR   < 16.825    to the left,  improve=6.844345, (0 missing)
##       Sy_Mean < 0.1862    to the right, improve=4.879643, (0 missing)
##       CC_SD   < 0.030115  to the right, improve=4.879643, (0 missing)
##   Surrogate splits:
##       NC_SD   < 0.0105105 to the right, agree=1.000, adj=1.0, (0 split)
##       Sy_Mean < 0.18025   to the right, agree=0.933, adj=0.8, (0 split)
##       CC_SD   < 0.030115  to the right, agree=0.933, adj=0.8, (0 split)
##       CP_Mean < 0.097055  to the right, agree=0.867, adj=0.6, (0 split)
##       Sy_SD   < 0.014925  to the right, agree=0.867, adj=0.6, (0 split)
## 
## Node number 15: 59 observations,    complexity param=0.01388889
##   predicted class=1  expected loss=0.01694915  P(node) =0.1878981
##     class counts:     1    58
##    probabilities: 0.017 0.983 
##   left son=30 (1 obs) right son=31 (58 obs)
##   Primary splits:
##       Sy_SD  < 0.01062   to the left,  improve=5.069015, (0 missing)
##       T_LAR  < 20.645    to the left,  improve=5.069015, (0 missing)
##       CC_LAR < 0.1894    to the left,  improve=5.069015, (0 missing)
##       NC_LAR < 0.1057    to the left,  improve=5.069015, (0 missing)
##       Sy_LAR < 0.21975   to the left,  improve=5.069015, (0 missing)
## 
## Node number 18: 9 observations
##   predicted class=0  expected loss=0  P(node) =0.02866242
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 19: 1 observations
##   predicted class=1  expected loss=0  P(node) =0.003184713
##     class counts:     0     1
##    probabilities: 0.000 1.000 
## 
## Node number 28: 5 observations
##   predicted class=0  expected loss=0  P(node) =0.01592357
##     class counts:     5     0
##    probabilities: 1.000 0.000 
## 
## Node number 29: 10 observations,    complexity param=0.01388889
##   predicted class=1  expected loss=0.1  P(node) =0.03184713
##     class counts:     1     9
##    probabilities: 0.100 0.900 
##   left son=58 (1 obs) right son=59 (9 obs)
##   Primary splits:
##       Sy_Mean < 0.14685   to the left,  improve=3.25083, (0 missing)
##       F_SD    < 0.001342  to the left,  improve=3.25083, (0 missing)
##       R_LAR   < 16.675    to the left,  improve=3.25083, (0 missing)
##       P_LAR   < 107.3     to the left,  improve=3.25083, (0 missing)
##       A_LAR   < 839.85    to the left,  improve=3.25083, (0 missing)
## 
## Node number 30: 1 observations
##   predicted class=0  expected loss=0  P(node) =0.003184713
##     class counts:     1     0
##    probabilities: 1.000 0.000 
## 
## Node number 31: 58 observations
##   predicted class=1  expected loss=0  P(node) =0.1847134
##     class counts:     0    58
##    probabilities: 0.000 1.000 
## 
## Node number 58: 1 observations
##   predicted class=0  expected loss=0  P(node) =0.003184713
##     class counts:     1     0
##    probabilities: 1.000 0.000 
## 
## Node number 59: 9 observations
##   predicted class=1  expected loss=0  P(node) =0.02866242
##     class counts:     0     9
##    probabilities: 0.000 1.000
The major predictors for the Train data as can be seen from the full depth decision tree are: - The root node: Largest Perimeter. - As we can see from the summary that the important variables of our diagnosis are Largest Perimter, Largest Area and Largest Radius etc. which will be our major predictors for the diagnosis as well.

Give two strong rules that describe who is likely to have cancer. Please justify your choices.
rules  = apriori(data = train_final, parameter = list(supp = 0.1, conf = 0.8))
## Warning: Column(s) 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
## 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 not logical or factor.
## Applying default discretization (see '? discretizeDF').
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen
##         0.8    0.1    1 none FALSE            TRUE       5     0.1      1
##  maxlen target  ext
##      10  rules TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 31 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[92 item(s), 314 transaction(s)] done [0.00s].
## sorting and recoding items ... [92 item(s)] done [0.00s].
## creating transaction tree ... done [0.00s].
## checking subsets of size 1 2 3 4 5 6 7 8 9 10
## Warning in apriori(data = train_final, parameter = list(supp = 0.1, conf =
## 0.8)): Mining stopped (maxlen reached). Only patterns up to a length of 10
## returned!
##  done [0.15s].
## writing ... [1377687 rule(s)] done [0.46s].
## creating S4 object  ... done [0.68s].
inspect(rules[1:10])
##      lhs            rhs                      support   confidence coverage 
## [1]  {Tissues=1} => {CC_LAR=[0.252,0.773]}   0.2038217 0.8888889  0.2292994
## [2]  {Tissues=1} => {NC_LAR=[0.109,0.254]}   0.2197452 0.9583333  0.2292994
## [3]  {Tissues=1} => {R_Mean=[13.8,20.6]}     0.2006369 0.8750000  0.2292994
## [4]  {Tissues=1} => {P_Mean=[88.9,138]}      0.2038217 0.8888889  0.2292994
## [5]  {Tissues=1} => {A_Mean=[586,1.32e+03]}  0.1974522 0.8611111  0.2292994
## [6]  {Tissues=1} => {NC_Mean=[0.0374,0.126]} 0.2101911 0.9166667  0.2292994
## [7]  {Tissues=1} => {CC_Mean=[0.0666,0.242]} 0.2038217 0.8888889  0.2292994
## [8]  {Tissues=1} => {A_LAR=[738,1.87e+03]}   0.2229299 0.9722222  0.2292994
## [9]  {Tissues=1} => {P_LAR=[102,166]}        0.2261146 0.9861111  0.2292994
## [10] {Tissues=1} => {R_LAR=[15.5,24.6]}      0.2229299 0.9722222  0.2292994
##      lift     count
## [1]  2.658201 64   
## [2]  2.865873 69   
## [3]  2.616667 63   
## [4]  2.658201 64   
## [5]  2.575132 62   
## [6]  2.741270 66   
## [7]  2.658201 64   
## [8]  2.907407 70   
## [9]  2.948942 71   
## [10] 2.907407 70
Using the above output, we can make analysis such as

98.6% of people having malignant cancerous tissues have largest value for Perimeter in the range of [102,166]

97.22% of people having malignant cancerous tissues have largest value for Radius in the range of [15.5,24.6]

What is the accuracy of your decision tree model on the training data? What is the accuracy of this model on the test data?
#prediction for train data
train_predicted = predict(C_R_full,train_final,type = "class")
print(train_predicted)
##   2   3   4   5   7   8  10  11  12  14  15  16  17  18  19  21  22  24  25  26 
##   0   1   1   0   0   0   0   0   0   0   0   1   0   1   0   0   0   0   1   1 
##  27  28  30  31  34  37  38  40  41  43  45  46  48  49  50  52  53  54  56  59 
##   1   0   1   1   1   0   0   1   0   0   0   0   1   0   0   0   1   0   0   0 
##  60  61  63  64  65  71  73  74  75  76  78  79  80  82  83  86  88  89  90  91 
##   0   0   0   1   0   0   1   0   0   0   0   0   1   0   0   0   0   0   0   0 
##  92  94  95  96 100 102 104 105 106 107 109 111 112 115 117 118 119 121 122 124 
##   1   1   0   0   1   0   1   0   0   0   0   0   0   1   1   0   0   0   1   0 
## 126 127 128 130 131 132 133 134 135 136 138 139 140 142 143 145 146 147 150 151 
##   0   0   0   1   0   0   0   0   0   1   0   0   0   1   0   0   0   0   0   0 
## 152 154 155 156 158 160 162 163 164 166 167 168 169 171 172 173 174 175 176 177 
##   0   1   0   0   0   0   0   0   0   0   0   0   0   1   0   1   0   0   0   0 
## 178 180 181 182 183 187 188 189 190 191 192 194 195 198 201 202 204 205 207 208 
##   0   0   0   1   1   0   0   0   0   0   0   0   0   0   0   1   1   1   0   1 
## 212 213 215 216 217 218 221 222 223 224 225 227 228 229 230 231 232 233 234 235 
##   0   0   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
## 236 237 238 239 240 242 243 244 245 248 249 250 252 253 254 255 256 257 260 261 
##   0   0   0   0   0   0   1   0   0   1   0   0   0   0   1   0   0   1   0   1 
## 266 268 272 274 275 276 277 279 282 283 284 285 286 287 289 291 292 293 294 296 
##   0   0   0   0   0   0   0   1   1   0   0   0   0   0   0   0   0   1   0   1 
## 297 300 301 302 303 305 306 307 310 311 312 314 319 321 322 323 324 328 329 330 
##   1   0   0   1   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
## 332 333 335 336 338 339 340 341 342 343 345 346 347 348 350 351 352 354 355 356 
##   0   0   0   0   1   1   0   0   1   0   0   0   0   0   1   0   0   0   0   0 
## 357 358 359 360 362 363 364 365 366 369 370 371 372 373 374 375 377 378 379 380 
##   0   0   0   0   1   0   1   0   1   1   0   0   1   0   0   1   1   0   0   0 
## 381 382 383 384 385 386 387 388 389 390 392 394 396 398 399 400 401 402 403 405 
##   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0   0 
## 410 411 413 414 415 416 417 418 420 421 422 423 425 426 427 428 431 432 433 434 
##   0   0   1   0   0   0   0   1   0   1   1   0   0   0   0   0   1   0   1   0 
## 435 437 438 439 444 445 446 447 448 449 451 453 454 455 
##   1   1   0   0   0   0   0   0   0   0   1   0   0   1 
## Levels: 0 1
mean(train_final$Tissues == train_predicted)
## [1] 1
#Confusion Matrix
confusionMatrix(train_predicted,train_final$Tissues)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   0   1
##          0 242   0
##          1   0  72
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9883, 1)
##     No Information Rate : 0.7707     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.0000     
##             Specificity : 1.0000     
##          Pos Pred Value : 1.0000     
##          Neg Pred Value : 1.0000     
##              Prevalence : 0.7707     
##          Detection Rate : 0.7707     
##    Detection Prevalence : 0.7707     
##       Balanced Accuracy : 1.0000     
##                                      
##        'Positive' Class : 0          
## 
#Prediction for test data
test_predicted_class = predict(C_R_full,testXY, type = "class")
print(test_predicted_class)
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
##  1  0  0  0  0  1  0  1  0  0  0  0  0  1  1  0  0  0  1  0  0  0  0  0  0  0 
## 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 
##  0  0  1  1  0  0  0  1  1  1  0  0  0  0  1  1  1  0  0  0  1  0  1  1  0  0 
## 53 54 55 56 57 
##  0  1  1  0  0 
## Levels: 0 1
#Confusion Matrix
confusionMatrix(test_predicted_class, testXY$Tissue)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  0  1
##          0 30  8
##          1  2 17
##                                           
##                Accuracy : 0.8246          
##                  95% CI : (0.7009, 0.9125)
##     No Information Rate : 0.5614          
##     P-Value [Acc > NIR] : 2.515e-05       
##                                           
##                   Kappa : 0.6341          
##                                           
##  Mcnemar's Test P-Value : 0.1138          
##                                           
##             Sensitivity : 0.9375          
##             Specificity : 0.6800          
##          Pos Pred Value : 0.7895          
##          Neg Pred Value : 0.8947          
##              Prevalence : 0.5614          
##          Detection Rate : 0.5263          
##    Detection Prevalence : 0.6667          
##       Balanced Accuracy : 0.8088          
##                                           
##        'Positive' Class : 0               
## 
Is it possible to improve the performance of your model?
Yes its possible for the test data as its accuracy is 82.4% so we can prune the test data model to improve its accuracy. The accuracy of our training model is already 1 so we do not need to prune it further.

#prune a test data
printcp(C_R_full)
## 
## Classification tree:
## rpart(formula = Tissues ~ ., data = train_final, parms = list(split = "information"), 
##     control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
## 
## Variables actually used in tree construction:
## [1] CC_LAR  CP_SD   NC_Mean P_LAR   R_LAR   Sy_Mean Sy_SD   T_LAR  
## 
## Root node error: 72/314 = 0.2293
## 
## n= 314 
## 
##           CP nsplit rel error  xerror     xstd
## 1  0.6527778      0  1.000000 1.00000 0.103461
## 2  0.1805556      1  0.347222 0.44444 0.074456
## 3  0.0347222      2  0.166667 0.25000 0.057212
## 4  0.0277778      4  0.097222 0.25000 0.057212
## 5  0.0138889      6  0.041667 0.29167 0.061482
## 6  0.0069444      8  0.013889 0.30556 0.062821
## 7 -1.0000000     10  0.000000 0.30556 0.062821
pruned = prune(C_R_full,cp = 0.65)
fancyRpartPlot(pruned)


pruned.pred = predict(pruned, testXY, type = 'class')

confusionMatrix(testXY$Tissue, pruned.pred)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  0  1
##          0 28  4
##          1  3 22
##                                           
##                Accuracy : 0.8772          
##                  95% CI : (0.7632, 0.9492)
##     No Information Rate : 0.5439          
##     P-Value [Acc > NIR] : 7.681e-08       
##                                           
##                   Kappa : 0.7517          
##                                           
##  Mcnemar's Test P-Value : 1               
##                                           
##             Sensitivity : 0.9032          
##             Specificity : 0.8462          
##          Pos Pred Value : 0.8750          
##          Neg Pred Value : 0.8800          
##              Prevalence : 0.5439          
##          Detection Rate : 0.4912          
##    Detection Prevalence : 0.5614          
##       Balanced Accuracy : 0.8747          
##                                           
##        'Positive' Class : 0               
## 
print(pruned.pred)
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
##  0  0  0  1  0  1  0  1  0  0  1  1  0  0  1  1  0  0  1  0  1  0  0  0  0  0 
## 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 
##  1  0  1  1  0  0  0  1  1  1  0  0  0  0  1  1  1  1  0  0  1  1  1  1  0  0 
## 53 54 55 56 57 
##  0  1  1  1  0 
## Levels: 0 1
view(pruned.pred)
We can see the accuracy has increased from 82.4% to 87% with the help of pruning

f)Construct the best possible decision tree to predict the Y labels. Explain how you construct such tree.

C_R_full_gini = rpart(Tissues~., data = train_final, parms = list(split = "gini"), control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))

summary(C_R_full_gini)
## Call:
## rpart(formula = Tissues ~ ., data = train_final, parms = list(split = "gini"), 
##     control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
##   n= 314 
## 
##             CP nsplit  rel error    xerror       xstd
## 1  0.652777778      0 1.00000000 1.0000000 0.10346099
## 2  0.180555556      1 0.34722222 0.4583333 0.07547668
## 3  0.034722222      2 0.16666667 0.2916667 0.06148174
## 4  0.027777778      4 0.09722222 0.2361111 0.05569361
## 5  0.013888889      5 0.06944444 0.2777778 0.06010233
## 6  0.006944444      8 0.02777778 0.2777778 0.06010233
## 7 -1.000000000     12 0.00000000 0.2777778 0.06010233
## 
## Variable importance
##   P_LAR   R_LAR   A_LAR  P_Mean  R_Mean  A_Mean   T_LAR  T_Mean Sy_Mean   NC_SD 
##      15      14      14      12      12      12       4       3       2       1 
##   CC_SD   CP_SD   Sy_SD CP_Mean CC_Mean    T_SD   F_LAR   S_LAR NC_Mean    P_SD 
##       1       1       1       1       1       1       1       1       1       1 
##    R_SD 
##       1 
## 
## Node number 1: 314 observations,    complexity param=0.6527778
##   predicted class=0  expected loss=0.2292994  P(node) =1
##     class counts:   242    72
##    probabilities: 0.771 0.229 
##   left son=2 (227 obs) right son=3 (87 obs)
##   Primary splits:
##       P_LAR   < 105.95    to the left,  improve=70.39656, (0 missing)
##       R_LAR   < 16.805    to the left,  improve=67.24995, (0 missing)
##       A_LAR   < 868.2     to the left,  improve=67.02889, (0 missing)
##       NC_Mean < 0.048785  to the left,  improve=65.91875, (0 missing)
##       NC_LAR  < 0.13595   to the left,  improve=62.66571, (0 missing)
##   Surrogate splits:
##       A_LAR  < 784.15    to the left,  agree=0.978, adj=0.920, (0 split)
##       R_LAR  < 16.205    to the left,  agree=0.971, adj=0.897, (0 split)
##       P_Mean < 91.405    to the left,  agree=0.939, adj=0.782, (0 split)
##       R_Mean < 14.315    to the left,  agree=0.933, adj=0.759, (0 split)
##       A_Mean < 632.8     to the left,  agree=0.933, adj=0.759, (0 split)
## 
## Node number 2: 227 observations,    complexity param=0.02777778
##   predicted class=0  expected loss=0.02202643  P(node) =0.7229299
##     class counts:   222     5
##    probabilities: 0.978 0.022 
##   left son=4 (225 obs) right son=5 (2 obs)
##   Primary splits:
##       S_LAR  < 0.17765   to the left,  improve=3.859736, (0 missing)
##       F_LAR  < 0.11785   to the left,  improve=3.859736, (0 missing)
##       CC_LAR < 0.40725   to the left,  improve=3.344466, (0 missing)
##       NC_LAR < 0.13485   to the left,  improve=3.344466, (0 missing)
##       CP_LAR < 0.4124    to the left,  improve=2.815935, (0 missing)
##   Surrogate splits:
##       F_LAR < 0.11785   to the left,  agree=1, adj=1, (0 split)
## 
## Node number 3: 87 observations,    complexity param=0.1805556
##   predicted class=1  expected loss=0.2298851  P(node) =0.2770701
##     class counts:    20    67
##    probabilities: 0.230 0.770 
##   left son=6 (13 obs) right son=7 (74 obs)
##   Primary splits:
##       T_LAR   < 20.045    to the left,  improve=18.128920, (0 missing)
##       T_Mean  < 16.95     to the left,  improve=16.882210, (0 missing)
##       CC_Mean < 0.08556   to the left,  improve= 9.771188, (0 missing)
##       NC_Mean < 0.048785  to the left,  improve= 8.973076, (0 missing)
##       P_LAR   < 116.05    to the left,  improve= 8.961460, (0 missing)
##   Surrogate splits:
##       T_Mean  < 15.745    to the left,  agree=0.977, adj=0.846, (0 split)
##       T_SD    < 0.47315   to the left,  agree=0.885, adj=0.231, (0 split)
##       Sy_Mean < 0.1384    to the left,  agree=0.874, adj=0.154, (0 split)
##       R_SD    < 0.18995   to the left,  agree=0.874, adj=0.154, (0 split)
##       P_SD    < 1.245     to the left,  agree=0.874, adj=0.154, (0 split)
## 
## Node number 4: 225 observations,    complexity param=0.01388889
##   predicted class=0  expected loss=0.01333333  P(node) =0.7165605
##     class counts:   222     3
##    probabilities: 0.987 0.013 
##   left son=8 (224 obs) right son=9 (1 obs)
##   Primary splits:
##       CC_Mean < 0.14185   to the left,  improve=1.9557140, (0 missing)
##       NC_Mean < 0.05576   to the left,  improve=1.0720310, (0 missing)
##       CC_LAR  < 0.40725   to the left,  improve=1.0720310, (0 missing)
##       NC_LAR  < 0.13485   to the left,  improve=1.0720310, (0 missing)
##       P_LAR   < 104.1     to the left,  improve=0.9292166, (0 missing)
## 
## Node number 5: 2 observations
##   predicted class=1  expected loss=0  P(node) =0.006369427
##     class counts:     0     2
##    probabilities: 0.000 1.000 
## 
## Node number 6: 13 observations
##   predicted class=0  expected loss=0  P(node) =0.04140127
##     class counts:    13     0
##    probabilities: 1.000 0.000 
## 
## Node number 7: 74 observations,    complexity param=0.03472222
##   predicted class=1  expected loss=0.09459459  P(node) =0.2356688
##     class counts:     7    67
##    probabilities: 0.095 0.905 
##   left son=14 (15 obs) right son=15 (59 obs)
##   Primary splits:
##       NC_Mean < 0.048785  to the left,  improve=3.509574, (0 missing)
##       R_LAR   < 16.8      to the left,  improve=3.210158, (0 missing)
##       S_LAR   < 0.1145    to the left,  improve=2.948403, (0 missing)
##       CC_Mean < 0.080135  to the left,  improve=2.946058, (0 missing)
##       A_LAR   < 868.2     to the left,  improve=2.946058, (0 missing)
##   Surrogate splits:
##       CC_Mean < 0.076355  to the left,  agree=0.932, adj=0.667, (0 split)
##       CP_Mean < 0.072635  to the left,  agree=0.865, adj=0.333, (0 split)
##       CC_SD   < 0.01873   to the left,  agree=0.865, adj=0.333, (0 split)
##       NC_LAR  < 0.12105   to the left,  agree=0.865, adj=0.333, (0 split)
##       NC_SD   < 0.0085795 to the left,  agree=0.851, adj=0.267, (0 split)
## 
## Node number 8: 224 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.008928571  P(node) =0.7133758
##     class counts:   222     2
##    probabilities: 0.991 0.009 
##   left son=16 (222 obs) right son=17 (2 obs)
##   Primary splits:
##       NC_LAR  < 0.16075   to the left,  improve=0.9732947, (0 missing)
##       Sy_LAR  < 0.3617    to the left,  improve=0.6400022, (0 missing)
##       CP_Mean < 0.15      to the left,  improve=0.4733766, (0 missing)
##       CP_LAR  < 0.4226    to the left,  improve=0.4733766, (0 missing)
##       CC_LAR  < 0.457     to the left,  improve=0.4733766, (0 missing)
## 
## Node number 9: 1 observations
##   predicted class=1  expected loss=0  P(node) =0.003184713
##     class counts:     0     1
##    probabilities: 0.000 1.000 
## 
## Node number 14: 15 observations,    complexity param=0.03472222
##   predicted class=1  expected loss=0.4  P(node) =0.0477707
##     class counts:     6     9
##    probabilities: 0.400 0.600 
##   left son=28 (5 obs) right son=29 (10 obs)
##   Primary splits:
##       CP_SD   < 0.02017   to the right, improve=5.400000, (0 missing)
##       NC_SD   < 0.0105105 to the right, improve=5.400000, (0 missing)
##       R_LAR   < 16.825    to the left,  improve=5.400000, (0 missing)
##       Sy_Mean < 0.1862    to the right, improve=3.927273, (0 missing)
##       CC_SD   < 0.030115  to the right, improve=3.927273, (0 missing)
##   Surrogate splits:
##       NC_SD   < 0.0105105 to the right, agree=1.000, adj=1.0, (0 split)
##       Sy_Mean < 0.18025   to the right, agree=0.933, adj=0.8, (0 split)
##       CC_SD   < 0.030115  to the right, agree=0.933, adj=0.8, (0 split)
##       CP_Mean < 0.097055  to the right, agree=0.867, adj=0.6, (0 split)
##       Sy_SD   < 0.014925  to the right, agree=0.867, adj=0.6, (0 split)
## 
## Node number 15: 59 observations,    complexity param=0.01388889
##   predicted class=1  expected loss=0.01694915  P(node) =0.1878981
##     class counts:     1    58
##    probabilities: 0.017 0.983 
##   left son=30 (1 obs) right son=31 (58 obs)
##   Primary splits:
##       Sy_SD  < 0.01062   to the left,  improve=1.966102, (0 missing)
##       T_LAR  < 20.645    to the left,  improve=1.966102, (0 missing)
##       CC_LAR < 0.1894    to the left,  improve=1.966102, (0 missing)
##       NC_LAR < 0.1057    to the left,  improve=1.966102, (0 missing)
##       Sy_LAR < 0.21975   to the left,  improve=1.966102, (0 missing)
## 
## Node number 16: 222 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.004504505  P(node) =0.7070064
##     class counts:   221     1
##    probabilities: 0.995 0.005 
##   left son=32 (212 obs) right son=33 (10 obs)
##   Primary splits:
##       T_LAR  < 33.27     to the left,  improve=0.19099100, (0 missing)
##       T_Mean < 22.455    to the left,  improve=0.07099099, (0 missing)
##       S_LAR  < 0.14185   to the left,  improve=0.04099099, (0 missing)
##       CC_LAR < 0.21395   to the left,  improve=0.03354418, (0 missing)
##       A_LAR  < 650.95    to the left,  improve=0.03099099, (0 missing)
##   Surrogate splits:
##       T_Mean < 25.175    to the left,  agree=0.977, adj=0.5, (0 split)
##       T_SD   < 2.2195    to the left,  agree=0.959, adj=0.1, (0 split)
## 
## Node number 17: 2 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.5  P(node) =0.006369427
##     class counts:     1     1
##    probabilities: 0.500 0.500 
##   left son=34 (1 obs) right son=35 (1 obs)
##   Primary splits:
##       R_Mean < 13.6      to the left,  improve=1, (0 missing)
##       T_Mean < 16.395    to the left,  improve=1, (0 missing)
##       P_Mean < 88.725    to the left,  improve=1, (0 missing)
##       A_Mean < 549.45    to the left,  improve=1, (0 missing)
##       S_Mean < 0.1052    to the right, improve=1, (0 missing)
## 
## Node number 28: 5 observations
##   predicted class=0  expected loss=0  P(node) =0.01592357
##     class counts:     5     0
##    probabilities: 1.000 0.000 
## 
## Node number 29: 10 observations,    complexity param=0.01388889
##   predicted class=1  expected loss=0.1  P(node) =0.03184713
##     class counts:     1     9
##    probabilities: 0.100 0.900 
##   left son=58 (1 obs) right son=59 (9 obs)
##   Primary splits:
##       Sy_Mean < 0.14685   to the left,  improve=1.8, (0 missing)
##       F_SD    < 0.001342  to the left,  improve=1.8, (0 missing)
##       R_LAR   < 16.675    to the left,  improve=1.8, (0 missing)
##       P_LAR   < 107.3     to the left,  improve=1.8, (0 missing)
##       A_LAR   < 839.85    to the left,  improve=1.8, (0 missing)
## 
## Node number 30: 1 observations
##   predicted class=0  expected loss=0  P(node) =0.003184713
##     class counts:     1     0
##    probabilities: 1.000 0.000 
## 
## Node number 31: 58 observations
##   predicted class=1  expected loss=0  P(node) =0.1847134
##     class counts:     0    58
##    probabilities: 0.000 1.000 
## 
## Node number 32: 212 observations
##   predicted class=0  expected loss=0  P(node) =0.6751592
##     class counts:   212     0
##    probabilities: 1.000 0.000 
## 
## Node number 33: 10 observations,    complexity param=0.006944444
##   predicted class=0  expected loss=0.1  P(node) =0.03184713
##     class counts:     9     1
##    probabilities: 0.900 0.100 
##   left son=66 (9 obs) right son=67 (1 obs)
##   Primary splits:
##       R_LAR  < 14.42     to the left,  improve=1.8, (0 missing)
##       T_LAR  < 33.56     to the right, improve=1.8, (0 missing)
##       P_LAR  < 91.665    to the left,  improve=1.8, (0 missing)
##       A_LAR  < 643.25    to the left,  improve=1.8, (0 missing)
##       NC_LAR < 0.09229   to the left,  improve=1.8, (0 missing)
## 
## Node number 34: 1 observations
##   predicted class=0  expected loss=0  P(node) =0.003184713
##     class counts:     1     0
##    probabilities: 1.000 0.000 
## 
## Node number 35: 1 observations
##   predicted class=1  expected loss=0  P(node) =0.003184713
##     class counts:     0     1
##    probabilities: 0.000 1.000 
## 
## Node number 58: 1 observations
##   predicted class=0  expected loss=0  P(node) =0.003184713
##     class counts:     1     0
##    probabilities: 1.000 0.000 
## 
## Node number 59: 9 observations
##   predicted class=1  expected loss=0  P(node) =0.02866242
##     class counts:     0     9
##    probabilities: 0.000 1.000 
## 
## Node number 66: 9 observations
##   predicted class=0  expected loss=0  P(node) =0.02866242
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 67: 1 observations
##   predicted class=1  expected loss=0  P(node) =0.003184713
##     class counts:     0     1
##    probabilities: 0.000 1.000
print(C_R_full_gini)
## n= 314 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 314 72 0 (0.770700637 0.229299363)  
##    2) P_LAR< 105.95 227  5 0 (0.977973568 0.022026432)  
##      4) S_LAR< 0.17765 225  3 0 (0.986666667 0.013333333)  
##        8) CC_Mean< 0.14185 224  2 0 (0.991071429 0.008928571)  
##         16) NC_LAR< 0.16075 222  1 0 (0.995495495 0.004504505)  
##           32) T_LAR< 33.27 212  0 0 (1.000000000 0.000000000) *
##           33) T_LAR>=33.27 10  1 0 (0.900000000 0.100000000)  
##             66) R_LAR< 14.42 9  0 0 (1.000000000 0.000000000) *
##             67) R_LAR>=14.42 1  0 1 (0.000000000 1.000000000) *
##         17) NC_LAR>=0.16075 2  1 0 (0.500000000 0.500000000)  
##           34) R_Mean< 13.6 1  0 0 (1.000000000 0.000000000) *
##           35) R_Mean>=13.6 1  0 1 (0.000000000 1.000000000) *
##        9) CC_Mean>=0.14185 1  0 1 (0.000000000 1.000000000) *
##      5) S_LAR>=0.17765 2  0 1 (0.000000000 1.000000000) *
##    3) P_LAR>=105.95 87 20 1 (0.229885057 0.770114943)  
##      6) T_LAR< 20.045 13  0 0 (1.000000000 0.000000000) *
##      7) T_LAR>=20.045 74  7 1 (0.094594595 0.905405405)  
##       14) NC_Mean< 0.048785 15  6 1 (0.400000000 0.600000000)  
##         28) CP_SD>=0.02017 5  0 0 (1.000000000 0.000000000) *
##         29) CP_SD< 0.02017 10  1 1 (0.100000000 0.900000000)  
##           58) Sy_Mean< 0.14685 1  0 0 (1.000000000 0.000000000) *
##           59) Sy_Mean>=0.14685 9  0 1 (0.000000000 1.000000000) *
##       15) NC_Mean>=0.048785 59  1 1 (0.016949153 0.983050847)  
##         30) Sy_SD< 0.01062 1  0 0 (1.000000000 0.000000000) *
##         31) Sy_SD>=0.01062 58  0 1 (0.000000000 1.000000000) *
rpart.plot(C_R_full_gini)


train_predicted_gini = predict(C_R_full_gini,train_final,type = "class")

print(train_predicted_gini)
##   2   3   4   5   7   8  10  11  12  14  15  16  17  18  19  21  22  24  25  26 
##   0   1   1   0   0   0   0   0   0   0   0   1   0   1   0   0   0   0   1   1 
##  27  28  30  31  34  37  38  40  41  43  45  46  48  49  50  52  53  54  56  59 
##   1   0   1   1   1   0   0   1   0   0   0   0   1   0   0   0   1   0   0   0 
##  60  61  63  64  65  71  73  74  75  76  78  79  80  82  83  86  88  89  90  91 
##   0   0   0   1   0   0   1   0   0   0   0   0   1   0   0   0   0   0   0   0 
##  92  94  95  96 100 102 104 105 106 107 109 111 112 115 117 118 119 121 122 124 
##   1   1   0   0   1   0   1   0   0   0   0   0   0   1   1   0   0   0   1   0 
## 126 127 128 130 131 132 133 134 135 136 138 139 140 142 143 145 146 147 150 151 
##   0   0   0   1   0   0   0   0   0   1   0   0   0   1   0   0   0   0   0   0 
## 152 154 155 156 158 160 162 163 164 166 167 168 169 171 172 173 174 175 176 177 
##   0   1   0   0   0   0   0   0   0   0   0   0   0   1   0   1   0   0   0   0 
## 178 180 181 182 183 187 188 189 190 191 192 194 195 198 201 202 204 205 207 208 
##   0   0   0   1   1   0   0   0   0   0   0   0   0   0   0   1   1   1   0   1 
## 212 213 215 216 217 218 221 222 223 224 225 227 228 229 230 231 232 233 234 235 
##   0   0   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
## 236 237 238 239 240 242 243 244 245 248 249 250 252 253 254 255 256 257 260 261 
##   0   0   0   0   0   0   1   0   0   1   0   0   0   0   1   0   0   1   0   1 
## 266 268 272 274 275 276 277 279 282 283 284 285 286 287 289 291 292 293 294 296 
##   0   0   0   0   0   0   0   1   1   0   0   0   0   0   0   0   0   1   0   1 
## 297 300 301 302 303 305 306 307 310 311 312 314 319 321 322 323 324 328 329 330 
##   1   0   0   1   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
## 332 333 335 336 338 339 340 341 342 343 345 346 347 348 350 351 352 354 355 356 
##   0   0   0   0   1   1   0   0   1   0   0   0   0   0   1   0   0   0   0   0 
## 357 358 359 360 362 363 364 365 366 369 370 371 372 373 374 375 377 378 379 380 
##   0   0   0   0   1   0   1   0   1   1   0   0   1   0   0   1   1   0   0   0 
## 381 382 383 384 385 386 387 388 389 390 392 394 396 398 399 400 401 402 403 405 
##   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0   0 
## 410 411 413 414 415 416 417 418 420 421 422 423 425 426 427 428 431 432 433 434 
##   0   0   1   0   0   0   0   1   0   1   1   0   0   0   0   0   1   0   1   0 
## 435 437 438 439 444 445 446 447 448 449 451 453 454 455 
##   1   1   0   0   0   0   0   0   0   0   1   0   0   1 
## Levels: 0 1
confusionMatrix(train_predicted_gini,train_final$Tissues)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   0   1
##          0 242   0
##          1   0  72
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9883, 1)
##     No Information Rate : 0.7707     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.0000     
##             Specificity : 1.0000     
##          Pos Pred Value : 1.0000     
##          Neg Pred Value : 1.0000     
##              Prevalence : 0.7707     
##          Detection Rate : 0.7707     
##    Detection Prevalence : 0.7707     
##       Balanced Accuracy : 1.0000     
##                                      
##        'Positive' Class : 0          
## 
Since our major predictors of diagnosis are Largest Perimeter, Largest Area, Largest Radius, Perimeter Mean, Area Mean, Radius Mean. We will construct a model based on these to predict our Y labels.

model_tree_train = tree(Tissues~ P_LAR  +
                           A_LAR +
                           R_LAR +
                           P_Mean +
                           A_Mean +
                           R_Mean, data = train_final)
summary(model_tree_train)
## 
## Classification tree:
## tree(formula = Tissues ~ P_LAR + A_LAR + R_LAR + P_Mean + A_Mean + 
##     R_Mean, data = train_final)
## Variables actually used in tree construction:
## [1] "P_LAR"  "A_LAR"  "A_Mean" "P_Mean"
## Number of terminal nodes:  12 
## Residual mean deviance:  0.1561 = 47.16 / 302 
## Misclassification error rate: 0.0414 = 13 / 314
plot(model_tree_train, type = "uniform")

text(model_tree_train, cex = 0.8)


Plot your final decision tree model.
#g
#Gini Index Decision Tree
rpart.plot(C_R_full_gini)


#Decision tree with Information 
rpart.plot(C_R_full)


#Tree based on major Predictors
plot(model_tree_train, type = "uniform")

text(model_tree_train, cex = 0.8)
