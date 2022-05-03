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


