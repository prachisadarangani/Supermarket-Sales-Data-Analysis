library(ggplot2)
library(tidyverse)


#loading the data
superstore <- read.csv("/Users/prachisadarangani/Documents/Syracuse University /fALL 2023 SEM 3/AML/Assignment 2/supermarket_sales.csv")
View(superstore)
#exploring the dataset

summary(superstore)
str(superstore)


#checking the data for null or missing values 

nrow(superstore[!complete.cases(superstore),])

#there are no missing values

#exploring the dataset

#checking the total customer in each branch
branch_tab = table(superstore$Branch)
branch_tab

sum(superstore$gross.income)

#Branch A has the most customers 

#checking the gender distribution

gender_table = table(superstore$Gender)
gender_table
#we can see that the male to female ratio is almost 1:1
#checking the gender ratio based on the branch
gender_branch_tab <- table(superstore$Gender, superstore$Branch)
gender_branch_tab

#Lets check which gender seems to spend more

male <- superstore[superstore$Gender == "Male",]
sum(male$Total)
mean(male$Total)
table(male$Product.line)
male %>% group_by(Product.line) %>% summarize(total = mean(Total))

female <- superstore[superstore$Gender == "Female",]
sum(female$Total)
mean(female$Total)
table(female$Product.line)
female %>% group_by(Product.line) %>% summarize(total = mean(Total))


#checking member and normal ratio
member_tab = table(superstore$Customer.type)
member_tab
#checking to see if any particular branches have difference in member and normal ratio 
member_branch_tab = table(superstore$Customer.type, superstore$Branch)
member_branch_tab

#lets find the gross income for each branch 

branch_total = aggregate(superstore$gross.income, list(superstore$Branch),FUN = sum )
branch_total 
#we can see that Branch C has the most gross income which is 5265.176

#let's check the mean of total 

branch_mean = aggregate(superstore$Total, list(superstore$Branch), FUN = mean)
branch_mean
#We can see that branch C also has the highest mean

#lets convert the time to categories

superstore$Time <- gsub(":","",superstore$Time)
View(superstore)
superstore$Time <- as.numeric(superstore$Time)
superstore$Time_label <- cut(superstore$Time, breaks = c(0959,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2101), labels = c("10am-11am","11am-12pm","12pm-1pm","1pm-2pm","2pm-3pm","3pm-4pm","4pm-5pm","5pm-6pm","6pm-7pm","7pm-8pm","8pm-9pm"))

#trying to find the peak hour
hist(superstore$Time)
#we can see from the graph that the peak hour is 1900 to 2000 which is from 7pm to 8pm

#lets try and see at what time do we get the most sales 

#now lets check the peak time in difference branches 
#Branch A
Branch_A <- superstore[superstore$Branch == 'A',]
View(Branch_A)
hist(Branch_A$Time)
#we can see that the peak hour in branch A is between 10Am to 11am and the least is before closing from 8pm to 9pm

Branch_B <- superstore[superstore$Branch == 'B',]
hist(Branch_B$Time)
#we can see that the peak hour for branch B is 7pm to 8pm and the least crowded hour is 4pm to 5pm

Branch_C <- superstore[superstore$Branch == 'C',]
hist(Branch_C$Time)
#we can see that the peak hour for branch C is from 10am to 11am and also 7pm to 8pm same as branch A and the least crowded hour is 11am to 12pm

#Now let's review the customer satisfaction for different branches
#Branch_A
hist(Branch_A$Rating)
mean(Branch_A$Rating)
#we can see that the most rating is "4" and their mean rating is 7

hist(Branch_B$Rating)
mean(Branch_B$Rating)
#Branch B has the most rating at 4 and 6 and their mean rating is 6.8

hist(Branch_C$Rating)
mean(Branch_C$Rating)
#Branch C has the "9" as their most rated and their mean rating is 7.07


#Now let's view the rating for each branch at different times

Branch_A %>% group_by(Time_label) %>% summarize(Rating= mean(Rating)) %>% ggplot(aes(x=Time_label, y=Rating)) +geom_col(color = "black", aes(fill = Rating))
Branch_B %>% group_by(Time_label) %>% summarize(Rating= mean(Rating)) %>% ggplot(aes(x=Time_label, y=Rating)) +geom_col(color = "black", aes(fill = Rating))
Branch_C %>% group_by(Time_label) %>% summarize(Rating= mean(Rating)) %>% ggplot(aes(x=Time_label, y=Rating)) +geom_col(color = "black", aes(fill = Rating))

#we can see that there is no major difference in the mean of the ratings for different hours, they are between 6.5 and 7.5 

#lets see which kind of product is sold the most throughout 
superstore %>% group_by(Product.line) %>% summarize(total = sum(Quantity))
#We can see that electronic accessories are sold the most 

#lets check which product has the most total sale 
superstore %>% group_by(Product.line) %>% summarize(total = sum(gross.income))
#we can see that the most gross income comes from food and beverages

#for different branches
superstore %>% group_by(Branch,Product.line) %>% summarize(total = sum(gross.income))
#Branch A: maximum income coming from home & lifestyle
#Branch B: max income coming form sports & travel
#Branch C: max income coming from food & beverages 


#lets check which payment method is used the most 
pie(table(superstore$Payment))

#Ewallet is used the most 

#Let's see what payment method do people use to pay huge amounts, let's first find the 75th quartile

#75% quartile
quartile <- quantile(superstore$Total,0.75)

high_total <- superstore[superstore$Total > quartile,]
table(high_total$Payment)/nrow(high_total)*100

#Most people use Cash when their total is high

#Do members spend more or less than normal?
member <- superstore[superstore$Customer.type == "Member",]
mean(member$Total)
sum(member$Total)


normal <- superstore[superstore$Customer.type == "Normal",]
mean(normal$Total)
sum(normal$Total)

#we can see that the mean and total value from members is higher than normal, now let's check for gross income
sum(member$gross.income)
sum(normal$gross.income)
#we can see that the gross income is also higher from the members

