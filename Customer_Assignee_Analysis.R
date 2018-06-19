

str(Business_Analyst_Project)

summary(Business_Analyst_Project)

#Checking for missing values
colSums(is.na(Business_Analyst_Project))


#Customer Effort Score

class(Business_Analyst_Project$`Customer Effort Score`)

#converting into numeric

Business_Analyst_Project$`Customer Effort Score`= as.numeric(as.character(Business_Analyst_Project$`Customer Effort Score`))

colSums(is.na(Business_Analyst_Project))

#now there are 956 rows with missing values of Cust effort score

#Removing those rows

Final_data= Business_Analyst_Project[ !(is.na(Business_Analyst_Project$`Customer Effort Score`)),  ]

colSums(is.na(Final_data))

class(Final_data$`Customer Effort Score`)

# removing ID, Assignee, Assigned at, SOlved At, Printing Problem

Final_data= Final_data[ , -c(1,2,5,6,16)]

#Correlations ####
D=data.frame(Business_Analyst_Project[,c("First resolution time in minutes within business hours", "Full resolution time in minutes within business hours")])
cor(D)

#Removing First time resolution time
Final_data= Final_data[, -6]


#Cart Model

#### MODEL : DECISION TREES ####


library(rpart)
library(rpart.plot)

CART_model=rpart(Final_data$`Customer Effort Score`~. , data=Final_data, method="class")
prp(CART_model)
summary(CART_model)



### EDA: Data Wrangling ####

attach(Business_Analyst_Project)

library(dplyr)
x= Business_Analyst_Project %>%
  group_by(Assignee) %>%
  summarize(count=n(), mean_effort = mean(`Effort `)) %>%
  arrange(count,mean_effort)
x
#recommendation1: More experienced assignee should take high effort queries

library(dplyr)
y= Business_Analyst_Project %>%
  group_by(Via) %>%
  summarize(count=n(), mean_reopens = mean(Reopens)) %>%
  arrange(count,mean_reopens)
y
#recommendation2: Channel: Most of the tickets are reopened when the channel is web service.

library(dplyr)
z= Business_Analyst_Project %>%
  group_by(`Printer Type`) %>%
  summarize(count=n(), mean_time = mean(`Full resolution time in minutes within business hours`)) %>%
  arrange(count,mean_time)
z
#recommendation2: Most of the tickets are generated for Printer Type: Form 2







