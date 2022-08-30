#Retail Analysis with Walmart Data

library(dplyr)
install.packages('sqldf')
library(sqldf)


#Reading data file - Walmart_Store_sales
data=read.csv("C:/data/Walmart_Store_sales.csv")

# getting information about data
summary(data)
nrow(data)
str(data)
head(data)
names(data)

#----------------------------------------------------------------
# Analysis Tasks - Basic Statistics Tasks
#----------------------------------------------------------------
# Question 1 - Which store has maximum sales? Solution below

# Step 1: Adds a new col called store sum that sums up sales for each store
data2 <- data %>% group_by(Store) %>% mutate(store_sum=sum(Weekly_Sales))

# Step 2: Finding Store with maximum sales
sqldf("SELECT Store, MAX(store_sum) as Max_Sales FROM data2")

# Output below :
#    Store   Max_Sales
#1    20     301397792

#-----------------------------------------------------------------

# Question 2 - Which store has maximum standard deviation i.e., the sales vary a lot

# Step 1: Grouping data by store and then calculating SD for each store
data3 <- data %>% group_by(Store=data$Store) %>% summarize(store_sd =sd(Weekly_Sales))

# Step 2: Finding the store with maximum SD
sqldf("SELECT Store, MAX(store_sd) as Max_SD FROM data3")

# Result Output below:
#    Store    Max_SD
# 1    14    317569.9

#------------------------------------------------------------------

# Question 3 - Which store/s has good quarterly growth rate in Q3'2012

# Solution steps - 
# Step 1: Read the data file
# Step 2: Format the date column
# Step 3: Add a Month column
# Step 4: Add a year column
# Step 5: Add a quarter column
# Step 6: Select data rows that match quarter=Q3 and year=2012
# Step 7: Group data by store and calculate sum
# Step 8: Calculate Max_sales, finding Store with maximum quarterly growth for Q3, 2012. Output the Store name and the Sales

#---------------------------------------------------------------
library(lubridate)

# Step 1: Reading the data file
data=read.csv("C:/data/Walmart_Store_sales.csv")

# Step 2: Formatting the date column
data$Date=as.Date(data$Date,format="%d-%m-%Y")

# Step 3: Add a Month column by extracting month from Date column
data$Month=month(data$Date)

# Step 4 : Add a year column by extracting year from Date column
data$Year=year(data$Date)

# Step 5: Add the Quarter column
data$Quarter=quarter(data$Date)

# Step 6: Select data rows that match quarter=Q3 and year=2012
data5 <- sqldf("SELECT * FROM data4 WHERE Year=2012 AND Quarter='Q3'")

# Step 7: Group data by store and calculate sum 
data6 <- data5 %>% group_by(Store) %>% summarize(Q3_store_sum=sum(Weekly_Sales))

# Step 8: Calculate Max_sales, finding Store with maximum quarterly growth for Q3, 2012. Output the Store name and the Sales
sqldf("SELECT Store, MAX(Q3_store_sum) as Max_Sales FROM data6")

# Result Output below
#   Store    Max_Sales
#1    4      27796792

#--------------------------------------------------------------------------------------------

# Question 4 - Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
# Solution - subsetting, aggregating, comparisons
# Step 1: Reading the data file
# Step 2: Calculate non-holiday mean of all stores together
# Step 3: Creating a new event column based on holiday dates
# Step 4: Find the sales average for all four holiday events

#--------------------------------------------------------------------------------------------

# Step 1: Reading the data file
data=read.csv("C:/data/Walmart_Store_sales.csv")

# Step 2: Calculating mean sales of non-holiday season for all stores
data1 <- subset(data, Holiday_Flag == 0)              # Subsetting for non-holiday data
head(data1)
# calculating Non-holiday mean
agg_mean <- aggregate(data1[,3],by=list(data1$Holiday_Flag),FUN=mean, na.rm=TRUE) %>% pull(x)
agg_mean

#Formatting the date column
data$Date=as.Date(data$Date,format="%d-%m-%Y")
head(data)

#Step 3: Creating a new event column for all four holiday events based on holiday dates
head(data)
data['Event'] <- NA

data %>% 
  mutate(Event = case_when(
    (Date == '12-02-2010' | Date == '11-02-2011' | Date == '10-02-2012' | Date == '08-02-2013') & Holiday_Flag == 1 ~ "Event1",
    (Date == '10-10-2010' | Date == '09-10-2011' | Date == '07-10-2012' | Date == '06-10-2013') & Holiday_Flag == 1 ~ "Event2",
    (Date == '26-11-2010' | Date == '25-11-2011' | Date == '23-11-2012' | Date == '29-11-2013') & Holiday_Flag == 1 ~ "Event3",
    (Date == '31-12-2010' | Date == '30-12-2011' | Date == '28-12-2012' | Date == '27-12-2013') & Holiday_Flag == 1 ~ "Event4"))

View(data)    


# Step 4: Find the sales average for all four holiday events
Event1 <- data %>% filter((Date == '12-02-2010' | Date == '11-02-2011' | Date == '10-02-2012' | Date == '08-02-2013') & Holiday_Flag == 1)
Avg_Event1 <- mean(Event1$Weekly_Sales) 
Avg_Event1

Event2 <- data %>% filter((Date == '10-10-2010' | Date == '09-10-2011' | Date == '07-10-2012' | Date == '06-10-2013') & Holiday_Flag == 1)
Avg_Event2 <- mean(Event2$Weekly_Sales) 
Avg_Event2

Event3 <- data %>% filter((Date == '26-11-2010' | Date == '25-11-2011' | Date == '23-11-2012' | Date == '29-11-2013') & Holiday_Flag == 1)
Avg_Event3 <- mean(Event3$Weekly_Sales) 
Avg_Event3

Event4 <- data %>% filter((Date == '31-12-2010' | Date == '30-12-2011' | Date == '28-12-2012' | Date == '27-12-2013') & Holiday_Flag == 1)
Avg_Event4 <- mean(Event4$Weekly_Sales) 
Avg_Event4
