## 1. The final score of the World Cup finals of football is France  4 and Croatia 2. 
##    Please use a control structure to print the results as Team ---- Wins -----.

France <- 4
Croatia <- 2

if (France < Croatia){
  print("Croatia wins")
} else {
  print ("France wins")
}

## 2. The mtcars data set has several factor variables. However, R is reading them as numeric. 
##    Please convert them into factors using a for loop. Please use column 8 to 11 for the loop.
##  You will receive a data set (Dataset 2.1). This data set is a subset of a real data set.  

str(mtcars)
for (i in 8:11)
{
  mtcars[, i] <- as.factor(mtcars[, i])
}
str(mtcars)

## 3. Write a function to get the percentage of NAs in each column.

library(readxl)
library(DT)
Data_set <- read_excel
datatable(Data_set)
head(is.na(Data_set))
colSums(is.na(Data_set))
perc <- function(x) {
  
(sum(is.na(x))/length(x))*100 ##function to calculate the percentage
}
m_perc_col <- apply(Data_set, perc)
head(m_perc_col)

## 4. Write a function to get the percentage of NAs in each row.

perc <- function(x) {
  
  (sum(is.na(x))/length(x))*100
}
m_perc_row <- apply(Data_set, perc) ## we just change it to row instead of col
head(m_perc_row)

## 5. Write a function to get a summary of numeric columns (use the summary function) such as THC, CO, CO2, and so on in the data set. 
##    With the same function, try to generate box plot using base R.

s_data <- function(a){
  print(summary(a))
  boxplot(a)
}
s_data(Data_set$THC)
s_data(Data_set$CO)
s_data(Data_set$CO2)

## 6. Write a function to create histograms of numeric columns, 
##    such as THC, CO, CO2, and so on in the data set. Use ggpot2 to generate figures.

library(ggplot2)
data <- function(x, a) ## I had difficulties doing this question, so I watched Professor's solution to it and understood it
ggplot(x) +

## 7. The data set contains date columns. All of these date columns are untidy. 
##    Please create a better formatted data set. The date should be dd/mm/yyyy in the final format. Use columns 2, 3, and 5 only.

library(readr)
library(tidyverse)
library(tidyr)
Date_set <- read.csv("")
head(Date_set)
Date_set$'First.Date' <- mdy(Data_set$'First.Date')
Date_set$'Last.Date' <- mdy(Data_set$'Last.Date')
head(Date_set)

## 8. The date of birth column contains months in string format. 
##    Please create a tidy data column with months in numeric format. 
##    Now your data should be similar to the previous question.

tidy_date_1 <- Data_set 
separate('Date.of.birth', c("D", "M", "Y"), sep = "/")
mutate ()
unite ()
head(tidy_date_1)

## 9. Convert all dates into date format; they are currently in character variable format.

df$date <- as.character(df$date)

## 10. Create a new column age based on the date of birth column and the first FD column. Date format is necessary to do basic arithmetic.

tidy_date_2 <- tidy_date_1 
mutate(Age = (First.Date - Date_of_birth)/365)
separate(Age, (c("Age","Decimal")))
select(-("Decimal"))
head(tidy_date_2)
