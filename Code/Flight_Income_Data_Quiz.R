library(nycflights13) 
library(tidyverse) 
View(flights)
Delays <- filter(flights, month == 12, day == 30)
filter(flights, dep_delay == "NULL")
is.null(flights$dep_delay)
if (any(is.na(flights[,"dep_delay"])))
is.na(flights$dep_delay)
sum(is.na(flights))
sum(is.null(flights))
summarise(Delays, delay = mean(dep_delay, na.rm = TRUE))
?is.null

summary(flights)
mean(income[["M_workers"]], na.rm = TRUE)
?mean
mean(income[["M_workers"]], na.rm = TRUE, trim = 0.1)
median(income[["M_workers"]], na.rm = TRUE)
library(matrixStats)

weighted_median <- function(x, w, ..., na.rm = FALSE){ 
  if(na.rm){ 
    df_omit <- na.omit(data.frame(x, w)) 
    return(weightedMedian(df_omit$x, df_omit$w, ...)) 
  }  
  weightedMedian(x, w, ...) 
} 

weightedMedian(income[["M_workers"]], w=income[["Industry"]], na.rm=TRUE)
weighted_median(income[["M_weekly"]], w=income[["Industry"]])



sd(income[["F_workers"]])
IQR(income[["F_workers"]])
mad(income[["F_workers"]])
income <- as.tibble(income)
NewVariable <- filter(income, M_weekly > 2000)
NewVariable






library(dplyr) 
library(tibble)
View(income)

income <- as.tibble(income)
NewVariable <- filter(income, M_weekly > 2000)
NewVariable
summary(income)


income_New <- mutate(income, difference = as.numeric(M_weekly) - as.numeric(F_weekly))
income_New %>% glimpse()
SD_Difference <- sd(income_New[["difference"]], na.rm = TRUE)
round(SD_Difference, 2)



weightedMedian(income[["M_workers"]], w=income[["Industry"]], na.rm=TRUE)
edit(weightedMedian)
weighted_median <- function(x, w, ..., na.rm = FALSE){ 
  if(na.rm){ 
    df_omit <- na.omit(data.frame(x, w)) 
    return(weightedMedian(df_omit$x, df_omit$w, ...)) 
  }  
  weightedMedian(x, w, ...) 
} 

income$M_weekly < as.integer(income$M_weekly)
weighted_median(income[["M_weekly"]], w = income[["Industry"]])
View(income_avi)

library(dplyr)
library(tibble)
income_avi <- as.tibble(income_avi)
ABC<- filter(income_avi, M_weekly > 2000)
arrange(ABC,M_weekly)

summary(flights)
filter(flights, dep_delay == "NULL")


ABC <- filter(flights, month == 12, day == 30)
filter(ABC, dep_delay == "NULL")
