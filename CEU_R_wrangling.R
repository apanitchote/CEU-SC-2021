#################################################################
################## Data wrangling ###############################
#################################################################

setwd("~/OneDrive - Khon Kaen University/6. Rscript/Data_wrangling")


# useful packages
library(tidyverse) # data transformation
library(readr)     # import .csv, .txt 
library(readxl)    # import excel file
library(rjson)     # import json file
library(rvest)     # import html file
library(RSQLite)   # import SQL file
library(haven)     # import SPSS, SAS, STATA file
library(lubridate) # date manipulation

# sort, order, and rank
library(dslabs)
data("murders")
str(murders)
View(murders)

sort(murders$total)
sort(murders$total, decreasing = TRUE)


x <- c(31, 4, 15, 92, 65)
sort(x)
(index <- order(x))

index <- order(murders$total)
murders$abb[index]

i_max<- which.max(murders$total)
murders$state[i_max]

x
rank(x)
order(x)


# Tibble data frame
library(hflights)
data("hflights")
head(hflights)

hflights <- as_tibble(hflights)

# Selecting variable
select(hflights, ActualElapsedTime, AirTime, ArrDelay,DepDelay)
select(hflights,14:19)
select(hflights,1:4,12:21)
select(hflights,contains("Delay"))
select(hflights,contains("Ca"),contains("Num"))
select(hflights,ends_with("Time"),ends_with("Delay"))
select(hflights,1:6,-3 )

# Filtering observation
filter(hflights, Distance>=3000)
filter(hflights, UniqueCarrier%in%c("AA", "US", "DL") )
filter(hflights, TaxiIn+TaxiOut>AirTime)
filter(hflights, DepTime<500|ArrTime>2200)
filter(hflights, DepDelay>0&ArrDelay<0)
filter(hflights, DepDelay>0&Cancelled==1)
c1 <- filter(hflights, Dest == "JFK")

# mutate variable
# ArrDelay (negative value = arrival early, positive = arrival late)
# DepDelay (negative value = depart late, positive = depart early)
g1 <- mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime)
g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut)
g3 <- mutate(g2, AverageSpeed = Distance / AirTime * 60)
m1 <- mutate(hflights, loss = ArrDelay - DepDelay, loss_ratio = loss/DepDelay)
m2 <- mutate(hflights, TotalTaxi = TaxiIn + TaxiOut, 
             ActualGroundTime = ActualElapsedTime - AirTime, 
             Diff = TotalTaxi - ActualGroundTime)
c2 <- mutate(c1, Date = paste(Year, Month, DayofMonth, sep = "-"))

# arrange
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))
# Arrange dtc by departure delays
arrange(dtc, DepDelay)
arrange(dtc, DepDelay) %>% pull(DepDelay)
arrange(dtc, DepDelay) %>% .$DepDelay

arrange(dtc, UniqueCarrier, DepDelay)
arrange(dtc, UniqueCarrier, DepDelay) %>% 
  select(UniqueCarrier, DepDelay)

arrange(hflights, UniqueCarrier, desc(DepDelay))

arrange(hflights, UniqueCarrier, desc(DepDelay)) %>% select(UniqueCarrier, DepDelay)

# summarise
# Print out a summary with variables min_dist and max_dist
summarise(hflights, min_dist = min(Distance), 
          max_dist = max(Distance))

# Print out a summary with variable max_div
summarise(filter(hflights,Diverted==1), max_div=max(Distance))


# Generate summary about ArrDelay column of temp1
hflights %>% filter(!is.na(ArrDelay)) %>% summarise(
  earliest = min(ArrDelay),
  average = mean(ArrDelay), 
  latest = max(ArrDelay),
  sd = sd(ArrDelay))

hflights %>% 
  filter(!is.na(TaxiIn), !is.na(TaxiOut)) %>% 
  summarise(max_taxi_diff = max(abs(TaxiIn-TaxiOut)))

summarise(hflights, 
          n_obs = n(),
          n_carrier = n_distinct(UniqueCarrier), 
          n_dest = n_distinct(Dest))

# All American Airline flights
aa <- filter(hflights, UniqueCarrier == "AA")
# Generate summarizing statistics for AA
summarise(aa, 
          n_flights = n(), 
          n_canc = sum(Cancelled == 1),
          avg_delay = mean(ArrDelay, na.rm = TRUE))

# pipe operator
hflights %>% 
  mutate(diff = TaxiOut-TaxiIn) %>% 
  filter(!is.na(diff)) %>% 
  summarise(avg = mean(diff))

hflights %>% 
  mutate(RealTime = ActualElapsedTime + 100, 
         mph = Distance/RealTime*60) %>%
  filter(!is.na(mph),mph < 70) %>% 
  summarise(n_less = n(),
            n_dest = n_distinct(Dest), 
            min_dist = min(Distance), 
            max_dist = max(Distance))

# group by
hflights %>% 
  group_by(UniqueCarrier) %>%
  summarise(p_canc = sum(Cancelled==1)/n()*100,
            avg_delay = mean(ArrDelay, na.rm=TRUE)) %>%
  arrange(avg_delay)

################ Working with A-line dataset #############################
aline <- read.csv("aline_full_cohort_data.csv", stringsAsFactors = TRUE)
aline <- tbl_df(aline)
head(aline)
str(aline)

# change data type in an variable
aline$aline_flg <- as.factor(aline$aline_flg)

# change data type in multiple column
cols_fac <- c("gender_num", "service_num", "hosp_exp_flg", 
              "icu_exp_flg", "day_28_flg")

aline[cols_fac] <- lapply(aline[cols_fac], as.factor)

# change variable name
aline <- aline %>% rename(male = gender_num)
names(aline)[names(aline)=='gender_num'] <- 'male'


# create group using if_else
# create obsity
aline$obesity <- with(aline, 
                      if_else(bmi >= 40, "Class III obesity",
                              if_else(bmi >= 35, "Class II obesity",
                                      if_else(bmi >= 30, "Class I obesity",
                                              if_else(bmi >= 25, "Overweight",
                                                      if_else(bmi >= 18.5, "Normal weight", 
                                                              "Underweight"))))))
table(aline$obesity)
class(aline$obesity)

# create using case_when
aline <- aline %>% mutate(obesity.2 = case_when(
  bmi >= 40 ~ "Class III obesity",
  bmi >= 35 ~ "Class II obesity",
  bmi >= 30 ~ "Class I obesity",
  bmi >= 25 ~ "Overweight",
  bmi >= 18.5 ~ "Normal weight",
  bmi < 18.5 ~ "Underweight"
))

aline %>% filter(obesity.2=="Underweight") %>% select(bmi, obesity.2, obesity)
table(aline$obesity)
table(aline$obesity.2)

# create group using cut
library(Hmisc)
aline$obesity.3 <- cut2(aline$bmi, c(18.5, 25, 30,35,40))

# change obesity to class factor and ordered
aline$obesity <- factor(aline$obesity, levels = 
                          c("Normal weight","Underweight","Overweight",
                            "Class I obesity" ,"Class II obesity", 
                            "Class III obesity" ))

# working with date
library(lubridate)
my_dates <- c(
  "2020-01-15", "2021-01-23", "2021-09-09"
)
class(my_dates)

# convert chac to date data type
my_dates <- ymd(my_dates)
class(my_dates)

# Thai date style
thai_dates <- c("15/12/1980", "12-09-1999", "14/Mar/2020", "23082018")
dmy(thai_dates)


## extract information my date object
my_dates
year(my_dates)
month(my_dates, label = TRUE, abbr = FALSE)
day(my_dates)
wday(my_dates, label=TRUE, abbr = FALSE)

# Current date and time
today()
timeNow <- now()
year(timeNow)
hour(timeNow)
minute(timeNow)
second(timeNow)

# Calculation with date
date1 <- ymd("2021-01-23")
date2 <- ymd("2021-05-15")
date2-date1
as.numeric(date2-date1)
date1+28
date1-10


# combing multiple table
library(tidyverse)
library(nycflights13)

data("flights")
data("airlines")
data("airports")
data("planes")
data("weather")

# Inner join
flightsJoined <- flights %>% 
  inner_join(airlines, by = c("carrier" = "carrier"))

flightsJoined %>% select(carrier, name)
nrow(flights)
nrow(flightsJoined)

# left join
str(airports)

# filter pacific time zone (UTC -8)
airportsPT <- airports %>% filter(tz == -8)
nrow(airportsPT)


# left_join with flights
nycDests <- left_join(flights, airportsPT, by = c("dest" = "faa"))
dim(flights)
dim(airportsPT)
dim(nycDests)
str(nycDests)

###################### tidy data ###############################
## example tidy data
devtools::install_github("rstudio/EDAWR")
library(EDAWR)
?EDAWR::storms
?cases
?pollution

# Storms is tidy data
storms

# Cases is not tidy data
cases

# we need to gather into country, year, n column
cases%>%gather("Year", "n",-country)
cases%>%pivot_longer(cols = '2011':'2013', names_to = "Year", values_to="n")

# Pollution: tidy data with city, large, small
pollution
pollution%>%spread(key=size, value = amount)
pollution%>%pivot_wider(names_from = size, values_from = amount)

# Unite() and separate ()
storms
storms2 <- storms%>%
  separate(col = date, into=c("year", "month", "day"), sep="-")
storms2%>%unite(col = date, year, month, day, sep="-")

####################### Basic function ####################
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  print(s/n)
}
x <- 1:100
avg(x)
mean(x)
identical(avg(x), mean(x))

# function can have multiple arguments
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
avg(x)
avg(x, F)

# for loop
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(3)
compute_s_n(100)
compute_s_n(2021)

for(i in 1:5){
  print(i)
}
i

m <- 25
# create an empty vector
s_n <- vector(length = m)
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

n <- 1:m
plot(n, s_n)
lines(n, n*(n+1)/2)

library(dslabs)
data(heights)
str(heights)
sum(ifelse(heights$sex=="Female",1,2))
mean(ifelse(heights$height>72,heights$height,0))

inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)

sum(inches_to_ft(heights$height)<5)
any(F,F,F)
all(T,T,F)
all(T, F,F)
all(F,F,F)

factorial(4)
