# set up your working directory
setwd("~/OneDrive - Khon Kaen University/5. PPT lectures/R_workshop")

# get working directory
getwd()

# looking files in current working directory
list.files()
dir()

# call library
library(tidyverse)

# basic calculation
1 + 1
5 - 2
5 * 3
4 / 3
10 %% 2
11 %% 3
6 ^ 6
6 ** 6

# Object in r
a1 <- 10
a2 <- 20
a1 + a2

# Data types: numeric, integer, character, factor, logical
# 1. numeric or double
temp <- c(36.7, 35.6, 38)
print(temp)

# 2. integer
age <- c(63L, 50L, 36L)
print(age)

# 3. character 
name <- c("David", "Susan", "Anny")
print(name)

# 4. factor
sex <- factor(c("male", "female", "female"))
print(sex)

# 5. logical
dead <- c(TRUE, FALSE, FALSE)
print(dead)

### Defining Strings: Single and Double Quotes and How to Escape
s <- "Hello!"   # double quotes define a string
s <- 'Hello!'   # single quotes define a string
s <- `Hello`    # backquotes do not


s <- '10"'      # correct
# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)
# to include both single and double quotes in string, escape with \
s <- "5'10\""
cat(s)
s <- '5\'10"'
cat(s)

# Data structures: vector, matrix, data frame, 
# 1. vector
c("David", "Susan", "Anny")

# you can also do:
codes <- c(italy = 380, canada=124, egypt = 818)
codes
codes <- c("italy"= 380, "canada" = 124, "egypt" = 818)
codes

# vector coercion
x <- c(1, "cannda", 3)
x
class(x)


# alternative
codes <- c(380, 124, 818)
country <- c("italy", "cannada", "egypt")
names(codes) <- country
codes

# seq
seq(1, 10)
seq(1, 10, 2)
seq(1:10)


# 2. matrix
d <- seq(1, 50, 2)
length(d)
d_matrix <- matrix(d, 5, 5)
d_matrix

# 3. data frame
ipd <- data.frame( name,age, sex,temp, dead)
View(ipd)

# built in df set in r
head(diamonds)
View(diamonds)

# 4. list
x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
print(x)

ipd_list <- list(ipd=ipd, address= c("123 Nai-Muange Khon Kaen",
                                     "23 Cleveland OH",
                                     "4423/58 Sukumvit BKK"))
######################## sub-setting (slice) ################################
# sub-setting vector
age
age[1]
age[-2]
age[1:2]
age[c(1,3)]

# sub-setting data frame
ipd
ipd[1,]  # extract row 1
ipd[,2]  # extract col 2
ipd[1,2]  # extract cell at row 1 and column 2
ipd[1, c(1,4)] #extract row 1 and col 1, 4

ipd$name  # "$" call accessor 
ipd[,"name"]
ipd[,c("name","age")]
ipd$name[2]

# sub-setting list
ipd_list[1]  # get class list as out
ipd_list["ipd"]

ipd_list[[1]]  
ipd_list[["address"]]

ipd_list[[1]][2]
ipd_list[[1]][[2]]

ipd_list[[1]][2,]

ipd_list[[1]][3]

############################# read data frame ####################
penguins <- read.csv("penguins.csv")
head(penguins)

############################# install packages ##################
install.packages("readr",dependencies = TRUE)
library(readr)


############################# working with df ##################
install.packages("palmerpenguins")
library(palmerpenguins)
data("penguins")
head(penguins)# get first 6 rows
tail(penguins)# get last 6 rows
dim(penguins)# check number of row and column
str(penguins)# explore data structures
View(penguins)# View as spreadsheet
summary(penguins)


# Subsetting By Conditions
penguins[penguins$body_mass_g >= 5000, c("species", "body_mass_g")]
penguins[penguins$body_mass_g >= 5000 & penguins$sex == "male", 
         c("species", "body_mass_g", "sex")]

subset(penguins, body_mass_g >=5000 | penguins$sex == "male")

# Five number statistics
mean(penguins$bill_length_mm, na.rm=TRUE)
sd(penguins$bill_length_mm, na.rm=TRUE)
min(penguins$bill_length_mm, na.rm=TRUE)
max(penguins$bill_length_mm, na.rm=TRUE)
median(penguins$bill_length_mm, na.rm = TRUE)
quantile(penguins$bill_length_mm,probs = c(.25,.5,.75), na.rm=TRUE)
fivenum(penguins$bill_length_mm)


# which 
which.max(penguins$bill_length_mm)
penguins$bill_length_mm[186]

which.min(penguins$bill_length_mm)
penguins$bill_length_mm[143]

outlier <- which(penguins$bill_length_mm >= 55)
penguins[outlier,]

# missing values
summary(penguins)
sum(is.na(penguins$bill_length_mm))
sapply(penguins, function(x)sum(is.na(x)))
sum(!complete.cases(penguins)) 

# Storing Data in an External File
# drop NA
clean_penguins <- na.omit(penguins)
write.csv(clean_penguins, "clean_penguins.csv", row.names = FALSE)



