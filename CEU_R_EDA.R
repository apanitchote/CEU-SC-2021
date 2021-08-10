# set your working directory
setwd("~/OneDrive - Khon Kaen University/5. PPT lectures/R_workshop")

# loading packages
library(tidyverse); library(dslabs); library(lubridate)
library(tableone); library(knitr); library(broom)
library(gtsummary)

# Install package devtools and MIMICbook
if(!("devtools" %in% installed.packages()[,1])) {
  install.packages("devtools",repos="https://cloud.r-project.org")
}
library(devtools)
if(!("MIMICbook" %in% installed.packages()[,1])) {
  install_github("jraffa/MIMICbook")
}
library(MIMICbook)

#######################################################
################# check and clean data ################
#######################################################

# Read data frame
dir()  # look for files in the directory
aline <- read.csv("aline_full_cohort_data.csv", stringsAsFactors = TRUE)

# explore data structure
str(aline)
glimpse(aline)  # alternative 

# look for variable names
names(aline)

# summary data and check dimension
summary(aline)
dim(aline)
nrow(aline)
ncol(aline)

# Before analysis, should check data type first and convert to an appropriate type
# Convert to factor
cols.fac <- c("aline_flg", "gender_num", "service_unit", "day_icu_intime")
aline[cols.fac] <- lapply(aline[cols.fac], as.factor)


#######################################################
################# One way and two way table ###########
#######################################################

# One-way table
.table <- table(aline$aline_flg)
print(.table)

# for percentage
.table/nrow(aline)*100
.table/sum(.table)*100
prop.table(.table)*100

# Two way table
.table <- table(aline$gender_num, aline$aline_flg, 
                dnn = c("Male","IAC"))
print(.table)

# Two way table with col percent
prop.table(.table, margin = 2)*100

# Two way table with row percent
prop.table(.table, margin = 1)*100


#######################################################
################# numerical summary ###################
#######################################################

# mean and sd
mean(aline$age, na.rm.= TRUE)
sd(aline$age, na.rm = TRUE)

# median and IQR
quantile(aline$wbc_first, 
         probs = c(.50, .25, .75), 
         na.rm = TRUE)
# five-number summaries
fivenum(aline$wbc_first)
summary(aline$wbc_first)

# summarise by group
options(digits = 4)
mean(aline$age)
aline %>% group_by(aline_flg) %>% 
  summarise(mean_age = mean(age), sd_age = sd(age),
            p50_age = quantile(age, .5),
            p25_age = quantile(age, .25),
            p75_age = quantile(age, .75)) %>% 
  kable()

aline %>% 
  select(aline_flg, age, sofa_first, wbc_first) %>% 
  tbl_summary(by = aline_flg, missing = "no")


aline %>% 
  select(aline_flg, age, gender_num, sofa_first) %>% 
  tbl_summary(by = aline_flg, missing ="no",
              statistic = 
                list(age ~ "{mean} ({sd})",
                     sofa_first ~ "{median} ({p25}, {p75})"),
              digits = 
                list(age ~ c(1,1),
                     gender_num ~ c(0,1),
                     sofa_first ~ c(0,1,1))) %>% 
  add_p(test = list(age ~ "t.test",
                    gender_num ~ "chisq.test",
                    sofa_first ~ "wilcox.test"))



aline %>% 
  select(aline_flg, age, sofa_first, service_unit, wbc_first) %>% 
  tbl_summary(by = aline_flg, missing = "no") %>% 
  add_p() %>% 
  add_overall()

#######################################################
################# 95% confidence interval #############
#######################################################

z_99 <- qnorm(0.995)  # calculate z to solve for 99% CI
print(z_99)
z_95 <- qnorm(0.975) # calculate z to solve for 95% CI
print(z_95)

aline %>% summarise(n = n(),mean_age = mean(age),
                    sd_age = sd(age),
                    se_age = sd_age/sqrt(n),
                    LCI = mean_age - z_95*se_age,
                    UCI = mean_age + z_95*se_age)


aline %>% summarise(p = mean(aline_flg),
                    n = n(),
                    se = sqrt(p*(1-p)/n),
                    LCI = p - z_95*se,
                    UCI = p + z_95*se) %>% kable()

##################################################
####### Test correlation #########################
##################################################

## Test correlation
cor.test(aline$bun_first,aline$creatinine_first)
cor.test(aline$bun_first,aline$creatinine_first,method="spearman")
cor.test(aline$bun_first,aline$creatinine_first,method="kendal")

#################################################
############ Visualization ######################
#################################################

# 1. Histogram
hist(aline$age)
ggplot(aline, aes(age))+
  geom_histogram(color = "black")

ggplot(aline, aes(age))+
  geom_histogram(binwidth = 1, color = "black")

# 2. Histogram with normal density plot
p <- ggplot(aline, aes(age))
p + geom_histogram(aes(y=stat(density)), 
                   fill = "red", alpha = 0.5) + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(aline$age), sd =sd(aline$age)),
                col = "red", size = 1)

# 3. histogram with kernel density plot
p + geom_histogram(aes(y=stat(density)), alpha =0.5) + 
  geom_density(color = "blue", size = 1)

# 4. Dot plot
aline %>% 
  ggplot(aes(y=age, x = factor(aline_flg)))+
  geom_dotplot(binaxis='y', stackdir='center', 
               dotsize=0.3, fill="azure1")

# 5. Box plot
aline$aline_flg <- as.factor(aline$aline_flg)
aline %>% ggplot(aes(x = aline_flg, y = sofa_first)) +
  geom_boxplot() + ylab("First SOFA score") + xlab("IAC")

# 6. Box plot with Dot plot
aline %>% ggplot(aes(x = aline_flg, y = sofa_first)) +
  geom_dotplot(binaxis='y', stackdir='center', 
               dotsize=0.1, fill="azure1", color = "red") +
  geom_boxplot() + ylab("First SOFA score") + xlab("IAC")

# 7. Violin plot
ggplot(aline, aes(x = "", y=age)) + 
  geom_violin(fill = "gray75", width = 0.3)+
  geom_boxplot(width = 0.1)

# 8. Violin + box plot + median point by group
ggplot(aline, aes(x = aline_flg, y=age)) + 
  geom_violin(fill = "gray75", width = 0.3)+
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=2, color="red") 

# 9. Bar chart
aline$day_icu_intime <- trimws(aline$day_icu_intime)
my_sum <- aline %>% 
  count(day_icu_intime) %>% 
  mutate(percent = n/sum(n)*100)

my_sum <- my_sum %>% 
  mutate(day_icu_intime = 
           fct_relevel(day_icu_intime, "Monday","Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday",
                       "Sunday"))

ggplot(my_sum, aes(x = day_icu_intime, y = n)) + 
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.7)

# 10. Bar chart with text
my_sum %>% mutate(day_icu_intime = reorder(day_icu_intime, n, median)) %>% 
  ggplot(aes(x= day_icu_intime, y =n)) + 
  geom_bar(stat = "identity", fill="forestgreen") + 
  geom_text(aes(label=n), vjust = -.5) +
  ylim(c(0, 300))

# 11. Bar chart with percent
my_sum %>% 
  mutate(day_icu_intime = reorder(day_icu_intime, n, median)) %>% 
  ggplot(aes(x = day_icu_intime, y = percent)) + 
  geom_bar(stat = 'identity', fill = "forestgreen")+
  geom_text(aes(label = round(percent,1)), vjust = -0.5)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  coord_cartesian(ylim = c(0,20))

# 12. Bar graph by group
aline %>% 
  mutate(weekend = ifelse(day_icu_intime=="Sunday"|day_icu_intime=="Saturday",
                          "Weekend", "Weekday"),
         icu_exp_flg = ifelse(icu_exp_flg==1,"Death", "Alive")) %>% 
  count(weekend = weekend, icu_exp_flg = icu_exp_flg) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = icu_exp_flg , y=pct, fill = weekend)) + 
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = function(x) paste0((x*100), "%"))

# 13. Bar group with stack
my_sum <- aline %>% filter(!is.na(sofa_first)) %>% 
  mutate(sofa3gr = ifelse(sofa_first<=6,"Mild",
                          ifelse(sofa_first<=12,"Moderate","Severe")),
         IAC = factor(aline_flg, labels = c("No","Yes"))) %>% 
  group_by(IAC) %>% 
  count(sofa3gr) %>% mutate(pct = n/sum(n))

my_sum %>% ggplot(aes(IAC, pct, fill = sofa3gr)) + 
  geom_col(position = 'stack') + 
  scale_y_continuous(labels = function(x)paste0(x*100,"%"))+
  scale_fill_brewer()

# 10. Dynamite plunger plot (bar chart with whisker)
aline$aline_flg <- as.factor(aline$aline_flg)
my_sum2 <- aline %>% group_by(aline_flg) %>% 
  summarise(mean = mean(age), sd = sd(age), n = n()) %>% 
  mutate(se = sd/sqrt(n), 
         ic = qt(0.975, n-1)*se)

p1 <- my_sum2 %>% ggplot(aes(aline_flg, mean)) + 
  geom_bar(stat = 'identity', fill = 'deepskyblue', alpha = 0.7)

A <- p1 + geom_errorbar(aes(x=aline_flg,ymin = mean-sd, ymax=mean+sd), width=0.2)+
  ylab("Mean age (years)") + xlab("") +
  scale_x_discrete(labels = c("0" = "No IAC", "1" = "IAC"))+
  ggtitle("Bar plot using standard deviation")+
  theme_linedraw()

B <- p1 + geom_errorbar(aes(x=aline_flg,ymin = mean-ic, ymax=mean+ic), width=0.1)+
  ylab("Mean age (years)") + xlab("") +
  scale_x_discrete(labels = c("0" = "No IAC", "1" = "IAC"))+
  ggtitle("Bar plot using 95%CI")+
  theme_linedraw()

# 11. combine plots in one pic
library(ggpubr)
ggarrange(A,B, labels = c("A", "B"), ncol=2, nrow = 1)


# 12. scatter plot
library(palmerpenguins)
data(package = 'palmerpenguins')  # look for dataset in a package
data("penguins")
str(penguins)
head(penguins)

penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm)) + 
  geom_point()

# 13. scatter point by group
penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm, color = species)) + 
  geom_point(size = 3)

# 14. scatter point by group and add line
penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm, color = species, 
             pch = species)) + 
  geom_point(size = 3) + geom_smooth(method = 'lm', se = F)

# 15. add facet
penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm))+ geom_point()+ 
  facet_wrap(~species)


