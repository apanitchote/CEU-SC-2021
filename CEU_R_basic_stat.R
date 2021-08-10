# set your working directory
setwd("~/OneDrive - Khon Kaen University/5. PPT lectures/R_workshop")

# loading packages
library(tidyverse); library(dslabs); library(lubridate)
library(tableone); library(knitr); library(broom)
library(gtsummary)

# Read data frame
dir()  # look for files in the directory
aline <- read.csv("aline_full_cohort_data.csv")

# explore data structure
str(aline)
glimpse(aline)  # alternative 

############# 1. Assessing normality ###################
#### 1.1 quantile-quantile plots
# q-q plot check correlation of observed quantile vs. theoretical quantile
# define x and z
x <- aline$age
z <- scale(x)  # scale standard unit (mean = 0, sd =1)
mean(x)
sd(x)
mean(z)
sd(z)

# calculate observed and theoretical quantile
p <- seq(0.05, 0.95, 0.01)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
# make qq-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# qq-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p, mean = 0, sd = 1)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# qq plot using base package
qqnorm(aline$age, pch=1, frame=FALSE)
qqline(aline$age)

# qq plot using car package
library(car)
qqPlot(aline$age)

#### 1.2 Shapiro-Wilk normality test
shapiro.test(aline$age)

#### 1.3 Shapiro-Wilk normality test by group
shapiro.test(aline$age[aline$aline_flg==0])
shapiro.test(aline$age[aline$aline_flg==1])

############# 2. Hypothesis testing one sample #########
dir()
dengue <- read.csv("Data_S1.csv")
str(dengue)

# 2.1 One sample for population mean
# step 1. check normality
hist(dengue$Lowestpl)
ggpubr::ggqqplot(dengue$Lowestpl)

# step 2. perform test without transformation (assume normal distribution)
t.test(dengue$Lowestpl, mu = 40000, 
       alternative = "two.sided", conf.level = 0.95)

# manual calculation t-statistic for p-value and 95%CI
mu <- 40000
dengue %>% filter(!is.na(Lowestpl)) %>% 
  summarise(mean = mean(Lowestpl),
            sd = sd(Lowestpl),
            n = n(),
            se = sd/sqrt(n),
            t = (mean-mu)/se,
            pvalue = 2*pt(-abs(t), n-1),
            LCI = mean-qt(0.975, n-1)*se,
            UCI = mean + qt(0.975, n-1)*se) %>% kable()

# Nonparametric test: one-sample Wilcoxon signed rank test
# Is the median of a sample (platelet) significantly different from a hypothesized value (40000)
wilcox.test(dengue$Lowestpl, mu = 40000,
            alternative = "two.sided", conf.level = 0.95)

# 2.2 One sample for population proportion
str(dengue)
table(dengue$Death)
prop.table(table(dengue$Death))

n <- length(dengue$Death)
x <- length(dengue$Death[dengue$Death==1])
p0 <- 0.05

# step 1: check normal approximation
n*p0*(1-p0)  # 11.305 > 5 is normal approximation

# step 2: using One-proportion test
prop.test(x=x, n=n, p=p0, correct = T)

# manual calculation
p0 <- 0.05
dengue %>% mutate(death = ifelse(Death==1,1,0)) %>% 
  summarise(p_hat = mean(death), 
            n = length(death),
            z = (p_hat - p0)/sqrt(p0*(1-p0)/n),
            pvalue = 2*pnorm(-abs(z)),
            chisq = z^2)

# In case of n*p0*(1-p0) < 5 use exact binomial test
p0 <- 0.02
n*p0*(1-p0)
binom.test(x=x, n=n, p=0.02)

########### 3. Hypothesis testing two samples ##########

# histogram of platelet by death
dengue %>% 
  mutate(Death = ifelse(Death==1, "Death", "Alive")) %>% 
  ggplot(aes(Lowestpl)) + 
  geom_histogram(aes(y=stat(density)), fill = "salmon")+
  geom_density(color= "forestgreen") + facet_wrap(~Death)

# Box plot of platelet by death
dengue %>% 
  mutate(Death = ifelse(Death==1, "Death", "Alive")) %>% 
  ggplot(aes(Death, Lowestpl)) + geom_boxplot(fill= "coral1")

# Shapiro test by death
shapiro.test(dengue$Lowestpl[dengue$Death==1])
shapiro.test(dengue$Lowestpl[dengue$Death==2])

# log transformation of platelet count
dengue <- dengue %>% mutate(log_plt = log(Lowestpl))
dengue %>% 
  mutate(Death = ifelse(Death==1, "Death", "Alive")) %>% 
  ggplot(aes(log(Lowestpl))) + 
  geom_histogram(aes(y=stat(density)), fill = "salmon")+
  geom_density(color= "forestgreen") + facet_wrap(~Death)

# Shapiro test by death
shapiro.test(log(dengue$Lowestpl)[dengue$Death==1])
shapiro.test(log(dengue$Lowestpl)[dengue$Death==2])

# variance test
var.test(log_plt ~ Death, data = dengue)

# two-sample t-test (equal variance)
t.test(log_plt ~ Death, data = dengue, var.equal =TRUE)

# Wilcoxon-Mann Whitney
wilcox.test(Lowestpl ~ Death, data=dengue)

# paired t-test
library(MASS)
data("anorexia")
str(anorexia)

# convert ibs to kg
anorexia <- anorexia %>% 
  transmute(Treat = Treat, 
            Prewt = Prewt*0.45359237,
            Postwt = Postwt*0.45359237,
            Diffwt = Postwt - Prewt) %>% 
  rowid_to_column()

head(anorexia) %>% kable()

# Convert wide form to long form data
anorexia_long <- anorexia %>% 
  pivot_longer(cols = 3:4, names_to = "pre_post", 
               names_ptypes = list(pre_post = factor()),
               values_to = "wt" )
head(anorexia_long) %>% kable()

# line plot
anorexia_long %>% mutate(Treat = relevel(Treat, ref = "Cont")) %>% 
  ggplot(aes(x=pre_post, y=wt, group = rowid))+
  geom_point()+ geom_line()+facet_wrap(~Treat)

# Filter patients received FT (family treatment)
anorexia_ft <- anorexia %>% filter(Treat =="FT")

par(mfrow=c(1,2))
hist(anorexia_ft$Diffwt)
qqnorm(anorexia_ft$Diffwt)
qqline(anorexia_ft$Diffwt)
shapiro.test(anorexia_ft$Diffwt)

# paired t-test
t.test(anorexia_ft$Postwt, anorexia_ft$Prewt, paired = TRUE)

# one sample t-test
t.test(anorexia_ft$Diffwt, mu = 0)

# paired sample Wilcoxon test
anorexia_cbt <- anorexia %>% filter(Treat =="CBT")
par(mfrow=c(1,2))
hist(anorexia_cbt$Diffwt)
qqnorm(anorexia_cbt$Diffwt)
qqline(anorexia_cbt$Diffwt)
shapiro.test(anorexia_cbt$Diffwt)

wilcox.test(anorexia_cbt$Postwt, anorexia_cbt$Prewt, paired = TRUE, correct = FALSE)

########### 4. ANOVA & Multiple testing ##########
anorexia_cont <- anorexia %>% filter(Treat =="Cont")
shapiro.test(anorexia_cont$Diffwt)

# Histogram of Diffwt by Treat
anorexia %>% mutate(Treat = relevel(Treat, ref="Cont")) %>% 
  ggplot(aes(Diffwt))+geom_histogram(binwidth = 0.6, fill = "darkgoldenrod")+
  facet_wrap(~Treat)

# Summary Diffwt by Treat
anorexia %>% mutate(Treat = relevel(Treat, ref="Cont")) %>% 
  group_by(Treat) %>% 
  summarise(mean_diff = mean(Diffwt), sd = sd(Diffwt), n = n()) %>% 
  kable()

# box plot by Treat
anorexia %>% mutate(Treat = relevel(Treat, ref="Cont")) %>% 
  ggplot(aes(Treat, Diffwt, fill = Treat)) + geom_boxplot()

# check equal variances assumption
library(car)
leveneTest(Diffwt ~ Treat, data = anorexia, center = "mean")

# ANOVA
anovaModel <- aov(Diffwt ~ Treat, data = anorexia)
summary(anovaModel)

# Multiple comparison adjustments
# bonferroni
pairwise.t.test(anorexia$Diffwt, anorexia$Treat, p.adjust.method = "bonferroni")

# Tukey's HSD
tukey.test <- TukeyHSD(anovaModel)
plot(tukey.test)

# Dunnett's test
library(multcomp)
anorexia$Treat <- relevel(anorexia$Treat, ref = "Cont")
anovaModel <- aov(Diffwt ~ Treat, data = anorexia)
post_test <- glht(anovaModel, linfct = mcp(Treat = "Dunnett"))
summary(post_test)
plot(post_test)

# Kruskal-Wallis test
kruskal.test(Diffwt ~ Treat, data = anorexia)
pairwise.wilcox.test(anorexia$Diffwt, anorexia$Treat, 
                     p.adjust.method = "bonferroni")

############ 4. categorical data analysis ##########
# contingency table
dengue <- dengue %>% 
  mutate(Shockrep = ifelse(Shockrep==1, 1, 0),
         Death = ifelse(Death ==1,1,0))

table <- table(dengue$Shockrep, dengue$Death, 
               dnn = c("shock > 1 times", "Death"))

dimnames(table) <- list(c("No", "Yes"),c("Alive", "Death"))
names(dimnames(table)) <- c("Shock > 1 times", "Death")

obs_freq.dat <- as.data.frame.matrix(table)

# calculate expected frequency
row1 <- sum(obs_freq.dat[1,])
row2 <- sum(obs_freq.dat[2,])
col1 <- sum(obs_freq.dat[,1])
col2 <- sum(obs_freq.dat[,2])
n <- sum(obs_freq.dat)

exp_freq.dat <- matrix(c(row1*col1/n, row1*col2/n, row2*col1/n,
                         row2*col2/n), nrow=2,ncol=2, byrow = T)

# calculate chi-square statistic
O <- as.vector(t(obs_freq.dat))
E <- as.vector(t(exp_freq.dat))

sum((O-E)^2/E)


# chi-square test
chisq_result <- chisq.test(dengue$Shockrep, dengue$Death, correct = F)
chisq_result$expected
chisq_result$observed

# Fischer exact test
table <- table(dengue$Obesity, dengue$Death)
chisq_result2 <- chisq.test(table)
chisq_result2$expected   # E < 5 more than 20%

fisher.test(dengue$Obesity, dengue$Death)

# Relative risk
library(EpiStats)
cs(dengue, cases = "Death",exposure = "Shockrep", exact = TRUE)

# Odds ratio
cc(dengue, cases = "Death",exposure = "Shockrep", exact = TRUE)
