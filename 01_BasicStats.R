# Load required packages.
library(tidyverse)
library(psych)
library(sjmisc)
library(sjPlot)
library(labelled)

## (0) Read data file into R
dat00 <- read_csv("Data/Chapman.csv")

## (1) Basic data manipulations
# Select variables by using "select" function in `dplyr` package
dat00 %>% select(CORON)
dat00 %>% select(AGE, SYSBP, DIABP, CHOLES, HT, WT, CORON) # or
dat00 %>% select(-ID)

# Subsetting the data: Use `filter` in `dplyr` package.
dat00 %>% filter(CORON==0)
dat00 %>% filter(CHOLES>275)
dat00 %>% filter(CHOLES>275 & CORON==0)

## (2) Basic Descriptives

# Run descriptives for selected variables: Use `describe` in `psych` package.
dat00 %>% select(SYSBP, DIABP, CHOLES) %>% describe(fast=T)

# Run descriptives by groups: Use `describeBy` in `psych` package
dat00 %>% select(SYSBP, DIABP, CHOLES) %>% describeBy(group=dat00$CORON, fast=T)

# Run frequency table for categorical variables: Use `frq` in `sjmisc` package. 
dat00 %>% select(CORON) %>% frq()

# Run correlations for selected variables: Use `lowerCor` in `psych` package.
dat00 %>% select(SYSBP, DIABP, CHOLES, HT, WT) %>% lowerCor()

# Run correlations by to get p-values: Use `corr.test` in `psych` package.
dat00 %>% select(SYSBP, DIABP, CHOLES, HT, WT) %>% corr.test()

# Run correlations for a subset of data.
dat00 %>% filter(CORON==0) %>% select(SYSBP, DIABP, CHOLES)  %>% lowerCor()
dat00 %>% filter(CORON==1) %>% select(SYSBP, DIABP, CHOLES)  %>% lowerCor()

## (3) Linear regression

# Run a model with one predictor and store the results in a result object.
reg01 <- lm(CHOLES ~ CORON, data=dat00)

# See the summary of the results.
summary(reg01) 

  # Note: if there is no need to store the results object, we can do:
  lm(CHOLES ~ CORON, data=dat00) %>% summary() # or
  dat00 %>% lm(CHOLES ~ CORON, .) %>% summary()

# Run another model with two predictors.
reg02 <- lm(CHOLES ~ CORON + SYSBP, data=dat00)
summary(reg02)

# Run another model with two predictors and an interaction effect.
reg03 <- lm(CHOLES ~ CORON + SYSBP + CORON:SYSBP, data=dat00)
summary(reg03)

# Run the previous model by standardizing the predictor `SYSBP`.
reg03b <- lm(CHOLES ~ CORON + scale(SYSBP) + CORON:scale(SYSBP), data=dat00)
summary(reg03b)

# Model comparisons.
anova(reg03, reg03b)
anova(reg01, reg02)
anova(reg02, reg03)

## (4) Logistic regression

# Run models and see summary results.
lreg01 <- glm(CORON ~ CHOLES, family="binomial", data=dat00)
summary(lreg01)

lreg02 <- glm(CORON ~ CHOLES + SYSBP, family="binomial", data=dat00)
summary(lreg02)

lreg03 <- glm(CORON ~ CHOLES + SYSBP + CHOLES:SYSBP, family="binomial", data=dat00)
summary(lreg03)

# Model comparisons.
anova(lreg01, lreg02, test="LRT")
anova(lreg02, lreg03, test="LRT")

## (5) t-test

# Equal-variance assumption assumed.
t.test(CHOLES ~ CORON, data=dat00, var.equal=T) 

# Equal-variance assumption not assumed.
t.test(CHOLES ~ CORON, data=dat00) 

## (6) Some more data manipulations: Conversion to a categorical variable

# The variable `CORON` is intended as a categorical variable. However,
# it is really an integer variable in the data set.
dat00

# Retain the numerical values, but convert to categorical.
dat00$CORON2 <- factor(dat00$CORON, levels=c(0,1), labels=c("0", "1"))
dat00

# Text values can be assigned too.
dat00$CORON3 <- factor(dat00$CORON, levels=c(0,1), labels=c("no", "yes")) 
dat00

## (7) Basics of graphics by `ggplot`

# Bar graph of counts on one variable
ggplot(dat00, 
       aes(x=CORON)) + geom_bar()     # Not convenient if `CORON` is not a categorical!
ggplot(dat00, 
       aes(x=CORON2)) + geom_bar()    # Looks better!

# Some customizations
ggplot(dat00, 
       aes(x=CORON3)) + 
       geom_bar(fill="lightblue", colour="black") +
       ylim(c(0,250))

# We can create a basic graphic object and add specifications to the object
coron.bar <-  
  ggplot(dat00, 
         aes(x=CORON3)) + 
         geom_bar(fill="lightblue", colour="black", width=.5)
coron.bar  
  # Use the black-while theme to eliminate the grwy background
  coron.bar + 
    theme_bw()
  # Change the axis labels 
  coron.bar + 
    theme_bw() + 
    xlab("Coronary Incident") + 
    ylab("Counts")

# Box plot
ggplot(dat00, aes(x=CORON3, y=CHOLES)) + 
  geom_boxplot()

# Scatter plot
bp.scat <-
  ggplot(dat00, aes(x=SYSBP, y=DIABP)) + 
  geom_point()
bp.scat
  # Add a fitted line & SE band
  bp.scat + 
    stat_smooth(method = "lm", formula = y ~ x)
  bp.scat + 
    stat_smooth(method = "lm", formula = y ~ poly(x, 2))
  bp.scat +  
    stat_smooth(method = "loess", formula = y ~ x)
  # We can even draw separate lines for groups
  ggplot(dat00, aes(x=SYSBP, y=DIABP, colour=CORON3)) +
    geom_point() +
    stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

# Bar graph for Likert-type scale items: Use `plot_likert` in `sjPlot` package.
lik.dat <- read_csv("Data/likert.csv")

lik.dat %>% 
  select(item1:item5) %>% 
  plot_likert(cat.neutral = 3,
              grid.range = c(1.5, .5),
              values = "sum.outside",
              show.prc.sign = T)
    
## (8) Some more data manipulations

# Create a new variables.
dat00 <- 
  dat00 %>% 
  mutate(bp.dif = SYSBP - DIABP)

# Recode a variable: For one value
dat00 <-
  dat00 %>% 
  mutate(CORON4 = recode(CORON3, no = 0,
                                 yes = 1),
         CORON5 = recode(CORON3, no = "No!",
                                 yes = "Yes!"),
         CORON6 = recode(CORON, `0` = 1,
                                `1` = 2),
         CORON7 = recode(CORON, `0` = "NO!",
                                `1` = "YES!"))

# Recode a variable: For a range of values
dat00 <- 
  dat00 %>% 
  mutate(SYSBP2 = case_when(SYSBP >= 140 ~ "high",
                            SYSBP < 140 & SYSBP > 110 ~ "normal",
                            SYSBP <= 110 ~ "low"))

# Save data externally.
save(dat00, file="dat00.Rdata")

# Load saved data.
load("dat00.Rdata")

# Save / load more than one data objects.
save(dat00, lik.dat, file="dat00_lik.Rdata")
load("dat00_lik.Rdata")

## (9) Some other data manipulations
# Add / edit labels of the variable by `labelled` package.
var_label(dat00) <- 
  list(bp.dif = "SYSBP-DIAPB", 
       SYSBP = "Systolic Blood Pressure")

# Give value labels.
dat00 %>% select(CORON) %>% frq()

val_label(dat00$CORON, 0) <- "No"
val_label(dat00$CORON, 1) <- "Yes"

dat00 %>% select(CORON) %>% frq()

# End of the file
