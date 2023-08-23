## Load some useful packages in
library(tidyverse)
library(dplyr)
library(pwr)
library(patchwork)

## Load the data, set working directory to the source file location
resume <- read.csv("resume.csv")

resume %>% 
  View()

## Changing the "call" variable to "Yes" and "No" instead of 1 and 0
resume$call <- ifelse(resume$call==1, "Yes", "No")

## Create the cross-tabulation
xtab <- table(resume$race, resume$call)
xtab

## Chi-squared test for risk difference
chisq.test(xtab)
b = 157 / (2278 + 157)
w = 235 / (2200 + 235)
list(b, w)
## Black applicants received callbacks 6.45% of the time, whereas whites 
## received callbacks 9.65% of the time p < 0.001

## Perform a Fisher's exact test to get the OR and 95% CI
fisher.test(xtab)
## OR = 1.54 [1.25, 1.93]

## Does sex have an impact?
xtab2 <- table(resume$sex, resume$call)
xtab2

fisher.test(xtab2)
## OR = 0.89 [0.68, 1.15]
## Males got 11% fewer callbacks than females, but this was statistically
## insignificant

## Conditional on sex, does race have an impact on calls back?
## Sort the data into only males and females
resume_m <- subset(resume, sex == "male")
resume_f <- subset(resume, sex == "female")

## Fisher test for black and white males
xtab_m <- table(resume_m$race, resume_m$call)
xtab_m
fisher.test(xtab_m)
chisq.test(xtab_m)
## OR [95% CI] for white male applicants: 1.57 [0.97, 2.57]

## This result may be driven by smaller sample size
## So, lets do a power calculation
## convert an OR of 1.57 to Cohen's d
or_cohen_d = function(x) {
  d = log(x) * sqrt(3) / pi
  d
}
d = or_cohen_d(1.57)
b = length(resume_m$firstname[resume_m$race=="black"])
w = length(resume_m$firstname[resume_m$race=="white"])
pwr.2p2n.test(h = d, n1 = b, n2 = w, sig.level = 0.05)
# The power calculation says that getting a sample with this effect size is
# somewhat rare, but not impossible

## Fisher test for black and white females
xtab_f <- table(resume_f$race, resume_f$call)
xtab_f
fisher.test(xtab_f)
## OR [95% CI] for white female applicants: 1.55 [1.21, 1.98]

## Are there gender effects *within* racial groups?

## Make a subgroup for black applicants
xtab_b <- subset(resume, race == "black")
xtab_b %>% 
  View()

## Make a subgroup for white applicants
xtab_w <- subset(resume, race == "white")
xtab_w %>% 
  View()

## Looking at the number of each name by sex

## Black females
xtab_b %>% 
  filter(sex == "female") %>% 
  count(firstname, sort = TRUE)
## The most common name was Tamika

## Black males
xtab_b %>% 
  filter(sex == "male") %>% 
  count(firstname, sort = TRUE)
## The most common name was Tyrone

## White females
xtab_w %>% 
  filter(sex == "female") %>% 
  count(firstname, sort = TRUE)
## The most common name was Anne

## White males
xtab_w %>% 
  filter(sex == "male") %>% 
  count(firstname, sort = TRUE)
## The most common name was Neil

## Cross tabs for within race sex effects

## Black applicants
blk <- table(xtab_b$sex, xtab_b$call)
blk
fisher.test(blk)
## No effect of sex on call backs for black applicants:
## OR = 0.87 [0.56, 1.31]

## White applicants
wht <- table(xtab_w$sex, xtab_w$call) 
wht
fisher.test(wht)
## No effect of sex on call backs for white applicants either
## OR = 0.89 [0.63, 1.24]

## Which names had the highest probabilities of getting call backs?
result = tapply(resume$call, resume$firstname, mean)
result = round(result, 3)
result[order(result)]
## The five names with the most call backs were Brad, Jay, Kristen, Carrie, 
## and Meredith

## Charts of 5 most common names in the experiment by race and sex
p1 <- xtab_b %>%
  filter(sex == "female") %>% 
  count(firstname, sort = T) %>% 
  filter(n > 200) %>% 
  ggplot(aes(reorder(firstname, -n), n)) +
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkred", col = "black") +
  labs(title = "Black Females") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 9))

p2 <- xtab_b %>% 
  filter(sex == "male") %>% 
  count(firstname, sort = T) %>% 
  filter(n > 61) %>% 
  ggplot(aes(reorder(firstname, -n), n)) +
  geom_bar(position = "dodge", stat = "identity",
           fill = "darkred", col = "black") +
  labs(title = "Black Males") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 9))

p3 <- xtab_w %>% 
  filter(sex == "female") %>% 
  count(firstname, sort = T) %>% 
  filter(n > 200) %>% 
  ggplot(aes(reorder(firstname, -n), n)) +
  geom_bar(position = "dodge", stat = "identity",
           fill = "lightblue", col = "black") +
  labs(title = "White Females") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 9))

p4 <- xtab_w %>% 
  filter(sex == "male") %>% 
  count(firstname, sort = T) %>% 
  filter(n > 63) %>% 
  ggplot(aes(reorder(firstname, -n), n)) +
  geom_bar(position = "dodge", stat = "identity",
           fill = "lightblue", col = "black") +
  labs(title = "White Males") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 9))

p1 + p2 + p3 + p4 + plot_annotation(title = "Most Common Names of People in the Bertrand & Mullainathan 2004 Experiment")

        