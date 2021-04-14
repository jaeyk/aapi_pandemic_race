library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)

dat <- read_csv("/Volumes/GoogleDrive/My Drive/COVID-19 Panel Data/complete_123.csv") #complete123 is all 3 waves merged together

dat[dat == 9999] <- NA

names(dat)

dat <- dat %>%  rename_with(~(sub("_", "_q_", .x)), starts_with("W"))

names(dat)

table(dat$W1_q_discrimination.asians)
table(dat$W2_q_discrimination.asians)
dat$W2_disc[dat$W2_q_discrimination.asians==1] <-5
dat$W2_disc[dat$W2_q_discrimination.asians==2] <-4
dat$W2_disc[dat$W2_q_discrimination.asians==3] <-3
dat$W2_disc[dat$W2_q_discrimination.asians==4] <-2
dat$W2_disc[dat$W2_q_discrimination.asians==5] <-1

dat$W2_q_discrimination.asians <- dat$W2_disc
table(dat$W2_q_discrimination.asians)
table(dat$W3_q_discrimination.asians)
table(dat$W3_q_how.much.discrim.asians.face)

dat$W3_disc[dat$W3_q_how.much.discrim.asians.face==1] <- 5
dat$W3_disc[dat$W3_q_how.much.discrim.asians.face==2] <- 4
dat$W3_disc[dat$W3_q_how.much.discrim.asians.face==3] <- 3
dat$W3_disc[dat$W3_q_how.much.discrim.asians.face==4] <- 2
dat$W3_disc[dat$W3_q_how.much.discrim.asians.face==5] <- 1

dat$W3_q_discrimination.asians <- dat$W3_disc
table(dat$W1_q_discrimination.asians)
table(dat$W2_q_discrimination.asians)
table(dat$W3_q_discrimination.asians)

table(dat$W1_q_gender)

table(dat$W1_q_edu)

table(dat$W1_q_party)
dat$W1_q_party[dat$W1_q_party == 1] <- "Independent"
dat$W1_q_party[dat$W1_q_party == 2] <- "Democrat"
dat$W1_q_party[dat$W1_q_party == 3] <- "Republican"

table(dat$W1_q_party)
dat$W1_q_gender <- ifelse(dat$W1_q_gender==1,1,0)

names(dat)
dat_long <- pivot_longer(dat, cols=!c(PID.y, ResponseId, PID, RESPONDENT_ID,                                   W1_q_NatOrigin, W2_q_NatOrigin, W3_q_NatOrigin,
                                      W1_q_income, W3_q_income,
                                      W1_q_gender,age,
                                      W1_q_edu, W1_q_usborn, W1_q_citizen,
                                      W1_q_party, W2_q_party, W3_q_party),
                         names_to = c("wave", ".value"), names_pattern = "(..)_(.*)")

dim(dat_long)
dat$edu
names(dat_long)
library(lme4)
library(lmerTest)

dat_long$wave <- factor(dat_long$wave)
dat_long$chinese <- dat_long$W1_q_NatOrigin == "Chinese"
table(dat_long$q_discrimination.asians)

dat_long$disc_cat <- ifelse(dat_long$q_discrimination.asians <= 3,1,0)
table(dat$ag)
class(dat_long$W1_q_edu)
table(dat_long$W1_q_party)
table(dat_long$W1_q_usborn)

dat_long$W1.usborn<-ifelse(dat_long$W1_q_usborn==1,1,0)
m1 <- glmer(q_discrimination.asians ~ chinese + W1_q_party + W1_q_gender + W1_q_income + W1_q_usborn * wave + (1|PID),
            family=binomial(), data=dat_long)
summary(m1)


m2 <- lmer(disc_cat ~ age + W1_q_edu +  W1_q_income + chinese + W1_q_gender  + W1_q_usborn + (1|wave) + (1|PID), data=dat_long)
summary(m2)

library(stargazer)
stargazer(m2, type= "html")

names(dat)
