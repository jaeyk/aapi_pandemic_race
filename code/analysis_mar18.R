#### Time 1/2 Analysis
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(qualtRics)

dta <- read_csv("/Users/vleung/Downloads/w1w2.csv")
dat <- read_csv("/Volumes/GoogleDrive/My Drive/COVID-19 Panel Data/data_cleaned_pivotMar9.csv")

dta$inc50k <- ifelse(dta$W1_13<=5,1,0)
dta$inc100k <- ifelse(dta$W1_13 >=11,1,0)
table(dta$inc100k)
prop.table(table(dta$inc100k))

names(dat)

table(dta$W2_38)

table(dat$W3_qhow.much.discrim.asians)
dat$W3_qhow.much.discrim.asians[dat$W3_qhow.much.discrim.asians==9999] <- NA
dat$W2_qhow.much.discrim.asians[dat$W2_qhow.much.discrim.asians==9999] <- NA


dat$W3_disc[dat$W3_qhow.much.discrim.asians==1] <- 5
dat$W3_disc[dat$W3_qhow.much.discrim.asians==2] <- 4
dat$W3_disc[dat$W3_qhow.much.discrim.asians==3] <- 3
dat$W3_disc[dat$W3_qhow.much.discrim.asians==4] <- 2
dat$W3_disc[dat$W3_qhow.much.discrim.asians==5] <- 1


dat$W2_disc[dat$W2_qhow.much.discrim.asians==1] <- 5
dat$W2_disc[dat$W2_qhow.much.discrim.asians==2] <- 4
dat$W2_disc[dat$W2_qhow.much.discrim.asians==3] <- 3
dat$W2_disc[dat$W2_qhow.much.discrim.asians==4] <- 2
dat$W2_disc[dat$W2_qhow.much.discrim.asians==5] <- 1

table(dat$W2_disc)
dat$W3.W2.disc.diff<-dat$W3_disc - dat$W2_disc
table(dat$W3.W2.disc.diff)

###group discrimination
dta$w1.discrim<-(dta$W1_42)
dta$w2.discrim[dta$W2_30==1]<-5
dta$w2.discrim[dta$W2_30==2]<-4
dta$w2.discrim[dta$W2_30==3]<-3
dta$w2.discrim[dta$W2_30==4]<-2
dta$w2.discrim[dta$W2_30==5]<-1

table(dta$w1.discrim)
table(dta$w2.discrim)

summary(no$w1.discrim)
summary(no$w2.discrim)

prop.table(table(dta$w1.discrim))
prop.table(table(dta$w2.discrim))
prop.table(table(dat$W3_qhow.much.discrim.asians))
summary(dat$W3_qhow.much.discrim.asians)

prop.table(table(df$W2_24))
prop.table(table(df$W3_23))

prop.

dta$discrim.change<- dta$w2.discrim - dta$w1.discrim
table(dta$discrim.change)


dta$w1.somediscrim <- ifelse(dta$w1.discrim >=2,1,0)
dta$w2.somediscrim<-ifelse(dta$w2.discrim >=2,1,0)

table(dta$w1.somediscrim)
table(dta$w2.somediscrim)
boxplot(dta$w1.discrim, dta$w2.discrim)

dta$w1.high.discrim<-ifelse(dta$w1.discrim>=4,1,0)
dta$w1.med.discrim <-ifelse(dta$w1.discrim==3 | dta$w1.discrim==2,1,0)
dta$w1.no.discrim <-ifelse(dta$w1.discrim ==1,1,0)



##perception of discrimatino due to covid
table(dta$W2_24)
dta$discrim.covid[dta$W2_24==1] <- 5
dta$discrim.covid[dta$W2_24==2] <- 4
dta$discrim.covid[dta$W2_24==3] <- 3
dta$discrim.covid[dta$W2_24==4] <- 2
dta$discrim.covid[dta$W2_24==5] <- 1

##chinese or not
dta$chinese<-ifelse(dta$W1_NatOrigin== "Chinese",1,0)
table(dta$chinese)

table(dta$discrim.covid)

#covid
table(dta$W2_11)
dta$had.covid<-ifelse(dta$W2_11==1,1,0)

#friend covid
dta$know.covid<-ifelse(dta$W2_12==1,1,0)

table(dta$W2_13)
#employment/econ issue covid
dta$covid.issue<-ifelse(dta$W2_13<=2,1,0)
table(dta$covid.issue)

table(dta$w1.high.discrim)
hi <- subset(dta, dta$w1.high.discrim==1)
med <- subset(dta, dta$w1.med.discrim==1)
no <- subset(dta, dta$w1.no.discrim==1)

##us born
dta$usborn<- ifelse(dta$W1_14==1,1,0)

dta$thought.discrim.inc<-ifelse(dta$discrim.covid>=4,1,0)
table(dta$thought.discrim.inc)

###hi discrim folks
h1<- lm(thought.discrim.inc ~  had.covid + covid.issue + w1.party + personal.disc+ age + usborn, data=hi)
summary(h1)

h2<- lm(discrim.change ~  had.covid + covid.issue + w1.party + personal.disc+ age + usborn, data=hi)
summary(h2)


## med discrim folks 
med1<-lm(thought.discrim.inc ~  had.covid + covid.issue + w1.party + personal.disc+ age + usborn, data=med)
summary(med1)

med2<- lm(discrim.change ~  had.covid + covid.issue + w1.party +  age + usborn, data=med)
summary(h2)

## nodiscrim folks
no1<-lm(thought.discrim.inc ~ had.covid + covid.issue + w1.party + personal.disc+ age + usborn, data=no)
summary(no1)

no2<- lm(discrim.change ~  had.covid + covid.issue + w1.party +  age + usborn, data=no)
summary(no2)

##basic LM
m1<- lm(thought.discrim.inc ~ chinese + had.covid + covid.issue + w1.party + w1.high.discrim + personal.disc+ age + usborn, data=dta)
summary(m1)

m2<- lm(discrim.change ~ thought.discrim.inc + chinese + had.covid + covid.issue + w1.party + w1.high.discrim + personal.disc+ age + usborn, data=dta)
summary(m2)

prop.table(table(dta$discrim.change))


write.csv(summary(dta),)
library(stargazer)
stargazer(m2, type = "html")
###personal discrimination
table(dta$W1_43)
dta$personal.disc<-ifelse(dta$W1_43==1,1,0)

#party
dta$w1.party[dta$W1_21==1] <- "Independent"
dta$w1.party[dta$W1_21==2] <- "Democrat"
dta$w1.party[dta$W1_21==3] <- "Republican"

table(dta$w1.party)

names(df)
table(df$age)
summary(df$age)
mean(df$age)
df$age[df$age==1950] <- 70
df$age[df$age==9999] <- NA
table(df$age)
summary(df$age)

table(df$W3_5)
prop.table(table(df$W3_NatOrigin))
table(df$W3_NatOrigin)

prop.table(table(dta$W2_NatOrigin))
summary(dta$age)

prop.table(table(dta$W2_4))
table(dta$W2_4)
