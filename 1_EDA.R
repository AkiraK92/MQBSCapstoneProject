#################################################
#################################################
### Capstone Project ###
### Using Ohlsson data to predict claim number
#################################################
#################################################


#################################################
### Exploratory Data Analysis ###
#################################################

###### Set up ########
rm(list=ls())
setwd(choose.dir()) #choose appropriate working directory
getwd()

library(tidyverse)
library(dplyr)
library(psych)

### Import and check structure of original data

#install.packages("insuranceData")
library(insuranceData)
data("dataOhlsson")

?dataOhlsson

dim(dataOhlsson)
str(dataOhlsson)
head(dataOhlsson)
summary(dataOhlsson)
describe(dataOhlsson)

# $ agarald (owner age): int  0 4 5 5 6 9 9 9 10 10 ...
# $ kon     (gender (K=Female, M=Male)): Factor w/ 2 levels "K","M": 2 2 1 1 1 1 1 2 2 2 ...
# $ zon     (geographic region): int  1 3 3 4 2 3 4 4 2 4 ...
# $ mcklass (EV ratio class - Vehicle power feature) : int  4 6 3 1 1 3 3 4 3 2 ...
# $ fordald (vehicle age) : int  12 9 18 25 26 8 6 20 16 17 ...
# $ bonuskl (bonus class - higher the better) : int  1 1 1 1 1 1 1 1 1 1 ...
# $ duration (policy exposure in year unit): num  0.175 0 0.455 0.173 0.181 ...
# $ antskad (number of claims) : int  0 0 0 0 0 0 0 0 0 0 ...
# $ skadkost (cost of claims) : int  0 0 0 0 0 0 0 0 0 0 ...

### Change the headers to comprehensible English names
data <- dataOhlsson
names(data) <- c("ownAge", "gender","zone","mcCl","vehAge","bonCl","duration","claimsNum","claimsAmt")
str(data)

### Check for N/As
apply(data, 2, function(x) any(is.na(x)))
apply(data, 2, function(x) any(is.null(x)))

library(simputation)
library(naniar)
miss_var_summary(data) #double-check

### Check for Uniqueness
## owner age
  unique(data$ownAge)[order(unique(data$ownAge))]
  prop.table(table(data$ownAge))
  
  ownAge.summ <- data %>% filter(ownAge < 16) %>% 
    summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
  ownAge.summ$exposure/sum(data$duration)
  ownAge.summ$claims/sum(data$claimsNum)
  # those below 16 will be floored at 16 (later)

## vehicle Age
  unique(data$vehAge)[order(unique(data$vehAge))]
  prop.table(table(data$vehAge))
  
  #It is unnatural to have cars beyond 50 years but possible so have left it as it is (not influential)
  vehAge.summ <- data %>% filter(vehAge > 20) %>% 
    summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
  vehAge.summ$exposure/sum(data$duration) #low in exposure
  vehAge.summ$claims/sum(data$claimsNum) # too high to be omitted

## gender
unique(data$gender)
prop.table(table(data$gender))

## zone
unique(data$zone)[order(unique(data$zone))]
prop.table(table(data$zone))

## mcClass
unique(data$mcCl)[order(unique(data$mcCl))]
prop.table(table(data$mcCl))

## Bonus Class
unique(data$bonCl)[order(unique(data$bonCl))]
prop.table(table(data$bonCl))

## duration
range(data$duration)
data %>% ggplot(aes(duration)) + geom_histogram(binwidth = 1)
duration.summ <- data %>% filter(duration > 5) %>% 
  summarise(policies = n(), exposure = sum(duration), 
            claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
duration.summ$exposure/sum(data$duration)
duration.summ$claims/sum(data$claimsNum)
# -> alternatively this could be due to typo in which case 32 means 3.2. 
# But without domain knowledge, difficult to tell so cap.

duration.summ2 <- data %>% filter(duration <= 0) %>% 
  summarise(policies = n(),exposure = sum(duration), claims = sum(claimsNum))
duration.summ2$claims/sum(data$claimsNum)
duration.summ2$policies/nrow(data)
# non-trivial number of rows --> to avoid data loss, we will fill this with the mean duration

### Claim number
unique(data$claimsNum)[order(unique(data$claimsNum))]
prop.table(table(data$claimsNum))

##############################################
###  data cleansing 1 ###
dur.mod.mean <- data %>% filter(pmin(duration,5) > 0) %>% summarise(mean = mean(duration))
data1 <- data[,c(-9)] %>% mutate(duration = as.numeric(ifelse(duration <=0, dur.mod.mean, duration)))
data1$duration <- pmin(data1$duration,5)
data1$ownAge <- pmax(data1$ownAge,16)

head(data1 %>% arrange(duration))
str(data1)
summary(data1)
#############################################

### Histograms (for cont) and Barplot (for discrete)
ownAge.hist <- data1 %>% ggplot(aes(ownAge)) + 
  geom_histogram(binwidth = 5,fill = "darkblue") + 
  labs(x = "Age" , y = "Count", title = "Owner Age")
gender.hist <- data1 %>% ggplot(aes(gender)) + 
  geom_bar(fill = "darkblue") + labs(x = "Gender" , y = "Count", title = "Gender (K=Female, M=Male)")
zone.hist <- data1 %>% ggplot(aes(zone)) + 
  geom_bar(fill = "darkblue") + labs(x = "Zone" , y = "Count", title = "Zone")
mcCl.hist <- data1 %>% ggplot(aes(mcCl)) + 
  geom_bar(fill = "darkblue") + labs(x = "Class" , y = "Count", title = "MC Class")
vehAge.hist <- data1 %>% ggplot(aes(vehAge)) + 
  geom_histogram(binwidth = 5, fill = "darkblue") + 
  labs(x = "Age" , y = "Count", title = "Vehicle Age") + xlim(0,50)
bonCl.hist <- data1 %>% ggplot(aes(bonCl)) + 
  geom_bar(fill = "darkblue") + labs(x = "Class" , y = "Count", title = "Bonus Class")

duration.hist <- data1 %>% ggplot(aes(duration)) + 
  geom_histogram(binwidth = 1, fill = "darkblue") + 
  labs(x = "Exposure" , y = "Count", title = "Duration Histogram")
duration.box <- data1 %>% ggplot(aes(duration)) + 
  geom_boxplot(fill = "white") + labs(x = "Exposure" , y = "Count", title = "Duration Boxplot")

claimsNum.hist <- data1 %>% ggplot(aes(claimsNum)) + 
  geom_histogram(binwidth = 1,fill = "darkblue") + 
  labs(x = "Exposure" , y = "Count", title = "Claim Number Histogram")
claimsNum.box <- data1 %>% ggplot(aes(claimsNum)) + 
  geom_boxplot(fill = "white")  + labs(x = "Exposure" , y = "Count", title = "Claim Number Boxplot")

data1.freq <- sum(data1$claimsNum)/sum(data1$duration)
data1.freq

library(gridExtra)
grid.arrange(ownAge.hist,gender.hist,zone.hist,mcCl.hist,vehAge.hist,bonCl.hist)
grid.arrange(duration.hist,duration.box,claimsNum.hist,claimsNum.box)

### Frequency summary and charts
library(gridExtra)
library(ggrepel)
library(ggthemes)

## by owner age bands
ownAge.summary <- data1  %>% 
  mutate(ownAge.Cat = case_when(ownAge <=25 ~ 1,
                                ownAge >25 & ownAge <=35 ~ 2,
                                ownAge >35 & ownAge <=45 ~ 3,
                                ownAge >45 & ownAge <=55 ~ 4,
                                ownAge >55 & ownAge <=65 ~ 5,
                                ownAge >65 & ownAge <=75 ~ 6,
                                ownAge >75 & ownAge <=85 ~ 7,
                                ownAge >85 ~ 8)) %>% 
  group_by(ownAge.Cat) %>% 
  summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
ownAge.exp <- ownAge.summary %>% ggplot(aes(ownAge.Cat,exposure)) + geom_col()
ownAge.num <- ownAge.summary %>% ggplot(aes(ownAge.Cat,claims)) + geom_col()
ownAge.freq <- ownAge.summary %>% 
  ggplot(aes(ownAge.Cat,frequency)) + 
  geom_smooth(color = "darkblue", linetype = "dashed", size = 0.5, se = TRUE) +  geom_point(size=3)
grid.arrange(ownAge.exp,ownAge.freq, ncol=2)

## by zone 
zone.summary <- data1 %>% group_by(zone) %>% 
  summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
zone.exp <- zone.summary %>% ggplot(aes(zone,exposure)) + geom_col()
zone.num <- zone.summary %>% ggplot(aes(zone,claims)) + geom_col()
zone.freq <- zone.summary %>% ggplot(aes(zone,frequency)) + 
  geom_smooth(color = "darkblue", linetype = "dashed", size = 0.5, se = TRUE) + geom_point(size=3)
grid.arrange(zone.exp,zone.freq, ncol=2)
# -> negative correlation with frequency

zone.summary2 <- data1 %>% 
  mutate(ownAge.Cat = case_when(ownAge <=25 ~ 1,
                               ownAge >25 & ownAge <=35 ~ 2,
                               ownAge >35 & ownAge <=45 ~ 3,
                               ownAge >45 & ownAge <=55 ~ 4,
                               ownAge >55 & ownAge <=65 ~ 5,
                               ownAge >65 & ownAge <=75 ~ 6,
                               ownAge >75 & ownAge <=85 ~ 7,
                               ownAge >85 ~ 8)) %>%
  group_by(zone, ownAge.Cat) %>% 
  summarise(exposure2 = sum(duration), claims2 = sum(claimsNum), frequency2 = sum(claimsNum)/sum(duration))

zone.ownAge.summ <- left_join(zone.summary2, zone.summary, key = zone) %>% 
  mutate(relexp = exposure2/exposure, relclaims = claims2/claims, relfreq = claims2/exposure)
zone.ownAge.summ %>% 
  ggplot(aes(zone, relclaims, fill = ownAge.Cat)) + geom_bar(stat = "identity")

zone.summary3 <- data1 %>% 
  mutate(vehAge.Cat = case_when(vehAge == 0 ~ 1, 
                                vehAge >0 & vehAge <=2 ~ 2,
                                vehAge >2 & vehAge <=5 ~ 3,
                                vehAge >5 & vehAge <=10 ~ 4,
                                vehAge >10 & vehAge <=15 ~ 5,
                                vehAge >15 & vehAge <=20 ~ 6,
                                vehAge >20 ~ 7)) %>%
  group_by(zone, vehAge.Cat) %>% 
  summarise(exposure2 = sum(duration), claims2 = sum(claimsNum), frequency2 = sum(claimsNum)/sum(duration))

zone.vehAge.summ <- left_join(zone.summary3, zone.summary, key = zone) %>% 
  mutate(relexp = exposure2/exposure, relclaims = claims2/claims, relfreq = claims2/exposure)
zone.vehAge.summ %>% ggplot(aes(zone, relclaims, fill = vehAge.Cat)) + geom_bar(stat = "identity")
# -> checked for correlation with owner age group but nothing significant was found. 
# This is not included in the report as the corr plot summarises it better

## by gender
gender.summary <- data1 %>% group_by(gender) %>% 
  summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
gender.exp <- gender.summary %>% ggplot(aes(gender,exposure)) + geom_col()
gender.num <- gender.summary %>% ggplot(aes(gender,claims)) + geom_col()
gender.freq <- gender.summary %>% ggplot(aes(gender,frequency)) + geom_point(size=3)

grid.arrange(gender.exp,gender.freq, ncol=2)


data1 %>% ggplot(aes(zone, ownAge)) + geom_jitter()

## by MC Class (equivalent to the power of the vehicle)
mcCl.summary <- data1 %>% group_by(mcCl) %>% 
  summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
mcCl.exp <- mcCl.summary %>% ggplot(aes(mcCl,exposure)) + geom_col()
mcCl.num <- mcCl.summary %>% ggplot(aes(mcCl,claims)) + geom_col()
mcCl.freq <- mcCl.summary %>% ggplot(aes(mcCl,frequency)) + 
  geom_smooth(color = "darkblue", linetype = "dashed", size = 0.5, se = TRUE) + geom_point(size=3)

grid.arrange(mcCl.exp,mcCl.freq, ncol=2)


## by vehicle age bands
vehAge.summary <- data1 %>% 
  mutate(vehAge.Cat = case_when(vehAge == 0 ~ 1, 
                                vehAge >0 & vehAge <=2 ~ 2,
                                vehAge >2 & vehAge <=5 ~ 3,
                                vehAge >5 & vehAge <=10 ~ 4,
                                vehAge >10 & vehAge <=15 ~ 5,
                                vehAge >15 & vehAge <=20 ~ 6,
                                vehAge >20 ~ 7)) %>% 
  group_by(vehAge.Cat) %>% 
  summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
vehAge.exp <- vehAge.summary %>% ggplot(aes(vehAge.Cat,exposure)) + geom_col()
vehAge.num <- vehAge.summary %>% ggplot(aes(vehAge.Cat,claims)) + geom_col()
vehAge.freq <- vehAge.summary %>% ggplot(aes(vehAge.Cat,frequency)) + 
  geom_smooth(color = "darkblue", linetype = "dashed", size = 0.5, se = TRUE) + geom_point(size=3)
grid.arrange(vehAge.exp,vehAge.freq, ncol=2)


## by Bonus Class
bonCl.summary <- data1 %>% 
  group_by(bonCl) %>% 
  summarise(exposure = sum(duration), claims = sum(claimsNum), frequency = sum(claimsNum)/sum(duration))
bonCl.exp <- bonCl.summary %>% ggplot(aes(bonCl,exposure)) + geom_col()
bonCl.num <- bonCl.summary %>% ggplot(aes(bonCl,claims)) + geom_col()
bonCl.freq <- bonCl.summary %>% ggplot(aes(bonCl,frequency)) + 
  geom_smooth(color = "darkblue", linetype = "dashed", size = 0.5, se = TRUE) + geom_point(size=3)

grid.arrange(bonCl.exp,bonCl.freq, ncol=2)


#compute correlations
library(corrplot)
str(data1)
data1.cor <- data1 %>% mutate(claimFreq = claimsNum/duration)
corrplot(cor(data1.cor[,c(-2)]), method = "ellipse") #excludes Gender (non-numeric)


###################################################
### Cleanse the data for use in data modelling ###
##################################################
# re-levelling with reference class (for all)

###  data cleansing 2 (for modelling) 
data2 <- data1


data2[,"gender"] <- relevel(data2[,"gender"], ref = "M")

data2$mcCl <- as.factor(data2$mcCl)
data2[,"mcCl"] <- relevel(data2[,"mcCl"], ref = "3")


data2$bonCl <- as.factor(data2$bonCl)
data2[,"bonCl"] <- relevel(data2[,"bonCl"], ref = "7")


data2$zone <- as.factor(data2$zone)
data2[,"zone"] <- relevel(data2[,"zone"], ref = "4")

str(data2)
summary(data2)
head(data2)
