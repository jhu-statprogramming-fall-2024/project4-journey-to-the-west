#load packages
library(tidyverse)
library(haven)
library(table1)
library(purrr)

#download NHANES data
demo_data <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DEMO.xpt")
diet_data <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DR1TOT.xpt")
bp_data <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BPXO.xpt")
body_data <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BMX.xpt")
lab_data <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_TCHOL.xpt")
ques_data <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BPQ.xpt")

#select variables needed
demo_data <- select(demo_data,SEQN,RIDAGEYR,RIAGENDR,RIDRETH3)
diet_data <- select(diet_data,SEQN,DR1TSODI,DR1TKCAL)
bp_data <- select(bp_data,SEQN,BPXOSY1:BPXODI3)
body_data <- select(body_data,SEQN,BMXBMI)
lab_data <- select(lab_data,SEQN,LBXTC)
ques_data <- select(ques_data,SEQN,BPQ040A)

#join datasets
demo_data <- left_join(demo_data,diet_data, by="SEQN")
demo_data <- left_join(demo_data,bp_data, by="SEQN")
demo_data <- left_join(demo_data,body_data, by="SEQN")
demo_data <- left_join(demo_data,lab_data, by="SEQN")
demo_data <- left_join(demo_data,ques_data, by="SEQN")

#exclude missing data in age, sex, race, sodium and blood pressure
#restrict to age over 18
data <- demo_data %>% 
  filter(is.na(RIDAGEYR)==FALSE) %>%
  filter(is.na(RIAGENDR)==FALSE) %>%
  filter(is.na(RIDRETH3)==FALSE) %>%
  filter(is.na(DR1TSODI)==FALSE) %>%
  filter(is.na(BPXOSY1)==FALSE) %>%
  filter(is.na(BPXOSY2)==FALSE) %>%
  filter(is.na(BPXOSY3)==FALSE) %>%
  filter(is.na(BPXODI1)==FALSE) %>%
  filter(is.na(BPXODI2)==FALSE) %>%
  filter(is.na(BPXODI3)==FALSE) %>%
  filter(RIDAGEYR>=18)

#use function-oriented programming to calculate the number of missing values
map_dbl(data, ~sum(is.na(.x)))

#create category variables
data <- data %>% mutate(male0female1=case_when(RIAGENDR==1~ 0,RIAGENDR==2~ 1, .default=NA))
data <- data %>% mutate(sex=factor(male0female1, levels=c(0,1), labels=c("Male","Female")))
data <- data %>% mutate(white1black2other0=case_when(RIDRETH3==3~ 1, RIDRETH3==4~ 2, .default=0))
data <- data %>% mutate(race=factor(white1black2other0, levels=c(1,2,0), 
                                      labels=c("White","Black", "Other")))
data <- data %>% mutate(medno0yes1=case_when(BPQ040A==2~ 0,BPQ040A==1~ 1, .default=NA))
data <- data %>% mutate(medication=factor(medno0yes1, levels=c(0,1), labels=c("No","Yes")))
# data <- data %>% mutate(sybp=((BPXOSY1+BPXOSY2+BPXOSY3)/3))
# data <- data %>% mutate(dibp=((BPXODI1+BPXODI2+BPXODI3)/3))
data <- data %>% mutate(sybp = rowMeans(select(., BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE))
data <- data %>% mutate(dibp = rowMeans(select(., BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE))

#check data format
str(data)

#labels and units
label(data$RIDAGEYR)="Age at Screening"
units(data$RIDAGEYR)="Year"
label(data$sex)="Sex"
label(data$race)="Race"
label(data$medication)="High Blood Pressure Medication"
label(data$sybp)="Systolic Blood Pressure"
units(data$sybp)="mmHg"
label(data$dibp)="Diastolic Blood Pressure"
units(data$dibp)="mmHg"

#table 1
table1(~RIDAGEYR+sex+race+DR1TSODI+DR1TKCAL+BMXBMI+LBXTC+medication,data=data)

#linear regression analysis
##model0: crude model
model0 <- lm(sybp~DR1TSODI,data=data)
summary(model0)

##model1: adjust for age, sex, race
model1 <- lm(sybp~DR1TSODI+RIDAGEYR+sex+race,data=data)
summary(model1)

##model2: adjust for energy intake, BMI, cholesterol and hypertension medication use
model2 <- lm(sybp~DR1TSODI+RIDAGEYR+sex+race
             +DR1TKCAL+BMXBMI+LBXTC+medication,data=data)
summary(model2)

hist(data$DR1TSODI,)
