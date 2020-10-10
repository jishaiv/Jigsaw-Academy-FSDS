### Capstone Project- telecom Churn Analysis

getwd()
setwd("C:\\Jig19936\\CapstoneProject\\final")
options(scipen=999) ## Switches off the scientific notations in terms of no. representation: 

## Loading required packages 

library(dplyr)
library(ggplot2)
library(dataQualityR)
library(forcats)
library(car)
library(ROCR)
library(irr)
library(lattice)
library(caret)
library(gains)

##Getting csv file and saving as dataframe 

data<-read.csv("telecomfinal.csv" ,header=TRUE,stringsAsFactors = TRUE)
head(data)

### Generating Data quality Report using dataQualityR package :

sample_report<-read_excel("Sample Data quality report.xlsx")
View(sample_report)
str(sample_report)

num_report<-paste("C:\\Jig19936\\CapstoneProject\\final\\report_numerical.csv" ,sep="")
cat_report<-paste("C:\\Jig19936\\CapstoneProject\\final\\report_categorical.csv" ,sep="")
checkDataQuality(data=data,out.file.num = num_report,out.file.cat = cat_report)


### Checking the data quality report generated :

report_numerical<-read.csv("report_numerical.csv" ,header=TRUE,stringsAsFactors = TRUE)
report_categorical<-read.csv("report_categorical.csv " ,header=TRUE,stringsAsFactors = TRUE)

head(report_numerical)
head(report_categorical)

View(report_numerical)
View(report_categorical)


### Hardcoded data quality report in the given format as the format of the report generated is different :


report<-data.frame(VariableName=character(),
                   DataType=character(),
                   NoOfRecords=numeric(),
                   UniqueRecords=numeric(),
                   DataAvailable=numeric(),
                   AvailablePercent=numeric(),
                   Missing=numeric(),
                   MissingPercent=numeric(),
                   Minimum=numeric(),
                   Maximum=numeric(),
                   Mean=numeric(),
                   Percentile_5th=numeric(),
                   Percentile_10th=numeric(),
                   Percentile_25th=numeric(),
                   Percentile_50th=numeric(),
                   Percentile_75th=numeric(),
                   Percentile_90th=numeric(),
                   Percentile_95th=numeric(),
                   stringsAsFactors = FALSE)

View(report)


for(i in 1:ncol(data))
{
  report[i,1]<-names(data)[i]
  report[i,2]<-class(data[ ,i])
  report[i,3]<-length(data[ ,i])
  report[i,4]<-length(unique(data[ ,i]))
  report[i,5]<-length(which(complete.cases(data[ ,i])))
  report[i,6]<-(report[i,5]/report[i,3])*100
  report[i,7]<-length(which(is.na(data[ ,i])))
  report[i,8]<-(report[i,7]/report[i,3])*100
  report[i,9]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",min(na.omit(data[ ,i])),0)
  report[i,10]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",max(na.omit(data[ ,i])),0)
  report[i,11]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",mean(na.omit(data[ ,i])),0)
  report[i,12]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",quantile(na.omit(data[ ,i]),0.05),0)
  report[i,13]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",quantile(na.omit(data[ ,i]),0.1),0)
  report[i,14]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",quantile(na.omit(data[ ,i]),0.25),0)
  report[i,15]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",quantile(na.omit(data[ ,i]),0.5),0)
  report[i,16]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",quantile(na.omit(data[ ,i]),0.75),0)
  report[i,17]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",quantile(na.omit(data[ ,i]),0.9),0)
  report[i,18]<-ifelse(report[i,2]=="numeric" | report[i,2]=="integer",quantile(na.omit(data[ ,i]),0.95),0)
  
  
}

View(report)


## Writing the generated report to a .csv filr

write.csv(report,"dataQualityReport.csv")


data_bkp1<-data # taking a back up data at this point

str(data_bkp1)


## Basic sanity checks :

names(data) ## No duplicate columns by observing the column names 
summary(data) ## Certain columns have missing values , need to be treated.

## The target variable is "churn" --> 1= churned and 0=Not churned


summary(data$churn) ## No missing value/anomalies in the dependant variable column. 

##getting the churn ratio in the dataset :

churn_ratio<-table(data$churn)/nrow(data)

## Churn Ratio - The customer likely to be churned in the dataset is 23.9%


## ANalysis of variable type, missing value etc from the report genearted. 

summary(retdays)  ## There are 64143 NAs in this column. This need to be considered as 0 , ie. customers who didnt make retension calls and customer who made rention call is treating as 1

data%>%filter(retdays==0)%>%summarise(n()) ## 4 zeros


## Creating a new variable retcal --> whether a customer made a retension call or not. (Retention calls include any calls from the customer regarding loyalty or retention, e.g. contract renewal, relating competitor's offer, etc.)



data$retcal<-ifelse((is.na(data$retdays)==TRUE),0,1)

summary(data$retcal)
class(data$retcal)
data$retcal<-as.factor(data$retcal)  



# filtering out variables whose missing value percentage is greater than 15% , retaining only the remianing rows in the master data set , data

mis<-report %>%
  filter(report$MissingPercent<15.00) %>%
  select(VariableName)


var_missing<-report %>%
  filter(report$MissingPercent>15.00) %>%
  select(VariableName)
print(var_missing) 


#### income,  dwlltype  , dwllsize  , mailordr ,  occu1,  numbcars , retdays, wrkwoman,  solflag
####    proptype , mailresp ,  cartype ,   children , div_type   ___> variables omitted from analysis 


data<-data[ ,c(mis[ ,1],"retcal")]
str(data)
colnames(data)

## 14 variables omitted based on NA percentage , Now the dataframe is having only 66 variables.

data_bkp2<-data ##2nd backup


### Variable Profiling  #####


## Variable Profiling Continous Variables Starts here : ###

data_continous<-report%>%
  filter((report$DataType=="numeric" | report$DataType=="integer" )& report$MissingPercent<15.00) %>%
  select(VariableName)

print(data_continous)

# 
# 1          mou_Mean
# 2       totmrc_Mean
# 3         rev_Range
# 4         mou_Range
# 5        change_mou
# 6     drop_blk_Mean
# 7    drop_vce_Range
# 8  owylis_vce_Range
# 9    mou_opkv_Range
# 10           months
# 11         totcalls
# 12          eqpdays
# 13    custcare_Mean
# 14    callwait_Mean
# 15  iwylis_vce_Mean
# 16   callwait_Range
# 17   ccrndmou_Range
# 18           adjqty
# 19      ovrrev_Mean
# 20         rev_Mean
# 21      ovrmou_Mean
# 22    comp_vce_Mean
# 23    plcd_vce_Mean
# 24          avg3mou
# 25           avgmou
# 26          avg3qty
# 27           avgqty
# 28          avg6mou
# 29          avg6qty
# 30             age1
# 31             age2
# 32           models
# 33        hnd_price
# 34         actvsubs
# 35         uniqsubs
# 36         forgntvl
# 37     opk_dat_Mean
# 38         mtrcycle
# 39            truck
# 40        roam_Mean
# 41    recv_sms_Mean
# 42    blck_dat_Mean
# 43    mou_pead_Mean
# 44            churn
# 45          da_Mean
# 46         da_Range
# 47      datovr_Mean
# 48     datovr_Range
# 49    drop_dat_Mean
# 50    drop_vce_Mean
# 51           adjmou
# 52           totrev
# 53           adjrev
# 54           avgrev
# 55      Customer_ID
#  

## 53 continous varibales needs to be profiled ( churn is the dependant variable and Customer_ID need not be considered )


### 1. mou_Mean --> Mean no of monthly minutes of use .### 

summary(mou_Mean)
unique(mou_Mean)

## Decile binning 

decile_mou_Mean<-data%>%
  mutate(dec=ntile(mou_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(mou_Mean,na.rm=TRUE),
    LessThan=max(mou_Mean,na.rm=TRUE),
    varname="mou_Mean"
    
  )
print(decile_mou_Mean)

##Clear downward trend in event rate(churn rate) as mou_Mean increases. ( ie,  churn rate is low for customers having higher  mean monthly minutes of use, which can be justifiable)
### Varible can be considered for modelling ###


### 2. totmrc_Mean ---> Mean total monthly recurring charge.

summary(totmrc_Mean)

#Decile binning 

decile_totmrc_Mean<-data%>%
  mutate(dec=ntile(totmrc_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(totmrc_Mean,na.rm=TRUE),
    LessThan=max(totmrc_Mean,na.rm=TRUE),
    varname="totmrc_Mean"
    
  )
print(decile_totmrc_Mean)

## No clear trend (shows both increasing or decreasing trend) for event rate as totmrc_Mean increases.

## Tryning variable transformation as thi svariable seems to be imporatant . 

summary(data$totmrc_Mean)  ## Some negative values which are not expected. Imputing those values with zero. 

data$totmrc_Mean<-ifelse(totmrc_Mean<0,0,totmrc_Mean)


data$log_totmrc_Mean<-log(data$totmrc_Mean)

#Decile binning 


decile_log_totmrc_Mean<-data%>%
  mutate(dec=ntile(log_totmrc_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(log_totmrc_Mean,na.rm=TRUE),
    LessThan=max(log_totmrc_Mean,na.rm=TRUE),
    varname="log_totmrc_Mean"
    
  )
print(decile_log_totmrc_Mean)

## No clear trend in transformed variable also.


data<-select(data,-log_totmrc_Mean) ## Removing the tranformed variable from dataset. 


##Keeping the variable as it seems important.


### 3. rev_Range ---> Range of Revenue (Charge Amount)

summary(rev_Range)

# Decile binning 

decile_rev_Range<-data%>%
  mutate(dec=ntile(rev_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(rev_Range,na.rm=TRUE),
    LessThan=max(rev_Range,na.rm=TRUE),
    varname="rev_Range"
    
  )
print(decile_rev_Range)


## No clear trend (shows both increasing or decreasing trend) for event rate as rev_Range increases.
#Considering variable transformation


data$log_rev_Range<-log(data$rev_Range)

#Decile binning 
attach(data)
decile_log_rev_Range<-data%>%
  mutate(dec=ntile(log_rev_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(log_rev_Range,na.rm=TRUE),
    LessThan=max(log_rev_Range,na.rm=TRUE),
    varname="log_rev_Range"
    
  )
print(decile_log_rev_Range)


## No clear trend in transformed variable also. 
data<-select(data,-log_rev_Range) # removing newly created variable 



### 4.mou_Range ---> Range of no of minutes of use : 

summary(mou_Range)

#Decile binning 

decile_mou_Range<-data%>%
  mutate(dec=ntile(mou_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(mou_Range,na.rm=TRUE),
    LessThan=max(mou_Range,na.rm=TRUE),
    varname="mou_Range"
    
  )
print(decile_mou_Range)


## 5. change_mou ---> Percentage change in monthly minutes of use vs previous threee month average.

summary(change_mou) 
quantile(change_mou,p=c(1:100)/100,na.rm=TRUE)

# Decile binning 

decile_change_mou<-data%>%
  mutate(dec=ntile(change_mou,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(change_mou,na.rm=TRUE),
    LessThan=max(change_mou,na.rm=TRUE),
    varname="change_mou"
    
  )
  
print(decile_change_mou)


## No clear trend (shows both increasing or decreasing trend) for event rate as change_mou increases.
## Variable is keeping for analysis as it seems an imporatant variable. 




## 6. drop_blk_Mean ---> Mean no of dropped or blocked calls

summary(drop_blk_Mean)

# Decile binning 

decile_drop_blk_Mean<-data%>%
  mutate(dec=ntile(drop_blk_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(drop_blk_Mean,na.rm=TRUE),
    LessThan=max(drop_blk_Mean,na.rm=TRUE),
    varname="drop_blk_Mean"
    
  )

print(decile_drop_blk_Mean)


## No clear trend (shows both increasing or decreasing trend) for event rate as drop_blk_Mean< increases.
## CHecking transformed variable for event rate. 

data$log_drop_blk_Mean<-log(data$drop_blk_Mean)


decile_log_drop_blk_Mean<-data%>%
  mutate(dec=ntile(log_drop_blk_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(log_drop_blk_Mean,na.rm=TRUE),
    LessThan=max(log_drop_blk_Mean,na.rm=TRUE),
    varname="log_drop_blk_Mean"
    
  )
print(decile_log_drop_blk_Mean)


## No  trend in event rate for tranformed variable also.

## Removing the transformed variable from datasets::

data<-select(data,-log_drop_blk_Mean)
colnames(data)






### 7. drop_vce_Range ---> Range of no of droped(failed) voice calls.

summary(drop_vce_Range)


# Decile binning 

decile_drop_vce_Range<-data%>%
  mutate(dec=ntile(drop_vce_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(drop_vce_Range,na.rm=TRUE),
    LessThan=max(drop_vce_Range,na.rm=TRUE),
    varname="drop_vce_Range"
    
  )

print(decile_drop_vce_Range)

## No clear trend (shows both increasing or decreasing trend) for event rate as drop_vce_Range increases.
## Checking trend for transformed variable


data$log_drop_vce_Range<-sqrt(data$drop_vce_Range)


decile_log_drop_vce_Range<-data%>%
  mutate(dec=ntile(log_drop_vce_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(log_drop_vce_Range,na.rm=TRUE),
    LessThan=max(log_drop_vce_Range,na.rm=TRUE),
    varname="log_drop_vce_Range"
    
  )
print(decile_log_drop_vce_Range)



## No clear trend for transformed variable also. Keepingthe original as it seems imporatant.

data<-select(data,-log_drop_vce_Range)


###8. owylis_vce_Range ---> Range of no of outbond wireless to wireless voice calls. 

summary(owylis_vce_Range)

#Decile binning 

decile_owylis_vce_Range<-data%>%
  mutate(dec=ntile(owylis_vce_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(owylis_vce_Range,na.rm=TRUE),
    LessThan=max(owylis_vce_Range,na.rm=TRUE),
    varname="owylis_vce_Range"
    
  )

print(decile_owylis_vce_Range)


# No clear trend (shows both increasing or decreasing trend) for event rate as owylis_vce_Range.
## variable owylis_vce_Range seems to be important hence consiedering for modelling



### 9.mou_opkv_Range ---> Range of unrounded minutes of use of off-peak voice calls. 

summary(mou_opkv_Range)

# Decile binning :


decile_mou_opkv_Range<-data%>%
  mutate(dec=ntile(mou_opkv_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(mou_opkv_Range,na.rm=TRUE),
    LessThan=max(mou_opkv_Range,na.rm=TRUE),
    varname="mou_opkv_Range"
    
  )

print(decile_mou_opkv_Range)

### Slightly downward trend in even rate as mou_opkv_Range increases




### 10. months  ----> Total no of months in service : 

summary(months)

#Decile bining 


decile_months<-data%>%
  mutate(dec=ntile(months,20))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(months,na.rm=TRUE),
    LessThan=max(months,na.rm=TRUE),
    varname="months"
    
  )

print(decile_months)


## No clear trend , but seems to be imporatnt varibale as per domain knowledge.
## Observing highest event rate  between 10 months and 12 months , which is justifiable. 
## so creating categorical variable with 3 levels (<10,11-13,&>13)
summary(data$months)

data$month_dummy[data$months<=10] <-"<=10"
data$month_dummy[(data$months>10 & data$months<=12)] <-"11-12"
data$month_dummy[data$months>12] <-">12"                          

#data<-select(data,-month_dummy)


data$month_dummy<-as.factor(data$month_dummy)
class(data$month_dummy)


profile_month_dummy<-data%>%
  group_by(level=fct_explicit_na(month_dummy))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="month_dummy")
print(profile_month_dummy)

##using this variable for further analysis

###11. totcalls ---> Total no of calls over the life of the customer 

summary(totcalls)

##Decile binning 


decile_totcalls<-data%>%
  mutate(dec=ntile(totcalls,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(totcalls,na.rm=TRUE),
    LessThan=max(totcalls,na.rm=TRUE),
    varname="totcalls"
    
  )

print(decile_totcalls)


### Slightly upward trend in event rate as total no of calls increases , which can be relatable. 

## Keeping the variable in the modelling 


### 12.eqpdays ---> No fo days of current equipment

summary(eqpdays)

#Decile binning 


decile_eqpdays<-data%>%
  mutate(dec=ntile(eqpdays,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(eqpdays,na.rm=TRUE),
    LessThan=max(eqpdays,na.rm=TRUE),
    varname="eqpdays"
    
  )

print(decile_eqpdays)


## No clear trend in event rate 
# This may have an impact , considering for modelling 

### 13. custcare_Mean  -->  Customer care calls (inbound)

summary(custcare_Mean)

#Decile Binning 


decile_custcare_Mean<-data%>%
  mutate(dec=ntile(custcare_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(custcare_Mean,na.rm=TRUE),
    LessThan=max(custcare_Mean,na.rm=TRUE),
    varname="custcare_Mean"
    
  )

print(decile_custcare_Mean)

data%>%filter(custcare_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## Only 4 decile, no clear trend and More than 50 Percentage data is 0. (54.45%), variable may not be useful for modelling.
## Varibale omitted from modelling . 


## 14. callwait_Mean  --> Mean no of call waiting calls.

summary(callwait_Mean)

# Decile binning 
decile_callwait_Mean<-data%>%
  mutate(dec=ntile(callwait_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(callwait_Mean,na.rm=TRUE),
    LessThan=max(callwait_Mean,na.rm=TRUE),
    varname="callwait_Mean"
    
  )

print(decile_callwait_Mean)


data%>%filter(callwait_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## Only 5 decile, no clear trend and a lot of data is 0. (48.78%), variable may not be useful for modelling.
## Varibale omitted from modelling . 

### 15. iwylis_vce_Mean ---> Mean no of inbound wireless to wireless voice calls

summary(iwylis_vce_Mean)

decile_iwylis_vce_Mean<-data%>%
  mutate(dec=ntile(iwylis_vce_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(iwylis_vce_Mean,na.rm=TRUE),
    LessThan=max(iwylis_vce_Mean,na.rm=TRUE),
    varname="iwylis_vce_Mean"
    
  )

print(decile_iwylis_vce_Mean)


data%>%filter(iwylis_vce_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## No clear trend , 29% data is zero .
## Considering as it may have some impact on churn

### 16. callwait_Range --->  Range of number of call waiting calls

summary(callwait_Range)

decile_callwait_Range<-data%>%
  mutate(dec=ntile(callwait_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(callwait_Range,na.rm=TRUE),
    LessThan=max(callwait_Range,na.rm=TRUE),
    varname="callwait_Range"
    
  )

print(decile_callwait_Range)

data%>%filter(callwait_Range==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))



## No clear trend , 50 percentage data is zero. Variable may not be useful for modelling . 

## Variable can be omitted fr0m Analysis.

## 17. ccrndmou_Range ---> Range of rounded minutes of use of customer care calls.

summary(ccrndmou_Range)  ##---> Median is zero

# Decile binning  
  
decile_ccrndmou_Range<-data%>%
  mutate(dec=ntile(ccrndmou_Range,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(ccrndmou_Range,na.rm=TRUE),
    LessThan=max(ccrndmou_Range,na.rm=TRUE),
    varname="ccrndmou_Range"
    
  )

print(decile_ccrndmou_Range)


data%>%filter(ccrndmou_Range==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## No clear trend and 54.9 % data is zero. Variable can not be used for modelling . 
## Variable is omitted from further analysis. 


## 18. adjqty ---> Billing adjusted total no of calls. 

summary(adjqty)

# Decile binning 

decile_adjqty<-data%>%
  mutate(dec=ntile(adjqty,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(adjqty,na.rm=TRUE),
    LessThan=max(adjqty,na.rm=TRUE),
    varname="adjqty"
    
  )

print(decile_adjqty)

## slightly increasing trend in event rate as adjqty increases.

## Variable can be considered for modelling .

## 19 ovrrev_Mean ---> Mean overage revenue

summary(ovrrev_Mean)

# Decile binning 

decile_ovrrev_Mean<-data%>%
  mutate(dec=ntile(ovrrev_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(ovrrev_Mean,na.rm=TRUE),
    LessThan=max(ovrrev_Mean,na.rm=TRUE),
    varname="ovrrev_Mean"
    
  )

print(decile_ovrrev_Mean)

data%>%filter(ovrrev_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## upward trend in event rate as ovrrrev increases. 42% of data is zero which is justifiable as there may be customer who doesnt have overage charges at all.
## Variable is keeping for modelling.

##20. rev_Mean --> Mean monthly revenue 

summary(rev_Mean)


# Decile binning 

decile_rev_Mean<-data%>%
  mutate(dec=ntile(rev_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(rev_Mean,na.rm=TRUE),
    LessThan=max(rev_Mean,na.rm=TRUE),
    varname="rev_Mean"
    
  )

print(decile_rev_Mean)


## No clear trend , but seems to be an imporatant variable. So considering variable transforation.


summary(data$rev_Mean)

data%>%filter(rev_Mean<0)%>%
  select(rev_Mean) ## Three negative values in the datset which doesnt hhave any meaning.

data$rev_Mean<-ifelse(data$rev_Mean<0,0,data$rev_Mean) ## Removing negative values for taking log



data$rev_Mean_log<-log(data$rev_Mean)

summary(data$rev_Mean_log)

# Decile binning 

decile_rev_Mean_log<-data%>%
  mutate(dec=ntile(rev_Mean_log,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(rev_Mean_log,na.rm=TRUE),
    LessThan=max(rev_Mean_log,na.rm=TRUE),
    varname="rev_Mean_log"
    
  )

print(decile_rev_Mean_log)

## Transformation also doesnt show any clear trend. Keepin as it seems important.


data<-select(data,-rev_Mean_log)



## 21. ovrmou_Mean ---> Mean overage minutes of use.

summary(ovrmou_Mean)

#Decile binning 

decile_ovrmou_Mean<-data%>%
  mutate(dec=ntile(ovrmou_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(ovrmou_Mean,na.rm=TRUE),
    LessThan=max(ovrmou_Mean,na.rm=TRUE),
    varname="ovrmou_Mean"
    
  )

print(decile_ovrmou_Mean)

data%>%filter(ovrmou_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## upward trend in event rate as ovrmou_Mean increases. 
## 42% data is zero , which can be related to the practical case as there may be a lot of customers which doesnt have overage usage.

## Variable is keeping for analysis. 

##22. comp_vce_Mean ---> Mean no of completed voice calls. 

summary(comp_vce_Mean)

#Decile binning 

decile_comp_vce_Mean<-data%>%
  mutate(dec=ntile(comp_vce_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(comp_vce_Mean,na.rm=TRUE),
    LessThan=max(comp_vce_Mean,na.rm=TRUE),
    varname="comp_vce_Mean"
    
  )

print(decile_comp_vce_Mean)

## Clear downward trend in event rate as comp_vce_Mean increases. 
## Variable is keeing for modelling. 

## 23. plcd_vce_Mean ---> Mean no of attempted voice calls placed. 

summary(plcd_vce_Mean)

##Decile binning

decile_plcd_vce_Mean<-data%>%
  mutate(dec=ntile(plcd_vce_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(plcd_vce_Mean,na.rm=TRUE),
    LessThan=max(plcd_vce_Mean,na.rm=TRUE),
    varname="plcd_vce_Mean"
    
  )

print(decile_plcd_vce_Mean)


## Clear downward trend in event rate as plcd_vce_Mean increases. 

## Variable is considering for modelling. 


### 24. avg3mou  --> Average monthly minutes of use over the previous three months


summary(avg3mou)

##Decile binning 

decile_avg3mou<-data%>%
  mutate(dec=ntile(avg3mou,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(avg3mou,na.rm=TRUE),
    LessThan=max(avg3mou,na.rm=TRUE),
    varname="avg3mou"
    
  )

print(decile_avg3mou)


## Clear downward trend in event rate as avg3mou increases. Variable is selected for modeeling. 

## Varibale is considering for modelling . 

### 25. avgmou --->Average monthly minutes of use over the life of the customer

summary(avgmou)

## Decile binning 

decile_avgmou<-data%>%
  mutate(dec=ntile(avgmou,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(avgmou,na.rm=TRUE),
    LessThan=max(avgmou,na.rm=TRUE),
    varname="avgmou"
    
  )

print(decile_avgmou)


## No clear trend event trate as avgmou increases. 
## Seems to be a significant variable, doing a log transformation to check the trend. 

data$log_avgmou<-log(data$avgmou)

summary(data$log_avgmou)
summary(avgmou)

decile_log_avgmou<-data%>%
  mutate(dec=ntile(log_avgmou,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(log_avgmou,na.rm=TRUE),
    LessThan=max(log_avgmou,na.rm=TRUE),
    varname="log_avgmou"
    
  )

print(decile_log_avgmou)




## No clear trend in transformed variable also.So removing it from the dataframe.

data<-select(data,-log_avgmou)

 


##26. avg3qty --> Average monthly number of calls over the previous three months

summary(avg3qty)

# Decile binning. 

decile_avg3qty<-data%>%
  mutate(dec=ntile(avg3qty,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(avg3qty,na.rm=TRUE),
    LessThan=max(avg3qty,na.rm=TRUE),
    varname="avg3qty"
    
  )

print(decile_avg3qty)

## Slightly downward trend in event rate as avg3qty increases.

# Doing log transformation to find if more clear trend will be seen. 

data$log_avg3qty<-log(data$avg3qty)

summary(data$log_avg3qty)

# Decile binning. 

decile_log_avg3qty<-data%>%
  mutate(dec=ntile(log_avg3qty,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(log_avg3qty,na.rm=TRUE),
    LessThan=max(log_avg3qty,na.rm=TRUE),
    varname="log_avg3qty"
    
  )

print(decile_log_avg3qty)

## No clear trend in tranformed variable also. Removing the variable from dataframe. 

data<-select(data,-log_avg3qty)

## Keeping the variable avg3qty for further analysis as it seems important and shows slightly downward trend. 


### 27. avgqty ---> Average monthly number of calls over the life of the customer

summary(avgqty)

##Decile binning 

decile_avgqty<-data%>%
  mutate(dec=ntile(avgqty,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(avgqty,na.rm=TRUE),
    LessThan=max(avgqty,na.rm=TRUE),
    varname="avgqty"
    
  )

print(decile_avgqty)


## No clear trend in event rate as avgqty increases. Considering

###   28. avg6mou ----> Average monthly minutes of use over the previous six months

summary(avg6mou)

## Decile binning 

decile_avg6mou<-data%>%
  mutate(dec=ntile(avg6mou,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(avg6mou,na.rm=TRUE),
    LessThan=max(avg6mou,na.rm=TRUE),
    varname="avg6mou"
    
  )

print(decile_avg6mou)

##Downward trend in event rate as avg6mou increaes. 
# Variable is considering for modelling . 

## 29. avg6qty --> Average monthly number of calls over the previous six months

summary(avg6qty)

#Decile binning 

decile_avg6qty<-data%>%
  mutate(dec=ntile(avg6qty,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(avg6qty,na.rm=TRUE),
    LessThan=max(avg6qty,na.rm=TRUE),
    varname="avg6qty"
    
  )

print(decile_avg6qty)

### slight downward trend in event rate as avg6qty increases. 

# Variable is keepin for modelling 


### 30. age1 ---> Age of first household member -- This variable needs to be considered as categorical as per data dictionary.


summary(age1)
unique(age1)

## Decile minning 


decile_age1<-data%>%
  mutate(dec=ntile(age1,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(age1,na.rm=TRUE),
    LessThan=max(age1,na.rm=TRUE),
    varname="age1"
    
  )

print(decile_age1)

data%>%filter(age1==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## 27% of data is zero which doesnt make any sense , so this variable may not be useful for modelling. 



### 31. age2 ---> Age of second household member - categorical variable as per data dictionary 

summary(age2)

unique(age2)

## Decile minning 


decile_age2<-data%>%
  mutate(dec=ntile(age2,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(age2,na.rm=TRUE),
    LessThan=max(age2,na.rm=TRUE),
    varname="age2"
    
  )

print(decile_age2)

## No clear trend.

data%>%filter(age2==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## 51.17 percentage data is zero. Variable can not be used for modelling . 
## Omitting variable from modelling . 


### 32. models ---> Number of models issued --- Categorical variable as per data disctionary 

summary(models)

unique(models)

##Decile binning 


decile_models<-data%>%
  mutate(dec=ntile(models,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(models,na.rm=TRUE),
    LessThan=max(models,na.rm=TRUE),
    varname="models"
    
  )

print(decile_models)

## No clear trend and only less no of deciles. Will analyse this variable by converting into categorical later. 


### 33. hnd_price  ----> Current handset price -- Categorical as per data dictionary. 

summary(hnd_price)
unique(hnd_price)

##Decile binning 

decile_hnd_price<-data%>%
  mutate(dec=ntile(hnd_price,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(hnd_price,na.rm=TRUE),
    LessThan=max(hnd_price,na.rm=TRUE),
    varname="hnd_price"
    
  )

print(decile_hnd_price)

## NO clear trend. As this variable is categorical as per data dictionary , will analyse agian in the categ variable section. 


### 34. actvsubs ---> Number of active subscribers in household -- Categorical variable as per data dictionary/

summary(actvsubs)
unique(actvsubs)

##Decile binning 

decile_actvsubs<-data%>%
  mutate(dec=ntile(actvsubs,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(actvsubs,na.rm=TRUE),
    LessThan=max(actvsubs,na.rm=TRUE),
    varname="actvsubs"
    
  )

print(decile_actvsubs)

### Giving less decile. Will analyse the same in categorical section agian. 

### 35.uniqsubs --- > Number of unique subscribers in the household --- Categorical variable as per data dictionary 

summary(uniqsubs)
unique(uniqsubs)

##Decile binning 

decile_uniqsubs<-data%>%
  mutate(dec=ntile(uniqsubs,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(uniqsubs,na.rm=TRUE),
    LessThan=max(uniqsubs,na.rm=TRUE),
    varname="uniqsubs"
    
  )

print(decile_uniqsubs)


## Less deciles, and no clear trend. Will analyse the same in the categorical section again. 


### 36. forgntvl --->  Foreign travel dummy variable -- Categorical variable as per data dictionary

summary(forgntvl)
unique(forgntvl)

## It is a categorical variabl need to analyse later. 

### 37.  opk_dat_Mean ---> Mean number of off-peak data calls 

summary(opk_dat_Mean)

# Decile binning 


decile_opk_dat_Mean<-data%>%
  mutate(dec=ntile(opk_dat_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(opk_dat_Mean,na.rm=TRUE),
    LessThan=max(opk_dat_Mean,na.rm=TRUE),
    varname="opk_dat_Mean"
    
  )

print(decile_opk_dat_Mean)

## Less no of deciles, will go for 2

decile_opk_dat_Mean<-data%>%
  mutate(dec=ntile(opk_dat_Mean,2))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(opk_dat_Mean,na.rm=TRUE),
    LessThan=max(opk_dat_Mean,na.rm=TRUE),
    varname="opk_dat_Mean"
    
  )

print(decile_opk_dat_Mean)

data%>%filter(opk_dat_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## 90% valueis zero for this variable. Need not be useful for modelling

## Variable is omitted from further analysis. 

### 38. mtrcycle ---> Motorcycle indicator -- Categorical as per data dictionary 

summary(mtrcycle)
unique(mtrcycle)

# Categorical variable , will analyuse in the categorical section. 

### 39. truck ---> Truck indicator - - Categorical as per data dictionary 

summary(truck)
unique(truck)

# Categorical variable , will analyuse in the categorical section. 


#### roam_Mean ---> Mean number of roaming calls 

summary(roam_Mean)  ## Median is zero, lot of zeros

## Decile binning 

decile_roam_Mean<-data%>%
  mutate(dec=ntile(roam_Mean,2))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(roam_Mean,na.rm=TRUE),
    LessThan=max(roam_Mean,na.rm=TRUE),
    varname="roam_Mean"
    
  )

print(decile_roam_Mean)

data%>%filter(roam_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## 68% data is zero, variable is omitting as it doesnt add value to model .

### 41. recv_sms_Mean --> Mean number of received SMS calls

summary(recv_sms_Mean) ## Median is zero 

#Decile binning 

decile_recv_sms_Mean<-data%>%
  mutate(dec=ntile(recv_sms_Mean,2))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(recv_sms_Mean,na.rm=TRUE),
    LessThan=max(recv_sms_Mean,na.rm=TRUE),
    varname="recv_sms_Mean"
    
  )

print(decile_recv_sms_Mean)


data%>%filter(recv_sms_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## 99% dat is zero, can not be used for modelling. 
## Variable is omitted from modelling. 

### 42. blck_dat_Mean ---> Mean number of blocked (failed) data calls

attach(data)
summary(blck_dat_Mean)


decile_blck_dat_Mean<-data%>%
  mutate(dec=ntile(blck_dat_Mean,2))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(blck_dat_Mean,na.rm=TRUE),
    LessThan=max(blck_dat_Mean,na.rm=TRUE),
    varname="blck_dat_Mean"
    
  )

print(decile_blck_dat_Mean)


data%>%filter(blck_dat_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## 98% of data is zero. Variable will not be sueful in modelling
## Omitting the variable from further analysis. 


### 43. mou_pead_Mean __> Mean unrounded minutes of use of peak data calls

summary(mou_pead_Mean)


decile_mou_pead_Mean<-data%>%
  mutate(dec=ntile(mou_pead_Mean,2))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(mou_pead_Mean,na.rm=TRUE),
    LessThan=max(mou_pead_Mean,na.rm=TRUE),
    varname="mou_pead_Mean"
    
  )

print(decile_mou_pead_Mean)


data%>%filter(mou_pead_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## 90% of data is zero. Variable will not be sueful in modelling
## Omitting the variable from further analysis. 

### 44. churn 
### 45 .da_Mean ---> Mean number of directory assisted calls

summary(da_Mean)

decile_da_Mean<-data%>%
  mutate(dec=ntile(da_Mean,6))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(da_Mean,na.rm=TRUE),
    LessThan=max(da_Mean,na.rm=TRUE),
    varname="da_Mean"
    
  )

print(decile_da_Mean)

##NO clear trend. 
data%>%filter(da_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## 47% of data is zero. Variable will not be sueful in modelling
## Omitting the variable from further analysis. 

### 46. da_Range --> Range of number of directory assisted calls

summary(da_Range)

#Decile binning 


decile_da_Range<-data%>%
  mutate(dec=ntile(da_Range,6))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(da_Range,na.rm=TRUE),
    LessThan=max(da_Range,na.rm=TRUE),
    varname="da_Range"
    
  )

print(decile_da_Range)


data%>%filter(da_Range==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## 48% of data is zero. Variable will not be sueful in modelling
## Omitting the variable from further analysis. 


### 47. datovr_Mean ---> Mean revenue of data overage

summary(datovr_Mean)



#Decile binning 


decile_datovr_Mean<-data%>%
  mutate(dec=ntile(datovr_Mean,2))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(datovr_Mean,na.rm=TRUE),
    LessThan=max(datovr_Mean,na.rm=TRUE),
    varname="datovr_Mean"
    
  )

print(decile_datovr_Mean)


data%>%filter(datovr_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## 85% of data is zero. Variable will not be sueful in modelling
## Omitting the variable from further analysis.


### 48. datovr_Range ---> Range of revenue of data overage

summary(datovr_Range) # Median is zero


#Decile binning 


decile_datovr_Range<-data%>%
  mutate(dec=ntile(datovr_Range,2))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(datovr_Range,na.rm=TRUE),
    LessThan=max(datovr_Range,na.rm=TRUE),
    varname="datovr_Range"
    
  )

print(decile_datovr_Range)


data%>%filter(datovr_Range==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))

## 85% of data is zero. Variable will not be sueful in modelling
## Omitting the variable from further analysis.

###  49 drop_dat_Mean  -->Mean number of dropped (failed) data calls

summary(drop_dat_Mean) # Median is zero 

#Decile binning 


decile_drop_dat_Mean<-data%>%
  mutate(dec=ntile(drop_dat_Mean,4))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(drop_dat_Mean,na.rm=TRUE),
    LessThan=max(drop_dat_Mean,na.rm=TRUE),
    varname="drop_dat_Mean"
    
  )

print(decile_drop_dat_Mean)


data%>%filter(drop_dat_Mean==0)%>%
  summarise(Total_zeros_Percent=(n()/nrow(data)*100))


## 97% of data is zero. Variable will not be sueful in modelling
## Omitting the variable from further analysis.

### 50. drop_vce_Mean ---> Mean number of dropped (failed) voice calls

summary(drop_vce_Mean)

#Decile binning 


decile_drop_vce_Mean<-data%>%
  mutate(dec=ntile(drop_vce_Mean,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(drop_vce_Mean,na.rm=TRUE),
    LessThan=max(drop_vce_Mean,na.rm=TRUE),
    varname="drop_vce_Mean"
    
  )

print(decile_drop_vce_Mean)





## No clear trend in event rate , but seems to be important variable
## kEEPING THE VARIABLE IN for further analysis. 

### 51. adjmou -->Billing adjusted total minutes of use over the life of the customer



summary(adjmou)

# Decile binning 


decile_adjmou<-data%>%
  mutate(dec=ntile(adjmou,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(adjmou,na.rm=TRUE),
    LessThan=max(adjmou,na.rm=TRUE),
    varname="adjmou"
    
  )

print(decile_adjmou)

## No clear trend.


##Keeping 



### 52. totrev ---> Total revenue

summary(totrev)


decile_totrev<-data%>%
  mutate(dec=ntile(totrev,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(totrev,na.rm=TRUE),
    LessThan=max(totrev,na.rm=TRUE),
    varname="totrev"
    
  )

print(decile_totrev)


## No clear trend in event rate as totrev increases. 

## Still keeping the same for further analysis as total revenue seems important.

### 53. adjrev ---> Billing adjusted total revenue over the life of the customer

summary(adjrev)


decile_adjrev<-data%>%
  mutate(dec=ntile(adjrev,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(adjrev,na.rm=TRUE),
    LessThan=max(adjrev,na.rm=TRUE),
    varname="adjrev"
    
  )

print(decile_adjrev)


## No clear tren in event rate as adjrev increases. 

## Keeping the variable for further analysis as this variable seems to be important. 

### 54. avgrev ---> Average monthly revenue over the life of the customer

summary(avgrev)

decile_avgrev<-data%>%
  mutate(dec=ntile(avgrev,10))%>%
  group_by(dec)%>%
  summarise(
    n=sum(churn),
    N=n(),
    churn_perc=round(n/N,2),
    GreaterThan=min(avgrev,na.rm=TRUE),
    LessThan=max(avgrev,na.rm=TRUE),
    varname="avgrev"
    
  )

print(decile_avgrev)


## No clear tren in event rate as avjrev increases. 

## Keeping the variable for further analysis as this variable seems to be important. 


###55. Customer_ID

### Primary analysis of contionus variable is done. 


## Writing Variable profiling details into a csv file : 



deciled_continous_data<-rbind(decile_mou_Mean,decile_totmrc_Mean,
                              decile_rev_Range, 
                              
                              decile_change_mou,
                              decile_drop_blk_Mean,
                              decile_drop_vce_Range,
                              decile_mou_opkv_Range,
                              decile_months,
                              decile_totcalls,
                              decile_eqpdays,
                              decile_custcare_Mean,
                              decile_callwait_Mean,
                              decile_iwylis_vce_Mean,
                              decile_callwait_Range,
                              decile_ccrndmou_Range,
                              decile_adjqty,
                              decile_ovrrev_Mean,
                              decile_rev_Mean,
                              decile_ovrmou_Mean,
                              decile_comp_vce_Mean,
                              decile_plcd_vce_Mean,
                              decile_avg3mou,
                              decile_avgmou,
                              decile_avg3qty,
                              decile_avgqty,
                              decile_avg6mou,
                              decile_avg6qty,
                              decile_age1,
                              decile_age2,
                              decile_models,
                              decile_hnd_price,
                              decile_actvsubs,
                              decile_uniqsubs,
                              decile_opk_dat_Mean,
                              decile_roam_Mean,
                              decile_recv_sms_Mean,
                              decile_blck_dat_Mean,
                              decile_mou_pead_Mean,
                              decile_da_Mean,
                              decile_da_Range,
                              decile_datovr_Mean,
                              decile_datovr_Range,
                              decile_drop_dat_Mean,
                              decile_drop_vce_Mean,
                              decile_adjmou,
                              decile_totrev,
                              decile_adjrev,
                              decile_avgrev)

View(deciled_continous_data)
 str(deciled_continous_data)
 
 write.csv(deciled_continous_data,"deciled_continous_data.csv")






### Categorical Variable Profiling starts here ###



View(report)
data_categorical<-report%>%
  filter(report$DataType=="factor"& report$MissingPercent<15.00) %>%
  select(VariableName)

print(data_categorical)

# 1. crclscod
# 2          asl_flag
# 3  prizm_social_one
# 4              area
# 5        refurb_new
# 6        hnd_webcap
# 7           marital
# 8            ethnic
# 9           car_buy
# 10              csa
#11  retcal


### 1. crclscod -->Credit class code 

summary(crclscod)

unique(crclscod)   ## 53 levels  

# Profiling 

profile_crclscod<-data%>%
  group_by(level=crclscod)%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="crclscod")
print(profile_crclscod)


#Reducing factor level by creating the dummy variable by considering only the top 10 levels. 
 data$crclscod_dummy<-fct_lump(data$crclscod,10)

class(data$crclscod_dummy)


profile_crclscod_dummy<-data%>%
  group_by(level=fct_explicit_na(crclscod_dummy))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="crclscod_dummy")
print(profile_crclscod_dummy)

fct_unique(data$crclscod_dummy)


## COnsidering this dummy variable for further analysis. 

### 2. asl_flag  ---> Account spending limit N


summary(asl_flag)

# N- No Account Spending limit , Y-Account Spending limit is set

# Profiling 


profile_asl_flag<-data%>%
  group_by(level=asl_flag)%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="asl_flag")
print(profile_asl_flag)

## There is difference between event rate in each level. 
## Variable is considering for modelling 


### 3. prizm_social_one ---> Social group letter only

summary(prizm_social_one)
unique(prizm_social_one)

# C --> City
# R-->Rural
# S-->SubUrban
# T-->Town
# U--->Urban


profile_prizm_social_one<-data%>%
  group_by(level=fct_explicit_na(prizm_social_one))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="prizm_social_one")
print(profile_prizm_social_one)



## ## There is difference between event rate in each level. 
## Variable is considering for modelling 




### 4.area -->Geographic area 

summary(area)
unique(area) ## 19 levels 

##Profiling 

profile_area<-data%>%
  group_by(level=fct_explicit_na(area))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="area")
print(profile_area)

# ## Considering only top 10 frequently occuring value
# 
data$area_dummy<-fct_lump(data$area,10)

class(data$area_dummy)


profile_area_dummy<-data%>%
  group_by(level=fct_explicit_na(area_dummy))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="area_dummy")
print(profile_area_dummy)


##Considering this dummy variable for modelling.  


### refurb_new --> Handset: refurbished or new 

summary(refurb_new)

## N -- New
## R --Refurbished


# Profiling 

profile_refurb_new<-data%>%
  group_by(level=fct_explicit_na(refurb_new))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="refurb_new")
print(profile_refurb_new)

## ## There is difference between event rate in each level. 
## Variable is considering for modelling 

### 6. hnd_webcap ---> Handset web capability 


summary(hnd_webcap)

# UNKW --Unknown
# WC--Web capable
# WCMB--Web Capable Mini Browser

# Profiling : 

profile_hnd_webcap<-data%>%
  group_by(level=fct_explicit_na(hnd_webcap))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="hnd_webcap")
print(profile_hnd_webcap)

## ## There is considerable difference between event rate in each level. 
## Variable is considering for modelling 


### 7.marital --> Marital status

summary(marital)

# A--Infered Married
# B--Infered single
# M--Married
# S--Single
# U--Unknown

##Profiling 

profile_marital<-data%>%
  group_by(level=fct_explicit_na(marital))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="marital")
print(profile_marital)
  

## Considerable event rate between levels except A & B.
##Variable is considering for modelling .


## 8. ethnic -->Ethnicity roll-up code 

summary(ethnic)
table(data$ethnic)
print(data$ethnic)

# B--> Asian (Non oriental )
# C-->
# D-->Southern Europe
# F-->French
# G-->German
# H-->Hispanic
# I-->Italian
# J-->Jewish
# M-->Miscellanious
# N-->North Europian
# 0-->Asian
# P-->Plynasian
# R-->Arab
# S-->Scottish
# U-->Unknown
# X-->
# Z-->African american


# Profiling 

profile_ethnic<-data%>%
  group_by(level=fct_explicit_na(ethnic))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="ethnic")
print(profile_ethnic)


## Different levels shows different event rate, so considering for modelling 




### 9. car_buy --> New or used car buyer

summary(car_buy)

# New__> New car buy
# UNKNOWN--> Unknown

#Profiling 


profile_car_buy<-data%>%
  group_by(level=fct_explicit_na(car_buy))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="car_buy")
print(profile_car_buy)
  

## Very Slight difference in event rate between each level.Not considering for modelling. 

### 10. csa ---> Communications local service area-- Specific location of th e customers. indicating city 

summary(csa)

# Profiling

profile_csa<-data%>%
  group_by(level=fct_explicit_na(csa))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="csa")
View(print(profile_csa))



length(unique(csa))


### 772 level , may not be useful for modelling. Creating a dummy variable for the same by considering only top 10 most occuring categories and rest as one common category. 


data$csa_dummy<-fct_lump(data$csa,10)

class(data$csa_dummy)


profile_csa_dummy<-data%>%
  group_by(level=fct_explicit_na(csa_dummy))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="csa_dummy")
print(profile_csa_dummy)

fct_unique(data$csa_dummy)

## Using this dummy variable for further analysis 


##11.  retcal ---> Indicates whether a customer did a retension call or not :

### 0--> No retension call , 1---> tention call

summary(data$retcal)


# Profiling

profile_retcal<-data%>%
  group_by(level=fct_explicit_na(retcal))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="retcal")
View(print(profile_retcal))

### Significant difference in event rate between the levels. Variable is considering for modelling 



#### Converting the variable stored as numeric , but supposed to be categorical :####


## age1 

## 00--->default
## Other values signifies a valid age 

summary(age1)

## Will create a dummy variable for this and will analyse. 

data$age1_dummy<-ifelse(age1=="0","default",
                        ifelse(age1<=30,"Young",
                               ifelse(age1>30 & age1<=55,"Middle","old")))




data$age1_dummy<-as.factor(data$age1_dummy)
summary(data$age1_dummy)
class(data$age1_dummy)


##11. age1_dummy --> Categorical variable created for age1

summary(age1_dummy)

# Profiling

profile_age1_dummy<-data%>%
  group_by(level=fct_explicit_na(age1_dummy))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="age1_dummy")
print(profile_age1_dummy)


## Different levels shows difference in event rate. Variable selected for modelling. 


## 12. models

summary(models)
class(data$models)
data$models<-as.factor(data$models)

# Profiling

profile_models<-data%>%
  group_by(level=fct_explicit_na(models))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="models")
print(profile_models)

## shows differenc in event rate in different levels. - keeping

##13> hnd_price -->current handset price :

summary(hnd_price)
data$hnd_price<-as.factor(data$hnd_price)
length(unique(data$hnd_price))

## Profiling :

profile_hnd_price<-data%>%
  group_by(level=fct_explicit_na(hnd_price))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="hnd_price")
print(profile_hnd_price)


## Difference in event retae among different levels. Keeping for further analysis


## 14. actvsubs --No of active subscreibers in the house hold: 

summary(data$actvsubs)
data$actvsubs<-as.factor(data$actvsubs)

## Profiling 


profile_actvsubs<-data%>%
  group_by(level=fct_explicit_na(actvsubs))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="actvsubs")
print(profile_actvsubs)

## different level sshows different event rate. Variable is considering. 

## 15. uniqsubs -> No of unique subscribers in the house hold

summary(data$uniqsubs)

data$uniqsubs<-as.factor(data$uniqsubs)

## Profiling 

profile_uniqsubs<-data%>%
  group_by(level=fct_explicit_na(uniqsubs))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="uniqsubs")
print(profile_uniqsubs)

## Different levels shows different event rate. Keepin for further analysi

##16. forgntvl --> Foreigh travel dummy variable 

##0-->No ,1-->Yes

data$forgntvl<-as.factor(data$forgntvl)

summary(data$forgntvl)

##Profiling 

profile_forgntvl<-data%>%
  group_by(level=fct_explicit_na(forgntvl))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="forgntvl")
print(profile_forgntvl)

## Different levels shows different event rate. Keepin for further analysis

##17, mtrcycle -->Motor cycle indicator(indicates motorcycle indicator in the house hold)

summary(data$mtrcycle)

data$mtrcycle<-as.factor(data$mtrcycle)

##Profiling 

profile_mtrcycle<-data%>%
  group_by(level=fct_explicit_na(mtrcycle))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="mtrcycle")
print(profile_mtrcycle)

## No significant change in event rate at different levels. Not considering for modelling .


## 18. truck --> Truck indicator 


summary(data$truck)

data$truck<-as.factor(data$truck)

##Profiling 

profile_truck<-data%>%
  group_by(level=fct_explicit_na(truck))%>%
  summarise(n=sum(churn),
            N=n(),
            churn_perc=round(n/N,4),
            Var_Name="truck")
print(profile_truck)

## No significant change in event rate at different levels. Not considering for modelling .




### Primary analysis of contionus variable is done. 


## Writing Variable profiling details into a csv file : 

profile_categorical_data<-rbind(profile_crclscod,profile_asl_flag,profile_prizm_social_one,
                              profile_area,profile_refurb_new,
                              profile_hnd_webcap,profile_marital,profile_ethnic,profile_car_buy,profile_csa,
                              profile_retcal,profile_age1_dummy,profile_models,profile_hnd_price,
                              profile_actvsubs,profile_uniqsubs,profile_forgntvl,profile_mtrcycle,profile_truck,profile_month_dummy
                              )

colnames(profile_categorical_data)

write.csv(profile_categorical_data,"profile_categorical_data.csv")



## Removing the varibales based on this analysis which may not be useful: 

data_bkp3<-data  ## taking back-up




# colsToDrop<-c("custcare_Mean","callwait_Mean","callwait_Range","age2","opk_dat_Mean","mtrcycle","truck",
#                      "roam_Mean","recv_sms_Mean","mou_pead_Mean","da_Mean","da_Range","datovr_Mean","datovr_Range","drop_dat_Mean","car_buy",
#                      "age1","ccrndmou_Range","blck_dat_Mean")


colsToDrop<-c("callwait_Mean","callwait_Range","age2","opk_dat_Mean","mtrcycle","truck",
              "roam_Mean","recv_sms_Mean","mou_pead_Mean","da_Mean","da_Range","datovr_Mean","datovr_Range","drop_dat_Mean","car_buy",
              "age1","ccrndmou_Range","blck_dat_Mean","ovrmou_Mean","crclscod","area","csa","months")


data<-data%>%select_(.dots=paste0("-",colsToDrop))

colnames(data)   ## Weare considering only these 48 variablse for further analysis : 

data_bkp4<-data ## taking bake up as data my lose in coming steps : 



######################################################################################################


###DATA PREPARATION - OUTLIER TREATMENT,MISSING VALUE TREATMENT AND DERIVATION OF NEW VARIABLES: 

## Before going further, we will derive some variables useful for case study: 


## Creating variable for assessing network quality 

##1. netquality

data$netquality<-(data$comp_vce_Mean/data$plcd_vce_Mean)

summary(data$netquality)  ## NA created due to 0/0 values. will consider this in missing value treatment



## 2. optimal ---> variable to identify relation ship between optimal/non optimal plan and chun

data$optimal<-(data$ovrrev_Mean/data$totrev)


summary(data$optimal) ## 181 NAs


colnames(data)

### New columns added are , netquality, optimal  




##OUTLIER TREATMENT:: for contionous variable: ##
##using box plot, and imputing with mean



length(list)

list<-colnames(data)

list<-list[-c(25:36,42:48)]

str(data)

 # which(names(data)=="asl_flag")
 # which(names(data)=="churn")
 # which(names(data)=="age1_dummy")
which(names(data)=="Customer_ID")

length(list) ### 31 , so will choose 3*11

## getting all the box plot (for continous variables in a frame)

par(mfrow=c(3,11))

for(i in 1:length(list))
{
  
  boxplot(data[ ,list[i]], main=list[i])
}
  
  

## for loop to replace all the outlier values with mean value:

for(i in 1 : length(list))
{
  
  x<-boxplot(data[ ,list[i]], main=list[i])
  out<-x$out
  index<-which(data[ ,list[i]] %in% x$out)
  data[index,list[i]]<-mean(data[ ,list[i]],na.rm=T)
  rm(x)
  rm(out)
  
}

##checking after treatment : 

par(mfrow=c(3,11))

for(i in 1:length(list))
{
  
  boxplot(data[ ,list[i]], main=list[i])
}


dev.off()

summary(data$mou_Mean) ## checking one variable :: 



data_bkp5<-data

##########   MISSING VALUE TREATMENT ############

colSums(is.na(data))  ## Getting the variables having misisng values :


##mou_Mean -- 181 missing values -omitting them 

mis_mou_Mean<-which(is.na(data$mou_Mean))
length(mis_mou_Mean)
data<-data[-mis_mou_Mean, ]
summary(data$mou_Mean)    ### No missing values 


colSums(is.na(data))


## change_mou ### 233 missing values - omitting 


mis_change_mou<-which(is.na(data$change_mou))
length(mis_change_mou)
data<-data[-mis_change_mou, ]
summary(data$change_mou) 


##eqpdays ## 1 missing value  ### 1 missing value- omitting 

mis_eqpdays<-which(is.na(data$eqpdays))
length(mis_eqpdays)
data<-data[-mis_eqpdays, ]
summary(data$eqpdays) 

##data_bk<-data



### avg6mou #### 2029 missing values --- imputing 

print(decile_avg6mou)   ### ---CHurn rate is similar to 10th decile. 

quantile(data$avg6mou,p=(0:10)/10,na.rm=T)

data$avg6mou[is.na(data$avg6mou)]<-(972+1528)/2  ## Imputing with middle value of 10thdecile

summary(data$avg6mou)



### avg6qty #### 2029 missing values --- imputing 

print(decile_avg6qty)   ### ---CHurn rate is similar to 10th decile. 

quantile(data$avg6qty,p=(0:10)/10,na.rm=T)

data$avg6qty[is.na(data$avg6qty)]<-(321+512)/2  ## Imputing with middle value of 10thdecile

summary(data$avg6qty)



##. netquality



summary(data$netquality)  ## NA created due to 0/0 values. will omit this observation


mis_netquality<-which(is.na(data$netquality))
length(mis_netquality)
data<-data[-mis_netquality, ]
summary(data$netquality) 


## summary(data$optimal)

colSums(is.na(data))  ### hnd_webcap ,marital,ethnic,hnd_price,forgntvl,

## prizm_social_one_dummy,area_dummy,age1_dummy

### hnd_webcap

summary(data$hnd_webcap)  ### 5984 missing values - imputing 

print(profile_hnd_webcap) ### churn rate is closer to WC , imputing with the same 




data$hnd_webcap[which(is.na(data$hnd_webcap))]<-"WC"


### marital 


summary(data$marital)  ## 1144 missing values - imputing 

print(profile_marital)  ## Imputing with "S"


data$marital[which(is.na(data$marital))]<-"S"


## ethnic

summary(data$ethnic)   ###1144 missing values - imputing 

print(profile_ethnic)  ### imputing with "M" as event rate are similar

data$ethnic[which(is.na(data$ethnic))]<-"M"


### hnd_price
summary(data$hnd_price)  ### 632 

print(profile_hnd_price)

data$hnd_price[which(is.na(data$hnd_price))]<-"299.9899902"

### forgntvl 

summary(data$forgntvl)    ###1144

print(profile_forgntvl) ###--imputing with 1 


data$forgntvl[is.na(data$forgntvl)]<-"1"


## prizm_social_one_dummy,area_dummy,age1_dummy


### prizm_social_one 

summary(data$prizm_social_one)  ## 4359 NAs ## imputing with T

print(profile_prizm_social_one) ## imputing with R_T

data$prizm_social_one[is.na(data$prizm_social_one)]<-"T"



##are_dummy



summary(data$area_dummy)  ### 17 NAS - imputing with "DC/MARYLAND/VIRGINIA AREA"


print(profile_area_dummy)

data$area_dummy[is.na(data$area_dummy)]<-"DC/MARYLAND/VIRGINIA AREA"



### age1_dummy

summary(data$age1_dummy)

print(profile_age1_dummy)


data$age1_dummy[is.na(data$age1_dummy)]<-"old"

## csa

summary(data$csa_dummy)

View(profile_csa_dummy)

data$csa_dummy[is.na(data$csa_dummy)]<-"APCFCH703"

colSums(is.na(data))    #### No missing values now ####
data<-select(data,-csa)


data_nomis<-data



colnames(data)

### New columns added are , netquality, optimal  

colSums(is.na(data)) ### ___ no NA values 
colnames(data)   ### final columns considering for modelling 

### Checking if the data preparation affetced churn ratio :

sum(data$churn)/nrow(data)   #### 23.35  ### churn ratio in the original data was 23.9 , so the preaparation doesnt affect the churn ratio::

### Making a backup at this stage also :



data_final<-data

################################################################


### LOGISTIC REGRESSION MODELLING STARTS HERE ####

##Splitting the data into test and train 

set.seed(200)
index<-sample(nrow(data),0.70*nrow(data),replace= FALSE)

train<-data[index, ]
test<-data[-index, ]

## Checking test and train dataset

#churn ratio:

sum(train$churn)/nrow(train)  ### 23.37%

sum(test$churn)/nrow(test)    ### 23.30% Almost same 


### Modelling 

colnames(train)
which(names(train)=="Customer_ID")

model1<-glm(churn~.,data=train[ ,-42], family = "binomial") ## Removing customerID from modelling 
summary(model1)




AIC ### 44210

## using step function to remove non-significant variables :

step(model1,direction="both") ## Step function is taking too much time hence doing it manually 

## Removing all non - significant variables first : Creating dummys first 

## creating dummys for the significant categorical levels: 

summary(data$prizm_social_one)

train$prizm_social_one_S<-ifelse(train$prizm_social_one=="S",1,0)
train$prizm_social_one_R<-ifelse(train$prizm_social_one=="R",1,0)

test$prizm_social_one_S<-ifelse(test$prizm_social_one=="S",1,0)
test$prizm_social_one_R<-ifelse(test$prizm_social_one=="R",1,0)

summary(data$marital)


train$marital_S<-ifelse(train$marital=="S",1,0)

test$marital_S<-ifelse(test$marital=="S",1,0)

summary(train$ethnic)

train$ethnic_C<-ifelse(train$ethnic=="C",1,0)

train$ethnic_N<-ifelse(train$ethnic=="N",1,0)
train$ethnic_S<-ifelse(train$ethnic=="S",1,0)
train$ethnic_U<-ifelse(train$ethnic=="U",1,0)
train$ethnic_Z<-ifelse(train$ethnic=="Z",1,0)


test$ethnic_C<-ifelse(test$ethnic=="C",1,0)

test$ethnic_N<-ifelse(test$ethnic=="N",1,0)
test$ethnic_S<-ifelse(test$ethnic=="S",1,0)
test$ethnic_U<-ifelse(test$ethnic=="U",1,0)
test$ethnic_Z<-ifelse(test$ethnic=="Z",1,0)

summary(train$models)


train$models_2<-ifelse(train$models=="2",1,0)
train$models_3<-ifelse(train$models=="3",1,0)


test$models_2<-ifelse(test$models=="2",1,0)
test$models_3<-ifelse(test$models=="3",1,0)

summary(data$hnd_price)

# 9.989997864 29.98999023 39.98999023 59.98999023 79.98999023 99.98999023 119.9899902 129.9899902 
# 2419       12408         366        4916        6036        4958           1        8452 
# 149.9899902 159.9899902 179.9899902 199.9899902 239.9899902 249.9899902 299.9899902 399.9899902 
# 13646           1          50        6748          29         192         661         130 
# 499.9899902 
# 56 
# > 

train$hnd_price_199.98<-ifelse(train$hnd_price=="199.9899902",1,0)
train$hnd_price_249.98<-ifelse(train$hnd_price=="249.9899902",1,0)
train$hnd_price_299.98<-ifelse(train$hnd_price=="299.9899902",1,0)
train$hnd_price_129.98<-ifelse(train$hnd_price=="129.9899902",1,0)



test$hnd_price_199.98<-ifelse(test$hnd_price=="199.9899902",1,0)
test$hnd_price_249.98<-ifelse(test$hnd_price=="249.9899902",1,0)
test$hnd_price_299.98<-ifelse(test$hnd_price=="299.9899902",1,0)
test$hnd_price_129.98<-ifelse(test$hnd_price=="129.9899902",1,0)


summary(train$hnd_price_199.98)






summary(data$uniqsubs)




train$uniqsubs_2<-ifelse(train$uniqsubs=="2",1,0)
train$uniqsubs_3<-ifelse(train$uniqsubs=="3",1,0)
train$uniqsubs_4<-ifelse(train$uniqsubs=="4",1,0)
train$uniqsubs_5<-ifelse(train$uniqsubs=="5",1,0)
train$uniqsubs_6<-ifelse(train$uniqsubs=="6",1,0)
train$uniqsubs_7<-ifelse(train$uniqsubs=="7",1,0)
train$uniqsubs_8<-ifelse(train$uniqsubs=="8",1,0)


test$uniqsubs_2<-ifelse(test$uniqsubs=="2",1,0)
test$uniqsubs_3<-ifelse(test$uniqsubs=="3",1,0)
test$uniqsubs_4<-ifelse(test$uniqsubs=="4",1,0)
test$uniqsubs_5<-ifelse(test$uniqsubs=="5",1,0)
test$uniqsubs_6<-ifelse(test$uniqsubs=="6",1,0)
test$uniqsubs_7<-ifelse(test$uniqsubs=="7",1,0)
test$uniqsubs_8<-ifelse(test$uniqsubs=="8",1,0)

summary(data$crclscod_dummy)


# A    AA     B    BA     C    CA    DA    E4    EA    ZA Other 
# 10329 21415  2506  7517   945  5286  2410   640  4417  2125  3479 
# >

train$crclscod_DA<-ifelse(train$crclscod_dummy=="DA",1,0)
train$crclscod_E4<-ifelse(train$crclscod_dummy=="E4",1,0)
train$crclscod_EA<-ifelse(train$crclscod_dummy=="EA",1,0)
train$crclscod_AA<-ifelse(train$crclscod_dummy=="AA",1,0)



test$crclscod_DA<-ifelse(test$crclscod_dummy=="DA",1,0)
test$crclscod_E4<-ifelse(test$crclscod_dummy=="E4",1,0)
test$crclscod_EA<-ifelse(test$crclscod_dummy=="EA",1,0)
test$crclscod_AA<-ifelse(test$crclscod_dummy=="AA",1,0)


summary(data$csa_dummy)

train$csa_dummy_HOUHOU281<-ifelse(train$csa_dummy=="HOUHOU281",1,0)

test$csa_dummy_HOUHOU281<-ifelse(test$csa_dummy=="HOUHOU281",1,0)


summary(data$age1_dummy)

train$age1_Middle<-ifelse(train$age1_dummy=="Middle",1,0)
train$age1_old<-ifelse(train$age1_dummy=="old",1,0)



test$age1_Middle<-ifelse(test$age1_dummy=="Middle",1,0)
test$age1_old<-ifelse(test$age1_dummy=="old",1,0)




summary(model1)


## Removing non significant variables and adding dummys for significant categorical


model2<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_vce_Range+
              owylis_vce_Range+mou_opkv_Range+eqpdays+custcare_Mean+iwylis_vce_Mean+ovrrev_Mean+rev_Mean+comp_vce_Mean+avgmou+avg3qty+avgqty+
              netquality+optimal+asl_flag+refurb_new+retcal+prizm_social_one_R+prizm_social_one_S+
              marital_S+ethnic_C+ethnic_N+
              ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+hnd_price_129.98+hnd_price_199.98+hnd_price_249.98+hnd_price_299.98+uniqsubs_2+
              uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_6+uniqsubs_7+uniqsubs_8+month_dummy+crclscod_DA+crclscod_AA+
              crclscod_E4+crclscod_EA+age1_Middle+age1_old+csa_dummy_HOUHOU281+models_2+models_3,data=train[ ,-42], family = "binomial")


summary(model2) ## AIC 44208




## Removing variables which are not significant now :



model3<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_vce_Range+
              owylis_vce_Range+mou_opkv_Range+eqpdays+custcare_Mean+iwylis_vce_Mean+ovrrev_Mean+rev_Mean+comp_vce_Mean+avgmou+avg3qty+avgqty+
              netquality+optimal+asl_flag+refurb_new+retcal+prizm_social_one_R+prizm_social_one_S+
              marital_S+ethnic_C+ethnic_N+
              ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+hnd_price_129.98+hnd_price_199.98+hnd_price_249.98+hnd_price_299.98+uniqsubs_2+
              uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_7+uniqsubs_8+month_dummy+crclscod_DA+
              crclscod_E4+crclscod_EA+age1_Middle+age1_old+csa_dummy_HOUHOU281+models_2+models_3,data=train[ ,-42], family = "binomial")





summary(model3) ## AIC 44207


model4<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_vce_Range+
              owylis_vce_Range+mou_opkv_Range+eqpdays+custcare_Mean+iwylis_vce_Mean+ovrrev_Mean+rev_Mean+comp_vce_Mean+avgmou+avg3qty+avgqty+
              netquality+optimal+asl_flag+refurb_new+retcal+prizm_social_one_R+prizm_social_one_S+
              marital_S+ethnic_C+ethnic_N+
              ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+hnd_price_129.98+hnd_price_199.98+hnd_price_249.98+hnd_price_299.98+uniqsubs_2+
              uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_8+month_dummy+crclscod_DA+
              crclscod_E4+crclscod_EA+age1_Middle+age1_old+csa_dummy_HOUHOU281+models_2+models_3,data=train[ ,-42], family = "binomial")


summary(model4) ##44207

model5<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_vce_Range+
              owylis_vce_Range+mou_opkv_Range+eqpdays+custcare_Mean+iwylis_vce_Mean+ovrrev_Mean+rev_Mean+comp_vce_Mean+avgmou+avgqty+
              netquality+optimal+asl_flag+refurb_new+retcal+prizm_social_one_R+prizm_social_one_S+
              marital_S+ethnic_C+ethnic_N+
              ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+hnd_price_129.98+hnd_price_199.98+hnd_price_249.98+hnd_price_299.98+uniqsubs_2+
              uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_8+month_dummy+crclscod_DA+
              crclscod_E4+crclscod_EA+age1_Middle+age1_old+csa_dummy_HOUHOU281+models_2+models_3,data=train[ ,-42], family = "binomial")


summary(model5) ##44208

##setting significance level as 0.05 and removing all other variables. 


model6<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_vce_Range+
              owylis_vce_Range+mou_opkv_Range+eqpdays+custcare_Mean+iwylis_vce_Mean+ovrrev_Mean+rev_Mean+comp_vce_Mean+avgmou+avgqty+
              netquality+optimal+asl_flag+refurb_new+retcal+prizm_social_one_R+prizm_social_one_S+
              marital_S+ethnic_C+ethnic_N+
              ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+hnd_price_199.98+hnd_price_249.98+hnd_price_299.98+uniqsubs_2+
              uniqsubs_3+uniqsubs_4+uniqsubs_5+month_dummy+crclscod_DA+
              crclscod_E4+crclscod_EA+age1_Middle+age1_old+csa_dummy_HOUHOU281+models_2+models_3,data=train[ ,-42], family = "binomial")


summary(model6) ##44211  ## still holds a good AIC value.


vif(model6) ## All VIF values are less than 5 , acceptable: 



##model6 contain only variables with significant p value(<0.05),and beta coefficient has got meaning.

##good difference between Null deviance and residual deviance . 

## VIF for all variables are less than 5 

## SO lets finalise this model::model6


#### ------>  Model Validation <--------------------- ####

##confusion matrix and AUC values for model validation 

##confusion matrix : 

## getting predicted values for test data :

test$prob<-predict(model6,type="response",newdata=test)

##looking evet rate in the data to get cut-off 

table(data$churn)/nrow(data)  ### 0.2335391 

## getting predicted class labels for test data :

test$result<-ifelse(test$prob>0.2335391,1,0)
test%>%select(result,churn)%>%head(20)
##confusion matrix and kappa matrix 

confusionMatrix(test$result,test$churn,positive="1")  ### 59.24% accuracy ,close to 60, which is acceptable
kappa2(data.frame(test$churn,test$result))  ##0.154 level of agreement 

## Getting the same for train data too ##
train$prob<-predict(model6,type="response",newdata=train)
train$result<-ifelse(train$prob>0.2335391,1,0)
confusionMatrix(train$result,train$churn,positive="1") ## 59.77 accuracy


### PLOTING ROCR curve for train data set :: 

pred_train<-prediction(train$prob,train$churn)
perf_train<-performance(pred_train,"tpr","fpr")

plot(perf_train,col="red")
abline(0,1,lty=8,col="grey")

auc_train<-performance(pred_train,measure="auc")
auc_value_train<-unlist(slot(auc_train,"y.values"))  ### 65.63
print(auc_value_train)

## AUC=65.62 is good as per the dataset given. Will check the same for test data.

## Getting AUC for test 

pred_test<-prediction(test$prob,test$churn)
perf_test<-performance(pred_test,"tpr","fpr")

plot(perf_test,col="red")  ##ROCR curve
abline(0,1,lty=8,col="grey")


auc_test<-performance(pred_test,measure="auc")
auc_value_test<-unlist(slot(auc_test,"y.values"))  ### 64.23

print(auc_value_test)


## Test data set also hold a descent AUC value as per the dataset. 

### Hence validated the model 


####    -----> Gains chart/Customer Targeting <--------    ####

gain_chart<-gains(test$churn,predict(model6,type="response",newdata=test),group=10)



# by observing the gains chart, we can conclude that if we choose top 30% probability score, we can target 44.1 % of churns

quantile(test$prob,prob=c(1:10)/10)

## ie, probabilities in the range of  0.2770584 and 0.7862912 will give 42.1% of curns.

##Subsetting data based on this ,

targeted<-test[test$prob>0.2770584 & test$prob<=0.7862912,c("Customer_ID","churn")]

## This will give 44.1 % of customers churn

head(targeted)

test%>%filter(churn==1)%>%summarise(n())
targeted%>%filter(churn==1)%>%summarise(n())
1882/4270  ##44.08 % 


##Ploting gains chart 

a<-gain_chart[1]
b<-gain_chart[6]
gains=as.data.frame(c(a,b))
ggplot(data=gains)+geom_line(aes(x=gains$depth,y=gains$cume.pct.of.total))



##### CASE STUDY QUESTIONS AND ANSWERS #####

##1. What are the top five factors driving likelyhood of churn at Mobicomm :: 

summary(model6)

head(sort(abs(model6$coefficients),decreasing = TRUE),10)

#1.optimal : 6.9010064## This is the ratio of overage revenue to total revenue(ovrrev_Mean/totrev)
#2.hnd_price_249.98 : 1.1528698 # current handset price 
#3.month_dummy11-12 : 1.1387547 ## categorical variable created for months variable.
#4.ethnic_C : 0.9276580 # ethnicity 
#5.netquality:0.8501007 ## Transformed variable to access network quality (comp_vce_Mean/plcd_vce_Mean)



##2. Validation of Survey Findings :: Whether cost and billing and service and networkquality are important factors:

##Cost and billing : Yes , rev_Range,ovrrev_Range
##Service and network quakity : Yes, netquality, drop_vce_Range,comp_vce_Mean 
##Service quality : custcare_Mean,retcal

## Data connectivity issues turning out to be costly. No variable relate dto data connectivity are significant 





##3. Would you recommend rate plan migration as proactive retention strategy?


##Yes. optimal is significant. is, non optimal customers are churning more. Hence rate plan migration will help.

##4.. What would be your reccomendation on how to use this churn model for prioritation of 
##customers for a proactive retention  campaigns in the future :: 

###a) Rate plan migration :: 


quantile(test$optimal,c(p=1:10)/10)


## Targeting customers who is likely to be churned and have an optimal value of greater than 20% (more than 20% overrage charge in theeir total revenue)

customer_rateplan_migration<-test[test$prob>0.2335391 & test$optimal>0.02, ]


nrow(customer_rateplan_migration)
class(customer_rateplan_migration)

write.csv(customer_rateplan_migration,"TargetCustomers_rateplanMigration.csv")


###b. Offers for low users.

##From the model, it is clear that , monthly minutes of use and churn is in inverse relationship. 
## Targeting customers with less than median value in the testing pool as low usage customers.

summary(test$mou_Mean) ## Median =380

customer_low_mouMean<-test[test$prob>0.2335391 & test$mou_Mean<380, ]

nrow(customer_low_mouMean)
class(customer_low_mouMean)

write.csv(customer_low_mouMean,"TargetCustomers_low_mouMean.csv")

##c. Annual offers :: Another finding from the model is that customer with months of service 
## between 11 and 12 months churn more. So annual offer has to be given to retain these customers 

summary(test$month_dummy)

customer_annualOffers<-test[test$prob>0.2335391 & test$month_dummy=="11-12", ]

nrow(customer_annualOffers)
class(customer_annualOffers)

write.csv(customer_annualOffers,"TargetCustomers_annualOffers.csv")




##d.Family bundling offers  :: customers having more than 1 unique customers in the house hold
## churn more as opinion from family members play a role in churn rate. This can be 
#reuced by providing family offers to the customers where unique customers in the house hold are
## more than 2

## offering bundling offers to families  where there is more than 3 unique customers and  higher churn


test$uniqsubs_int<-as.numeric(test$uniqsubs)
customer_familyBundle<-test[test$prob>0.2335391 & test$uniqsubs_int>2, ]
nrow(customer_familyBundle)
class(customer_familyBundle)

write.csv(customer_familyBundle,"TargetCustomers_familyBundle.csv")


###5. What would be the target segments for pro active retention campaigns? 
## given budget constraint of aceess to 20% of the subscriber pool , which customers should be prioritized if
## controlling churn is primary priority and revenue saves is the secondary priority

##Ans: Assuming test data as subscriber pool :: 

gain_chart

## as we have budget constraint of availability of 20% customer, we will choose top 20 customers in gains chart having higher churn rate -- --> 3664 customers
quantile(test$prob,prob=c(1:10)/10)

## Top 20 customers have probabilties between 0.3116320 & 0.7862912

customers_proactiveRetention<-test[test$prob>0.3116320, ]

quantile(customers_proactiveRetention$prob,prob=c(1:10)/10)## Splitting less than 30% as low prob and 30-70 % as medium prob and greater than 70% as probability of churn

customers_proactiveRetention$ProbabilityOfChurn<-ifelse(customers_proactiveRetention$prob<0.3397220, "Low",
                                                        ifelse(customers_proactiveRetention$prob>=0.3397220 & customers_proactiveRetention$prob<0.3994175, "Medium", "high"))
                                                               

head(customers_proactiveRetention$ProbabilityOfChurn)
head(customers_proactiveRetention$prob)



quantile(customers_proactiveRetention$avgrev,prob=c(1:10)/10) ## Splitting less than 30% as low usage and 30-70 % as medium usage and greater than 70% as high usage.



quantile(customers_proactiveRetention$avgrev,prob=c(1:10)/10)



customers_proactiveRetention$Revenue<-ifelse(customers_proactiveRetention$avgrev<37.26, "Low",
                                                        ifelse(customers_proactiveRetention$avgrev>=37.26 & customers_proactiveRetention$avgrev<63.88, "Medium", "high"))


head(customers_proactiveRetention$avgrev)
head(customers_proactiveRetention$Revenue)


table(Churn=customers_proactiveRetention$ProbabilityOfChurn,Revenue=customers_proactiveRetention$Revenue)

## Targetting customers having high churn rate and high revenue , high churn rate and medium revenue ,high revenue and medium churn 

Customer_Target<-customers_proactiveRetention%>%
  filter((ProbabilityOfChurn=="high"&Revenue=="high")| (ProbabilityOfChurn=="Medium"& Revenue=="high") | (ProbabilityOfChurn=="high" & Revenue=="Medium"))

head(Customer_Target,20)
nrow(Customer_Target)

write.csv(Customer_Target,"TargetCustomers_proactiveRetention.csv")






save.image("C:/Jig19936/CapstoneProject/final/telecomdata.RData")
