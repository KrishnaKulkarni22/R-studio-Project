
library(ggplot2)
library(dplyr)

library(naniar)

library(nacleanR)

setwd("C:\\Users\\Krishna\\Downloads")
insurance_dirty <- read.csv("C:\\Users\\Krishna\\Downloads\\Insurance Pricing Dataset\\home_insurance.csv")   ## 
insurance_dirty

View(insurance_dirty)

dim(insurance_dirty)

gg_miss_var(insurance_dirty)

# 1) Removing the unnecessary column
insurance_dirty <- select(insurance_dirty,-c(i,CAMPAIGN_DESC))
insurance_dirty
dim(insurance_dirty)

## 2 )) Getting the percentage of missing values
p <- function(x){sum(is.na(x))/length(x)*100}
apply(insurance_dirty,2,p)


## 3)) Replacing empty cells in column "COVER_START" with NA's and removing them
insurance_dirty_1 <- insurance_dirty
insurance_dirty_1[insurance_dirty_1 == ""] <- NA

insurance_dirty_1
View(insurance_dirty_1)

insurance_dirty_2 <- insurance_dirty_1 %>%
  filter(!is.na(COVER_START))
insurance_dirty_2



# 4)) Again removing the unnecessary column "CLERICAL"
insurance_dirty_2 <- select(insurance_dirty_2,-c(CLERICAL))
insurance_dirty_2



## 5)) Finding out outliers based on null value percentage in this for Column = RISK_RATED_AREA_B outliers were observed via a boxplot and NA's can be replaced by the median
boxplot(insurance_dirty_2$RISK_RATED_AREA_B,
        vertical=T,
        notch = T,
        main = "boxplot",
        sub = "(source::ggplot2:insurance-dirty_2)",
        col = "red"
)

## 6)) Replacing NA's in RISK_RATED_AREA_B with its MEDIAN

insurance_dirty_2$RISK_RATED_AREA_B[insurance_dirty_2$RISK_RATED_AREA_B == 0] <- NA
insurance_dirty_2$RISK_RATED_AREA_B[is.na(insurance_dirty_2$RISK_RATED_AREA_B)] <- median(insurance_dirty_2$RISK_RATED_AREA_B,na.rm = TRUE)
insurance_dirty_2

## 7)) Checking outliers in the column RISK_RATED_AREA_B

boxplot(insurance_dirty_2$RISK_RATED_AREA_B,
        vertical=T,
        notch = T,
        main = "boxplot",
        sub = "(source::ggplot2:insurance-dirty_2)",
        col = "red"
)


##   Removing outliers

no_outliers <- insurance_dirty_2$RISK_RATED_AREA_B[!insurance_dirty_2$RISK_RATED_AREA_B%in% boxplot.stats(insurance_dirty_2$RISK_RATED_AREA_B)$out]


## length of remaining data after removing outlier


length(insurance_dirty_2$RISK_RATED_AREA_B) - length(no_outliers)


boxplot(no_outliers,
        col = "red")


## 8)) Removing unnecessary column "P1_PT_EMP_STATUS" as % of NA is 99

insurance_dirty_2 = subset(insurance_dirty_2,select = -c(P1_PT_EMP_STATUS))
insurance_dirty_2



gg_miss_var(insurance_dirty_2)   ## checking NA's in pct.

attach(insurance_dirty_2)

boxplot(RISK_RATED_AREA_C,
        col = "red")



##   9))  Analysis for missing values in column "RISK_RATED_AREA_C"


insurance_dirty_2$RISK_RATED_AREA_C[insurance_dirty_2$RISK_RATED_AREA_C == 0]<- NA         ## Replacing zeros w/ NA's

insurance_dirty_2$RISK_RATED_AREA_C[is.na(insurance_dirty_2$RISK_RATED_AREA_C)] <- median(insurance_dirty_2$RISK_RATED_AREA_C,na.rm = TRUE) ## Replacing NA's W/ Median

insurance_dirty_2$RISK_RATED_AREA_C[!insurance_dirty_2$RISK_RATED_AREA_C%in%boxplot.stats(insurance_dirty_2$RISK_RATED_AREA_C)$out]      ## Removing outliers




miss_var_summary(insurance_dirty_2)


##   10)) Analysis for missing values in column "PAYMENT_FREQUENCY"

insurance_dirty_2$PAYMENT_FREQUENCY[is.na(insurance_dirty_2$PAYMENT_FREQUENCY)] <- 0    ## Replacing NA's W/ Zero's
insurance_dirty_2$PAYMENT_FREQUENCY


## 11)) Dropping Unnecessary Column "PAYING_GUESTS"  

insurance_dirty_2 <- select(insurance_dirty_2,-c(PAYING_GUESTS))                     
insurance_dirty_2



##  12))  Analysis for missing values in column MTA_FAP 


boxplot(MTA_FAP,                                                         ## to check for outliers
        col = "red")


length(MTA_FAP)                                                          ## to find out the length of column                        



insurance_dirty_2$MTA_FAP[is.na(insurance_dirty_2$MTA_FAP)] <- median(insurance_dirty_2$MTA_FAP,na.rm = TRUE) ## Replacing NA's with MEDIAN after presence of outliers

outlied_data_in_MTA_FAP <-  insurance_dirty_2$MTA_FAP[!insurance_dirty_2$MTA_FAP%in%boxplot.stats(insurance_dirty_2$MTA_FAP)$out] ## Outliers in the column

length(outlied_data_in_MTA_FAP)                                         ## Length of the outliers

actual_length_of_MTA_FAP <- length(MTA_FAP) - length(outlied_data_in_MTA_FAP) ## 1224895   no of actual values
actual_length_of_MTA_FAP

boxplot(outlied_data_in_MTA_FAP,      ## To show that the outliers have been removed
        col = "pink")




## 13)) Analysis for missing values in column MTA_APRP

boxplot(MTA_APRP)                        ## To find out outliers

hist(MTA_APRP)


## Replacing NA's with median because of presence of outliers

insurance_dirty_2$MTA_APRP[is.na(insurance_dirty_2$MTA_APRP)]<- median(insurance_dirty_2$MTA_APRP,na.rm = TRUE) 


## removing outliers

insurance_dirty_2$MTA_APRP[!insurance_dirty_2$MTA_APRP%in%boxplot.stats(insurance_dirty_2$MTA_APRP)$out]


boxplot(MTA_APRP)  ## CHECKING FOR OUTLIERS WHETHER THEY'VE BEEN REMOVED OR NOT






gg_miss_var(insurance_dirty_2)


##  14)) Replacing NA's in column "QUOTE DATE" with "YES" as even in cells with NA there is data in subseqquent columns

insurance_dirty_2$QUOTE_DATE <- ifelse(is.na(insurance_dirty_2$QUOTE_DATE),"YES",insurance_dirty_2$QUOTE_DATE)

insurance_dirty_2



##  15)) Dropping unnecessary columns


colnames(insurance_dirty_2)

insurance_dirty_2 <- select(insurance_dirty_2,-c(MTA_DATE,Police,BUS_USE,CLAIM3YEARS,QUOTE_DATE))
insurance_dirty_2

dim(insurance_dirty_2)


colnames(insurance_dirty_2)



insurance_data_clean <- insurance_dirty_2
insurance_data_clean




## 16)) Checking if data is cleaned or not


Clean_data <-  percent_na(insurance_data_clean)  ## Percent of NA's = 0.0
Clean_data








### 1)) ATTACHING DATA TO NEW DF

Insurance <- insurance_data_clean
Insurance

attach(Insurance)

colnames(Insurance)

str(Insurance)

ncol(Insurance)

length(LAST_ANN_PREM_GROSS)



#### 2))) SUBSETTING ABOVE DF 

building_premium <- Insurance %>%
  select(YEARBUILT,OWNERSHIP_TYPE,PROP_TYPE,FLOODING,OCC_STATUS,SAFE_INSTALLED,SUBSIDENCE,ROOF_CONSTRUCTION,WALL_CONSTRUCTION,RISK_RATED_AREA_B,BUILDINGS_COVER,APPR_LOCKS,APPR_ALARM,NEIGH_WATCH,LAST_ANN_PREM_GROSS,BEDROOMS)
building_premium

length(LAST_ANN_PREM_GROSS)

View(building_premium)
colnames(building_premium)


attach(building_premium)




k <- building_premium %>%
  group_by(YEARBUILT) %>%
  summarise(tot_pre = sum(LAST_ANN_PREM_GROSS))
k

attach(k)

lineplt <- ggplot(k) +aes(x=YEARBUILT,y=tot_pre) + geom_line(stat = 'identity' , col = "brown" ) + geom_text(aes(label =paste("$", round(tot_pre,2))),vjust = -0.50) + 
  ggtitle("YEARWISE TOTAL PREMIUM") + ylab("PREMIUM") +
  theme_classic()
lineplt




##### 3 ))) ANALYSING EFFECT OF SUBSIDENCE ON PREMIUM PRICES


m <- building_premium %>%
  select(SUBSIDENCE,LAST_ANN_PREM_GROSS)
m

tot_premium <- m %>%
  group_by(SUBSIDENCE)%>%
  summarise(totalpremium = mean(LAST_ANN_PREM_GROSS))
tot_premium

attach(tot_premium)



subsidence <- ggplot(tot_premium) + aes(x=SUBSIDENCE,y=totalpremium) + geom_bar(stat = 'identity',fill ="#FA0879",width = 0.5) +
  geom_text(aes(label = round(totalpremium,2)),vjust=0.9) + 
  labs(title = "SUBSIDENCE V/S PREMIUM", y = "AVG OF PREMIUM") +
  theme(axis.text.y = element_blank())


subsidence                              ### PRESENCE OF SUBSiDENCE DOES NOt DRIVE UP THE PREMIUM 




##### 4))) ANALYSING EFFECT OF FLOODING ON PREMIUM PRICES
flooding<- building_premium %>%
  select(FLOODING,LAST_ANN_PREM_GROSS)
flooding




flooding_scat <- ggplot(flooding) + aes(x=FLOODING,y=LAST_ANN_PREM_GROSS) + geom_point(stat = 'identity') +labs(title = "FLOODING V/S PREMIUM SCAT",
                                                                                                                y= "PREMIUM")
flooding_scat


##### FOR MEAN OF PrEMIUM PRICES

barplot_for_flooding <- flooding %>%
  group_by(FLOODING)%>%
  summarise(avg_premium = mean(LAST_ANN_PREM_GROSS))
barplot_for_flooding


bar_plt_for_flooding <- ggplot(barplot_for_flooding, aes(x=FLOODING, y=avg_premium , col = "blue")) +geom_bar(stat = 'identity') +theme_classic()
bar_plt_for_flooding


##### FOR SUM OF PREMIUM PRICES
barplot_for_flooding <- flooding %>%
  group_by(FLOODING)%>%
  summarise(avg_premium = sum(LAST_ANN_PREM_GROSS))
barplot_for_flooding  


flooding_bar <- ggplot(barplot_for_flooding,aes(x= FLOODING,y=avg_premium)) + geom_bar(stat = 'identity',
                                                                                       fill = "#FA0879") +
  labs(title = "FLOODING V/S PREMIUM",y="PREMIUM") +
  geom_text(aes(label=avg_premium,2),vjust=0.5) +
  theme(axis.text.y = element_blank())
flooding_bar  #### FLOODING HAS A POSITIVE IMPACT ON THE PREMIUM PRICES


##### 5))  EFFECT OF OWNERSHIP TYPE ON PREMIUM PRICES

ownership <- building_premium %>%
  select(OWNERSHIP_TYPE,LAST_ANN_PREM_GROSS)%>%
  group_by(OWNERSHIP_TYPE)%>%
  summarise(tot_premium = mean(LAST_ANN_PREM_GROSS))
ownership



ownership_bar <- ggplot(ownership,aes(x=reorder(OWNERSHIP_TYPE,tot_premium),y=tot_premium)) + 
  geom_bar(stat = 'identity', fill = " burlywood") + 
  geom_text(aes(label = paste("$",round(tot_premium,2))),vjust=1) + 
  labs(title = "OWNERSHIP TYPe V/S PREMIUM", x = "OWNERSHIP TYPE", y = "PREMIUM") + 
  coord_flip() + theme_classic()
ownership_bar   #### PROPERTY WITH OWNERSHIP TYPE OF 3 WILL HAVE HIGHER PREMIUM PRICE





ownership_scatter <- ggplot(building_premium,aes(x=OWNERSHIP_TYPE,y=LAST_ANN_PREM_GROSS)) + geom_point(stat = 'identity',position = 'identity',colour = as.factor(OWNERSHIP_TYPE)) +
  
  labs(title = "OWNERSHIP TYPe V/S PREMIUM", x = "OWNERSHIP TYPE", y = "PREMIUM") + theme(legend.position = 'top')
ownership_scatter





###### 6)) EFFECT OF PROPERTY TYPE ON PREMIUM PRICES


property <- building_premium%>%
  select(PROP_TYPE,LAST_ANN_PREM_GROSS)%>%
  group_by(PROP_TYPE)%>%
  summarise(tot_premium_prop_wise =mean(LAST_ANN_PREM_GROSS))
property

class(property$PROP_TYPE)
attach(property)

property_bar <- ggplot(property,aes(x=reorder(PROP_TYPE,tot_premium_prop_wise),y=tot_premium_prop_wise)) + geom_bar(stat = 'identity',fill = "pink" ,colour = "black") + 
  labs(title = "Property Type V/S Premium",x = "PROPERTY TYPE", y = "PREMIUM") + 
  geom_text(aes(label = paste("$",round(tot_premium_prop_wise,2))),hjust = 1) + 
  theme_classic() + coord_flip()
property_bar   ##### Customer owning PROPERTY TYPE 37 will fetch a higher premium price




attach(building_premium)


######## 7)) IMPACT OF NO. OF BEDROOM ON PREMIUM PRICES


construction <- building_premium %>%
  group_by(BEDROOMS)%>%
  summarise(premium = mean(LAST_ANN_PREM_GROSS))
construction
class(construction$BEDROOMS)
attach(construction)

construction_sct <- ggplot(data = construction) + geom_point(mapping = aes(x=BEDROOMS,y = premium,col = "red")) + labs(title = "BEDROOMS V/S PREMIUM",x= "BEDROOMS", y= "PREMIUM")  +theme_classic()

construction_sct   #### PREMIUM PRICE INCREASES WITH NO OF BEDROOMS


construction_bar <- ggplot(construction,aes(x=reorder(BEDROOMS,premium),y=premium)) + 
  geom_bar(stat = 'identity' ,fill = "pink", color = "black") + 
  geom_text(aes(label=paste("$",round(premium,2))),hjust = 1) +
  ggtitle("BEDROOM V/S PREMIUM") + theme(plot.title = element_text(colour = "black", hjust = 0.5)) +
  coord_flip() +xlab("BEDROOMS") +ylab("PREMIUM")

construction_bar  ##### PREMIUM PRICES INCREASE WITH THE NO OF BEDROOMS IN HOUSES







######## 8)) IMPACT OF WALLCONSTRUCTION  oN PREMIUM PRICES

wallconstruction <- building_premium%>%
  group_by(WALL_CONSTRUCTION)%>%
  summarise(avgpre = mean(LAST_ANN_PREM_GROSS))
wallconstruction
attach(wallconstruction)
attach(building_premium)

wallconstruction_bar <- ggplot(wallconstruction,aes(x=reorder(WALL_CONSTRUCTION,avgpre),y=avgpre)) + 
  geom_bar(stat = 'identity' ,fill = "pink", color = "black") + 
  geom_text(aes(label=paste("$",round(avgpre,2))),hjust = 1) +
  ggtitle("Wall construction V/S PREMIUM") + theme(plot.title = element_text(colour = "black", hjust = 0.5)) +
  coord_flip() +xlab("Wall Construction") +ylab("PREMIUM")
wallconstruction_bar
class(wallconstruction$WALL_CONSTRUCTION)

roofconstruction <- building_premium%>%
  group_by(ROOF_CONSTRUCTION)%>%
  summarise(avgpremium = mean(LAST_ANN_PREM_GROSS))
roofconstruction

attach(roofconstruction)

####### 9 )) IMPACT OF ROOF CONSTRUCTION ON PREMIUM PRICES
roofconstruction_bar <- ggplot(roofconstruction,aes(x=reorder(ROOF_CONSTRUCTION,avgpremium),y=avgpremium)) + 
  geom_bar(stat = 'identity' ,fill = "pink", color = "black") + 
  geom_text(aes(label=paste("$",round(avgpremium,2))),hjust = 1) +
  ggtitle("Roof Construction V/S PREMIUM") + theme(plot.title = element_text(colour = "black", hjust = 0.5)) +
  coord_flip() +xlab("Roof Construction") +ylab("PREMIUM")
roofconstruction_bar





####### 10)) EFFECT OF NEIGHBOURHOOD ON PREMIUM PRICES
neighbour <- building_premium%>%
  group_by(NEIGH_WATCH)%>%
  summarise(avg_premium = mean(LAST_ANN_PREM_GROSS))
neighbour
attach(neighbour)

neighbour_bar <- ggplot(neighbour,aes(x=NEIGH_WATCH,y=avg_premium)) + 
  geom_bar(stat = 'identity' , fill = "lightblue", width = 0.75) + 
  labs(title = "NEIGHBOUR V/S PREMIUM",x="PRESENCE OF NEIGHBOUR", y = "PREMIUM") + 
  geom_text(aes(label =paste("$" , round(avg_premium,2))),vjust = -0.25)


neighbour_bar    #### HAVING A HOusE In A NEIGHBURHOOD HAS AN IMPACT ON PREMIUM PRICES







##### 11)) PREMIUM PRICES OF APARTMENTS HAVING SAFES, LOCKS AND ALARMS

safe <- building_premium%>%
  group_by(SAFE_INSTALLED)%>%
  summarise(premium1 = mean(LAST_ANN_PREM_GROSS))
safe
attach(safe)

safe_bar <- ggplot(safe,aes(x=SAFE_INSTALLED,y=premium1)) + 
  geom_bar(stat = 'identity' , fill = "grey", colour = "black", width = 0.75) + 
  labs(title = "SAFE INSTALLED V/S PREMIUM",x="SAFE INSTALLED", y = "PREMIUM") + 
  geom_text(aes(label =paste("$" , round(premium1,2))),vjust = -0.25) + theme_classic() 


safe_bar    #### HOUSES HAVING SAFES INSTALLED WILL INCREASE THE PREMIUM PRICES



aprt_locks <- building_premium%>% 
  group_by(APPR_LOCKS) %>%
  summarise(avgofaprtlocks = mean(LAST_ANN_PREM_GROSS))
aprt_locks
attach(aprt_locks)


aprt_locks_bar <- ggplot(aprt_locks,aes(x=APPR_LOCKS,y=avgofaprtlocks)) + 
  geom_bar(stat = 'identity' , fill = "red", colour = "black", width = 0.75) + 
  labs(title = "LOCKS V/S PREMIUM",x="LOCKS INSTALLED", y = "PREMIUM") + 
  geom_text(aes(label =paste("$" , round(avgofaprtlocks,2))),vjust = -0.25) + theme_classic() 

aprt_locks_bar  ##### HOUSES WITH LOCKS DRIVE UP THE PREMIUM PRICES




aprt_alarms <- building_premium%>%
  group_by(APPR_ALARM)%>%
  summarise(avgofaprtalarms =mean(LAST_ANN_PREM_GROSS))
aprt_alarms

attach(aprt_alarms)


aprt_alarms_bar <- ggplot(aprt_alarms,aes(x=APPR_ALARM,y=avgofaprtalarms)) + 
  geom_bar(stat = 'identity' , fill = "orange", colour = "black", width = 0.75) + 
  labs(title = "ALARMS V/S PREMIUM",x="ALARMS INSTALLED", y = "PREMIUM") + 
  geom_text(aes(label =paste("$" , round(avgofaprtalarms,2))),vjust = -0.25) + theme_classic() 


aprt_alarms_bar #### HOUSES WITH ALARMS DRIVE UP THE PREMIUM PRICES








colnames(insurance_data_clean)

## 1))  Creating a new DF to identify customer traits with following columns


Personnel_Policy <- insurance_data_clean%>%
  select(P1_DOB,P1_MAR_STATUS,P1_SEX,P1_EMP_STATUS,P1_POLICY_REFUSED,PAYMENT_FREQUENCY,POL_STATUS,PAYMENT_METHOD,LAST_ANN_PREM_GROSS)
Personnel_Policy




## 2)) Filtering the DF with Policy Status as "Live" and PAYMENT FREQUENCY AS 0 IN ORDER TO CALCULATE CUSTOMER DEFAULT TENDENCY

policy_stat <- Personnel_Policy%>%
  filter(POL_STATUS == "Live")
policy_stat

View(policy_stat)



policy_stat_with_payment_freq <- policy_stat %>%
  filter(PAYMENT_FREQUENCY == 0)
policy_stat_with_payment_freq

View(policy_stat_with_payment_freq)
dim(policy_stat_with_payment_freq)

length(policy_stat_with_payment_freq$P1_MAR_STATUS)




### 3)) We will find out the genderwise trait of customer who are likely to default for aforementioned criteria

stats <- policy_stat_with_payment_freq %>%     ###### THIS WILL GET US GENDERWISE PERCENT OF DEFAULT WITH POL_STATUS = "Live" AND PAYMENT_FREQUENCY = 0 AND GeNDER
  group_by(P1_SEX)%>%
  summarise(cnt = n())%>%
  mutate(genderpercentage = cnt/sum(cnt)*100)
stats
View(stats)

sum(stats$cnt)

stats$genderpercentage <- round(stats$genderpercentage,2)
stats

stats_bar <- ggplot(stats) + aes(x=P1_SEX,y=genderpercentage) + geom_bar(stat = 'identity' ,width = 0.5 ,fill = "pink" ) +geom_text(aes(label=paste(genderpercentage,"%")),vjust=-0.25) + 
  xlab("GENDER") +ggtitle("GENDER V/S DEFAULT") + theme(axis.title.y = element_blank(),axis.text.y = element_blank())

stats_bar   ### BARPLOT REPRESENTATION OF THE SAME


View(stats_update)





#### 4)) Now WE FIND OUT THE CUSTOMER TENDENCY TO DEFAULT WITH FOLLOWING CRITERIA : EMPLOYEE STATUS 

j <- policy_stat_with_payment_freq%>%
  group_by(P1_EMP_STATUS)%>%
  summarise(cnt = n())%>%
  mutate(freq = cnt/sum(cnt)*100)

j  ##### DATAFRAME J PROVIDES IN DEPTH DETAILS OF AFOREMENTIONED CRITERIA AGAINST PAYMENT FREQUNCY

j$freq <- round(j$freq,2)
j

attach(j)

j_bar <- ggplot(j) + aes(x=P1_EMP_STATUS, y = freq) + geom_bar(stat = 'identity' ,width = 0.85, fill = "purple") + 
  geom_text(aes(label =paste(freq,"%")),vjust = -0.25) + ggtitle("DEFAULT CHANCES V/S EMPLOYEE STATUS FOR 0 PAYMENT FREQ") + xlab("EMPLOYEE STATUS") + ylab("PERCENTAGE")+
theme_classic()
j_bar


#### 5)) Now WE FIND OUT THE CUSTOMER TENDENCY TO DEFAULT WITH FOLLOWING CRITERIA : MARTIAL STATUS 

emp_mar_stat <- policy_stat_with_payment_freq%>%
  group_by(P1_MAR_STATUS)%>%
  summarise(cnt = n())%>%
  mutate(perc= cnt/sum(cnt)*100)
emp_mar_stat

emp_mar_stat$perc <- round(emp_mar_stat$perc,2)
emp_mar_stat


emp_mar_stat_bar <- ggplot(emp_mar_stat) + aes(x=P1_MAR_STATUS , y= perc) + geom_bar(stat = 'identity', width = 0.75 , fill = "black" ,col = "orange") + 
  geom_text(aes(label =paste(perc,"%")),vjust = -0.25) + labs(title = "DEFAULT CHANCES AGAINST MARTAL STATUS FOR 0 PAYMENT FREQ", x= "MARTIAL STATUS" , y = "PERCENTAGE")+
theme_classic()
emp_mar_stat_bar




k <- policy_stat_with_payment_freq%>%
  group_by(P1_SEX)%>%
  summarise(cnt = n())%>%
  mutate(perc= cnt/sum(cnt)*100)
k




#### 6)) FILTERING PERSONNEL_POLICY DF WITH POL_STATUS != "Live" AND PAYMENT_FREQ = 1


Personnel_Policy
dim(Personnel_Policy)

print(Personnel_Policy)


policy_stat_other <- Personnel_Policy%>%
  filter(POL_STATUS !="Live", PAYMENT_FREQUENCY == 1)
policy_stat_other
View(policy_stat_other)



##### 7)) GETTING A PERCNTAGE FOR ALL THE VARIABLES IN THE DF


z <- policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_MAR_STATUS,P1_POLICY_REFUSED,P1_SEX,PAYMENT_METHOD)%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
z

View(z)

sum(z$cnt)



### ANALYING EMPLOYEE STATUS BASED ON AFOREMENTIONED CONDITIONS


z_1 <- policy_stat_other%>%
  group_by(P1_EMP_STATUS)%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
z_1
dim(z_1)

z_1$percentage <- round(z_1$percentage,2)
z_1

z_1_bar <- ggplot(z_1,aes(x=P1_EMP_STATUS,y =percentage)) + geom_bar(stat = 'identity' , fill = "orange") + 
  geom_text(aes(label = paste(percentage,"%")),vjust = -0.25) + labs(title = "DEFAULT CHANCES V/S EMPLOYEMENT STATUS" , x="EMPLOYEMENT STATUS" , y= "PERCENTAGE") +
  theme_grey()
z_1_bar #### Barplot for above criteria



#### ANALYSING THE DEFAULT TENDENCY OF EMPLOYED CUSTOMERS GENDERWISE
plot1 <-policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_SEX)%>%
  filter(P1_EMP_STATUS == "E")%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot1

plot1$percentage <- round(plot1$percentage,2)
plot1

z_2_bar <- ggplot(plot1, aes(x=P1_SEX,y= percentage)) + geom_text(aes(label = paste(percentage,"%")),vjust = -0.25) + geom_bar(stat = 'identity' , fill = "lightblue") + 
                                                        ggtitle("DEFAULT CHANCES OF EMPLOYED CUSTOMER WHO'VE CANCELLED POLICIES") + xlab("GENDER") + 
                                                        theme(axis.title.y = element_blank() , axis.text.y = element_blank())
z_2_bar

#### FINDING OUT GENDERWISE CUSTOMERS WHO ARE RETIRED AND HAVE CANCELLED THEIR POLICIES

plot2 <- policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_SEX)%>%
  filter(P1_EMP_STATUS == "R")%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot2


plot2$percentage <- round(plot2$percentage,2)
plot2
attach(plot2)

z_3_bar <- ggplot(plot2,aes(x=P1_SEX,y= percentage)) + geom_bar(stat = 'identity', width = 0.85, fill = "brown") + 
            geom_text(aes(label=paste(percentage,"%")),vjust=-0.25) + xlab("GENDER") + ggtitle("DEFAULT CHANCES OF  CUSTOMER WHO'VE CANCELLED POLICIES") +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())
z_3_bar

#### FINDING OUT THE MARTIAL STATUS OF CUSTOMERS WHO'VE CANCELLED THEIR POLICIES


plot3 <-  policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_SEX)%>%
  filter(P1_MAR_STATUS == "M",P1_EMP_STATUS == "E")%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot3

sum(plot3$cnt)
plot3$percentage <- round(plot3$percentage,2)
plot3

z_4_bar <- ggplot(plot3,aes(x=P1_SEX,y=percentage))+geom_bar(stat = 'identity', width = 0.85, fill ="magenta")+ geom_text(aes(label =paste(percentage,"%"),vjust = -0.25))+
                           labs(title = " GENDER OF CUSTOMERS WHO ARE MARRIED AND EMPLOYED AND HAVE THEIR POLICIES CANCELLED", x="GENDER") + 
                           theme(axis.title.y = element_blank(), axis.text.y = element_blank())
z_4_bar


plot5 <-  policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_SEX)%>%
  filter(P1_MAR_STATUS == "M",P1_EMP_STATUS == "R")%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot5

plot5$percentage <- round(plot5$percentage,2)
plot5

z_5_bar <- ggplot(plot5,aes(x=P1_SEX,y=percentage))+geom_bar(stat = 'identity', width = 0.85, fill ="yellow")+ geom_text(aes(label =paste(percentage,"%"),vjust = -0.25))+
  labs(title = " GENDER OF CUSTOMERS WHO ARE MARRIED AND RETIRED AND HAVE THEIR POLICIES CANCELLED", x="GENDER") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
z_5_bar

sum(plot5$cnt)

plot6 <- policy_stat_other%>%
  group_by(P1_SEX)%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot6

plot6$percentage <- round(plot6$percentage,2)
plot6

sum(plot6$cnt)

z_6_bar <- ggplot(plot6,aes(x=P1_SEX,y=percentage))+geom_bar(stat = 'identity', width = 0.85, fill ="orange")+ geom_text(aes(label =paste(percentage,"%"),vjust = -0.25))+
  labs(title = " GENDER OF CUSTOMERS WHO ARE MARRIED AND RETIRED AND HAVE THEIR POLICIES CANCELLED", x="GENDER") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
z_6_bar




plot7 <-  policy_stat_other%>%
          group_by(P1_MAR_STATUS)%>%
          summarise(cnt = n())%>%
    mutate(percentage = cnt/sum(cnt)*100)
plot7

attach(plot7)

plot7$percentage <- round(plot7$percentage,2)
plot7

sum(plot7$cnt)

z_7_bar <- ggplot(plot7,aes(x=P1_MAR_STATUS,y=percentage))+geom_bar(stat = 'identity', width = 0.85, fill ="coral1")+ geom_text(aes(label =paste(percentage,"%"),vjust = -0.25))+
  labs(title = " Martial Status of customers", x="Martial Status") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
z_7_bar



