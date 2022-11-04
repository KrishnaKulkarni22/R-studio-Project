
library(ggplot2)
library(dplyr)

library(naniar)

attach(insurance)
insurance_dirty <- read.csv(file.choose())
insurance_dirty

dim(insurance_dirty)

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



# 4)) Again removing the unnecessary columns "CLERICAL"
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

## 6)) Replacing NA's in RISK_RATED_AREA_B with its mean

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



insurance_dirty_2$MTA_FAP[is.na(insurance_dirty_2$MTA_FAP)] <- median(insurance_dirty_2$MTA_FAP,na.rm = TRUE) ## Replacing NA's with after presence of outliers

outlied_data_in_MTA_FAP <-  insurance_dirty_2$MTA_FAP[!insurance_dirty_2$MTA_FAP%in%boxplot.stats(insurance_dirty_2$MTA_FAP)$out] ## Outliers in the column

length(outlied_data_in_MTA_FAP)                                         ## Length of the outliers

actual_length_of_MTA_FAP <- length(MTA_FAP) - length(outlied_data_in_MTA_FAP) ## 1224895   no of actual values
actual_length_of_MTA_FAP

boxplot(outlied_data_in_MTA_FAP,      ## To show that the outliers have been removed
        col = "pink")




## 13)) Analysis for missing values in column MTA_APRP

boxplot(MTA_APRP)                        ## To find out outliers

hist(MTA_APRP)


## Replacing NA's with median beacause of presence of outliers
insurance_dirty_2$MTA_APRP[is.na(insurance_dirty_2$MTA_APRP)]<- median(insurance_dirty_2$MTA_APRP,na.rm = TRUE) 


## removing outliers
insurance_dirty_2$MTA_APRP[!insurance_dirty_2$MTA_APRP%in%boxplot.stats(insurance_dirty_2$MTA_APRP)$out]


boxplot(MTA_APRP)






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



## 16)) Drpping unnecessary columns 
insurance_data_clean <- select(insurance_data_clean,-c(P1_POLICY_REFUSED,NEIGH_WATCH))
insurance_data_clean


## 17)) Checking if data is cleaned or not


percent_na(insurance_data_clean)  ## Percent of NA's = 0.0

dim(insurance_data_clean)

View(insurance_data_clean)
