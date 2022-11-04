

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


stats_bar <- ggplot(stats) + aes(x=P1_SEX,y=genderpercentage) + geom_bar(stat = 'identity' ,width = 0.5 ) +geom_text(aes(label=paste(round(genderpercentage,2))),vjust=-0.25) + 
  scale_color_brewer(palette = "Dark2")

stats_bar   ### BARPLOT REPRESENTATION OF THE SAME


View(stats_update)





#### 4)) Now WE FIND OUT THE CUSTOMER TENDENCY TO DEFAULT WITH FOLLOWING CRITERIA : EMPLOYEE STATUS 

j <- policy_stat_with_payment_freq%>%
  group_by(P1_EMP_STATUS)%>%
  summarise(cnt = n())%>%
  mutate(freq = cnt/sum(cnt)*100)

j  ##### DATAFRAME J PROVIDES IN DEPTH DETAILS OF AFOREMENTIONED CRITERIA AGAINST PAYMENT FREQUNCY


attach(j)

j_bar <- ggplot(j) + aes(x=P1_EMP_STATUS, y = freq) + geom_bar(stat = 'identity' ,width = 0.5, fill = rainbow(length(P1_EMP_STATUS))) + 
  geom_text(aes(label =paste(round(freq,2))),vjust = -0.25) + ggtitle("DEFAULT CHANCES V/S EMPLOYEE STATUS FOR 0 PAYMENT FREQ") + xlab("EMPLOYEE STATUS") + ylab("PERCENTAGE")
  theme_classic()
j_bar


 #### 5)) Now WE FIND OUT THE CUSTOMER TENDENCY TO DEFAULT WITH FOLLOWING CRITERIA : MARTIAL STATUS 

emp_mar_stat <- policy_stat_with_payment_freq%>%
  group_by(P1_MAR_STATUS)%>%
  summarise(cnt = n())%>%
  mutate(perc= cnt/sum(cnt)*100)
emp_mar_stat


emp_mar_stat_bar <- ggplot(emp_mar_stat) + aes(x=P1_MAR_STATUS , y= perc) + geom_bar(stat = 'identity', width = 0.75 , fill = "steelblue" ,col = "red") + 
  geom_text(aes(label =paste(round(perc,2))),vjust = -0.25) + labs(title = "DEFAULT CHANCES AGAINST MARTAL STATUS FOR 0 PAYMENT FREQ", x= "MARTIAL STATUS" , y = "PERCENTAGE")
  theme_classic()
emp_mar_stat_bar




 k <- policy_stat_with_payment_freq%>%
   group_by(P1_SEX)%>%
   summarise(cnt = n())%>%
   mutate(perc= cnt/sum(cnt)*100)
 k
 
 
 
 
 #### 6)) FILTERING PERSONNEL_POLICY DF WITH POL_STATUS = "Live" AND PAYMENT_FREQ = 0
 
 
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



z_1_bar <- ggplot(z_1,aes(x=P1_EMP_STATUS,y =percentage)) + geom_bar(stat = 'identity' , fill = "orange") + 
  geom_text(aes(label = paste(round(percentage,2))),vjust = -0.25) + labs(title = "DEFAULT CHANCES V/S EMPLOYEMENT STATUS" , x="EMPLOYEMENT STATUS" , y= "PERCENTAGE") +
  theme_grey()
z_1_bar #### Barplot for above criteria



#### ANALYSING THE DEFAULT TENDENCY OF EMPLOYED CUSTOMERS GENDERWISE
plot1 <-policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_SEX)%>%
  filter(P1_EMP_STATUS == "E")%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot1


z_2_bar <- ggplot(plot1) + aes(x=P1_SEX,y= percentage) + geom_bar(stat = 'identity' , fill = "grey")
z_2_bar

#### FINDING OUT GENDERWISE CUSTOMERS WHO'VE CANCELLED THEIR POLICIES

plot2 <- policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_SEX)%>%
  filter(P1_EMP_STATUS == "R")%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot2



#### FINDING OUT THE MARTIAL STATUS OF CUSTOMERS WHO'VE CANCELLED THEI POLICIES


plot3 <-  policy_stat_other%>%
  group_by(P1_EMP_STATUS,P1_SEX)%>%
  filter(P1_MAR_STATUS == "M",P1_EMP_STATUS == "E")%>%
  summarise(cnt = n())%>%
  mutate(percentage = cnt/sum(cnt)*100)
plot3



z_4_bar <- ggplot(plot3) + aes(x=P1_SEX,y=percentage) +geom_bar(stat = 'identity') + geom_text(aes(label=paste(round(percentage,2))),vjust = -0.25)+xlab("GENDER") + theme(axis.title.y = element_blank())
z_4_bar
 
