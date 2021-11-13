#session 1: accessing the elements in the R

cust_churn2 <- read.csv("~/iCloudDrive/Data Science Architect Master's Course/Data Science Course (R Language)/datasets_r_programming/customer_churn.csv")
View(customer_churn)

View(customer_churn$tenure)

customer_churn$PaymentMethod -> c_PaymentMethod
customer_churn[1]
customer_churn[,2] -> c_gender
View(c_gender)
customer_churn[,c(2,6,7)] -> c_267
View(c_267)
customer_churn[,"MonthlyCharges"] -> c_MonthlyCharges
View(c_MonthlyCharges)
customer_churn[,3:8] -> c_3to8
View(c_3to8)
customer_churn[2,] -> c_row2

View(c_row2)

customer_churn[2:10,] -> c_row2-10
View(c_row2to10)

customer_churn[50:60,c(2,3)]->c_rand1

# session 2 operators in R
cust_churn1 <- read.csv("C:/Users/Admin/iCloudDrive/Data Science Architect Master's Course/Data Science Course (R Language)/customer_churn.csv")
View(cust_churn1)

customer_churn$MonthlyCharges[1] -1 -> customer_churn$MonthlyCharges[1]
customer_churn$MonthlyCharges[1]


cust_churn1$TotalCharges>8500 -> c_TotalChargesmorethan8500
subset(cust_churn1,c_TotalChargesmorethan8500) -> c_TotalChargesmorethan8500
View(c_TotalChargesmorethan8500)



# inbuilt functions in R
#table()   str()  head()  mean()  sample()  range()  tail()
# nrow()   ncol() min() max()   range()   
  

# flow control statements
#  continue()   if()  ifelse() repeat()  break() 
# for()  while()  switch()


# basic data structures in R

# Vector  list array matrix


#Module 2 introduction to Data Manupulation

#dplyr and #sqldf 


#select()
library(dplyr)
select(cust_churn2,2) -> c_2
View(c_2)

select(cust_churn2,6) -> c_6
View(c_6)

select(cust_churn2, 1,4,7,12) -> c_bunch
View(c_bunch)

select(cust_churn2,5:10) ->c_5to10
View(c_5to10)


select(cust_churn2,"gender")->c_gender
View(c_gender)

select(cust_churn2,gender, Partner, tenure) ->c_gpt
View(c_gpt)

select(cust_churn2,gender:Contract) -> c_gender_contract
View(c_gender_contract)


select(cust_churn2, starts_with("Stream")) -> c_stream
View(c_stream)


select(cust_churn2, ends_with("charges")) -> c_charges
View(c_charges)



#filter()
library(dplyr)
filter(cust_churn2,gender == "Female") -> c_female
View(c_female)

filter(cust_churn2, MonthlyCharges>100) -> c_monthlycharges_100
View(c_monthlycharges_100)
range(c_monthlycharges_100$MonthlyCharges)


filter(cust_churn2, gender == "Female" & MonthlyCharges >100) -> c_high_female
View(c_high_female)

filter(cust_churn2, StreamingTV == "Yes" & StreamingMovies == "Yes") ->c_stream
View(c_stream)

filter(cust_churn2, tenure >50 & InternetService == "DSL" & Contract == "One year") -> c_tic
View(c_tic)

filter(cust_churn2, PaymentMethod == "Mailed ceck" | PaymentMethod == "Electronic check") -> c_pay
View(c_pay)

filter(cust_churn2,(Contract == "One year" | Contract == "Two year") & gender == "Female") -> c_con_gen
View(c_con_gen)

table(c_con_gen$Contract)


filter(cust_churn2, (InternetService == "DSL" | InternetService == "Fiber optic") & (tenure>50 & MonthlyCharges >100)) ->c_int_ten_50
View(c_int_ten_50)



#mutate()
library(dplyr)
mutate(cust_churn2,
       Age = ifelse (SeniorCitizen == 0,
                     sample(x = 16:55),
                     sample(x = 56:100)
                     )
       ) -> cust_churn2



range(cust_churn2$MonthlyCharges)

mutate(cust_churn2,
       Cust_cat = ifelse (
         MonthlyCharges<45,
         "Low Paying",
         ifelse(
           MonthlyCharges<90, 
           "Medium paying", 
           "High Paying"
         )
        )
       ) -> cust_churn2


# Sample()

# sample_n()
# sample_frac()

library(dplyr)
sample_n(cust_churn2, 10) -> random1
View(random1)

sample_n(cust_churn2, 100) -> random2
View(random2)

sample_frac(cust_churn2,0.12) -> frac1
View(frac1)

sample_frac(cust_churn2,0.5) -> frac1
View(frac1)


count(cust_churn2, gender)
count(cust_churn2, InternetService)
count(cust_churn2, Contract)
count(cust_churn2, Cust_cat)


# summarise() and groupby()
library(dplyr)
summarise(cust_churn2,mean_tenure = mean(tenure))
summarise(cust_churn2,mean_tenure = min(tenure))
summarise(cust_churn2,mean_tenure = max(tenure))

summarise(cust_churn2, mean_monthly_charges = mean(MonthlyCharges))
summarise(cust_churn2, mean_monthly_charges = min(MonthlyCharges))
summarise(cust_churn2, mean_monthly_charges = max(MonthlyCharges))

summarise(cust_churn2, mean_total_charges = mean(TotalCharges, na.rm = T))
summarise(cust_churn2, min_total_charges = min(TotalCharges, na.rm = T))
summarise(cust_churn2, max_total_charges = max(TotalCharges, na.rm = T))


summarise(group_by(cust_churn2,PaymentMethod), mean_tenure = mean(tenure))

summarise(group_by(cust_churn2,InternetService), mean_tenure = mean(tenure))

summarise(group_by(cust_churn2,Partner), mean_monthly_charges = mean(MonthlyCharges))

summarise(group_by(cust_churn2,InternetService), mean_monthly_charges = mean(MonthlyCharges))

# pipe()

library(dplyr)


cust_churn2 %>% select(1:5) %>% filter(gender == "Male") -> c_pipe
View(c_pipe)

cust_churn2 %>% filter(InternetService == "DSL") %>% group_by(gender) %>% summarise(mean_monthly_charges = mean(MonthlyCharges))


cust_churn2 %>% select(1,2,"MonthlyCharges", "PaymentMethod") %>% filter(MonthlyCharges > 100 & gender == "Male") -> c_male_payment
View(c_male_payment)


cust_churn2 %>% group_by(PaymentMethod) %>% summarise(mean_tenure = mean(tenure))%>% arrange(desc(PaymentMethod))


cust_churn2 %>% select(1,2, 10:21) %>% filter(Contract == "One year" | Contract == "Two year") %>% arrange(Contract) -> c_contract
View(c_contract)

cust_churn2 %>% filter(PaperlessBilling == "No") %>% group_by(TechSupport)%>% summarise(mean_tenure = mean(tenure))%>%arrange(desc(mean_tenure))


library(sqldf)
#selet  where  and  or  count  groupby

sqldf("select customerID from cust_churn2") -> c_id
View(c_id)

sqldf("select customerID, gender, MonthlyCharges, TotalCharges from cust_churn2") ->c_multiple
View(c_multiple)

sqldf("select * from cust_churn2") -> c_all
View(c_all)

sqldf("select * from cust_churn2 where gender = 'Male'") ->c_male
table(c_male$gender)


sqldf("select * from cust_churn2 where tenure >50") -> c_high_tenure
range(c_high_tenure$tenure)


sqldf("select * from cust_churn2 where InternetService == 'DSL'")-> c_dsl
table(c_dsl$InternetService)


sqldf("select* from cust_churn2 where gender== 'Male' and Contract = 'One year'") -> c_male1
View(c_male1)

table(c_male1$gender)
table(c_male1$Contract)


sqldf("select * from cust_churn2 where SeniorCitizen == 1 and InternetService == 'Fiber optic' and TechSupport = 'Yes'")-> c_sip
View(c_sip)


sqldf("select * from cust_churn2 where Contract ='One year' or Contract = 'Two year'") -> c_contract
table(c_contract$Contract)


sqldf("select * from cust_churn2 where PaymentMethod = 'Electronic check' or PaymentMethod = 'Mailed check'") -> c_payment
View(c_payment)
table(c_payment$PaymentMethod)



sqldf("select count(gender) from cust_churn2 where gender = 'Female'")

sqldf("select avg(tenure), PaymentMethod from cust_churn2 group by PaymentMethod")

sqldf("select avg(MonthlyCharges), Contract from cust_churn2 group by Contract")




#Module 3 Data Visualization


#barplot  histogram desnsityplot
str(cust_churn2)
plot(cust_churn2$)

cust_churn2$gender = as.factor(cust_churn2$gender)
cust_churn2$SeniorCitizen = as.factor(cust_churn2$SeniorCitizen)
cust_churn2$Partner = as.factor(cust_churn2$Partner)
cust_churn2$Dependents = as.factor(cust_churn2$Dependents)
cust_churn2$PhoneService = as.factor(cust_churn2$PhoneService)
cust_churn2$MultipleLines = as.factor(cust_churn2$MultipleLines)
cust_churn2$InternetService = as.factor(cust_churn2$InternetService)
cust_churn2$OnlineSecurity = as.factor(cust_churn2$OnlineSecurity)
cust_churn2$OnlineBackup = as.factor(cust_churn2$OnlineBackup)
cust_churn2$DeviceProtection = as.factor(cust_churn2$DeviceProtection)
cust_churn2$TechSupport = as.factor(cust_churn2$TechSupport)
cust_churn2$StreamingTV = as.factor(cust_churn2$StreamingTV)
cust_churn2$StreamingMovies = as.factor(cust_churn2$StreamingMovies)
cust_churn2$Contract = as.factor(cust_churn2$Contract)
cust_churn2$PaperlessBilling = as.factor(cust_churn2$PaperlessBilling)
cust_churn2$PaymentMethod   = as.factor(cust_churn2$PaymentMethod)
cust_churn2$Churn = as.factor(cust_churn2$Churn)



plot(cust_churn2$Dependents)

plot(cust_churn2$Dependents, col = "coral")


plot(cust_churn2$Dependents, col = "coral", xlab = "Dependents", main = "Distribution of dependence")


plot(cust_churn2$PhoneService, col = "aquamarine4", xlab ="Phone Service", main= "Distriution of Phone Service" )

plot(cust_churn2$InternetService, col = "orange", xlab = "Internet Service", main =" Distribution of Internet Service")

plot(cust_churn2$Contract, col = "palegreen4", xlab = "Contract", main = " Distribution of contract")



hist(cust_churn2$tenure, col = "olivedrab", breaks = 30)

hist(cust_churn2$MonthlyCharges, col= "palevioletred", breaks = 50)


plot(density(cust_churn2$tenure), col = "red")

plot(density(cust_churn2$MonthlyCharges))


# geom_hist()


library(ggplot2)
ggplot(data= cust_churn2, aes(x = tenure)) + geom_histogram(bins = 50)
ggplot(data= cust_churn2, aes(x = tenure)) + geom_histogram(fill = "palegreen4",bins = 50, col = "green")

ggplot(data= cust_churn2, aes(x = tenure, fill = Partner)) + geom_histogram(position = identity, bins = 50)



ggplot(data= cust_churn2, aes(x = tenure, fill = OnlineSecurity)) + geom_histogram(position = "identity")

ggplot(data= cust_churn2, aes(x = tenure, fill = Contract)) + geom_histogram(position = "identity")

ggplot(data= cust_churn2, aes(x = tenure, fill = Churn)) + geom_histogram(position = "identity")


ggplot(data = cust_churn2, aes(x = MonthlyCharges)) + geom_histogram()

ggplot(data = cust_churn2, aes(x = MonthlyCharges)) + geom_histogram(fill = "Coral")

ggplot(data = cust_churn2, aes(x = MonthlyCharges, fill = PhoneService)) + geom_histogram(position = "identity")

ggplot(data = cust_churn2, aes(x = MonthlyCharges, fill = InternetService)) + geom_histogram(position = "identity")

ggplot(data = cust_churn2, aes(x = MonthlyCharges, fill = StreamingTV)) + geom_histogram(position = "identity")

ggplot(data = cust_churn2, aes(x = TotalCharges)) + geom_histogram(fill = "FireBrick4",col = "gold",bins = 50)

ggplot(data = cust_churn2, aes(x = TotalCharges, fill = DeviceProtection)) + geom_histogram(postion = "identity")


# geom bar()

library(ggplot2)
ggplot(data = cust_churn2, aes(x = SeniorCitizen)) + geom_bar()
ggplot(data = cust_churn2, aes(x = SeniorCitizen)) + geom_bar(fill = "aquamarine3", col = "black")
ggplot(data = cust_churn2, aes(x = SeniorCitizen, fill = SeniorCitizen)) + geom_bar()
ggplot(data = cust_churn2, aes(x = SeniorCitizen, fill = InternetService)) + geom_bar(position = "dodge")

ggplot(data = cust_churn2, aes(x = SeniorCitizen, fill = PaymentMethod)) + geom_bar(position = "dodge")

ggplot(data = cust_churn2, aes(x = gender)) + geom_bar(fill = "blue")

ggplot(data = cust_churn2, aes(x = gender, fill = gender)) + geom_bar()

ggplot(data = cust_churn2, aes(x = gender, fill = OnlineSecurity)) + geom_bar(position = "dodge")
ggplot(data = cust_churn2, aes(x = gender, fill = StreamingMovies)) + geom_bar(position = "dodge")

ggplot(data = cust_churn2, aes(x = Dependents)) + geom_bar()
ggplot(data = cust_churn2, aes(x = Dependents)) + geom_bar(fill = "chocolate")
ggplot(data = cust_churn2, aes(x = Dependents, fill = DeviceProtection)) + geom_bar(position = "dodge")


#geom point()

library(ggplot2)
ggplot(data = cust_churn2, aes(x = tenure, y = TotalCharges)) +geom_point(col = "slateblue3")
ggplot(data = cust_churn2, aes(x = tenure, y = TotalCharges, col = Partner)) + geom_point()

ggplot(data = cust_churn2, aes(x = tenure, y = TotalCharges, col = InternetService)) + geom_point()
ggplot(data = cust_churn2, aes(x = tenure, y = TotalCharges, col = OnlineSecurity)) + geom_point()

ggplot(data = cust_churn2, aes(x = MonthlyCharges, y = TotalCharges)) +geom_point(col = "orange", shape = 1)
ggplot(data = cust_churn2, aes(x = MonthlyCharges, y = TotalCharges)) +geom_point(col = "orange", shape = 2)
ggplot(data = cust_churn2, aes(x = MonthlyCharges, y = TotalCharges, col = InternetService, shape = InternetService)) +geom_point()
ggplot(data = cust_churn2, aes(x = MonthlyCharges, y = TotalCharges, col = InternetService, shape = InternetService, size = InternetService)) +geom_point()

ggplot(data = cust_churn2, aes(x = MonthlyCharges, y = TotalCharges, col = InternetService, shape = InternetService)) +geom_point() + geom_smooth()



# geom_boxplot()

ggplot(data = cust_churn2, aes(x = SeniorCitizen, y  = MonthlyCharges)) + geom_boxplot()
ggplot(data = cust_churn2, aes(x = SeniorCitizen, y  = MonthlyCharges)) + geom_boxplot(fill = "palegreen4", outlier.colour =  "orange", outlier.shape = 3)
ggplot(data = cust_churn2, aes(x = Dependents, y  = MonthlyCharges)) + geom_boxplot()

ggplot(data = cust_churn2, aes(x = Dependents, y  = MonthlyCharges)) + geom_boxplot(fill = "yellowgreen")
ggplot(data = cust_churn2, aes(x = InternetService, y  = MonthlyCharges)) + geom_boxplot(fill = "violetred4")

ggplot(data = cust_churn2, aes(x = PaymentMethod, y  = MonthlyCharges)) + geom_boxplot(fill = "tomato3")

ggplot(data = cust_churn2, aes(x = PaymentMethod, y  = MonthlyCharges, fill = PaperlessBilling)) + geom_boxplot()

ggplot(data = cust_churn2, aes(x = PaymentMethod, y  = MonthlyCharges, fill = Churn)) + geom_boxplot()

ggplot(data = cust_churn2, aes(x = PaymentMethod, y  = MonthlyCharges, fill = TechSupport)) + geom_boxplot()

ggplot(data = cust_churn2, aes(x = PaymentMethod, y  = MonthlyCharges, fill = InternetService)) + geom_boxplot()


# fact_grid()

ggplot(data = cust_churn2, aes(x = tenure, fill = InternetService)) + geom_histogram() 

ggplot(data = cust_churn2, aes(x = tenure, fill = InternetService)) + geom_histogram() -> g1

g1 + facet_grid(~InternetService)

ggplot(data = cust_churn2, aes(x = PaymentMethod, fill = Contract)) + geom_bar()-> g1
g1 + facet_grid(~Contract)

ggplot(data = cust_churn2, aes(x = tenure, y = TotalCharges, col = Contract)) + geom_point() + geom_smooth() -> g1

g1 + facet_grid(~Contract)

ggplot(data = cust_churn2, aes(x = PaymentMethod, y = MonthlyCharges, fill = InternetService)) + geom_boxplot() -> g1

g1 + facet_grid(~InternetService)





#theme layer
#theme()

ggplot(data = cust_churn2, aes(x = tenure)) + geom_histogram(bins = 30, fill = "tomato3", col = "mediumaquamarine")

ggplot(data = cust_churn2, aes(x = tenure)) + geom_histogram(bins = 30, fill = "tomato3", col = "mediumaquamarine") -> g1

g1 + labs(title = "Distribution of tenute") -> g1

g1 + theme(panel.background = element_rect(fill = "olivedrab3")) -> g1

g1 + theme(plot.background = element_rect(fill = "palegreen4")) -> g1

g1 + theme(plot.title = element_text(hjust = 0.5), face = "bold")


# Statistics and probability are the main skills for any machine learning
# sampling technique
# census and survey
# mean miu  variance sigma square standard deviation is sigma
# sampling technique: 
      #simple Random sampling with replacement (equal chance of being selected)
      # simple random sampling without replacement
      # stratified sampling 
      # cluster sampling
# categories od statistics:
      # descriptive statistics
      # inferential statistics
      # 
# types of data:
      # numerical and categorical 
      # numerical : discrete or continuous  (finite of infinite)
      # categorical : ordinal or nominal 
# describing the date to the statstics: central tendency or measure of spread
      # central tendency: mean median or more
      # measure of spread: range quartules, variance and standard deviation
      # mean, trimmed mean, weighted mean, 

# median: middle number on a sorted list of the data
# mode; most frequently occuring data point
#range
# quartile
#variance how far away the values are from the mean
# standard deviation



mean(cust_churn2$MonthlyCharges)
median(cust_churn2$MonthlyCharges)
range(cust_churn2$MonthlyCharges)
IQR(cust_churn2$MonthlyCharges)
var(cust_churn2$MonthlyCharges)
sd(cust_churn2$MonthlyCharges)




# probability: prob vs statistics  

# what is a chance of the event happening. 
# probability vs statistics: predicting and analysing
# probability rules: P(A or B )  = P(A ) + P(B)  mutually exclusive
# probability:  frequent and conditional probability
# probability distribution


#correlation and covariance: 
# Correlation coefficient  (-1 to 1) positive and negative and zero 
# covariance: positive and negative covariance
# 


# hypothesis testing: significance testing 
# null and alternative hypotheses:
# step 1: state the hypotheses, formulate an analysis plan, analyse the sample data
  # interpret the results

# Decision error 
# Type I and Type II error lower the alplha number
# beta symbol for type II error   pwoer = 1 - beta
# z-value measure of standard deviation    =(X - miu) / sigma
# z<0 element less than the mean
# z > 0 element greater than mean
# Z = 0 equa;l to mean
#Z = 1 element that is 1 standard deviation
# p values are probabilities 


# p<0.05 reject null
# p > 0.05 accept null


# types of hypothesis testing:
# degres of free dom = (row-1) (col - 1)

# standardisaion and normalising
# standardisation is z-score normalisation

# normalisation : min-max scaling
# 


# machine learning
# supervised machine learning: category or regression
# un-supervised learning only input data no output values
# 

# # linear regression:
# line of best fit   residual value,   RSS(residual sum of squares)
# coefficient of X influences the relationship bw dep and indep variables


# y = b0 + b1x


# simple linar regression:
# 
# tran and test sets
#  model belding
#  predicting
#  accuracy

# train and test data
# 

library(caTools)
sample.split(cust_churn2$MonthlyCharges, SplitRatio = 0.65) -> split_tag
subset(cust_churn2, split_tag == T) -> train
subset(cust_churn2, split_tag == F) -> test
nrow(train)
nrow(test)

lm(MonthlyCharges~tenure, data = train) -> mod1
predict(mod1, newdata = test)-> res1
cbind(Actual = test$MonthlyCharges, Predicted = res1) -> final_data_1

View(final_data_1)
as.data.frame(final_data_1)  -> final_data_1
class(final_data_1)

final_data_1$Actual - final_data_1$Predicted -> error
cbind(final_data_1, error) -> final_data_1
View(final_data_1)


rmse_mod1 = sqrt(mean((final_data_1$error)^2))

lm(MonthlyCharges~InternetService, data = train) -> mod2
predict(mod2, newdata = test) -> res2
cbind(Actual = test$MonthlyCharges, Predicted = res2) -> final_data_2
as.data.frame(final_data_2) -> final_data_2
error = final_data_2$Actual - final_data_2$Predicted
cbind(final_data_2, error) -> final_data_2
View(final_data_2)

rmse_mod2 =    sqrt(mean((final_data_2$error)^2))
rmse_mod2
rmse_mod1

lm(MonthlyCharges~TechSupport, data = train) -> mod3
predict(mod3, newdata = test) -> res3
cbind(Actual = test$MonthlyCharges, Predicted = res3) -> final_data_3
as.data.frame(final_data_3) -> final_data_3
error = final_data_3$Actual - final_data_3$Predicted
cbind(final_data_3, error) -> final_data_3

View(final_data_3)

rmse_mod3 =   sqrt(mean((final_data_3$error)^2))
rmse_mod3
rmse_mod2
rmse_mod1


# assmptions in linear regression
# linearity (scatter plot of the data) positve or negative
# equal error variance 
# normality of error (qqnorm and qq9 functions)
# 


ggplot(data = cust_churn2, aes(x= tenure, y = TotalCharges)) + geom_point() + geom_smooth(method = "lm")


lm(TotalCharges~tenure, data = cust_churn2) -> mod4
predict(mod4, data = cust_churn2) -> res1
cbind(Actual = cust_churn2$TotalCharges, Predicted = res1) -> final_data_4
View(final_data_4)
as.data.frame(final_data_4) ->final_data_4
error = final_data_4$Actual - final_data_4$Predicted 
cbind(final_data_4, error) -> final_data_4

ggplot(data=final_data_4, aes(x = Predicted, y = error)) + geom_point()
# there is a pattern so cant build the lm on this

qqnorm(final_data_4$error)
qqline(final_data_4)
# this assumption also fails so cant build the lm 


# multiple linear regression

# library(caTools)
# sample.split(cust_churn2$MonthlyCharges, SplitRatio = 0.65) -> split_tag
# subset(cust_churn2, split_tag == T) -> train
# subset(cust_churn2, split_tag == F) -> test
# nrow(train)


library(caTools)
sample.split(cust_churn2$tenure, SplitRatio = 0.65) -> split_tag
subset(cust_churn2, split_tag == T) -> train
subset(cust_churn2, split_tag == F) -> test
nrow(train)
nrow(test)


lm(tenure~MonthlyCharges+gender+InternetService+Contract, data = train) -> mod5
predict(mod5, newdata = test) -> res4
cbind(Actual = test$tenure, Predicted = res4) -> final_data_5
as.data.frame(final_data_5) -> final_data_5
View(final_data_5)

error = final_data_5$Actual - final_data_5$Predicted
cbind(final_data_5, error) -> final_data_5

rmse_mod5 =   sqrt(mean((final_data_5$error)^2))
rmse_mod5


lm(tenure~Partner+PhoneService+TotalCharges+PaymentMethod, data = train) -> mod6
predict(mod6, newdata = test) -> res6
cbind(Actual = test$tenure, Predicted = res6) -> final_data_6
as.data.frame(final_data_6) -> final_data_6

error = final_data_6$Actual - final_data_6$Predicted
cbind(final_data_6, error) -> final_data_6

rmse_mod6 =     sqrt(mean((final_data_6$error)^2, na.rm = T))
rmse_mod6


# Logistic regression  we have a S curve
#dependent variable is categorical


# simple logistic regression in R


#model  summary and predicting

glm(Churn~MonthlyCharges, data = cust_churn2, family = "binomial") -> glm_mod1
summary(glm_mod1)

#Acoic information criteria


predict(glm_mod1, data.frame(MonthlyCharges = 10:50), type = "response")


glm(Churn~tenure, data = cust_churn2, family = "binomial") -> glm_mod2
summary(glm_mod2)
summary(glm_mod1)

predict(glm_mod2, data.frame(tenure = 50), type = "response")
predict(glm_mod2, data.frame(tenure = 10:50), type = "response")



# multiple logistic regression
library(caTools)
sample.split(cust_churn2$Churn, SplitRatio =0.65) -> split_tag
subset(cust_churn2, split_tag == T) -> train
subset(cust_churn2, split_tag == F) -> test

glm(Churn~gender + Partner + InternetService+MonthlyCharges, data = train, family = "binomial") -> glm_mod3
summary(glm_mod3)

predict(glm_mod3, newdata = test, type = "response") -> res_8
range(res_8)


table(test$Churn,res_8>0.4) -> acc1

# FALSE TRUE
# No   1553  258
# Yes   332  322


(1553+322)/(1553+258+332+322)

glm(Churn~PaymentMethod + TechSupport+tenure + PaperlessBilling, data = train, family = "binomial")-> glm_mod4

predict(glm_mod4, newdata = test, type = "response") -> res_9
range(res_9)

table(test$Churn, res_9>0.4)

# FALSE TRUE
# No   1537  274
# Yes   275  379

acc2 = (1537+376)/(1537+274+275+379)
acc2



glm(Churn~Contract + Dependents + MultipleLines + DeviceProtection, data = train, family = "binomial") -> glm_mod5
predict(glm_mod5, newdata = test, type = "response") -> res_10
range(res_10)
table(test$Churn, res_10>0.4)
# 
# FALSE TRUE
# No   1329  482
# Yes   199  455

acc3 = (1329+455)/(1329+482+199+455)
acc3
acc1 = (1553+322)/(1553+258+332+322)
acc1
acc2

# Confusion matrix

# performace metrics, thersolding, implementing confusion matrix in R

# precision   TP / TP + FP

# recall TP / TP + FN

# thersholding 

library(caTools)
sample.split(cust_churn2$Churn, SplitRatio = 0.65) -> split_tag
subset(cust_churn2, split_tag ==T) -> train
subset(cust_churn2, split_tag == F) -> test
nrow(train)
nrow(test)
nrow(cust_churn2)


glm(Churn~MonthlyCharges, data = train, family = "binomial") ->glm_mod6
predict(glm_mod6, newdata = test, type = "response")-> res_11
range(res_11)

table(test$Churn, res_11>0.3)

# 
# FALSE TRUE
# No   1163  648
# Yes   302  352


acc4 = (1163 + 352) /(1163+648+302+352)
acc4
table(test$Churn, res_11>0.35)
# 
# FALSE TRUE
# No   1489  322
# Yes   487  167
acc4 = (1489 + 167) /(1489+322+487+167)
acc4



#ROC curve receiver operating characteristics
#Thershold evaluation and ROC Cure and Area under curve

library(caTools)
sample.split(cust_churn2$Churn, SplitRatio = 0.65) -> split_tag
subset(cust_churn2, split_tag == T) -> train
subset(cust_churn2, split_tag == F) -> test

glm(Churn~MonthlyCharges, data = train, family = "binomial") -> glm_mod7
predict(glm_mod7, newdata = test, type = "response") -> res_12

View(res_12)

table(test$Churn, res_12>0.1)

library(ROCR)
prediction(res_12, test$Churn) -> pred_log
performance(pred_log, "acc") -> acc
plot(acc)

table(test$Churn, res_12>0.41)
# 
# FALSE TRUE
# No   1760   51
# Yes   646    8

acc5 = (1760+8) / (1760+51+646+8)
acc5

TP = (8)/(8+646)
TP
FP = (51)/(51+1760)
FP

performance(pred_log, "tpr", "fpr") -> roc_curve
plot(roc_curve, colorize = T)

table(test$Churn, res_12>0.28)
# FALSE TRUE
# No   1044  767
# Yes   234  420

acc6 = (1044+420)/(1044+767+234+420)
acc6

TP = (420) / (234+420)
TP

FP = (767) /(1044+767)
FP


performance(pred_log, "auc") -> auc
auc

# decision trees

#root node, leaf node and branch node

# types of classification or regression tree


library(caTools)
sample.split(cust_churn2$Churn, SplitRatio = 0.65) -> split_tag
subset(cust_churn2, split_tag == T) -> train
subset(cust_churn2, split_tag ==F) ->test

library(tree)
tree(Churn~tenure, data = train) -> mod_tree1
plot(tree_mod1)
text(tree_mod1)


tree(Churn~tenure + MonthlyCharges, data = train) -> mod_tree2
plot(mod_tree2)
text(mod_tree2)

tree(Churn~MonthlyCharges + tenure + Contract + TechSupport, data = train ) -> mod_tree3
plot(mod_tree3)
text(mod_tree3)
levels(test$Contract)


predict(mod_tree1, newdata = test, type = "class") -> res_15
View(res_15)
table(test$Churn, res_15)

# No  Yes
# No  1598  213
# Yes  396  258
acc8 = (1598+258)/ (1598+213+396+258)
acc8


predict(mod_tree2, newdata = test, type = "class") -> res_16

View(res_16)
table(test$Churn, res_16)
# No  Yes
# No  1662  149
# Yes  371  283
acc9 = (1662+283)/(1662+149+371+283)
acc9


predict(mod_tree3, newdata = test, type = "class") -> res_17
table(test$Churn,res_17)
acc10 = (1737+194)/(1737+74+460+194)
acc10
acc9
acc8

# things to keep in mind while classification tree

# how to choose the right split?
#some positive change the split should bring
# classification error rate
# information gain

# when to declare a node to be terminal?
# GIni index and prune our trees


#shortcomings of classification error rate
#GIni index 
#pruning  when do we stop splitting the trees:
# pre-pruning and post-pruning
# threshold can be from Information Gain 
# threshold can be from Minimum Instances per Node
# threshold for Maximum Depth


































