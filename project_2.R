file = "C:/Users/h8man/Downloads/Project5/bank-full_train.csv"
bank_train = read.csv(file, stringsAsFactors = F)

file = "C:/users/h8man/Downloads/Project5/bank-full_test.csv"
bank_test = read.csv(file,stringsAsFactors = F)

bank_test$data = "test"
bank_train$data = "train"
bank_test$y = NA
bank = rbind(bank_train,bank_test)
lapply(bank, function(x) sum(is.na(x)))

library(dplyr)
library(tidyr)
glimpse(bank)
names(bank)[sapply(bank, function(x) is.character(x))]
table(bank$job)
table(bank$housing)
bank = bank%>%
  mutate(default = as.numeric(ifelse(default == "yes",1,0)),
           loan  = as.numeric(ifelse(loan == "yes",1,0)),
             y        = as.numeric(ifelse(y == "yes",1,0)),
         housing = as.numeric(ifelse(housing == "yes",1,0)))
table(bank$marital)
table(bank$education)
table(bank$loan)
table(bank$contact)
table(bank$poutcome)
table(bank$month)
table(bank$housing)
table(bank$default)
CreateDummies = function(data,var,freq_cutoff=0){
  t = table(data[,var])
  t = t[t>freq_cutoff]
  t = sort(t)
  categories = names(t)[-1]
  for (cat in categories){
    name = paste(var,cat,sep = "-")
    name = gsub(" ","",name)
    name = gsub("-","_",name)
    name = gsub("\\?","Q",name)
    name = gsub("<","LT_",name)
    name = gsub("\\+",",",name)
    data[,name]= as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
for (col in c("job","education","marital","contact","month")) {
  bank = CreateDummies(bank,col,100)
  
}
for (col in "poutcome") {
  bank = CreateDummies(bank,col,100)
  
}

names(bank)[sapply(bank, function(x) is.integer(x))]
bank = bank%>%
  select(-ID)
glimpse(bank)
bank = bank%>%
  mutate(age = as.numeric(age),
         balance = as.numeric(balance),
         day = as.numeric(day),
         duration = as.numeric(duration),
         campaign = as.numeric(campaign),
         pdays = as.numeric(pdays),
         previous = as.numeric(previous))
glimpse(bank)

lapply(bank, function(x) sum(is.na(x)))
names(bank)[sapply(bank, function(x) is.character(x))]
bank_train = bank%>%
  filter(data == "train")%>%
  select(-data)
bank_test = bank%>%
  filter(data == "test")%>%
  select(-y,-data)
mycorr = cor(bank_train)
library(eigenmodel)
eigen(cor(bank_train))$values
library(corrplot)
corrplot(cor(bank_train),method = "number")
glimpse(bank_train)

set.seed(2)
s = sample(1:nrow(bank_train),0.7*nrow(bank_train))
bank_train1 = bank_train[s,]
bank_train2 = bank_train[-s,]
library(car)





fit = lm(y ~. , data = bank_train1)


vif(fit)
# drop month_may
fit = lm(y ~. -month_may , data = bank_train1)
sort(vif(fit),decreasing = T)[1:3]
# job_blue_collar
fit = lm(y ~. -month_may-job_blue_collar , data = bank_train1)
sort(vif(fit),decreasing = T)[1:3]
# drop poutcome_unknown
fit = lm(y ~. -month_may-job_blue_collar-poutcome_unknown , data = bank_train1)
sort(vif(fit),decreasing = T)[1:3]
# now vif values for all the variables are in control.
log_fit = glm(y ~. -month_may-job_blue_collar-poutcome_unknown ,data = bank_train1,family = "binomial")
log_fit = step(log_fit)
formula(log_fit)
log_fit = glm(y ~ balance + housing + loan + day + duration + campaign + pdays + 
                previous + job_student + job_housemaid + job_retired + job_admin.  
             + education_primary + education_tertiary + 
                marital_married + contact_unknown  + month_mar + 
                month_sep + month_oct + month_jan + month_feb + month_apr + 
                month_nov + month_jun + month_aug + month_jul + poutcome_other + 
                poutcome_failure,data = bank_train1,family = "binomial")
summary(log_fit)
library(pROC)
citation("pROC")
value.IR = predict(log_fit,newdata = bank_train2,type = "response")
auc_score = auc(roc(bank_train2$y,value.IR))
auc_score
# now build our model
for_vif = lm(y ~.-month_may-job_blue_collar- poutcome_unknown  , data = bank_train)
sort(vif(for_vif),decreasing = T)[1:3]
log_fit_final = glm(y ~. -month_may-job_blue_collar- poutcome_unknown  , data = bank_train,family = "binomial")
log_fit_final = step(log_fit_final)
formula(log_fit_final)
log_fit_final = glm(y ~ balance + housing + loan + day + duration + campaign + pdays + 
                      previous + job_student + job_housemaid + job_retired + job_admin. + 
                      job_technician + job_management + education_primary + education_tertiary  
                      + marital_married + contact_unknown + contact_cellular + 
                      month_mar + month_sep + month_oct + month_jan + month_feb + 
                      month_apr + month_nov + month_jun + month_aug + month_jul + 
                      poutcome_other + poutcome_failure,data = bank_train,family = "binomial")
summary(log_fit_final)
# we are done with this now.
test.prob.score= predict(log_fit_final,newdata = bank_test,type='response')


train.score = predict(log_fit_final,newdata = bank_train,type = "response")
real = bank_train$y
cutoffs = seq(0.001,0.999,0.001)
cutoff_data = data.frame(cutoff = 99,Sn = 99,Sp = 99,KS = 99,F5 = 99,F1 = 99, Custom = 99)


for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F1=(1.01*precision*recall)/((.01*precision)+recall)
  
  Custom=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F1,Custom))
}

cutoff_data=cutoff_data[-1,]

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:Custom)

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

test.predicted=as.numeric(test.prob.score>my_cutoff)
write.csv(test.predicted,"proper_submission_file_banking_name.csv",row.names = F)

