setwd("C:/Users/Sanika/DMMLProject")
getwd()

install.packages("caTools")
install.packages('PRROC')
install.packages("corrplot")
install.packages('Amelia')

library(dplyr)
library(Amelia)
library(ggplot2)
library(dplyr)
library(caTools)
library(yardstick)
library(ggplot2)
library(caret)
library(pROC)

###########################Load Dataset##################################
df <- read.csv("Income_Classification.csv")
head(df)
str(df)

############################Cleaning#####################################

#Employement
unemp <- function(job_role){
    job_role <- as.character(job_role)
    if (job_role=="Never-worked" | job_role=="Without-pay"){
        return("Unemployed")
    }else{
        return(job_role)
    }
}
df$workclass <- sapply(df$workclass,unemp)
table(df$workclass)

#Job Role
grp_emp <- function(job_role){
    if (job_role=="Local-gov" | job_role=="State-gov"){
        return("SL-gov")
    }else if (job_role=="Self-emp-inc" | job_role=="Self-emp-not-inc"){
        return("self-emp")
    }else{
        return(job_role)
    }
}
df$workclass <- sapply(df$workclass,grp_emp)
table(df$workclass)


#Maratial Status
grp_marital_sts <- function(mart_sts){
    mart_sts <- as.character(mart_sts)
    
    # Not-Married
    if (mart_sts=="Separated" | mart_sts=="Divorced" | mart_sts=="Widowed"){
        return("Not-Married")
    
    # Never-Married   
    }else if(mart_sts=="Never-married"){
        return(mart_sts)
    
     #Married
    }else{
        return("Married")
    }
}
df$marital.status <- sapply(df$marital.status,grp_marital_sts)
table(df$marital.status)


#Naitive Country
Asia <- c("China","Hong","India","Iran","Cambodia","Japan", "Laos","Philippines" ,"Vietnam" ,"Taiwan", "Thailand")

N.A <- c("Canada","United-States","Puerto-Rico")

Europe <- c("England","France","Germany" ,"Greece","Holand-Netherlands","Hungary","Ireland","Italy","Poland","Portugal","Scotland"
            ,"Yugoslavia")

S.A <- c("Columbia","Cuba","Dominican-Republic","Ecuador","El-Salvador","Guatemala","Haiti","Honduras","Mexico","Nicaragua"
                   ,"Outlying-US","Peru","Jamaica","Trinadad&Tobago")
Remaining_count <- c("South")

grp_cntry <- function(cntry){
    if (cntry %in% Asia){
        return("Asia")
    }else if (cntry %in% N.A){
        return("N.A")
    }else if (cntry %in% Europe){
        return("Europe")
    }else if (cntry %in% S.A){
        return("S.A")
    }else{
        return("Remaining_count")      
    }
}
df$native.country <- sapply(df$native.country,grp_cntry)
table(df$native.country)


#Converting into factor datatype
df$workclass <- as.factor(df$workclass)
df$native.country <- as.factor(df$native.country)
df$marital.status <- as.factor(df$marital.status)


################### Missing Data #############################

#Converting ? to NA
df[df == "?"] <- NA

df$workclass <- as.factor(df$workclass)
df$native.country <- as.factor(df$native.country)
df$marital.status <- as.factor(df$marital.status)
df$occupation <- as.factor(df$occupation)

#Before removing missing values
missmap(df)

#remove NA
df <- na.omit(df)
missmap(df)
str(df)

df$class <- ifelse( df$income == "<=50K", 1, 0)

########################### Visualization ############################

# Creating a histogram of ages
ggplot(df,aes(hours.per.week)) + geom_histogram(aes(color="red"),bins = 30) + theme_bw()

# Ploting a histogram of hours worked per week
ggplot(df,aes(age)) + geom_histogram(aes(fill=income),color="white",binwidth=1) + theme_bw()

# Creating a barplot of country
ggplot(df,aes(native.country)) + geom_bar(aes(fill=income))+theme_bw()


#########################Split Train Test##############################

set.seed(101)
df<- df%>%select(-income)
str(df)
sample <- sample.split(df$class, SplitRatio = 0.70) 
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)


########################Logistic model###################################
model <- glm(class ~.,family = binomial(logit), data = train)
result <- predict(model, test, type="response")
result [result >=0.5] = 1
result [result <0.5] = 0
table <- table(result,test$class)

#confusion matrix
cm <- confusionMatrix(table )
cm
cm <- confusionMatrix(factor(test$class), factor(result), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
        geom_tile() + geom_text(aes(label=Freq)) +
        scale_fill_gradient(low="white", high="#009194") +
        labs(x = "Reference",y = "Prediction") +
        scale_x_discrete(labels=c("Class_1","Class_0")) +
        scale_y_discrete(labels=c("Class_0","Class_1"))

#AucRoc
plot(roc(test$class, result, direction="<"),
     col="yellow", lwd=3, main="AUC-ROC Curve")


########################## Naive Bayes ###################################
library(e1071)
nb <- naiveBayes(class~., data=train)
result <- predict(nb , test)
result [result >=0.5] = 1
result [result <0.5] = 0
table <- table(result,test$class)

#confusion matrix
cm <- confusionMatrix(table )
cm
cm <- confusionMatrix(factor(test$class), factor(result), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
        geom_tile() + geom_text(aes(label=Freq)) +
        scale_fill_gradient(low="white", high="#009194") +
        labs(x = "Reference",y = "Prediction") +
        scale_x_discrete(labels=c("Class_1","Class_0")) +
        scale_y_discrete(labels=c("Class_0","Class_1"))

#AucRoc
library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = result, weights.class0=test$class,curve=TRUE)
plot(PRROC_obj,col="yellow", lwd=3, main="AUC-ROC Curve")

