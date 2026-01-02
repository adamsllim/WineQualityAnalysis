########### GROUP PROJECT ###########
# Load dataset

library(readr)
library(memisc)
library(Metrics)
#read in the data
wine <- read_csv("multivariate stats/winequality-red.csv")
wine$quality <- factor(wine$quality, ordered = T)

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))
#Scale the data 
wine$fixed.acidity<-scale(wine$fixed.acidity)
wine$volatile.acidity<-scale(wine$volatile.acidity)
wine$citric.acid<-scale(wine$citric.acid)
wine$residual.sugar<-scale(wine$residual.sugar)
wine$free.sulfur.dioxide<-scale(wine$free.sulfur.dioxide)
wine$total.sulfur.dioxide<-scale(wine$total.sulfur.dioxide)
wine$pH<-scale(wine$pH)
wine$chlorides<-scale(wine$chlorides)
wine$density<-scale(wine$density)
wine$alcohol<-scale(wine$alcohol)


#make the linear model 
set.seed(233)
#split the data 
sample <- sample(c(TRUE, FALSE), nrow(wine), replace=TRUE, prob=c(0.7,0.3))
training_data  <- wine[sample, ]
test_data   <- wine[!sample, ]
#make the models and add some features each time 
m1 <- lm(as.numeric(quality) ~ alcohol, data = training_data)
m2 <- update(m1, ~ . + sulphates)
m3 <- update(m2, ~ . + volatile.acidity)
m4 <- update(m3, ~ . + citric.acid)
m5 <- update(m4, ~ . + fixed.acidity)
m6 <- update(m4, ~ . + chlorides)
m7 <- update(m4, ~ . + free.sulfur.dioxide)
m8 <- update(m4, ~ . + total.sulfur.dioxide)
m9 <- update(m4, ~ . + density)
#show R^2 results in a big table 
mtable(m1,m2,m3,m4,m5,m6, m7, m8, m9)

#run the predictions over the test set
predm5<- predict(m1, test_data)# change the model number every time rerun 
predm5<- predict(s_model, test_data)

ym5<- as.numeric(test_data$quality)

SS.total      <- sum((ym5 - mean(ym5))^2)
SS.residual   <- sum((ym5 - predm5)^2)
SS.regression <- sum((predm5 - mean(ym5))^2)
SS.total - (SS.regression+SS.residual)
test.rsq <- 1 - SS.residual/SS.total  
test.rsq #r squared value of the best model for the test set 

# fraction of variability explained by the model
SS.regression/SS.total 

#rmse: 
rmse(ym5, predm5) #this is the root mean squared error 


#-----for the entire dataset we can run step lrm like the homework ----------------

model<- lm(formula = as.numeric(quality) ~ alcohol, data = wine)
summary(model)


#trial 2
lrm <- lm(as.numeric(quality) ~ fixed.acidity+volatile.acidity+citric.acid
         +residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide
         +density+pH+sulphates+alcohol, data=wine)

red_model <- lm(as.numeric(quality) ~ 1, data = wine)
anova <- anova(red_model, lrm)
print(anova)

# calculate aic------------------------
aci <- AIC(lrm) # full model
s_model <- step(lrm) #get the best one from the full thing 
aci_2 <- AIC(s_model) # selected model

# print
print(c("This is the AIC for the full model:", aci))
print(c("This is the AIC for the selected model:", aci_2))


#-------------------some plots--------------------------------------------

library(ggplot2) # Data visualization
library(memisc)
library(corrgram) 

#look at the correlation 
corrgram(wine, lower.panel=panel.shade, upper.panel=panel.ellipse)

# look at the linear relatinoships 
plot(wine$pH, wine$alcohol)
abline(lm(wine$alcohol ~ wine$pH), col='green')

plot(wine$sulphates, wine$alcohol)
abline(lm(wine$alcohol ~ wine$sulphates), col='green')
