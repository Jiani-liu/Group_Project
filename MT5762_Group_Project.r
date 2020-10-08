library(tidyverse)
library(mice)
library(VIM)

data <- read.table("/Users/jianiliu/Desktop/MT5762/babies23.data",header = T) 
head(data)

summary(data)

dataB <- read.table("/Users/jianiliu/Desktop/MT5762/babies23.data",header = T) %>% 
select (-id, -pluralty, -outcome, -sex) %>% # remove one level factor
mutate (date = as.Date(date, origin = '1961-01-01') - 1096)  %>% # convert date number to data object
set_names ("Date", "Gestation", "Birth_Weight", "Parity", 
           "Race_M", "Age_M", "Education_M", "Height_M", "Weight_M",
          "Race_F", "Age_F", "Education_F", "Height_F", "Weight_F",
           "Marital", "Income", "Smoke", "Quit_Time", "Number_Smoke") %>% # rename the variables to make it easy to understand
select ("Birth_Weight", "Date", "Gestation", "Parity", "Age_M", "Height_M", "Weight_M", 
        "Age_F","Height_F", "Weight_F",  "Income", "Race_M", "Education_M","Race_F", "Education_F",
       "Marital","Smoke", "Quit_Time", "Number_Smoke") %>%
mutate(Race_M = recode (Race_M, '0' = 5L, '1' = 5L, '2' = 5L, '3' = 5L, '4' = 5L, '10' = 99L)) %>%
mutate(Race_F = recode (Race_M, '0' = 5L, '1' = 5L, '2' = 5L, '3' = 5L, '4' = 5L, '10' = 99L)) %>% # combine 1-5 to 5
mutate(Education_M = recode(Education_M, '7' = 6L)) %>%
mutate(Education_F = recode(Education_F, '7' = 6L)) %>% # combine 6-7 to 6
mutate_at(vars(Birth_Weight, Parity, Weight_M, Weight_F,Gestation), na_if, 999) %>% 
mutate_at(vars(Age_M, Height_M, Age_F,Race_M, Race_F, Height_F, Income,Quit_Time), na_if, 99) %>%
mutate_at(vars(Income, Quit_Time, Number_Smoke), na_if, 98) %>%
mutate_at(vars(Education_M, Education_F, Smoke), na_if, 9) %>%
mutate_at(vars(Marital), na_if, 0) %>%  #replace unknown variables to NA
mutate(Race_M = as.factor(Race_M), Race_F = as.factor(Race_F),
       Education_M = as.factor(Education_M), Education_F = as.factor(Education_F),
      Income = as.factor(Income), Marital = as.factor(Marital),
      Smoke = as.factor(Smoke), Quit_Time = as.factor(Quit_Time),
       Number_Smoke = as.factor(Number_Smoke)) # 

for(i in c(3:10)){
    
    boxplot(dataB[,i],data=dataB) 
    print(names(dataB[i])) 
}

for(i in names(dataB[11:19])){ 
    print(ggplot(data = dataB, aes_string(x= i)) + geom_bar())
}

sum(complete.cases(dataB))         # number of complete cases
sum(!complete.cases(dataB))        # number of missing cases
mean(!complete.cases(dataB))       # proportion of missingdata
#data[!complete.cases(data),]

require(VIM)
aggr_plot <- aggr(dataB, col = c('navyblue','red'),
                  numbers = TRUE,
                  sortVars = TRUE, 
                  labels=names(dataB),
                  cex.axis = 0.7,
                  gap = 2,
                  ylabs = c("Missing data histogram","Missing value pattern"))

cor(dataB[,3:10],use = "na.or.complete")

# Select out variables that could cause problems in the imputation process
dataB <- dataB %>% 
  dplyr::select(-Height_F,-Weight_F,-Race_F,-Age_F)

x <- as.data.frame(abs(is.na(dataB)))
y <- x[which(apply(x,2,sum)>0)]
cor(y)

# We run the mice code with 5 iterations
imp <- mice(dataB, m=5, seed = 42, print = FALSE)

xyplot(imp,Gestation ~ Age_M+ Height_M+ Weight_M|.imp,pch=18,cex=1)
xyplot(imp,Gestation ~ Race_M + Income + Education_M|.imp,pch=18,cex=1)

densityplot(imp)

stripplot(imp, pch = 20, cex = 1.2)

fitols <- lm(Birth_Weight ~ Date + Gestation + Parity + Age_M +
             Height_M + Weight_M +Income + Race_M + Education_F +
             Marital + Smoke + Quit_Time + Number_Smoke, dataB)

summary(fitols)

options(scipen=200)

modelFit1 <- with(imp, lm(Birth_Weight ~ .,data=dataB[,2:15]))
summary(pool(modelFit1))

imp$imp$Education_M

# generate a new dataset without missing data
data_complete <- complete(imp)

sum(complete.cases(data_complete))         # number of complete cases
sum(!complete.cases(data_complete))        # number of missing cases
mean(!complete.cases(data_complete))

fit1 <- lm(Birth_Weight ~ Date + Gestation + Parity + Age_M +
             Height_M + Weight_M +Income + Race_M + Education_F +
             Marital + Smoke + Quit_Time + Number_Smoke, data_complete)

summary(fit1)

library(MASS)
stepAIC(fit1, direction = "both")

fit2 <- lm(formula = Birth_Weight ~ Gestation + Parity + Height_M + Weight_M + 
    Race_M + Quit_Time + Number_Smoke + Race_M*Weight_M, data = data_complete)
summary(fit2)

library(caret)

# Build the model
model2 <- train( Birth_Weight ~ Gestation + Parity + Height_M + Weight_M + 
    Race_M + Quit_Time + Number_Smoke,data_complete, 
               method = "lm",
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        verboseIter = TRUE))


# Summarize the model
summary(model2)

# Build the model
model3 <- train( Birth_Weight ~ Gestation + Parity + Height_M + Weight_M + 
    Race_M + Quit_Time + Number_Smoke + Race_M*Weight_M,data_complete, 
               method = "lm",
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        verboseIter = TRUE))


# Summarize the model
summary(model3)

model <- lm(formula = log(Birth_Weight) ~ Gestation + Parity + Height_M + Weight_M + 
    Race_M + Quit_Time + Number_Smoke + Race_M*Weight_M, data = data_complete)
vif(model)
plot(model,1)

plot(model,3)

plot(model, 2)

plot(model,5)

# Bootstrap 95% CI for regression coefficients
library(boot)
# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
results <- boot(data=data_complete, statistic=bs,
   R=1000, formula=model)


#View results as density histogram and qqplot 
#The data are normally distributed for all variables 
results
plot(results, index=1) # intercept 
plot(results, index=2) # Gestation 
plot(results, index=3) # Parity
plot(results, index=4) # Height_M
plot(results, index=5) # Weight_M
plot(results, index=6) # Race_M
plot(results, index=7) # Quit_Time
plot(results, index=8) # Number_Smoke


# Get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # Gestation 
boot.ci(results, type="bca", index=3) # Parity
boot.ci(results, type="bca", index=4) # Height_M
boot.ci(results, type="bca", index=5) # Weight_M
boot.ci(results, type="bca", index=6) # Race_M
boot.ci(results, type="bca", index=7) # Quit_Time
boot.ci(results, type="bca", index=8) # Number_Smoke


