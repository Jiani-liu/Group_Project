library(tidyverse)
library(car)
library(caret)
library(ModelMetrics)

data <- read.csv("GroupProject.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Race_M=as.factor(Race_M),
         Education_M=as.factor(Education_M),
         Education_F=as.factor(Education_F),
         Income=as.factor(Income),
         Marital=as.factor(Marital),
         Smoke=as.factor(Smoke),
         Quit_Time=as.factor(Quit_Time),
         Number_Smoke=as.factor(Number_Smoke))

# fit a big model
bigmdl <- lm(Birth_Weight ~ Gestation + Parity + Age_M + Height_M + Weight_M + 
               Income + Race_M + Education_M + Education_F + Marital + Smoke ,
               data = data)

# check for colinearity
vif(bigmdl)

# best models using AIC backwards elimination
smallmdl <- step(bigmdl)
summary(smallmdl)
Anova(smallmdl)

# best model using p-values backward elimination
Anova(bigmdl)

bigmdl_1 <- update(bigmdl, .~. -Marital)
Anova(bigmdl_1)

bigmdl_2 <- update(bigmdl_1, .~. -Age_M)
Anova(bigmdl_2)

bigmdl_3 <- update(bigmdl_2, .~. -Income)
Anova(bigmdl_3)

bigmdl_4 <- update(bigmdl_3, .~. -Education_M)
Anova(bigmdl_4)

bigmdl_5 <- update(bigmdl_4, .~. -Education_F)
Anova(bigmdl_5)

summary(bigmdl_5)

#best model using BIC backward elimination
smallmdl_2 <- stepAIC(bigmdl, k = log(nrow(data)))
summary(smallmdl_2)
Anova(smallmdl_2)


# first order interaction models
newmdl <- lm(Birth_Weight ~ Gestation * Weight_M,
             data = data)
summary(newmdl)
Anova(newmdl)

newmdl_1 <- lm(Birth_Weight ~  Gestation * Education_M + Parity + Weight_M + Race_M,
             data = data)
summary(newmdl_1)
Anova(newmdl_1)

newmdl_2 <- lm(Birth_Weight ~  Smoke * Gestation + Parity + Height_M + Race_M + Weight_M ,
               data = data)
summary(newmdl_2)
Anova(newmdl_2)



# compare my two best models using validation
# useful link: http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

#two best models are : newmdl_2 and smallmdl

#split the data into training and validation set
set.seed(1234)  # for reproducibility
training_samples <- data$Birth_Weight %>%
  createDataPartition(p = 0.8, list = FALSE)
train_data  <- data[training_samples, ]
test_data <- data[-training_samples, ]

# Build the models based on training data
bestmdl_1 <- lm(Birth_Weight ~ Gestation + Parity + Height_M + Weight_M
                 + Race_M + Smoke, data = train_data)
bestmdl_2 <- lm(Birth_Weight ~ Smoke * Gestation + Parity + Height_M + Weight_M 
                + Race_M, data = train_data)

# Make predictions and compute the MSE
predictions_1 <- bestmdl_1 %>% predict(test_data)
predictions_2 <- bestmdl_2 %>% predict(test_data)
MSE_1 <- mse(predictions_1, test_data$Birth_Weight)
MSE_1
MSE_2 <- mse(predictions_2, test_data$Birth_Weight)
MSE_2

# newmdl_2 has smaller MSE so we prefer this over smallmdl 

# using 5 - fold cross - validation
set.seed(1234) # for reproducibility
train_control <- trainControl(method = "cv", number = 5)
# Train the models
model_1 <- train(Birth_Weight ~ Gestation + Parity + Height_M + Weight_M +
               Race_M + Smoke, data = data, method = "lm",
               trControl = train_control)
model_2 <- train(Birth_Weight ~ Smoke * Gestation + Parity + Height_M + Weight_M 
                 + Race_M, data = data, method = "lm",
                 trControl = train_control)
# Summarize the results
print(model_1)
print(model_2)

# check assumptions

# normality assumption:
# histogram of residuals
hist(smallmdl$residuals) # for model 1 
hist(newmdl_2$residuals) # for model 2 

# qq norm plot of residuals
qqnorm(smallmdl$residuals) # for model 1
qqnorm(newmdl_2$residuals) # for model 2

# Shapiro-Wilks test
shapiro.test(residuals(smallmdl)) #model 1
shapiro.test(residuals(newmdl_2)) #model 2 

# constant error variance assumption
# plot of residuals and fitted values
# these plots also check the linearity in the model for signal
plot(x = fitted(smallmdl), y = residuals(smallmdl)) # model 1
plot(x = fitted(newmdl_2), y = residuals(newmdl_2)) # model 2

# Breusch - Pagan test
ncvTest(smallmdl) # model 1
ncvTest(newmdl_2) # model 2

# check independence
# plot of residuals in observation order
plot (seq(1,100), residuals(smallmdl)[1:100], type="l", xlab="Sequence") # model 1
points(seq(1,100), residuals(smallmdl)[1:100])
abline (h=0,lty=2)

plot (seq(1,100), residuals(newmdl_2)[1:100], type="l", xlab="Sequence") # model 2
points(seq(1,100), residuals(newmdl_2)[1:100])
abline (h=0,lty=2)

# Formal test - Durbin-Watson test
durbinWatsonTest(smallmdl) # model 1
durbinWatsonTest(newmdl_2) # model 2


