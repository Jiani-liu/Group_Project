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
smallmdl_2 <- step(bigmdl, k = log(nrow(data)))
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
par(mfrow = c(1,2))
hist_1 <- hist(smallmdl$residuals, main="Histogram of residuals model A",
               xlab = 'Residuals model A', cex.axis = 1.25,
               cex.lab = 1.25, col = 'palevioletred4') # for model 1 
hist_2 <- hist(newmdl_2$residuals, main="Histogram of residuals model B",
               xlab = 'Residuals model B', cex.axis = 1.25,
               cex.lab = 1.25, col = 'turquoise4') # for model 2 

# qq norm plot of residuals
# used bty = "l" from https://stackoverflow.com/questions/22470114/removing-top-and-right-borders-from-boxplot-frame-in-r
qqnorm(smallmdl$residuals, cex.axis = 1.25, main = "Normal Q-Q Plot for model A ",
       cex.lab = 1.25, col = 'palevioletred4', bty = "l",
       ylab = "Sample Residuals") # for model 1
qqline(smallmdl$residuals)
qqnorm(newmdl_2$residuals, cex.axis = 1.25, main = "Normal Q-Q Plot for model B",
       cex.lab = 1.25, col = 'turquoise4', bty = "l",
       ylab = "Sample Residuals") # for model 2
qqline(newmdl_2$residuals)


# Shapiro-Wilks test
shapiro.test(residuals(smallmdl)) #model 1
shapiro.test(residuals(newmdl_2)) #model 2 

# constant error variance assumption
# plot of residuals and fitted values
# these plots also check the linearity in the model for signal

plot(x = fitted(smallmdl), y = residuals(smallmdl), cex.axis = 1.25,
     main = "Comparing residuals for model A 
     with fitted values", xlab = 'Fitted values',
     ylab  = "Residuals", cex.lab = 1.25, col = 'palevioletred4', bty = "l") # model 1
plot(x = fitted(newmdl_2), y = residuals(newmdl_2), cex.axis = 1.25,
     main = "Comparing residuals for model B 
     with fitted values", xlab = 'Fitted values',
     ylab  = "Residuals", cex.lab = 1.25, col = 'turquoise4', bty = "l") # model 2

# Breusch - Pagan test
ncvTest(smallmdl) # model 1
ncvTest(newmdl_2) # model 2

# check independence
# plot of residuals in observation order
plot(seq(1,100), residuals(smallmdl)[1:100], type="l", xlab="Sequence",
      ylab = "Residuals", main = "Check of independance of residuals 
     for Model A", cex.lab = 1.25, 
      cex.axis = 1.25, col = "palevioletred4", lwd = 1.5, bty = "l") # model 1
points(seq(1,100), residuals(smallmdl)[1:100], col = "palevioletred4")
abline (h=0,lty=3, col = "palevioletred4", lwd = 3) # model 1

plot(seq(1,100), residuals(newmdl_2)[1:100], type="l", xlab="Sequence", 
      ylab = "Residuals", main = "Check of independance of residuals 
     for Model B", cex.lab = 1.25, 
      cex.axis = 1.25, col = "turquoise4", lwd = 1.5, bty = "l") # model 2
points(seq(1,100), residuals(newmdl_2)[1:100], col = "turquoise4")
abline (h=0,lty=3, col = "turquoise4", lwd = 3)


# Formal test - Durbin-Watson test
durbinWatsonTest(smallmdl) # model 1
durbinWatsonTest(newmdl_2) # model 2









#BOOTSTRAPPING

#Write it as a function

bootstrap <- function(form, NBoot) {
  #Initialisation
  set.seed(1234)
  Ncol <- length(coef(lm(form, data = data)))
  bootStore <- array(dim = c(NBoot, Ncol))
  
  #Create loop
  for (i in 1:NBoot) {
    #Create data
    newData <- data[sample(x = nrow(data),
                           size = nrow(data),
                           replace = TRUE), ]
    
    #Create model for new data
    mdl <- lm(form,
              data = newData)
    
    #assign coefficients to store
    bootStore[i, ] <- coef(mdl)
  }
  
  #Get 95% confidence intervals for coefficients 
  bootCI <- apply(bootStore, 2, quantile, c(0.025, 0.975))
  
  #rename columns
  colnames(bootCI) <- colnames(t(data.frame(coef(mdl))))
  
  #make data frame with model coef for comparison
  return(cbind(data.frame(coef = coef(lm(form, data = data))), t(data.frame(bootCI))))
}

#Find bootstrap CI for newmdl_2
bootCI_newmdl_2 <- bootstrap("Birth_Weight ~  Smoke * Gestation + Parity + Height_M + Race_M + Weight_M", 1000)

#Add column of coef names
bootCI_newmdl_2$coef_names = rownames(bootCI_newmdl_2)


#Get model CI of newmdl_2, tidy and add column containing model 
modelCI <- data.frame(confint(newmdl_2))
colnames(modelCI) <- c("2.5%", "97.5%")
modelCI$coef_names <- rownames(modelCI)
modelCI$group <- "Model"

#prepare to combine with model by adding a group column containing bootstrap
bootCI_newmdl_2 <- bootCI_newmdl_2 %>%
  select(-coef) %>%
  mutate(group = "Bootstrap") 

#combine CI for plotting
CI_mod_boot <- rbind(bootCI_newmdl_2, modelCI)

#Relevel for plot
CI_mod_boot$coef_names <- factor(CI_mod_boot$coef_names, levels = c("(Intercept)", "Smoke1","Smoke2", "Smoke3", "Gestation", "Parity", "Height_M",  "Race_M6", "Race_M7", 
                                                                    "Race_M8", "Race_M9", "Weight_M", "Smoke1:Gestation", "Smoke2:Gestation", "Smoke3:Gestation"))



#Plot error bars for bootstrap and model CI
ggplot(CI_mod_boot,
       aes(y = coef_names, xmin= `2.5%`, xmax = `97.5%`, colour = group)) +
  geom_errorbarh(alpha = 0.7, 
                 size = 1.5) +
  theme_classic(base_size = 16) +
  geom_vline(aes(xintercept=0)) +
  labs(title = "95% Confidence Intervals for the bootstrap 
       method and linear model method",
       x = "Coefficient Estimate",
       y = "Coefficient",
       colour = "") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "top", axis.text = element_text(size=16))

