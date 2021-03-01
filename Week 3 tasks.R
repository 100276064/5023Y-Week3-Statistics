library(tidyverse)
library(ggplot2)
library(patchwork)
library(broom)

wood_density <- read.csv("Data/wood_density.csv")

wood_density %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_point()+
  geom_smooth(method = "lm")

density_model <- lm(Hardness~Density, data=wood_density)
density_model #hardness = -1160.50 + 57.51 (Density)

(24.7*57.507)+-1160.5 #calculating the predicted hardness of the lowest density value using straight line equations and the values of the intercept and slope

coef(density_model)[1]+
  coef(density_model)[2]* 24.7 # Using the coefficients of the model to predict the hardness of the lowest density wood, this prevents rounding errors, the predicted vale is slightly lower

fitted(density_model) # Adds predictions to the data points so we can compare the model's predicted vales to the realsied values
# You can calculate the difference between the predictions/model-fitted values and the observed values - this is what is known as a risidual

wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions) # This adds the model predictions and the residuals onot the original dataframe

p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")

p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")

p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")

p1+p2+p3 #plotting observed timber hardness, predicted hardness value and the residuals in turn

glance(density_model) # concise one-row summary of the model consists of R squared, adjusted R squared, F values, degrees of freedom and P
tidy(density_model, conf.int=TRUE) #A small tibble with most of the model's summary data
augment(density_model, wood_density, interval="confidence")# Takes computations from our model fit and adds them onto original dataframe 
#.fitted = predictions of the model
#.resid = residuals
#.upper is the 95% confidence interval upper value for our fit line
#.lower is the 95% confidence interval lower value for our fit line

plot1 <- augment(density_model, wood_density, interval="confidence") %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line(aes(x=Density, y=.fitted))+
  geom_line(aes(x=Density, y=.upper), linetype="dashed")+
  geom_line(aes(x=Density, y=.lower), linetype="dashed")+
  geom_point()+
  ggtitle("Manually fitting linear model \n and confidence intervals")

plot2 <- wood_density %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_smooth(method=lm)+
  geom_point()+
  ggtitle("Geom smooth method to plotting \n a linear model")

plot1+plot2

#Wood density is an excellent predictor of timber hardness. On average for every pound per cubic foot increase in the density of wood, we see a 57.5 point increase in the Janka “hardness scale” (F1,34= 637, P <0.001, R^2 = 0.94).