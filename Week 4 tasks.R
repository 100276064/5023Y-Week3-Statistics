library(tidyverse)
library(emmeans) # A handy package for estimating means from the fit of our model
library(ggplot2)

darwin <- read_csv("Data/darwin.csv")
darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"),names_to = "type", values_to = "height") %>% 
  mutate(type=factor(type,levels=c("Self","Cross"))) %>% 
  mutate(pair=factor(pair))

darwin_model <- lm(formula = height ~ type + pair, data = darwin)
darwin_model
summary(aov(darwin_model))

estimates <- emmeans(darwin_model, specs="type") ### here it will take the average of the values across all the pairs to calculate means for type

estimates %>% 
  as_tibble %>% ### emmeans outputs a grid by default, but can easily                       be changed
  ggplot(aes(x=type, y=emmean, colour=type))+
  geom_pointrange(aes(ymin=lower.CL,ymax=upper.CL))+
  geom_pointrange(aes(ymin=emmean-SE,ymax=emmean+SE),size=1.2)

tidymodel <- broom::tidy(darwin_model) %>% 
  mutate(lwr=((estimate-(std.error*2))),upr=(estimate+(std.error*2)))

tidymodel %>% 
  ggplot(aes(x=estimate,y=term))+
  geom_pointrange(aes(xmin=lwr,xmax=upr))+
  geom_vline(xintercept=0,linetype="dashed")

tidymodel2 <- broom::tidy(darwin_model, conf.int=T) 
tidymodel2[2,] ## only pull out row 2 

tidymodel2 %>% 
  ggplot(aes(x=estimate,y=term))+
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high))+
  geom_vline(xintercept=0,linetype="dashed")

t.crit <- qt(0.975, df=14)

upr <- 2.62+t.crit*1.22
lwr <- 2.62-t.crit*1.22

