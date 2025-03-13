## Homework 4 Submission ## 
library(MASS)
library(performance)
library(marginaleffects)
library(modelr)
library(ggplot2)

## Question 1:
mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)
hist(mistletoe$Seedlings)

## 1a) fit a glm
#fit poisson distribution
mis.pois <- glm(Seedlings~Treatment, data=mistletoe, family = poisson(link = "log"))
summary(mis.pois)

#check for overdispersion
check_overdispersion(mis.pois)#detected so negatuve bionomial

#overdispersion detected, fit negative binomial distribution
mis.nbin <-glm.nb(Seedlings~Treatment, data=mistletoe)
summary(mis.nbin)

##MAE
predicted <- predict(mis.nbin, type = "response")
mae <- mean(abs(mistletoe$Seedlings - predicted))
mae
#MAE: predicted seedling counts vary from observed by 145 seedlings (that doesn't sound very good, but might be biologically relevant if seedling counts vary in an extreme way)

####I chose this model because the response variable (seedlings) is count data making the Poisson distribution an appropriate approach.
#However, because this involves seedlings and biologically the factors that condition whether they are present or not may lead to variable counts I checked for overdispersion.
#After checking for overdispersion, the results indicated that this was true for the model generated. So, I refit the data to a negative binomial distribution for my final model approach.

## 1b) Use visual (e.g. a marginal effects plot) and written (e.g. effect sizes on scale of response) approaches to interpret the results of your model.
plot_predictions(mis.nbin, condition = c("Treatment")) #shows the difference between parasitized and unparasitized
predictions(mis.pois, newdata = data.frame(Treatment = c("parasitized", "unparasitized"))) #quantifies how different the two treatment groups are

#Based on your fitted model results and model fit, write 3-4 sentences on:
#Does mistletoe infection alter seedling density? How much does seedling
#recruitment differ beneath parasitized and unparasitized trees? Explain
#which elements of your glm results informed your conclusions and annotate the
#steps you needed to take to interpret your parameters. 

##Mistletoe does have an effect on seedlings density where parasitized trees showed greater seedling density compared to unparasitized.
#When comparing the effect of mistletoe on seedlings density, there are 295 fewer seedlings under unparasitized compared to parasitized trees.
#Seedling recruitment is 24 times higher under parasitzed trees when compared to unparasitized.


## 1c) Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study.
mis.interx <- glm.nb(Seedlings ~ Treatment * Year, data = mistletoe)
summary(mis.interx)
coef <- coef(mis.interx)
exp <- exp(coef)
exp
#Ultimately, parasitism is associated with higher seedling density, but this effect changes over time as indicated by the interaction between treatment and year.
#The estimate for year increases in unparasitized trees suggesting that over time the difference between treatment diminishes potentially from
#other ecological factors including environmental fluctuations or competition.

newdata <- expand.grid(Treatment = c("parasitized", "unparasitized"),
                       Year = unique(mistletoe$Year)) 
newdata$predicted_seedlings <- predict(mis.interx, newdata = newdata, type = "response")

ggplot(newdata, aes(x = Year, y = predicted_seedlings, color = Treatment, group = Treatment)) +
  geom_line() +  # Lines showing trends over years
  geom_point() +  # Points showing predictions
  labs( x = "Year",
       y = "Predicted Seedling Density") +
  theme_minimal()



## Question 2:
treemort <- read.csv("treemortality.csv")
head(treemort)

## 2a) fit a glm
thin.binom <- glm(mortality~thinning, data=treemort, family="binomial"(link="logit"))
summary(thin.binom)
##MAE
predicted1 <- predict(thin.binom, type = "response")
mae2 <- mean(abs(treemort$mortality - predicted1))
mae2
coef2 <- coef(thin.binom)
exp2 <- exp(coef2)
exp2
1-0.157
#In this case, thinning significantly reduces tree mortality where the probability of mortality in thinned forests is 84% lower
#and that on average unthinned forests experience 2.7 times more mortality than unthinned. When assessing model fit, a MAE of 0.40
#indicates that thinning has a significant effect on mortality, but other factors are likely influencing tree mortality.


## 2b) do the researchers need to incorporate tree size into their glm to accurately estimate the effect of thinning? Why or why not?
#I believe that the act of randomization should have already accounted for confounding effect of tree size. Including tree size in the model
#may add complexity without improving the accuracy of the effect for thinning. 


## 2c) 
DAG.glm<-glm(formula = mortality ~ thinning + slope + roaddist, family = binomial(link = "logit"), data = treemort)
summary(DAG.glm)
coef3 <- coef(DAG.glm)
exp3 <- exp(coef3)
exp3
#When accounting for slope and road distance, the effect of mortality on thinned forests is still significant, but the effect size is smaller.
#Not including these two new variables caused an overestimation for the effect of thinning. Now the effect of thinning reduced mortality by 60%.