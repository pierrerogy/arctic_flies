#Packages
##car for Anova() instead of anova()
##anova() does type I test, all variable tested in sequential order
##Anova() does 
library(car)
##visreg for visualization of model, displays ech variable of th model when maintaining the others
##at a median
##not necessarily for paper, but more to see whats going on
library(visreg)

# Explanations/Assumptions ------------------------------------------------------
##First always visualize data to see what kind of assumption/distribution
head(siteanalysis)
###nrow() as a short way to see degrees of freedom
nrow(siteanalysis)
###nice way to display data distribution
hist(siteanalysis$Abundance)

##Do dummy basic model to check for overdspersion
##glm because NOT assuming that the errors of variance are equal here
##poisson because abundance data (integer) and skewed
model1 <- glm(Abundance ~ bio1 + bio2 + bio3, family = poisson(link = "log"), data = siteanalysis)
##check it should be less than 2
dispersion.parameter <-model1$deviance/model1$df.residual
##correct if too dispersed
model1<- glm((Abundance/dispersion.parameter) ~ bio1 + bio3  + bio12 + bio17, 
             family = poisson(link = "log"), data = siteanalysis)

##Include a relevant number of parameters depending on degree of freedom
##Here 8 should be enough
model1<- glm((Abundance/dispersion.parameter) ~ bio1 + bio3  + bio12 + bio17, 
             family = poisson(link = "log"), data = siteanalysis)
##then use step() to remove non-relevant variables
final_model1 <- step(model1)

##link() tells R what the data is like MANDATORY for the function, family-specific
###usually log for count data, usually logistic or logit for binary data
##always test all of them at the end -> fine-tuning
model1 <- glm(Abundance ~ bio1 + bio2 + bio3, family = poisson(link = "log"), data = siteanalysis)
model1.i <- glm(Abundance ~ bio1 + bio2 + bio3, family = poisson(link = "identity"), data = siteanalysis)
model1.s <- glm(Abundance ~ bio1 + bio2 + bio3, family = poisson(link = "sqrt"), data = siteanalysis)
model1.in <- glm(Abundance ~ bio1 + bio2 + bio3, family = poisson(link = "inverse"), data = siteanalysis)
##Some of them wil obviously not run, especially if lot of 0s

##Then can see if one link is better using AIC
AIC(model1, model1.in)


##summary() is for estimation not testing
## Wald z summry statistic tends to be unreliable with small to moderate sample size
summary(model1.in)



# Models for abundance -----------------------------------------------------

##Do the dummy model 1
###selected variables are mean T, mean dirunal range, isothermality, seasonality, 
###minimum temperature of coldest month, T annual range, and annual precipitation
model_abundance_1 <- glm(Abundance ~ bio1 + bio2 + bio3 + bio4 + bio6 + bio7 + bio12 + bio14,
                       family = poisson(link = "log"), data = siteanalysis)
##check it should be less than 2
dispersion.parameter.1 <-model_abundance_1$deviance/model_abundance_1$df.residual
print(dispersion.parameter.1)
##correct because too dispersed
model_abundance_1 <- glm(Abundance/dispersion.parameter.1 ~ bio1 + bio2 + bio3 + bio4 + bio6 + bio7 + 
                           bio12 + bio14, family = poisson(link = "log"), data = siteanalysis)
##look at model
summary(model_abundance_1)
##then use step() to remove non-relevant variables
final_model_abundance_1 <- step(model_abundance_1)
summary(final_model_abundance_1)
##plot model
plot(final_model_abundance_1)
Anova(final_model_abundance_1)
visreg(final_model_abundance_1)

##Do the dummy model 2
###other variables included
model_abundance_2 <- glm(Abundance ~ bio5 + bio8 + bio9 + bio10 + bio11 + bio13 + bio12 + bio15,
                         family = poisson(link = "log"), data = siteanalysis)
##check it should be less than 2
dispersion.parameter.2 <-model_abundance_2$deviance/model_abundance_2$df.residual
print(dispersion.parameter.2)
##correct because too dispersed
model_abundance_2 <- glm(Abundance/dispersion.parameter.2 ~ bio5 + bio8 + bio9 + bio10 + bio11 + bio13 + bio12 + bio15,
                         family = poisson(link = "log"), data = siteanalysis)
##look at model
summary(model_abundance_2)
##then use step() to remove non-relevant variables
final_model_abundance_2 <- step(model_abundance_2)
summary(final_model_abundance_2)
##plot model
plot(final_model_abundance_2)
Anova(final_model_abundance_2)
visreg(final_model_abundance_2)



# Models for richness -----------------------------------------------------

##Do the dummy model 1
###selected variables are mean T, mean dirunal range, isothermality, seasonality, 
###minimum temperature of coldest month, T annual range, and annual precipitation
model_richness_1 <- glm(siterichness ~ bio1 + bio2 + bio3 + bio4 + bio6 + bio7 + bio12 + bio14,
                         family = poisson(link = "log"), data = siteanalysis)
##check it should be less than 2
dispersion.parameter.1 <-model_richness_1$deviance/model_richness_1$df.residual
print(dispersion.parameter.1)
##correct because too dispersed
model_richness_1 <- glm(siterichness/dispersion.parameter.1 ~ bio1 + bio2 + bio3 + bio4 + bio6 + bio7 + 
                           bio12 + bio14, family = poisson(link = "log"), data = siteanalysis)
##look at model
summary(model_richness_1)
##then use step() to remove non-relevant variables
final_model_richness_1 <- step(model_richness_1)
summary(final_model_richness_1)
##plot model
plot(final_model_richness_1)
Anova(final_model_richness_1)
visreg(final_model_richness_1)

##Do the dummy model 2
###selected variables are mean T, mean dirunal range, isothermality, seasonality, 
###minimum temperature of coldest month, T annual range, and annual precipitation
model_richness_2 <- glm(siterichness ~ bio5 + bio8 + bio9 + bio10 + bio11 + bio13 + bio12 + bio15,
                         family = poisson(link = "log"), data = siteanalysis)
##check it should be less than 2
dispersion.parameter.2 <-model_richness_2$deviance/model_richness_2$df.residual
print(dispersion.parameter.2)
##correct because too dispersed
model_richness_2 <- glm(siterichness/dispersion.parameter.2 ~ bio5 + bio8 + bio9 + bio10 + bio11 + bio13 + bio12 + bio15,
                        family = poisson(link = "log"), data = siteanalysis)
##look at model
summary(model_richness_2)
##then use step() to remove non-relevant variables
final_model_richness_2 <- step(model_richness_2)
summary(final_model_richness_2)
##plot model
plot(final_model_richness_2)
Anova(final_model_richness_2)
visreg(final_model_richness_2)

