## Module 2 Code

library(nlme)
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(kableExtra)
library(corrplot)
library(joineR)
library(MASS)
library(lattice)
library(geepack)
library(multcomp)
pal2use <- c("darkblue", "firebrick")

## Load dental dataset
orthoad <- read.csv("Data/ortho_additional.csv")

## ~~~~ Modeling the Mean ~~~~ ##

ggplot(data=orthoad, 
       aes(x=age, y=distance)) +
  geom_line(aes(group=Subject), alpha=0.4) +
  geom_point(data = orthoad %>% group_by(age) %>% summarise(mean=mean(distance)),
             aes(x=age, y=mean), size=3) + 
  xlab("Age (years)") + ylab("Distance (mm)") + theme_bw() + 
  scale_x_continuous(breaks=seq(8,18,2))


## Categorical
summary(lm(distance ~ as.factor(age), data = orthoad))

ggplot(data=orthoad, 
       aes(x=age, y=distance))+
  geom_point(colour = "black", alpha=0.2) +
  geom_point(data = orthoad %>% group_by(age) %>% summarise(mean=mean(distance)),
             aes(x=age, y=mean), size=5, col="goldenrod") + 
  xlab("Age (years)") + ylab("Distance (mm)") + theme_bw() + 
  scale_x_continuous(breaks=seq(8,18,2))


## Linear
summary(lm(distance ~ age, data = orthoad))

ggplot(data=orthoad, 
       aes(x=age, y=distance))+
  geom_point(colour = "black", alpha=0.2) +
  geom_point(data = orthoad %>% group_by(age) %>% summarise(mean=mean(distance)),
             aes(x=age, y=mean), size=3, col="black") + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, col="goldenrod", size=2)  + 
  xlab("Age (years)") + ylab("Distance (mm)") + theme_bw() + 
  scale_x_continuous(breaks=seq(8,18,2))


## Quadratic
summary(lm(distance ~ age + I(age^2), data = orthoad))

ggplot(data=orthoad, 
       aes(x=age, y=distance))+
  geom_point(colour = "black", alpha=0.2) +
  geom_point(data = orthoad %>% group_by(age) %>% summarise(mean=mean(distance)),
             aes(x=age, y=mean), size=3, col="black") + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F, col="goldenrod", size=2)  + 
  xlab("Age (years)") + ylab("Distance (mm)") + theme_bw() + 
  scale_x_continuous(breaks=seq(8,18,2))


## Linear Spline
summary(lm(distance ~ age + I((age-14)*(age>14)), data = orthoad))

ggplot(data=orthoad, 
       aes(x=age, y=distance))+
  geom_point(colour = "black", alpha=0.2) +
  geom_point(data = orthoad %>% group_by(age) %>% summarise(mean=mean(distance)),
             aes(x=age, y=mean), size=3, col="black") + 
  geom_smooth(method = "lm", formula = y ~ x + I((x-14)*(x>14)), se = F, col="goldenrod", size=2)  + 
  xlab("Age (years)") + ylab("Distance (mm)") + theme_bw() + 
  scale_x_continuous(breaks=seq(8,18,2))


## TLC trial: Analysis of response profiles
tlc <- read.csv("Data/tlc.csv") %>% dplyr::select(-X)
tlc_long <- tlc %>%
  pivot_longer(cols = starts_with("y"),
               names_to = "week",
               names_prefix="y",
               values_to = "value")
tlc_long$week <- as.numeric(tlc_long$week)
tlc_long$time <- as.numeric(as.factor(tlc_long$week))
p <- ggplot(data = tlc_long, aes(x = week, y = value, group = id, col = tx))
p + geom_line(alpha=0.1) + 
  geom_line(data = tlc_long %>% group_by(tx, week) %>% summarise(value=mean(value)),
            aes(x = week, y = value, group = tx), size=2) +
  theme_bw() + ylab("Lead level (ug/dL)")


tlc_long$tx <- factor(tlc_long$tx, levels = c("P", "A"))
summary(lm(value ~ tx*factor(week), data = tlc_long))


## Dental data: linear time trend
data(Orthodont)
Orthodont$age8 <- Orthodont$age - 8
Orthodont$Sex <- relevel(Orthodont$Sex, ref = "Female")

model <- lm(distance ~ Sex*age8, data = Orthodont)
summary(model)

## ~~~~ Linear Mixed Models ~~~~ ##

library(nlme)
## Cow dataset
cow <- read.csv("Data/cow.csv")
ggplot(cow, aes(x = day, y = logweight, group = animal, colour = factor(txt))) +
  geom_line(aes(linetype = factor(txt))) + theme_bw() + 
  ylab("Log Weight (log(kg))") + xlab("Day") +
  labs(color = "Treatment", linetype = "Treatment")

model <- lm(logweight ~ as.factor(day)*as.factor(txt), data=cow)
cow$resids <- resid(model)
ggplot(cow, aes(x = day, y = resids, group = animal, colour = factor(txt))) +
  geom_line(aes(linetype = factor(txt))) + theme_bw() + 
  ylab("Residuals") + xlab("Day") +
  labs(color = "Treatment", linetype = "Treatment")

library(joineR)
vgm <- variogram(cow$animal, cow$day, cow$resids)
par(mar=c(5, 4, 1.5, 1.5) + 0.1)
plot(vgm, ylim=c(0, 0.01), smooth=T)



#Dental Growth Case Study
## random intercepts only
mod1 <- lme(distance ~ Sex*age8, data = Orthodont, method = "ML",
            random = reStruct( ~ 1 | Subject, pdClass="pdDiag", REML=F)) 

## random intercepts + slopes, uncorrelated
mod2 <- lme(distance ~ Sex*age8, data = Orthodont, method = "ML",
            random = reStruct( ~ 1 + age8 | Subject, pdClass="pdDiag", REML=F)) 

## random intercepts + slopes, correlated
mod3 <- lme(distance ~ Sex*age8, data = Orthodont, method = "ML",
            random = reStruct( ~ 1 + age8 | Subject, pdClass="pdSymm", REML=F)) 


summary(mod3)

## LRT
mod3_red <- lme(distance ~ Sex + age8, data = Orthodont, method = "ML",
                random = reStruct( ~ 1 + age8 | Subject, pdClass="pdSymm", REML=F))
print(anova(mod3_red, mod3))

## Model for Covariance
print(anova(mod1, mod2, mod3))


summary(mod1)

## CF Analysis
load("Data/NewCFkids.RData")
cf.long$sex <- relevel(cf.long$sex, ref="male")
cf.long$f508 <- relevel(cf.long$f508, ref="none")

mod1_cf <- lme( fev1 ~ age0 + ageL + sex*ageL + f508*ageL, 
                method = "ML", data = cf.long, 
                random = reStruct( ~ 1 | id, pdClass="pdDiag", REML=F))
mod2_cf <- lme( fev1 ~ age0 + ageL + sex*ageL + f508*ageL, 
                method = "ML", data = cf.long, 
                random = reStruct( ~ 1 + ageL | id, pdClass="pdDiag", REML=F))
mod3_cf <- lme( fev1 ~ age0 + ageL + sex*ageL + f508*ageL, 
                method = "ML", data = cf.long, 
                random = reStruct( ~ 1 + ageL | id, pdClass="pdSymm", REML=F))

summary(mod1_cf)
summary(mod3_cf)

mod3_cf.noint <- lme(fev1 ~ age0 + ageL + sex + f508, 
                     method = "ML", data = cf.long, 
                     random = reStruct( ~ 1 + ageL | id, 
                                        pdClass="pdSymm", REML=F))
print(anova(mod3_cf.noint, mod3_cf))


print(anova(mod1_cf, mod2_cf, mod3_cf))


## Variogram from CF analysis
plot(Variogram(mod1_cf, form = ~ age | id, resType="normalized"), ylim=c(0, 1.5))


mod4_cf <- lme(fev1 ~ age0 + ageL + sex*ageL + f508*ageL,
               method = "ML", data = cf.long,
               random = reStruct( ~ 1 | id, pdClass="pdDiag", REML=F),
               correlation = corExp(form = ~ ageL | id) )
mod5_cf <- lme(fev1 ~ age0 + ageL + sex*ageL + f508*ageL,
               method = "ML", data = cf.long,
               random = reStruct( ~ 1 | id, pdClass="pdDiag", REML=F),
               correlation = corExp(form = ~ ageL | id, nugget=T) )


summary(mod4_cf)
summary(mod5_cf)

plot(Variogram(mod5_cf, form = ~ age | id, resType="normalized"))


## Dental Data
## Predicted subject specific random effects
bi <- mod3$coefficients$random$Subject
head(bi)

## Predicted responses and visualization
Orthodont$pred.fit <- predict(mod3)
head(Orthodont)


to_predict = rbind(Orthodont %>% filter(Subject=="M01"),
                   Orthodont %>% filter(Subject=="F01"))

to_predict$pred.fit <- predict(mod3, newdata=to_predict)
to_predict$pred.fit.OLS_SS <- predict(lm(distance ~ Sex*age8, data = to_predict), newdata=to_predict)
to_predict$pred.fit.OLS <- predict(lm(distance ~ Sex*age8, data = Orthodont), newdata=to_predict)

to_predict <- to_predict %>% pivot_longer(cols = 6:8, names_to = "ID", values_to="preds")

to_predict$ID <- recode(to_predict$ID, "pred.fit"="LMM", "pred.fit.OLS_SS"="Subject Specific OLS", "pred.fit.OLS"="OLS")

ggplot(data=to_predict)+
  geom_point(aes(x=age, y=distance, color=Sex))+geom_line(aes(x=age, y=distance, color=Sex))+
  geom_line(aes(x=age, y=preds, lty=ID, color=Sex))+
  facet_wrap(~Sex)+
  theme_bw()


vis_reff=cbind("Sex"=c(rep(1, 16), rep(0, 11)), mod3$coefficients$random$Subject)

par(mar=c(1,1,1,1))
par(mfrow=c(1,2))
plot( x=vis_reff[,1], y=vis_reff[,2], type="n", xaxt="n", xlab="", ylab="Random Intercept", main="Intercepts") 
text(x=vis_reff[,1], y=vis_reff[,2], label=rownames(vis_reff), col=c(rep("blue", 16) , rep("red", 11)))
axis(side = 1, at = c(0,1), labels = c("Male", "Female"))
abline(a=0, b=0)

plot( x=vis_reff[,1], y=vis_reff[,3], type="n", xaxt="n", xlab="", ylab="Random Slope", main="Slopes") 
text(x=vis_reff[,1], y=vis_reff[,3], label=rownames(vis_reff), col=c(rep("blue", 16) , rep("red", 11)))
axis(side = 1, at = c(0,1), labels = c("Male", "Female"))
abline(a=0, b=0)




## ~~~~ GEE and Robust Inference ~~~~ ##

library(geepack)

## Dental Data
mod_naive <- lm(distance ~ age, 
                data = Orthodont %>% filter(Sex == "Male"))


mod_clust <- geeglm(distance ~ age, 
                    data = Orthodont %>% filter(Sex == "Male"), 
                    id = Subject)

summary(mod_clust)

# Predict distance for various ages using the geeglm() output
predict(mod_clust, newdata = data.frame(age = c(8, 10, 12, 14)))

## TLC Trial
mod_clust <- geeglm(value ~ tx*factor(week),
                    data = tlc_long, id = id)
summary(mod_clust)
anova(mod_clust)

## Dental Data
mod_clust <- geeglm(distance ~ Sex*age8, 
                    data = Orthodont, id = Subject)
summary(mod_clust)

library(multcomp)
dgrowth_males <- glht(mod_clust, 
                      linfct = c("age8 + SexMale:age8 = 0"))
confint(dgrowth_males)


mod_clust_reduced <- geeglm(distance ~ age8, 
                            data = Orthodont, id = Subject)
anova(mod_clust, mod_clust_reduced)

## Various working correlation structures
mod1 <- geeglm(distance ~ Sex*age8, data = Orthodont, 
               id = Subject, corstr = "independence") # default is independence
mod2 <- geeglm(distance ~ Sex*age8, data = Orthodont, 
               id = Subject, corstr = "exchangeable")
mod3 <- geeglm(distance ~ Sex*age8, data = Orthodont, 
               id = Subject, corstr = "ar1")
mod4 <- geeglm(distance ~ Sex*age8, data = Orthodont, 
               id = Subject, corstr = "unstructured")

summary(mod4)

## ~~~~ Diagnostics ~~~~ ##
## Residuals
pop.res <- resid(mod3_cf, level=0)
cluster.res <- resid(mod3_cf, level=1)

## Predicted subject specific random intercepts and slopes
bi <- mod3_cf$coefficients$random$id

## Predicted subject specific and population-level response profiles
cf.long$pred.mod3_cf_marg <- predict(mod3_cf, level=0)
cf.long$pred.mod3_cf_cond <- predict(mod3_cf, level=1)


## Plots
par(mfrow=c(1,2))
plot(cf.long$pred.mod3_cf_marg, pop.res, ylab = "Pop Residuals (untransformed)", 
     xlab="Predicted (pop)", main = "Residuals (pop) vs\n Predicted Mean Response")
lines(lowess(cf.long$pred.mod3_cf_marg, pop.res), col="red")
qqnorm(pop.res, main="Pop Residuals")
qqline(pop.res)

par(mfrow=c(1,2))
plot(cf.long$age0, cluster.res, xlab="Age0", main = "Residuals (cluster) vs Age0")
lines(lowess(cf.long$age0, cluster.res), col="red")
plot(cf.long$ageL, cluster.res, xlab="AgeL", main = "Residuals (cluster) vs AgeL")
lines(lowess(cf.long$ageL, cluster.res), col="red")

qqnorm(cluster.res, main="Cluster Residuals")
qqline(cluster.res)

par(mfrow=c(1,2))
qqnorm(bi[,1], main="Random Intercepts")
qqline(bi[,1])
qqnorm(bi[,2], main="Random Slopes")
qqline(bi[,2])