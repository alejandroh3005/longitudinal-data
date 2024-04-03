## Module 3 Code

library(tidyverse)
library(geepack)
library(lme4)
library(mice)

pal2use <- c("#E69F00", "#0072B2")
data(toenail)
toenail <- toenail[,c("ID", "outcome", "treatment", "visit")]
head(toenail)


summary_tab <- toenail %>% group_by(treatment, visit) %>% 
  summarise(nobs = n(),
            nsev = sum(outcome),
            percent_sev = mean(outcome))
summary_tab


visit_mo <- c(0, 4, 8, 12, 24, 36, 48)/4
summary_tab$visit_mo <- visit_mo[summary_tab$visit]
toenail$visit_mo <- visit_mo[toenail$visit]
summary_tab$treatment <- factor(summary_tab$treatment)
levels(summary_tab$treatment) <- c("Itraconazole", "Terbinafine")

ggplot(summary_tab, aes(x=visit_mo, y=percent_sev*100, group=treatment)) +
  geom_line(aes(color=treatment, linetype=treatment), linewidth=1.2) + 
  theme_bw() + ylab("% severe infections") + ylim(c(0, 50)) + 
  xlab("Time (months)") + scale_color_manual(values = pal2use) +
  theme(legend.position = "bottom")

gee.fit <- geeglm(outcome ~ treatment*visit_mo, id = ID, 
                  data = toenail,
                  family=binomial(link="logit"), 
                  waves = visit, 
                  scale.fix = T, # this sets phi = 1
                  corstr = "exchangeable")
summary(gee.fit)

dat <- unique(toenail[,c("treatment", "visit_mo")])

dat$phat <- predict(gee.fit, newdata = dat, type = "response")
dat$treatment <- factor(dat$treatment)
levels(dat$treatment) <- c("Itraconazole", "Terbinafine")


ggplot(summary_tab, aes(x=visit_mo, y=percent_sev*100, group=treatment)) +
  geom_line(aes(color=treatment, linetype=treatment), alpha = 0.5, linewidth = 0.8) + 
  geom_line(data = dat, aes(y = phat*100, color = treatment, linetype = treatment), size = 1.2) +
  theme_bw() + ylab("% severe infections") + ylim(c(0, 50)) + 
  xlab("Time (months)") + scale_color_manual(values = pal2use) +
  theme(legend.position = "bottom")

## Seizure dataset

dat <- read.csv(file = "Data/Seizure.csv")[,-1]
dat_long <- dat %>%
  pivot_longer(cols = starts_with("y"),
               names_to = "time",
               names_prefix="y",
               values_to = "seizures")

dat_long <- dat_long %>% arrange(id, time)

ggplot(dat_long, aes(x = tx, y = seizures)) + 
  geom_boxplot() + facet_wrap(~time) + theme_bw()


dat$rate_change <- dat$y4/2 - dat$y0/8

ggplot(dat, aes(x = tx, y = rate_change)) + 
  geom_boxplot() + theme_bw()

# Summaries
library(psych)
describeBy(dat[c(2,4:8)], dat$tx, skew=F)

# Correlation
by(dat[,-c(1,2,3,9)], INDICES = dat$tx, FUN=cor)


dat_long$obstime <- ifelse(dat_long$time>0, 2, 8)
dat_long$post <- dat_long$time > 0


gee.robust <- geeglm(seizures ~ post*as.factor(tx=="progabide") +
                       offset(log(obstime)),
                     data = dat_long, id = id,
                     family=poisson(link="log"), 
                     corstr="exchangeable", std.err="san.se")
summary(gee.robust)

gee.jack <- geeglm(seizures ~ post*as.factor(tx=="progabide") +
                     offset(log(obstime)),
                   data = dat_long, id = id,
                   family=poisson(link="log"), 
                   corstr="exchangeable", std.err="fij")
summary(gee.jack)

## GLMM

library(lme4)
glmm.fit <- glmer(outcome ~ treatment*visit_mo + (1|ID), 
                  data=toenail, 
                  family=binomial(link="logit"), 
                  nAGQ=20)
summary(glmm.fit)


## Transition Model
toenail_full <- toenail %>% complete(ID, visit_mo) %>% 
  arrange(ID, visit_mo)
toenail_full <- toenail_full %>% group_by(ID) %>% 
  mutate(lag1 = dplyr::lag(outcome, default=NA))
head(toenail_full,10)

transit.fit <- geeglm(outcome ~ treatment*visit_mo + lag1, 
                      id = ID, 
                      data = toenail_full %>% na.omit,
                      family=binomial(link="logit"), 
                      scale.fix = T,
                      corstr = "independence")

glm(outcome ~ treatment*visit_mo + lag1, 
    data = toenail_full[complete.cases(toenail_full),],
    family=binomial(link="logit"))
summary(transit.fit)
