## Module 1 Code
# Written by Gary Chan, edited by Alejandro Hernandez
# Mar 28, 2024

library(tidyverse)
library(corrplot)

## Load dental dataset
library(nlme)
data(Orthodont)
head(Orthodont)

##### Exploratory Data Analysis ####

## Wide Format
ortho_wide <- Orthodont %>%
  pivot_wider(names_from = age,
              names_prefix = "distance.",
              values_from = distance)
head(ortho_wide)

## Long Format
ortho_long <- ortho_wide %>%
  pivot_longer(cols = starts_with("distance."),
               names_to = "age",
               names_prefix = "distance.",
               values_to = "distance")

## Summary statistics by sex
by(ortho_wide[,-c(1,2)], INDICES = ortho_wide$Sex, FUN=summary)

## Summary of means
summary_ortho_long <- Orthodont %>% 
  group_by(Sex, age) %>%
  summarise(mean_dist = mean(distance))

## Plot of means

pal2use <- c("darkblue", "firebrick")

ggplot(data=summary_ortho_long, 
       aes(x=age, y=mean_dist, group=Sex, colour=Sex)) +
  geom_line() + 
  geom_point(aes(shape = Sex), size=2.5) +
  labs(title="Average Distance per Age by Sex", 
       x="Age (years)", y="Distance (mm)") +
  theme_bw() + scale_colour_manual(values = pal2use)

## Spaghetti plot: Distance vs Age

ggplot(data=Orthodont, 
       aes(x=age, y=distance, group=Subject, colour=Sex))+
  geom_line() +
  xlab("Age (years)") + ylab("Distance (mm)") +
  theme_bw() + scale_colour_manual(values = pal2use)

## Subject-specific Plots

ggplot(data=Orthodont, 
       aes(x=age, y=distance, group=Sex, colour=Subject)) +
  facet_wrap(. ~ Sex, ncol=6) + 
  geom_line() + geom_point() +
  xlab("Age (years)") + ylab("Distance (mm)") + 
  theme_bw() + scale_colour_manual(values = pal2use)

## Spaghetti plot: Growth vs Age

Orthodont <- Orthodont %>% 
  arrange(Subject, age) %>% 
  group_by(Subject) %>% 
  mutate(growth = distance-distance[1L])

ggplot(data=Orthodont, 
       aes(x=age, y=growth, group=Subject, colour=Sex))+
  geom_line() +
  xlab("Age (years)") + ylab("Distance (mm) from Age 8") + theme_bw() + 
  scale_colour_manual(values = pal2use)

## Spaghetti plot: Residual vs Time 

### Subtract out the mean by sex and age to get the "residual"
Orthodont <- Orthodont %>% group_by(Sex, age) %>% 
  mutate(mean_dist = mean(distance))

Orthodont$residual <- Orthodont$distance - Orthodont$mean_dist

ggplot(data=Orthodont, 
       aes(x=age, y=residual, group=Subject, colour=Sex))+
  geom_line() +
  xlab("Age (years)") + ylab("Residual (mm)") + theme_bw() + 
  scale_colour_manual(values = pal2use)

## Empirical covariance and correlation
cov_mat_f <- cov(ortho_wide %>% filter(Sex == "Female") %>% 
                   dplyr::select(dplyr::starts_with("distance")))
cov_mat_f

cor_mat_f <- cor(ortho_wide %>% filter(Sex == "Female") %>% 
                   dplyr::select(dplyr::starts_with("distance")))
cor_mat_f

## Visualizations of correlation

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_mat_f, method="color", col=col(200),  
         type="upper", order="original", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=FALSE )

Orthodont <- Orthodont %>% 
  group_by(Sex, age) %>% 
  mutate(residual = distance - mean(distance)) %>% 
  ungroup()
Orthodontf <- Orthodont %>% 
  filter(Sex=="Female")
ortho_widef <- Orthodontf %>% 
  dplyr::select(age, Subject, residual) %>%
  pivot_wider(names_from = age,
              names_prefix = "residual",
              values_from = residual)

pairs(ortho_widef[,2:5], upper.panel = NULL, diag.panel = NULL,
      xlim=c(-5,5), ylim=c(-5,5))


