setwd("C:/Users/renee/OneDrive - KU Leuven/Biologie/vakken/2de master/semester_2/Concepts of complex, longitudinal and mixed models/exam project")

library(ggplot2)
library(effects)
library(lme4)
library(car)
library(MASS)
library(emmeans)
library(nlme)

cb_palette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mmse <- read.csv("mmse.csv", header = T)

mmse$NEURO <- as.factor(mmse$NEURO)

table(mmse$NEURO)
levels(mmse$NEURO) <- c("not neuro-psychiatric", "neuro-psychiatric")

mmse$housing <- as.factor(mmse$housing)

summary(mmse)

density_neuro <- ggplot(mmse, aes(x= mmse)) +
  geom_density()
density_neuro



#exploratory analysis

plot(mmse$mmse~mmse$NEURO)

##model exploration

#linear mixed model with subject-specific slopes and intercepts
model_1 <- lmer(mmse ~ log(time) + NEURO + (log(time)|id), data = mmse)

AIC(model_1)

summary(model_1)
Anova(model_1, type="III")  #testing significance off overall effects

plot(allEffects(model_1),type="response")

contrast(emmeans(model_1,~NEURO, type="response"),
         method="pairwise", adjust="Tukey")

ggplot() + 
  geom_line(data=mmse, aes(x=log(time), y=fitted(model_1), group = id, color= NEURO)) +
  scale_color_manual(values = cb_palette) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

#estimating random effects + scatterplot (OPLOSSEN)

rand_effects_1 <- coef(model_1)$id

colnames(rand_effects_1) <- c("Intercept", "Slope", "NEURO")
  
ggplot() + 
  geom_point(data=rand_effects_1, aes(x=Intercept, y=Slope)) +
  scale_color_manual(values = cb_palette) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

#implementing housing and age

model_2 <- lmer(mmse ~ log(time) + NEURO + housing + age + (log(time)|id), data = mmse)

summary(model_2)

Anova(model_2, type = 'III')

fitted(model_2)  #3 data points are removed, does anyone have an idea to why?

?fitted

ggplot() + 
  geom_line(data=mmse, aes(x=log(time), y=fitted(model_2), group = id, color= NEURO)) +
  scale_color_manual(values = cb_palette) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

#estimating random effects

rand_effects <- coef(model_2)$id

colnames(rand_effects) <- c("Intercept", "Slope", "NEURO")

ggplot() + 
  geom_point(data=rand_effects, aes(x=Intercept, y=Slope)) +
  scale_color_manual(values = cb_palette) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))                    #less variance within the random slopes and intercepts (+better AIC!)




