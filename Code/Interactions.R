library(interplot)
library(jtools)
library(interactions)
library(ggplot2)
library(sjPlot)
library(sjmisc)


gay = read.csv("C:/Users/afisher/Documents/R Code/qss/CAUSALITY/gay.csv")
gay_waves = gay %>%
  filter(study == 1 & treatment %in% c("No Contact", 'Same-Sex Marriage Script by Gay Canvasser'))

m1 <- lm(ssm ~ treatment * as.factor(wave), data = gay_waves)
fit <- lm(ssm ~ treatment * as.factor(wave), data = gay_waves)
plot_model(fit, type = "pred", terms = c("wave", "treatment"))

m1 <- lm(ssm ~ treatment * wave, data = gay_waves)
interplot(m1, var1='treatment', var2='wave')
interact_plot(m1, pred = wave, modx = treatment, interval=TRUE)

gay_summary = gay_waves %>%
  group_by(treatment, wave) %>%  
  dplyr::summarise(mean_ssm =mean(ssm),  
                   sd_ssm = sd(ssm), 
                   n_group = n(),  
                   SE_ssm = sd(ssm)/sqrt(n())) 


pd <- position_dodge(0.2) # move them .05 to the left and right
ggplot(gay_summary, aes(x=wave, y=mean_ssm, colour=treatment, group=treatment)) + 
  geom_errorbar(aes(ymin = mean_ssm - 1.96*SE_ssm, 
                    ymax = mean_ssm + 1.96*SE_ssm,), width=0.2, position=pd)  +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Wave") +
  ylab("Support for Gay Marriage") +
  ggtitle("Canvaser Effect on Support for Same-Sex Marriage") +
  scale_y_continuous() +         # Set tick every 4
  theme_bw() +
  guides(col=guide_legend(ncol=3)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        legend.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5))


# https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
