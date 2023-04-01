##################################
# Hollyer et al. (2022)
# Replication Cross-National Analysis 
# Stata to R
# 18.03.23
##################################

#file.rename(here::here("original_materials", "main_paper", 
#                       "cross-national-data.tab"),
#            here::here("original_materials", "main_paper", 
#                       "cross-national-data.dta"))

cnat.data <- haven::read_dta(
    here::here("original_materials", "main_paper", "cross-national-data.dta"))

############
# Figure 3
############


#Personalism
fig3l <- ggplot(data = cnat.data, aes(ev_total, personal)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Personalism", x = "Volatility Index", y = "Personalism Score") 

#Programmaticness
fig3r <- ggplot(data = cnat.data, aes(ev_total, programmatic)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Programmaticness",x = "Volatility Index", y = "Programaticness Score")

fig3 <- gridExtra::grid.arrange(fig3l, fig3r, nrow=1)

ggsave(here::here("outputs", "figs", "original-paper-fig3.pdf"), fig3,
       width = 14,
       height = 8)

rm(fig3l, fig3r, fig3)

############
# Figure 4
############

#split data
non.ethnic <- cnat.data |>
  dplyr::filter(ethnic == 0)

ethnic <- cnat.data |>
  dplyr::filter(ethnic == 1)

#Personalism Non-Ethnic
fig4tl <- ggplot(data = non.ethnic, aes(ev_total, personal)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Personalism", subtitle = "Non-Ethnic Parties",x = "Volatility Index", y = "Personalism Score")

#Personalism Ethnic
fig4tr <- ggplot(data = ethnic, aes(ev_total, personal)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Personalism", subtitle = "Ethnic Parties",x = "Volatility Index", y = "Personalism Score")

#Programmaticism Non-Ethnic
fig4bl <- ggplot(data = non.ethnic, aes(ev_total, programmatic)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Progammaticness", subtitle = "Non-Ethnic Parties",x = "Volatility Index", y = "Progammaticness Score")

#Programmaticism Ethnic
fig4br <-ggplot(data = ethnic, aes(ev_total, programmatic)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Progammaticness", subtitle = "Ethnic Parties",
       x = "Volatility Index", y = "Progammaticness Score")

fig4 <- gridExtra::grid.arrange(fig4tl, fig4tr, fig4bl, fig4br, nrow = 2)

ggsave(here::here("outputs", "figs", "original-paper-fig4.pdf"), fig4,
       width = 14,
       height = 14)

rm(fig4tl, fig4tr,fig4bl, fig4br, fig4)

############
# Figure 5
############

moderate <- cnat.data |>
  dplyr::filter(extremist2 <= 3)

extreme <- cnat.data |>
  dplyr::filter(extremist2 >= 3)

#Personalism moderate
fig5tl <- ggplot(data = moderate, aes(ev_total, personal)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Personalism", subtitle = "Moderate Parties",
       x = "Volatily Index", y = "Personalism Score")

#Personalism extreme
fig5tr <- ggplot(data = extreme, aes(ev_total, personal)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Personalism", subtitle = "Extreme Parties",
       x = "Volatily Index", y = "Personalism Score")

#Programmaticism moderate
fig5bl <- ggplot(data = moderate, aes(ev_total, programmatic)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Progrmmaticness", subtitle = "Moderate Parties",
       x = "Volatily Index", y = "Programmaticness Score")

#Programmaticism Ethnic
fig5br <- ggplot(data = extreme, aes(ev_total, programmatic)) + 
  geom_hline(yintercept = 2.5, color = "grey", size=0.5) + 
  geom_vline(xintercept = 0.4, color = "grey", size=0.5) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = TRUE, size = 0.5, color = "red") +
  xlim(0,0.8) + ylim(1,4) +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) +
  labs(title = "Progrmmaticness", subtitle = "Extreme Parties",
       x = "Volatily Index", y = "Programmaticness Score")


fig5 <- gridExtra::grid.arrange(fig5tl, fig5tr, fig5bl, fig5br, nrow = 2)

ggsave(here::here("outputs", "figs", "original-paper-fig5.pdf"), fig5,
       width = 14,
       height = 14)

rm(fig5tl, fig5tr,fig5bl, fig5br, fig5)



############
# Parametric Analyses
############
#SEs clustered by country

cnat.data <- cnat.data %>%
  mutate(extremist.bin = ifelse(extremist2 >= 3, 1, 0))

#Hypo 1
lm.personal.base <- lm_robust(personal ~ ev_total + 
                    partysize + unions + business + religious +
                    pluralty + pr + factor(cl) + mdmh + factor(system) + demsys + polity + gdppc + gini + ethfrac,
                  data = cnat.data,
                  clusters = ccode)

lm.program.base <- lm_robust(programmatic ~ ev_total + 
                               partysize + unions + business + religious +
                               pluralty + pr + factor(cl) + mdmh + factor(system) + demsys + polity + gdppc + gini + ethfrac,
                             data = cnat.data,
                             clusters = ccode)

#Hypo 2 & 3
lm.personal.ideo <- lm_robust(personal ~ ev_total*extremist2 + 
                         partysize + unions + business + religious +
                         pluralty + pr + factor(cl) + mdmh + factor(system) + demsys + polity + gdppc + gini + ethfrac,
                         data = cnat.data,
                         clusters = ccode)

lm.program.ideo <- lm_robust(programmatic ~ ev_total*extremist2 + 
                        partysize + unions + business + religious +
                        pluralty + pr + factor(cl) + mdmh + factor(system) + demsys + polity + gdppc + gini + ethfrac,
                        data = cnat.data,
                        clusters = ccode)

lm.personal.ethn <- lm_robust(personal ~ ev_total*ethnic + 
                                partysize + unions + business + religious +
                                pluralty + pr + factor(cl) + mdmh + factor(system) + demsys + polity + gdppc + gini + ethfrac,
                              data = cnat.data,
                              clusters = ccode)

lm.program.ethn <- lm_robust(programmatic ~ ev_total*ethnic + 
                               partysize + unions + business + religious +
                               pluralty + pr + factor(cl) + mdmh + factor(system) + demsys + polity + gdppc + gini + ethfrac,
                             data = cnat.data,
                             clusters = ccode)


#Calculate Predicted Values

#total
predict.personal <- ggpredict(lm.personal.base, terms = "ev_total[quart2]")
predict.program <- ggpredict(lm.program.base, terms = "ev_total[quart2]")

#ethnicity

predict.personal.ethnic <- ggpredict(lm.personal.ethn, terms = c("ev_total[quart2]", "ethnic"))
predict.program.ethnic <- ggpredict(lm.program.ethn, terms =  c("ev_total[quart2]", "ethnic"))

#ideology

predict.personal.ideo.low <- ggpredict(lm.personal.ideo, terms = c("ev_total[quart2]", "extremist2[0]"))
predict.personal.ideo.high <- ggpredict(lm.personal.ideo, terms = c("ev_total[quart2]", "extremist2[3]"))
predict.program.ideo.low <- ggpredict(lm.program.ideo, terms =  c("ev_total[quart2]", "extremist2[0]"))
predict.program.ideo.high <- ggpredict(lm.program.ideo, terms =  c("ev_total[quart2]", "extremist2[3]"))

#put into table

pred.probs <- matrix(nrow = 5, ncol = 8, 
                     dimnames = list(c("Overall", "Nonethnic", "Ethnic", "Moderate", "Extremist"),
                                     c("Low.Personal", "SE", "High.Personal", "SE", "Low.Program", "SE", "High.Program", "SE")))
#pred values
pred.probs[1,c(1, 3)] <- predict.personal$predicted[c(1, 3)]
pred.probs[1,c(5, 7)] <- predict.program$predicted[c(1, 3)]
pred.probs[2,c(1, 3)] <- predict.personal.ethnic$predicted[c(1, 3)]
pred.probs[2,c(5, 7)] <- predict.program.ethnic$predicted[c(1, 3)]
pred.probs[3,c(1, 3)] <- predict.personal.ethnic$predicted[c(4, 6)]
pred.probs[3,c(5, 7)] <- predict.program.ethnic$predicted[c(4, 6)]
pred.probs[4,c(1, 3)] <- predict.personal.ideo.low$predicted[c(1, 3)]
pred.probs[4,c(5, 7)] <- predict.program.ideo.low$predicted[c(1, 3)]
pred.probs[5,c(1, 3)] <- predict.personal.ideo.high$predicted[c(1, 3)]
pred.probs[5,c(5, 7)] <- predict.program.ideo.high$predicted[c(1, 3)]

#SEs
pred.probs[1,c(2, 4)] <- predict.personal$std.error[c(1, 3)]
pred.probs[1,c(6, 8)] <- predict.program$std.error[c(1, 3)]
pred.probs[2,c(2, 4)] <- predict.personal.ethnic$std.error[c(1, 3)]
pred.probs[2,c(6, 8)] <- predict.program.ethnic$std.error[c(1, 3)]
pred.probs[3,c(2, 4)] <- predict.personal.ethnic$std.error[c(4, 6)]
pred.probs[3,c(6, 8)] <- predict.program.ethnic$std.error[c(4, 6)]
pred.probs[4,c(2, 4)] <- predict.personal.ideo.low$std.error[c(1, 3)]
pred.probs[4,c(6, 8)] <- predict.program.ideo.low$std.error[c(1, 3)]
pred.probs[5,c(2, 4)] <- predict.personal.ideo.high$std.error[c(1, 3)]
pred.probs[5,c(6, 8)] <- predict.program.ideo.high$std.error[c(1, 3)]


#calculate differences

pred.probs <- cbind(pred.probs, pred.probs[,"High.Personal"] - pred.probs[,"Low.Personal"])
pred.probs <- cbind(pred.probs, pred.probs[,"High.Program"] - pred.probs[,"Low.Program"])
colnames(pred.probs)[9] <- "diff.personal"
colnames(pred.probs)[10] <- "diff.personal"

#check how SE of differences are calculated
se.diff.personal <- sqrt(pred.probs[, 2]^2+pred.probs[, 4]^2)
se.diff.program <- sqrt(pred.probs[, 6]^2+pred.probs[, 8]^2)
pred.probs <- cbind(pred.probs, se.diff.personal)
pred.probs <- cbind(pred.probs, se.diff.program)
colnames(pred.probs)[11] <- "se.diff.personal"
colnames(pred.probs)[12] <- "se.diff.program"


######################
# further predicted probabilities to check thresholds of extremism variable
######################

#Personalism

range.ideo.personal <- ggpredict(lm.personal.ideo, terms = c("extremist2", "ev_total[quart2]"))
ggplot(range.ideo.personal, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  facet_wrap(~group)+
  ggtitle("Predicted Personalism Scores across Ideology Range at Volatility Quantiles") +
  xlab("Ideology Extremism") +
  ylab("Predicted Values") +
  theme_minimal()

#Programmaticism

range.ideo.programatic <- ggpredict(lm.program.ideo, terms = c("extremist2", "ev_total[quart2]"))
ggplot(range.ideo.programatic, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  facet_wrap(~group)+
  ggtitle("Predicted Programmaticism Scores across Ideology Range at Volatility Quantiles") +
  xlab("Ideology Extremism") +
  ylab("Predicted Values") +
  theme_minimal()

                  

sessionInfo()
