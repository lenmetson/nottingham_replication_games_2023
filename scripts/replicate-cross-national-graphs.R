##################################
# Hollyer et al. (2022)
# Replication Cross-National Analysis 
# Stata to R
# 18.03.23
# Angela Odermatt, Lennard Metson, Edmund Kelly
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
  dplyr::filter(extremist2 < 3)

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

#summary statistics
sum.stats <- cnat.data %>%
  mutate(presidential = ifelse(system == 0, 1, 0),
         parliamentary = ifelse(system == 2, 1, 0),
         assembly = ifelse(system == 1, 1, 0),
         closed.list = ifelse(cl == 1, 1, 0),
         open.list = ifelse(cl == 2, 1, 0),
         non.list = ifelse(cl == 3, 1, 0)) %>%
  select(-ccode, -system, -cl) %>%
  summarise(across(everything(), 
                   list(mean = ~mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE), 
                        max = ~max(.x, na.rm = TRUE), min = ~min(.x, na.rm = TRUE),
                        no.values = ~ sum(!is.na(.x))))) %>%
  pivot_longer(cols = c(ends_with("mean"), ends_with("sd"), ends_with("max"), ends_with("min"), ends_with("no.values")), 
               names_to = c('Variables', '.value'), 
               names_pattern = c("(.*)_(.*)"))


print(xtable(sum.stats,
       digits = c(2, 2, 2, 2, 2, 2, 2),
       align = c("l", "c", "c", "c", "c", "c", "c")), 
      include.rownames=FALSE)



#linear models
#factor variables
cnat.data$cl <- factor(case_when(cnat.data$cl == 1 ~ "closed list",
                                 cnat.data$cl == 2 ~ "open list",
                                 TRUE ~ "non list")) %>%
  relevel(ref = "non list")

cnat.data$system <- factor(case_when(cnat.data$system == 0 ~ "presidential",
                                cnat.data$system == 2 ~ "parliamentary",
                                TRUE ~ "assembly")) %>%
  relevel(ref = "assembly")


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


##########
# Table D 1
#######
texreg(list(lm.personal.base, lm.program.base, 
            lm.personal.ideo, lm.program.ideo, 
            lm.personal.ethn, lm.program.ethn),
       stars = c(0.01, 0.05, 0.1),
       include.ci = F,
       fontsize = "tiny",
       custom.header = list("Baseline Model" = 1:2, "Extended Model" = 3:6),
       custom.model.names = c("Personalism", "Programmaticness", 
                              "Personalism", "Programmaticness", 
                              "Personalism", "Programmaticness"),
       caption = "Replication of Table D1 in the Appendix",
       booktabs = T,
       float.pos = "p")


##########
#Calculate Predicted Values
##########

#################
#Personalism
#################

##overall
predict.personal.base <- avg_predictions(lm.personal.base, 
                                         variables = list(ev_total = c(0.1510, 0.4197)))
diff.personal.base <- avg_comparisons(lm.personal.base, 
                                      variables = list(ev_total = c(0.1510, 0.4197)))

##ethnicity
#nonethnic parties
ndat <- cnat.data
ndat$ethnic <- 0

predict.personal.nonethn <- avg_predictions(lm.personal.ethn, 
                                         newdata = ndat, 
                                         variables = list(ev_total = c(0.1510, 0.4197)))
diff.personal.nonethn <- avg_comparisons(lm.personal.ethn, 
                                      newdata = ndat,
                                      variables = list(ev_total = c(0.1510, 0.4197)))
#ethnic parties
ndat <- cnat.data
ndat$ethnic <- 1
predict.personal.ethn <- avg_predictions(lm.personal.ethn, 
                                         newdata = ndat, 
                                         variables = list(ev_total = c(0.1510, 0.4197)))
diff.personal.ethn <- avg_comparisons(lm.personal.ethn, 
                                      newdata = ndat, 
                                      variables = list(ev_total = c(0.1510, 0.4197)))

##difference ethnic -- non-ethnic
#low volatility
ndat <- cnat.data
ndat$ev_total <- 0.1510
diff.personal.ethn.low <- avg_comparisons(lm.personal.ethn, 
                                      newdata = ndat,  
                                      variables = list(ethnic = c(0, 1)))

#high volatility
ndat <- cnat.data
ndat$ev_total <- 0.4197
diff.personal.ethn.high <- avg_comparisons(lm.personal.ethn, 
                                      newdata = ndat,  
                                      variables = list(ethnic = c(0, 1)))

##Ideology
#moderate parties
ndat <- cnat.data
ndat$extremist2 <- 0

predict.personal.mod <- avg_predictions(lm.personal.ideo, 
                                            newdata = ndat, 
                                            variables = list(ev_total = c(0.1510, 0.4197)))
diff.personal.mod <- avg_comparisons(lm.personal.ideo, 
                                         newdata = ndat,
                                         variables = list(ev_total = c(0.1510, 0.4197)))
#extremist parties
ndat <- cnat.data
ndat$extremist2 <- 3
predict.personal.ext <- avg_predictions(lm.personal.ideo, 
                                         newdata = ndat, 
                                         variables = list(ev_total = c(0.1510, 0.4197)))
diff.personal.ext <- avg_comparisons(lm.personal.ideo, 
                                      newdata = ndat, 
                                      variables = list(ev_total = c(0.1510, 0.4197)))

##difference moderate -- extremist
#low volatility
ndat <- cnat.data
ndat$ev_total <- 0.1510
diff.personal.ideo.low <- avg_comparisons(lm.personal.ideo, 
                                          newdata = ndat,  
                                          variables = list(extremist2 = c(0, 3)))

#high volatility
ndat <- cnat.data
ndat$ev_total <- 0.4197
diff.personal.ideo.high <- avg_comparisons(lm.personal.ideo, 
                                           newdata = ndat,  
                                           variables = list(extremist2 = c(0, 3)))


#################
#Programmaticness
#################

##overall
predict.program.base <- avg_predictions(lm.program.base, 
                                         variables = list(ev_total = c(0.1510, 0.4197)))
diff.program.base <- avg_comparisons(lm.program.base, 
                                      variables = list(ev_total = c(0.1510, 0.4197)))

##ethnicity
#nonethnic parties
ndat <- cnat.data
ndat$ethnic <- 0

predict.program.nonethn <- avg_predictions(lm.program.ethn, 
                                            newdata = ndat, 
                                            variables = list(ev_total = c(0.1510, 0.4197)))
diff.program.nonethn <- avg_comparisons(lm.program.ethn, 
                                         newdata = ndat,
                                         variables = list(ev_total = c(0.1510, 0.4197)))
#ethnic parties
ndat <- cnat.data
ndat$ethnic <- 1
predict.program.ethn <- avg_predictions(lm.program.ethn, 
                                         newdata = ndat, 
                                         variables = list(ev_total = c(0.1510, 0.4197)))
diff.program.ethn <- avg_comparisons(lm.program.ethn, 
                                      newdata = ndat, 
                                      variables = list(ev_total = c(0.1510, 0.4197)))

##difference ethnic -- non-ethnic
#low volatility
ndat <- cnat.data
ndat$ev_total <- 0.1510
diff.program.ethn.low <- avg_comparisons(lm.program.ethn, 
                                          newdata = ndat,  
                                          variables = list(ethnic = c(0, 1)))

#high volatility
ndat <- cnat.data
ndat$ev_total <- 0.4197
diff.program.ethn.high <- avg_comparisons(lm.program.ethn, 
                                           newdata = ndat,  
                                           variables = list(ethnic = c(0, 1)))


##Ideology
#moderate parties
ndat <- cnat.data
ndat$extremist2 <- 0

predict.program.mod <- avg_predictions(lm.program.ideo, 
                                        newdata = ndat, 
                                        variables = list(ev_total = c(0.1510, 0.4197)))
diff.program.mod <- avg_comparisons(lm.program.ideo, 
                                     newdata = ndat,
                                     variables = list(ev_total = c(0.1510, 0.4197)))
#extremist parties
ndat <- cnat.data
ndat$extremist2 <- 3
predict.program.ext <- avg_predictions(lm.program.ideo, 
                                        newdata = ndat, 
                                        variables = list(ev_total = c(0.1510, 0.4197)))
diff.program.ext <- avg_comparisons(lm.program.ideo, 
                                     newdata = ndat, 
                                     variables = list(ev_total = c(0.1510, 0.4197)))

##difference moderate -- extremist
#low volatility
ndat <- cnat.data
ndat$ev_total <- 0.1510
diff.program.ideo.low <- avg_comparisons(lm.program.ideo, 
                                          newdata = ndat,  
                                          variables = list(extremist2 = c(0, 3)))

#high volatility
ndat <- cnat.data
ndat$ev_total <- 0.4197
diff.program.ideo.high <- avg_comparisons(lm.program.ideo, 
                                           newdata = ndat,  
                                           variables = list(extremist2 = c(0, 3)))


##put into table

pred.probs <- matrix(ncol = 4, nrow = 10,
                     dimnames = list(c("Overall", "Overall.SE", "Nonethnic", "Nonethnic.SE",
                                       "Ethnic", "Ethnic.SE", "Moderate", "Moderate.SE",
                                       "Extremist", "Extremist.SE"),
                                     c("Low.Volatility.Person", "High.Volatility.Person", 
                                       "Low.Volatility.Program", "High.Volatility.Program")))

#personal
p.mods <- c("predict.personal.base", "predict.personal.nonethn", "predict.personal.ethn",
            "predict.personal.mod", "predict.personal.ext")

for (m in 1:length(p.mods)){
  
  df <- get(p.mods[m])
  
  if(m>1){i=(m*2)-1}
  if(m==1){i=m}
  
  pred.probs[i, 1] <- df[1, 2]
  pred.probs[i, 2] <- df[2, 2]
  pred.probs[i + 1, 1] <- df[1, 3]
  pred.probs[i + 1, 2] <- df[2, 3]
  
}

#program
p.mods <- c("predict.program.base", "predict.program.nonethn", "predict.program.ethn",
            "predict.program.mod", "predict.program.ext")

for (m in 1:length(p.mods)){
  
  df <- get(p.mods[m])
  
  if(m>1){i=(m*2)-1}
  if(m==1){i=m}
  
  pred.probs[i, 3] <- df[1, 2]
  pred.probs[i, 4] <- df[2, 2]
  pred.probs[i + 1, 3] <- df[1, 3]
  pred.probs[i + 1, 4] <- df[2, 3]
  
}


#differences
pred.probs <- cbind(pred.probs[, 1:2], Differences.Person = " ", pred.probs[, 3:ncol(pred.probs)])
pred.probs <- cbind(pred.probs[, 1:5], Differences.Program = " ")

#Personalism
diff.mods <- c("diff.personal.base", "diff.personal.nonethn", "diff.personal.ethn",
               "diff.personal.mod", "diff.personal.ext")

for (m in 1:length(p.mods)){
  
  df <- get(diff.mods[m])
  
  if(m>1){i=(m*2)-1}
  if(m==1){i=m}
  
  pred.probs[i, 3] <- df[1, 3]
  pred.probs[i + 1, 3] <- df[1, 4]
  
}

#Programmaticness
diff.mods <- c("diff.program.base", "diff.program.nonethn", "diff.program.ethn",
               "diff.program.mod", "diff.program.ext")

for (m in 1:length(p.mods)){
  
  df <- get(diff.mods[m])
  
  if(m>1){i=(m*2)-1}
  if(m==1){i=m}
  
  pred.probs[i, 6] <- df[1, 3]
  pred.probs[i + 1, 6] <- df[1, 4]
  
}


pred.probs <- rbind(pred.probs[1:6,], Differences.Ethnic = " ", Differences.Ethnic.SE = " ",pred.probs[7:10,])
pred.probs <- rbind(pred.probs[1:12,], Differences.Ideo = " ", Differences.Ideo.SE = " ")

diff.mods <- c("diff.personal.ethn.low", "diff.personal.ethn.high",
               "diff.program.ethn.low", "diff.program.ethn.high")

for (m in 1:length(diff.mods)){
  
  df <- get(diff.mods[m])
  
  if(m > 2){i=m+1}
  if(m <= 2){i=m}
  
  pred.probs[7, i] <- df[1, 3]
  pred.probs[8, i] <- df[1, 4]
}

diff.mods <- c("diff.personal.ideo.low", "diff.personal.ideo.low",
               "diff.program.ideo.low","diff.program.ideo.high")

for (m in 1:length(diff.mods)){
  
  df <- get(diff.mods[m])
  
  if(m > 2){i=m+1}
  if(m <= 2){i=m}
  
  pred.probs[13, i] <- df[1, 3]
  pred.probs[14, i] <- df[1, 4]
}

#print to tex
pred.probs <- as.data.frame(pred.probs)
pred.probs[] <- sapply(pred.probs, as.numeric)
xtable(pred.probs, caption = "Replication of Table 1",
       digits = c(2, 2, 2, 2, 2, 2, 2),
       align = c("l", "c", "c", "c", "c", "c", "c"))


######################
# further predicted probabilities to check thresholds of extremism variable
######################

#Personalism

range.ideo.personal <- ggpredict(lm.personal.ideo, terms = c("extremist2", "ev_total[quart2]"))
personal.ideo <- ggplot(range.ideo.personal, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  facet_wrap(~group)+
  xlab("Ideology Extremism") +
  ylab("Predicted Values") +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank())

ggsave(here::here("outputs", "figs", "new-personal-ideo.pdf"), personal.ideo,
       width = 14,
       height = 8)

#Programmaticism

range.ideo.programatic <- ggpredict(lm.program.ideo, terms = c("extremist2", "ev_total[quart2]"))
program.ideo <- ggplot(range.ideo.programatic, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  facet_wrap(~group)+
  xlab("Ideology Extremism") +
  ylab("Predicted Values") +
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank())


ggsave(here::here("outputs", "figs", "new-program-ideo.pdf"), program.ideo,
       width = 14,
       height = 8)


session.info()
