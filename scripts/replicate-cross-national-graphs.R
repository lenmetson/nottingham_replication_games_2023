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

#split data => HOW DID THEY SPLIT THE DATA???
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

sessionInfo()
