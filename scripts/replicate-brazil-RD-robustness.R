##################################
# Hollyer et al. (2022)
# Replication and Additional Robustness checks for RD case study 
# 18.03.23
##################################

#file.rename(here::here("original_materials", "main_paper", 
#                       "brazil-RD-data.tab"),
#            here::here("original_materials", "main_paper", 
#                       "brazil-RD-data.dta"))

data <- haven::read_dta(here::here("original_materials", "main_paper", 
                                   "brazil-RD-data.dta"))

#######################################
# Replication of Figure 6, as in paper 
#######################################

data_noducks <- data[data$dlameduck_runs == 0, ]

fig6l <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
       data_noducks$mv_incparty, c = 0, ci = FALSE, 
       binselect = "esmv", 
       subset = abs(data_noducks$mv_incparty)<50, 
       p = 2)

fig6r <- rdrobust::rdplot(data_noducks$outyg1F_inc_for1, 
       data_noducks$mv_incparty, c = 0, ci = FALSE, 
       binselect = "esmv", 
       subset = abs(data_noducks$mv_incparty)<50, 
       p = 2)

fig6l <- fig6l$rdplot +
  labs(title = "(b) Young Outsider",
       x = "Incumbent party's margin of victory at t",
       y = "Incumbent party's candidate t+1 young indicator") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank())


fig6r <- fig6r$rdplot +
  labs(title = "(a) Young",
       x = "Incumbent party's margin of victory at t",
       y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank())



fig6 <- gridExtra::grid.arrange(fig6l, fig6r, nrow=1)


ggsave(here::here("outputs", "figs", "original-paper-fig6.pdf"), fig6,
       width = 14,
       height = 8)

rm(fig6l, fig6r, fig6)

#######################################
# Replicate Figure 6, with Polynomial 1 (6.1)
#######################################

fig6l.1 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
       data_noducks$mv_incparty, c = 0, ci = FALSE, 
       binselect = "esmv", 
       subset = abs(data_noducks$mv_incparty)<50, 
       p = 1)

fig6r.1 <- rdrobust::rdplot(data_noducks$outyg1F_inc_for1, 
       data_noducks$mv_incparty, c = 0, ci = FALSE, 
       binselect = "esmv", 
       subset = abs(data_noducks$mv_incparty)<50, 
       p = 1)

fig6l.1 <- fig6l.1$rdplot +
  labs(title = "(b) Young Outsider, 1st Polynomial",
       x = "Incumbent party's margin of victory at t",
       y = "Incumbent party's candidate t+1 young indicator") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank())


fig6r.1 <- fig6r.1$rdplot +
  labs(title = "(a) Young, 1st Polynomial",
       x = "Incumbent party's margin of victory at t",
       y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank())



fig6.1 <- gridExtra::grid.arrange(fig6l.1, fig6r.1, nrow=1)


ggsave(here::here("outputs", "figs", "original-paper-fig6_1.pdf"), fig6.1,
       width = 14,
       height = 8)

rm(fig6l.1, fig6r.1, fig6.1)

#########
#Fig 6 Matrix bins 
#########


fig6.2.1 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                 data_noducks$mv_incparty, c = 0, ci = FALSE, 
                 binselect = "esmv", 
                 p = 2)
fig6.2.1 <- fig6.2.1$rdplot + labs(title = "esmv binning", subtitle = "2nd Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))

fig6.2.2 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                 data_noducks$mv_incparty, c = 0, ci = FALSE, 
                 binselect = "esmv", 
                 p = 1)

fig6.2.2 <- fig6.2.2$rdplot + labs(title = "esmv binning", subtitle = "1st Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))



fig6.2.3 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "es", 
                             p = 2)
fig6.2.3 <- fig6.2.3$rdplot + labs(title = "es binning", subtitle = "2nd Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))

fig6.2.4 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "es", 
                             p = 1)

fig6.2.4 <- fig6.2.4$rdplot + labs(title = "es binning", subtitle = "1st Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))


fig6.2.5 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "espr", 
                             p = 2)
fig6.2.5 <- fig6.2.5$rdplot + labs(title = "espr binning", subtitle = "2nd Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))

fig6.2.6 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "espr", 
                             p = 1)

fig6.2.6 <- fig6.2.6$rdplot + labs(title = "espr binning", subtitle = "1st Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))




fig6.2.7 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "esmvpr", 
                             p = 2)
fig6.2.7 <- fig6.2.7$rdplot + labs(title = "esmvpr binning", subtitle = "2nd Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))
                                   

fig6.2.8 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "esmvpr", 
                             p = 1)

fig6.2.8 <- fig6.2.8$rdplot + labs(title = "esmvpr binning", subtitle = "1st Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))


fig6.2.9 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "qs", 
                             p = 2)
fig6.2.9 <- fig6.2.9$rdplot + labs(title = "qs binning", subtitle = "2nd Polynomial",
                                   x = "Incumbent party's margin of victory at t",
                                   y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))

fig6.2.10 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "qs", 
                             p = 1)

fig6.2.10 <- fig6.2.10$rdplot + labs(title = "qs binning", subtitle = "1st Polynomial",
                                     x = "Incumbent party's margin of victory at t",
                                     y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))


fig6.2.11 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                             data_noducks$mv_incparty, c = 0, ci = FALSE, 
                             binselect = "qspr", 
                             p = 2)
fig6.2.11 <- fig6.2.11$rdplot + labs(title = "qspr binning", subtitle = "2nd Polynomial",
                                     x = "Incumbent party's margin of victory at t",
                                     y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))

fig6.2.12 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                              data_noducks$mv_incparty, c = 0, ci = FALSE, 
                              binselect = "qspr",
                              p = 1)

fig6.2.12 <- fig6.2.12$rdplot + labs(title = "qspr binning", subtitle = "1st Polynomial",
                                     x = "Incumbent party's margin of victory at t",
                                     y = "Incumbent party's candidate t+1 young indicator (original report)") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        title = element_text(size = 8),
        axis.text = element_text(size = 6))


fig6.2.13 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                              data_noducks$mv_incparty, c = 0, ci = FALSE, 
                              binselect = "qsmv",
                              p = 2)
fig6.2.13 <- fig6.2.13$rdplot + labs(title = "qsmv binning", subtitle = "2nd Polynomial")

fig6.2.14 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                              data_noducks$mv_incparty, c = 0, ci = FALSE, 
                              binselect = "qsmv",
                              p = 1)

fig6.2.14 <- fig6.2.14$rdplot + labs(title = "qsmv binning", subtitle = "1st Polynomial")


fig6.2.15 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                              data_noducks$mv_incparty, c = 0, ci = FALSE, 
                              binselect = "qsmvpr", 
                              p = 2)
fig6.2.15 <- fig6.2.15$rdplot + labs(title = "qsmvpr binning", subtitle = "2nd Polynomial")

fig6.2.16 <- rdrobust::rdplot(data_noducks$youngF_inc_for1, 
                              data_noducks$mv_incparty, c = 0, ci = FALSE, 
                              binselect = "qsmvpr", 
                              p = 1)

fig6.2.16 <- fig6.2.16$rdplot + labs(title = "qsmvpr binning", subtitle = "1st Polynomial")


fig6.2 <- gridExtra::grid.arrange(fig6.2.1, fig6.2.2, fig6.2.3, fig6.2.4, 
                        fig6.2.5, fig6.2.6, fig6.2.7, fig6.2.8,
                        fig6.2.9, fig6.2.10, fig6.2.11, fig6.2.12, 
                        fig6.2.13, fig6.2.14, fig6.2.15, fig6.2.16,
                        nrow= 8,
                        top = "Replication of Fig 6(a) with different binning methods")

ggsave(here::here("outputs", "figs", "original-paper-fig6_2.pdf"), fig6.2,
       width = 8,
       height = 30)
