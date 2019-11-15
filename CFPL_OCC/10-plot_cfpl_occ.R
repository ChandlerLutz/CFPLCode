##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-07

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(cowplot); library(ggalt)})

DT.forc.start <- fread("data-raw/R_CFPL_OCC_CsvFiles/010-DT_est_forc_start.csv") %>%
  .[grepl("occ.dummyOCC:ACTIVITYDT", x = term)] %>%
  .[, date := gsub(".*([0-9]{4}-[0-9]{2}-01)$", "\\1", x = term)] %>%
  .[, date := as.Date(date)] %>%
  .[date >= "2008-01-01"] %>%
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000]

p.forc.start <- ggplot(DT.forc.start, aes(x = date, y = estimate)) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-15")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = estimate - 2.5 * std.error,
                  ymax = estimate + 2.5 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  scale_x_date(breaks = as.Date(c("2008-01-01","2009-01-01", "2010-01-01")),
               date_labels = "%Y") +
  labs(title = "A: Foreclosure Start Probabilities for Owner Occupied versus Non-Owner Occupied Homes",
       subtitle = "California Only PLS Mortgages: Monthly Difference-in-Differences Estimates",
       y = "Monthly Probability of Foreclosure Start\n(Basis Points)") +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(6,20,6,6), "pt"),
        plot.title = element_text(hjust = 0))

p.forc.start


##ggsave(p.forc.start, "PlotsFinal/10-p_occ_dd_forc_start.R")

DT.mod <- fread("data-raw/R_CFPL_OCC_CsvFiles/010-DT_est_mod.csv") %>%
  .[grepl("occ.dummyOCC:ACTIVITYDT", x = term)] %>%
  .[, date := gsub(".*([0-9]{4}-[0-9]{2}-01)$", "\\1", x = term)] %>%
  .[, date := as.Date(date)] %>%
  .[date >= "2008-01-01"] %>%
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000]
p.mod <- ggplot(DT.mod, aes(x = date, y = estimate)) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-15")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = estimate - 2.5 * std.error,
                  ymax = estimate + 2.5 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  scale_x_date(breaks = as.Date(c("2008-01-01","2009-01-01", "2010-01-01")),
               date_labels = "%Y") +
  labs(title = "B: Modification Probabilities for Owner Occupied vs. Non-Owner Occupied Homes",
       subtitle = "California Only PLS Mortgages: Monthly Difference-in-Differences estimates",
       y = "Monthly Probability of Modification\n(Basis Points)") +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(6,20,6,6), "pt"),
        plot.title = element_text(hjust = 0))


p.all <- plot_grid(p.forc.start, p.mod,
                   align = "hv", nrow = 2)

ggsave(filename = "PlotsFinal/10-p_occ_vs_non_occ_ca_forc_start_mod.pdf",
       plot = p.all, width = 9.5, height = 8.5)
