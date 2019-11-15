##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-12

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(ggalt); library(cowplot)})

DT.hh2000 <- fread("../Data/Data_Census/data-raw-zip/DEC_00_SF1_H001_with_ann.csv",
                   skip = 1) %>%
  .[, .(zip = Id2, hh2000 = Total)] %>%
  .[, .(hh2000 = sum(hh2000)), keyby = .(zip3 = substr(zip, 1, 3))]



DT.cw.zip3.to.state <-
  readRDS("../Data/Data_Crosswalks/RdsFiles/10-DT_cw_zip3_to_state.rds")


DT <- list.files("../../R_CFPL_Step1_Default_CsvFiles/040-delin0_to_delin90/",
                 full.names = TRUE) %>%
  lapply(fread) %>%
  rbindlist(use.names = TRUE) %>%
  .[, zip3 := sprintf("%03.f", as.numeric(idx))] %>%
  merge(DT.cw.zip3.to.state, by = "zip3", all.x = TRUE) %>%
  merge(DT.hh2000, by = "zip3", all.x = TRUE) %>%
  .[!is.na(state) & !is.na(hh2000)] %>%
  .[state %chin% c("04", "06", "32")] %>%
  .[, CA := as.integer(state == "06")] %>%
  .[period <= "2008-12-01"] %>%
  .[, period := factor(period)] %>%
  .[, period := relevel(period, "2008-06-01")]


DT.stragetic.default <- felm(effect ~ CA * period | zip3 + period | 0 | zip3,
                             DT, weights = DT$hh2000) %>%
  broom::tidy(.) %>% setDT %>%
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000] %>%
  .[grepl("CA:period", x = term)] %>%
  .[, period := gsub("^CA:period", "", x = term)] %>%
  .[, period := as.Date(period)]

p.stragetic.default <- ggplot(DT.stragetic.default, aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date(c("2008-10-01")),
             color = "green", linetype = "dashed",
             size = 1.01) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-30")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_ribbon(aes(ymin = estimate - 1.96 * std.error,
                  ymax = estimate + 1.96 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  labs(title = "Strategic Default Transition Probabilities - From Current Straight to 90 Days Delinquent",
    ##title = "A: Probability of Transition to REO Foreclosure for Loans 90 Days Delinquent Prior to the CFPLs",
       subtitle = "PLS Mortgages - Monthly Difference-in-Differences Estimates",
       y = "Probability of Rolling Straight from Current to\n90 Days Delinquent (Basis Points)") +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(axis.title.x = element_blank(),
        ##axis.title.y = element_blank(),
        plot.margin = unit(c(6,6,6,6), "pt"),
        plot.title = element_text(hjust = 0))

ggsave(filename = "PlotsFinal/030-p_trans_prob_strategic_default_current_straight_to_delin90.pdf",
       plot = p.stragetic.default,
       width = 9.25, height = 5.2)


DT.delin.strategic.default.occ <- fread("../../R_CFPL_Step1_Default_CsvFiles/041-delin0_to_delin90_occ/041-occ_delin_gdp.csv") %>%
  .[, period := gsub("occ.dummyOCC:ACTIVITYDT", "", x = term)] %>%
  .[, period := as.Date(period)]

p.stragetic.default.occ <- ggplot(DT.delin.strategic.default.occ, aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date(c("2008-10-01")),
             color = "green", linetype = "dashed",
             size = 1.01) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-30")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_ribbon(aes(ymin = estimate - 1.96 * std.error,
                  ymax = estimate + 1.96 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  labs(title = "Owner Occupied Strategic Default Transition Probabilities - From Current Straight to 90 Days Delinquent",
    ##title = "A: Probability of Transition to REO Foreclosure for Loans 90 Days Delinquent Prior to the CFPLs",
       subtitle = "California Only PLS Mortgages - Monthly Difference-in-Differences Estimates",
       y = "Probability of Rolling Straight from Current to\n90 Days Delinquent (Basis Points)") +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(axis.title.x = element_blank(),
        ##axis.title.y = element_blank(),
        plot.margin = unit(c(6,6,6,6), "pt"),
        plot.title = element_text(hjust = 0))

ggsave(filename = "PlotsFinal/030-p_occ_ca_only_trans_prob_strategic_default_current_straight_to_delin90.pdf",
       plot = p.stragetic.default,
       width = 9.25, height = 5.2)
