##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-08


##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(cowplot); library(ggalt)})


DT.hh2000 <- fread("../Data/Data_Census/data-raw-zip/DEC_00_SF1_H001_with_ann.csv",
                   skip = 1) %>%
  .[, .(zip = Id2, hh2000 = Total)] %>%
  .[, .(hh2000 = sum(hh2000)), keyby = .(zip3 = substr(zip, 1, 3))]

DT.cw.zip3.to.state <-
  readRDS("../Data/Data_Crosswalks/RdsFiles/10-DT_cw_zip3_to_state.rds")

DT.delin90 <- list.files("../../R_CFPL_Step1_Default_CsvFiles/010-delin90_to_reo_forc/",
                         full.names = TRUE) %>%
    lapply(fread) %>%
  rbindlist(use.names = TRUE) %>%
  .[, zip3 := gsub("ZIP3", "", term)] %>%
  merge(DT.cw.zip3.to.state, by = "zip3", all.x = TRUE) %>%
  .[state %chin% c("04", "06", "32")] %>%
  .[, CA := as.integer(state == "06")] %>%
  .[, period := as.factor(as.character(period))] %>%
  .[, period := relevel(period, ref = "2008-06-01")] %>%
  merge(DT.hh2000, by = "zip3") %>%
  .[!is.na(CA) & !is.na(period)]


DT.mod.delin90 <- felm(estimate ~ CA * period | zip3 + period | 0 | zip3,
     DT.delin90, weights = DT.delin90$hh2000) %>%
  broom::tidy(.) %>% setDT %>%
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000] %>%
  .[grepl("CA:period", x = term)] %>%
  .[, period := gsub("^CA:period", "", x = term)] %>%
  .[, period := as.Date(period)]

p.delin90.to.reo <- ggplot(DT.mod.delin90, aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date(c("2008-06-01")),
             color = "red", linetype = "dashed",
             size = 1.01) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-30")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_ribbon(aes(ymin = estimate - 2 * std.error,
                  ymax = estimate + 2 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  labs(title = "A: Pre-CFPLs 90 Days Delinquent Mortgages to REO Foreclosure Transition Probabilities",
       subtitle = "PLS Mortgages -- Monthly CFPL REO Foreclosure Difference-in-Differences Estimates",
       y = "Probability of REO Forc.\n(Basis Points)") +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(axis.title.x = element_blank(),
        ##axis.title.y = element_blank(),
        plot.margin = unit(c(6,6,6,6), "pt"),
        plot.title = element_text(hjust = 0))


## -- Delin 60 to REO -- ##

DT.delin60.to.reo <-
  list.files("../../R_CFPL_Step1_Default_CsvFiles/010-delin60_to_reo_forc/",
                         full.names = TRUE) %>%
  lapply(fread) %>%
  rbindlist(use.names = TRUE) %>%
  .[, zip3 := gsub("ZIP3", "", term)] %>%
  merge(DT.cw.zip3.to.state, by = "zip3", all.x = TRUE) %>%
  .[, CA := as.integer(state == "06")] %>%
  .[state %chin% c("04", "06", "32")] %>%
  .[, period := as.factor(as.character(period))] %>%
  .[, period := relevel(period, ref = "2008-06-01")] %>%
  merge(DT.hh2000, by = c("zip3"), all.x = TRUE) %>%
  .[!is.na(CA) & !is.na(period)] %>%
  .[!is.na(hh2000)]


DT.mod.delin60.to.reo <- felm(estimate ~ CA * period | zip3 + period | 0 | zip3,
     DT.delin60.to.reo, weights = DT.delin60.to.reo$hh2000) %>%
  broom::tidy(.) %>% setDT %>%
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000] %>%
  .[grepl("CA:period", x = term)] %>%
  .[, period := gsub("^CA:period", "", x = term)] %>%
  .[, period := as.Date(period)]

p.delin60.to.reo <- ggplot(DT.mod.delin60.to.reo, aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date(c("2008-06-01")),
             color = "red", linetype = "dashed",
             size = 1.01) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-30")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_ribbon(aes(ymin = estimate - 2 * std.error,
                  ymax = estimate + 2 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  labs(
    title = "B: Pre-CFPLs 60 Days Delinquent Mortgages to REO Foreclosure Transition Probabilities",
    subtitle = "PLS Mortgages -- Monthly CFPL REO Foreclosure Difference-in-Differences Estimates",
    y = "Probability of REO Forc.\n(Basis Points)") +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(axis.title.x = element_blank(),
        ##axis.title.y = element_blank(),
        plot.margin = unit(c(6,6,6,6), "pt"),
        plot.title = element_text(hjust = 0))





## -- Delin 60 to FORC.START -- ##

DT.delin60.to.forc.start <-
  list.files("../../R_CFPL_Step1_Default_CsvFiles/010-delin60_to_forc_start/",
                         full.names = TRUE) %>%
  lapply(fread) %>%
  rbindlist(use.names = TRUE) %>%
  .[, zip3 := gsub("ZIP3", "", term)] %>%
  merge(DT.cw.zip3.to.state, by = "zip3", all.x = TRUE) %>%
  .[, CA := as.integer(state == "06")] %>%
  .[state %chin% c("04", "06", "32")] %>%
  .[, period := as.factor(as.character(period))] %>%
  .[, period := relevel(period, ref = "2008-06-01")] %>%
  merge(DT.hh2000, by = c("zip3"), all.x = TRUE) %>%
  .[!is.na(CA) & !is.na(period)] %>%
  .[!is.na(hh2000)]


DT.mod.delin60.to.forc.start <- felm(estimate ~ CA * period | zip3 + period | 0 | zip3,
     DT.delin60.to.forc.start, weights = DT.delin60.to.forc.start$hh2000) %>%
  broom::tidy(.) %>% setDT %>%
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000] %>%
  .[grepl("CA:period", x = term)] %>%
  .[, period := gsub("^CA:period", "", x = term)] %>%
  .[, period := as.Date(period)]

p.delin60.to.forc.start <- ggplot(DT.mod.delin60.to.forc.start,
                                  aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date(c("2008-06-01")),
             color = "red", linetype = "dashed",
             size = 1.01) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-30")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_ribbon(aes(ymin = estimate - 2 * std.error,
                  ymax = estimate + 2 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  labs(
    title = "C: Pre-CFPLs 60 Days Delinquent Mortgages to Foreclosure Start Transition Probabilities",
    subtitle = "PLS Mortgages -- Monthly CFPL Foreclosure Start Difference-in-Differences Estimates",
    y = "Probability of Forc. Start\n(Basis Points)") +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(axis.title.x = element_blank(),
        ##axis.title.y = element_blank(),
        plot.margin = unit(c(6,6,6,6), "pt"),
        plot.title = element_text(hjust = 0))


p.all <- plot_grid(p.delin90.to.reo,
                   p.delin60.to.reo,
                   p.delin60.to.forc.start,
                   nrow = 3, align = "hv")

ggsave(filename = "PlotsFinal/010-p_trans_probs_distressed_to_forc.pdf",
       plot = p.all, width = 9.5, height = 8.5)
