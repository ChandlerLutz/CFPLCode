##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-12

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

DT.delin90 <- list.files("../../R_CFPL_Step1_Default_CsvFiles/010-delin90_to_delin0/",
                         full.names = TRUE) %>%
    lapply(fread) %>%
  rbindlist(use.names = TRUE) %>%
  .[, zip3 := gsub("ZIP3", "", term)] %>%
  merge(DT.cw.zip3.to.state, by = "zip3", all.x = TRUE) %>%
  .[state %chin% c("04", "06", "32")] %>%
  .[, CA := as.integer(state == "06")] %>%
  .[, period := as.factor(as.character(period))] %>%
  .[, period := relevel(period, ref = "2008-04-01")] %>%
  merge(DT.hh2000, by = "zip3") %>%
  .[!is.na(CA) & !is.na(period)]


DT.mod.delin90 <- felm(estimate ~ CA * period | zip3 + period | 0 | zip3,
                       DT.delin90, weights = DT.delin90$hh2000
                       ) %>%
  broom::tidy(.) %>% setDT %>%
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000] %>%
  .[grepl("CA:period", x = term)] %>%
  .[, period := gsub("^CA:period", "", x = term)] %>%
  .[, period := as.Date(period)]

p.delin90.to.current <- ggplot(DT.mod.delin90, aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date(c("2008-06-01")),
             color = "red", linetype = "dashed",
             size = 1.01) +
  geom_vline(xintercept = as.Date(c("2008-07-01", "2009-06-30")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_ribbon(aes(ymin = estimate - 1.96 * std.error,
                  ymax = estimate + 1.96 * std.error),
              fill = "gray50", alpha = 0.5,
              stat = "stepribbon", direction = "hv") +
  geom_step(size = 1.01) +
  labs(title = "Pre-CFPL 90 Days Delinquent Mortgages to Current Transition Probabilities",
    ##title = "A: Probability of Transition to REO Foreclosure for Loans 90 Days Delinquent Prior to the CFPLs",
       subtitle = "PLS Mortgages -- Monthly Difference-in-Differences Estimates",
       y = "Probability of Current\n(Basis Points)") +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(axis.title.x = element_blank(),
        ##axis.title.y = element_blank(),
        plot.margin = unit(c(6,6,6,6), "pt"),
        plot.title = element_text(hjust = 0))

ggsave("PlotsFinal/020-p_trans_probs_distressed_to_current.pdf",
       plot = p.delin90.to.current, width = 9.25, height = 5.2)
