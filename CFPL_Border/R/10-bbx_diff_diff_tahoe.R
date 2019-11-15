##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-06



##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())



suppressMessages({library(CLmisc); library(lfe); library(cowplot)})


DT <- fread("../data-raw/30-bbx_data_lake_tahoe_zip3_894_961.csv") %>%
  ##keep just what we need for the regression
  select_by_ref(c("ACTIVITYDT", "ORIG_DATE", "PROPERTYSTATECALC", "ZIP", "ZIP3", "ZIP4",
                  "HAS_BEEN_REO_FORC_CUMSUM", "HAS_BEEN_MODIFIED_CUMSUM",
                  "FICOSCOREORIGINATIONCALC", "ORIGLTVRATIOCALC", "ORIGINALBALCALC",
                  "HYBRIDARMIND", "OPTIONARMIND", "NEGAMSTATUSIND",
                  "BALLOONSTATUSIND", "IOSTATUSIND", "ARMCONVERTSTATUSIND",
                  "INDEXCDSUMMARY", "OCCTYPE",
                  "PURPOSETYPE", "PROPTYPE", "SERVICER")) %>%
  .[, `:=`(ACTIVITYDT = as.Date(ACTIVITYDT),
           ORIG_DATE = as.Date(ORIG_DATE))] %>%
  .[PROPERTYSTATECALC %chin% c("NV", "CA")] %>%
  .[ACTIVITYDT >= "2008-01-01" & ACTIVITYDT <= "2009-12-01"] %>%
  .[, ACTIVITYDT := as.character(ACTIVITYDT)]

##Some summary stats
DT <- DT %>%
  .[, ZIP := as.character(ZIP)] %>%
  .[, ZIP3 := as.character(ZIP3)] %>%
  .[, ZIP4 := substr(ZIP, 1, 4)]


ventile.vars <- c("FICOSCOREORIGINATIONCALC", "ORIGLTVRATIOCALC", "ORIGINALBALCALC")

f_ventile <- function(x) {
  x <- dplyr::ntile(x, 20)
  x[is.na(x)] <- 0
  return(x)
}


DT <- DT[, c(ventile.vars) := lapply(.SD, f_ventile),
         .SDcols = ventile.vars]




DT <- DT[, ACTIVITYDT := factor(ACTIVITYDT)] %>%
  .[, ACTIVITYDT := relevel(ACTIVITYDT, "2008-07-01")] %>%
  .[, PROPERTYSTATECALC := factor(PROPERTYSTATECALC)] %>%
  .[, PROPERTYSTATECALC := relevel(PROPERTYSTATECALC, "NV")]


mod <- felm(HAS_BEEN_REO_FORC_CUMSUM ~ PROPERTYSTATECALC * ACTIVITYDT |
              ZIP + ACTIVITYDT + ORIG_DATE +
              HYBRIDARMIND + OPTIONARMIND + NEGAMSTATUSIND +
              BALLOONSTATUSIND + IOSTATUSIND + ARMCONVERTSTATUSIND +
              ORIGINALBALCALC +
              FICOSCOREORIGINATIONCALC +
              ORIGLTVRATIOCALC + INDEXCDSUMMARY + OCCTYPE +
              PURPOSETYPE + PROPTYPE + SERVICER
            | 0 | ZIP4, DT[HAS_BEEN_REO_FORC_CUMSUM <= 1])

DT.mod <- broom::tidy(mod) %>% setDT %>%
  .[grepl(":ACTIVITYDT", term)] %>%
  .[, time := gsub("PROPERTYSTATECALCCA:ACTIVITYDT(.*)$", "\\1", term)] %>%
  .[, time := as.Date(time)] %>%
  ##get the result in basis points
  .[, estimate := estimate * 1000] %>%
  .[, std.error := std.error * 1000]

p.bbx.tahoe <- ggplot(DT.mod, aes(x = time, y = estimate)) +
  geom_vline(xintercept = as.Date(c("2008-07-31", "2009-06-30")),
             color = "blue", linetype = "dashed",
             size = 1.01) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = estimate - 2 * std.error,
                  ymax = estimate + 2 * std.error), fill = "gray50",
              alpha = 0.25) +
  geom_line(size = 1.01) +
  scale_x_date(breaks = as.Date(c("2008-01-01","2009-01-01", "2010-01-01")),
               date_labels = "%Y") +
  labs(title = "A: Lake Tahoe California and Nevada Border Region",
       subtitle = "PLS Mortgages -- CFPL REO Foreclosure Difference-in-Differences Estimates",
       y = "Monthly Probability of REO Foreclosure") +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(6,20,6,6), "pt"),
        plot.title = element_text(hjust = 0))
saveRDS(p.bbx.tahoe, "RdsFiles/10-p_bbx_diff_diff_tahoe.rds")
