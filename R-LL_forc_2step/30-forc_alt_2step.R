##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-11

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe)})

##Preliminaries
source("00-pre.R")

##Forc high
DT.forc.high <- readRDS("RdsFiles/10-DT_forc_high_zip3.rds")

## -- Forc alt data -- ##
DT.forc.alt <- readRDS("../DataLL_2step/RdsFiles/10-2step_forc_alt_step1.rds") %>%
  merge(DT.forc.high, by = "zip3", all.x = TRUE)

## -- Get the dummies -- ##
f_get_dummies_trends(DT.forc.alt)

## -- Add the dummies to the data -- ##
DT.forc.alt <- DT.forc.alt %>%
  .[, c(forc.high.CA.time) := lapply(.SD, function(x) x * CA * forc.high),
    .SDcols = time.dummies] %>%
  .[, c(forc.high.time) := lapply(.SD, function(x) x * forc.high),
    .SDcols = time.dummies] %>%
  .[, c(CA.time) := lapply(.SD, function(x) x * CA),
    .SDcols = time.dummies] %>%
  ##time trends
  .[, c(zip3.trends) := lapply(.SD, function(x) x * time.trend),
    .SDcols = zip3.dummies]

DT.forc.alt <- DT.forc.alt %>%
  .[, c(lu.dummies) := lapply(.SD, function(x) x * total.unavailable),
    .SDcols = time.dummies]

##  -- The formulas -- ##

## the base formula
f.base.formula <- paste0("forc.alt ~ ",
                         paste0(CA.time, collapse = " + "),
                         " | zip3 + period | 0 | state")

## the full formula
f.full.formula <- paste0("forc.alt ~ ",
                         paste0(CA.time, collapse = " + "), " + ",
                         paste0(lu.dummies, collapse = " + "),
                         " + bartik + bartik.qcew",
                         " | zip3 + period | 0 | state")

##the formulas with trends
f.trends.formula <- paste0("forc.alt ~ ",
                           paste0(CA.time, collapse = " + "), " + ",
                           paste0(zip3.trends, collapse = " + "),
                           " | zip3 + period | 0 | state")

f.full.trends.formula <- paste0("forc.alt ~ ",
                                paste0(CA.time, collapse = " + "), " + ",
                                paste0(zip3.trends, collapse = " + "), " + ",
                                paste0(lu.dummies, collapse = " + "),
                                " + bartik + bartik.qcew",
                                " | zip3 + period | 0 | state")

##A function to tidy the DD regression output
f_CA.time_tidy_reg <- function(felm.mod) {
  if (!("felm" %in% class(felm.mod)))
    stop("Error: lm.mod must be of class 'lm'")

  ##Tidy the model,
  out <- felm.mod %>%
    broom::tidy(.) %>%
    as.data.table %>%
    ##only get the DDD terms
    .[grepl("^CA_period_", term)] %>%
    ##Add a time column
    .[, time := gsub("CA_period_", "", term)] %>%
    .[, time := as.Date(time, "%Y_%m_%d")]

  return(out)

}


##A function to tidy the DDD regression output
f_forc.high_tidy_reg <- function(felm.mod) {
  if (!("felm" %in% class(felm.mod)))
    stop("Error: lm.mod must be of class 'lm'")

  ##Tidy the model,
  out <- felm.mod %>%
    broom::tidy(.) %>%
    as.data.table %>%
    ##only get the DDD terms
    .[grepl("forc.high_CA_period_", term)] %>%
    ##Add a time column
    .[, time := gsub("forc.high_CA_period_", "", term)] %>%
    .[, time := as.Date(time, "%Y_%m_%d")]

  return(out)

}



## -- DD Estimates forc high only -- ##

DT.forc.alt.dd <- DT.forc.alt[forc.high == 1]

mod1 <- felm(as.formula(f.base.formula), DT.forc.alt.dd,
             weights = DT.forc.alt.dd$hh2000)

##Add some F-stats
f_hypothesis <- function(start.date, end.date) {
  hypothesis <- seq(from = as.Date(start.date), to = as.Date(end.date),
                    by = "1 month") %>%
    gsub("-", "_", x = .) %>%
    paste0("CA_period_", .) %>%
    paste0(., collapse = " + ") %>%
    paste0(., " = 0")
  return(hypothesis)
}
hypothesis0 <- f_hypothesis("2008-01-01", "2008-05-01")
hypothesis1 <- f_hypothesis("2008-07-01", "2009-02-01")
hypothesis2 <- f_hypothesis("2008-07-01", "2010-12-01")
hypothesis3 <- f_hypothesis("2008-07-01", "2010-02-01")


car::linearHypothesis(mod1, hypothesis0, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/30-f_stat_forc_alt_rate_sum_2008M01_2008M05.csv")

car::linearHypothesis(mod1, hypothesis1, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/30-f_stat_forc_alt_rate_sum_2008M07_2009M02.csv")

car::linearHypothesis(mod1, hypothesis2, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/30-f_stat_forc_alt_rate_sum_2008M07_2010M12.csv")

car::linearHypothesis(mod1, hypothesis3, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/30-f_stat_forc_alt_rate_sum_2008M07_2010M02.csv")



mod1 <- mod1 %>% f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "No")]



mod2 <- felm(as.formula(f.full.formula), DT.forc.alt.dd,
             weights = DT.forc.alt.dd$hh2000) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "No")]

mod3 <- felm(as.formula(f.trends.formula), DT.forc.alt.dd,
             weights = DT.forc.alt.dd$hh2000) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "Yes")]

mod4 <- felm(as.formula(f.full.trends.formula), DT.forc.alt.dd,
             weights = DT.forc.alt.dd$hh2000) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "Yes")]

DT.dd.mod.out <- rbind(mod1, mod2, mod3, mod4) %>%
  setcolorder(c("mod", "trends", "time"))

saveRDS(DT.dd.mod.out, "RdsFiles/30-forc_alt_rate_reg_2step_dd_forc_high_only.rds")


## -- DD High forc only -- ##

mod1 <- felm(as.formula(f.base.formula), DT.forc.alt[forc.high == 1],
             weights = DT.forc.alt[forc.high == 1, hh2000]) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "No")]

mod2 <- felm(as.formula(f.full.formula), DT.forc.alt[forc.high == 1],
             weights = DT.forc.alt[forc.high == 1, hh2000]) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "No")]

mod3 <- felm(as.formula(f.trends.formula), DT.forc.alt[forc.high == 1],
             weights = DT.forc.alt[forc.high == 1, hh2000]) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "Yes")]

mod4 <- felm(as.formula(f.full.trends.formula), DT.forc.alt[forc.high == 1],
             weights = DT.forc.alt[forc.high == 1, hh2000]) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "Yes")]


##clean up
rm(mod1, mod2, mod3, mod4, DT.dd.mod.out)

##  -- The DDD formulas -- ##

## the base formula
f.base.formula <- paste0("forc.alt ~ ",
                         paste0(forc.high.CA.time, collapse = " + "), " + ",
                         paste0(forc.high.time, collapse = " + "), " + ",
                         paste0(CA.time, collapse = " + "),
                         " | zip3 + period | 0 | state")

## the full formula
f.full.formula <- paste0("forc.alt ~ ",
                         paste0(forc.high.CA.time, collapse = " + "), " + ",
                         paste0(forc.high.time, collapse = " + "), " + ",
                         paste0(CA.time, collapse = " + "), " + ",
                         paste0(lu.dummies, collapse = " + "),
                         " + bartik + bartik.qcew",
                         " | zip3 + period | 0 | state")

##the formulas with trends
f.trends.formula <- paste0("forc.alt ~ ",
                           paste0(forc.high.CA.time, collapse = " + "), " + ",
                           paste0(forc.high.time, collapse = " + "), " + ",
                           paste0(CA.time, collapse = " + "), " + ",
                           paste0(zip3.trends, collapse = " + "),
                           " | zip3 + period | 0 | state")

f.full.trends.formula <- paste0("forc.alt ~ ",
                                paste0(forc.high.CA.time, collapse = " + "), " + ",
                                paste0(forc.high.time, collapse = " + "), " + ",
                                paste0(CA.time, collapse = " + "), " + ",
                                paste0(zip3.trends, collapse = " + "), " + ",
                                paste0(lu.dummies, collapse = " + "),
                                " + bartik + bartik.qcew",
                                " | zip3 + period | 0 | state")


mod1 <- felm(as.formula(f.base.formula), DT.forc.alt,
             weights = DT.forc.alt$hh2000)

print("pre-Hamp announcement DDD F-stat")
cfpl.pre.hamp.f.stat <- seq(from = as.Date("2008-07-01"), to = as.Date("2009-02-01"),
                            by = "1 month") %>%
  gsub("-", "_", x = .) %>%
  paste0("forc.high_CA_period_", .) %>%
  paste0(collapse = " + ") %>%
  car::linearHypothesis(mod1, hypothesis.matrix = ., vcov = mod1$clustervcv) %>%
  broom::tidy(.) %>%
  print

print("Up to 2010-12-01 DDD F-stat")
cfpl.f.state.pre.2010 <- seq(from = as.Date("2008-07-01"), to = as.Date("2010-12-01"),
                            by = "1 month") %>%
  gsub("-", "_", x = .) %>%
  paste0("forc.high_CA_period_", .) %>%
  paste0(collapse = " + ") %>%
  car::linearHypothesis(mod1, hypothesis.matrix = ., vcov = mod1$clustervcv) %>%
  broom::tidy(.) %>%
  print

print("Up to 2011-12-01 DDD F-stat")
cfpl.f.state.pre.2011 <- seq(from = as.Date("2008-07-01"), to = as.Date("2011-12-01"),
                            by = "1 month") %>%
  gsub("-", "_", x = .) %>%
  paste0("forc.high_CA_period_", .) %>%
  paste0(collapse = " + ") %>%
  car::linearHypothesis(mod1, hypothesis.matrix = ., vcov = mod1$clustervcv) %>%
  broom::tidy(.) %>%
  print


mod1 <- mod1 %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "No")]

mod2 <- felm(as.formula(f.full.formula), DT.forc.alt,
             weights = DT.forc.alt$hh2000) %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "No")]

mod3 <- felm(as.formula(f.trends.formula), DT.forc.alt,
             weights = DT.forc.alt$hh2000) %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "Yes")]

mod4 <- felm(as.formula(f.full.trends.formula), DT.forc.alt,
             weights = DT.forc.alt$hh2000)

mod4 <- mod4 %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "Yes")]

DT.ddd.mod.out <- rbind(mod1, mod2, mod3, mod4) %>%
  setcolorder(c("mod", "trends", "time"))

saveRDS(DT.ddd.mod.out, "RdsFiles/30-forc_alt_reg_2step_ddd.rds")




