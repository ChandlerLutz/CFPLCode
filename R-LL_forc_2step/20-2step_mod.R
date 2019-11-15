##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-01

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe)})

##Preliminaries
source("00-pre.R")

##Forc high
DT.forc.high <- readRDS("RdsFiles/10-DT_forc_high_zip3.rds")

## -- Mod rate data -- ##
DT.mod <- readRDS("../DataLL_2step/RdsFiles/10-2step_mod_step1.rds") %>%
  merge(DT.forc.high, by = "zip3", all.x = TRUE)

##The observations for each month
DT.mod.month.obs <- DT.mod %>%
  .[, .(obs = sum(obs)), by = .(time = as.Date(period))]


## -- Get the dummies -- ##
f_get_dummies_trends(DT.mod)

## -- Add the dummies to the data -- ##
DT.mod <- DT.mod %>%
  .[, c(forc.high.CA.time) := lapply(.SD, function(x) x * CA * forc.high),
    .SDcols = time.dummies] %>%
  .[, c(forc.high.time) := lapply(.SD, function(x) x * forc.high),
    .SDcols = time.dummies] %>%
  .[, c(CA.time) := lapply(.SD, function(x) x * CA),
    .SDcols = time.dummies] %>%
  ##time trends
  .[, c(zip3.trends) := lapply(.SD, function(x) x * time.trend),
    .SDcols = zip3.dummies]

DT.mod <- DT.mod %>%
  .[, c(lu.dummies) := lapply(.SD, function(x) x * total.unavailable),
    .SDcols = time.dummies]

##  -- The formulas -- ##

## the base formula
f.base.formula <- paste0("mod ~ ",
                         paste0(CA.time, collapse = " + "),
                         " | zip3 + period | 0 | state")

## the full formula
f.full.formula <- paste0("mod ~ ",
                         paste0(CA.time, collapse = " + "), " + ",
                         paste0(lu.dummies, collapse = " + "),
                         " + bartik + bartik.qcew",
                         " | zip3 + period | 0 | state")

##the formulas with trends
f.trends.formula <- paste0("mod ~ ",
                           paste0(CA.time, collapse = " + "), " + ",
                           paste0(zip3.trends, collapse = " + "),
                           " | zip3 + period | 0 | state")

f.full.trends.formula <- paste0("mod ~ ",
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
    .[, time := as.Date(time, "%Y_%m_%d")] %>%
    merge(DT.mod.month.obs, by = "time")

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
    .[, time := as.Date(time, "%Y_%m_%d")] %>%
    merge(DT.mod.month.obs, by = "time")

  return(out)

}



## -- DD Estimates -- ##

mod1 <- felm(as.formula(f.base.formula), DT.mod,
             weights = DT.mod$hh2000)

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
    fwrite("CsvFiles/20-f_stat_mod_rate_sum_2008M01_2008M05.csv")

car::linearHypothesis(mod1, hypothesis1, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/20-f_stat_mod_rate_sum_2008M07_2009M02.csv")

car::linearHypothesis(mod1, hypothesis2, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/20-f_stat_mod_rate_sum_2008M07_2010M12.csv")

car::linearHypothesis(mod1, hypothesis3, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/20-f_stat_mod_rate_sum_2008M07_2010M02.csv")



mod1 <- mod1 %>% f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "No")]

mod1 %>%
  .[time %between% as.Date(c("2008-07-01", "2009-02-01"))] %>%
  .[, .(wtd.mean = weighted.mean(estimate, w = obs))] %>%
  fwrite("CsvFiles/20-avg_monthly_mod_rate_reduction_2008M07_2009M02.csv")

mod1 %>%
  .[time %between% as.Date(c("2008-07-01", "2010-12-01"))] %>%
  .[, .(wtd.mean = weighted.mean(estimate, w = obs))] %>%
  fwrite("CsvFiles/20-avg_monthly_mod_rate_reduction_2008M07_2010M12.csv")


mod1 %>%
  .[mod == "Loan Controls" & trends == "No"] %>%
  .[time %between% as.Date(c("2008-07-01", "2009-02-01"))] %>%
  .[, .(sum(estimate))] %>%
  fwrite("CsvFiles/20-sum_ddd_est_mod_rate_2008M07_2009M02.csv")

mod1 %>%
  .[mod == "Loan Controls" & trends == "No"] %>%
  .[time %between% as.Date(c("2008-07-01", "2010-02-01"))] %>%
  .[, .(sum(estimate))] %>%
  fwrite("CsvFiles/20-sum_ddd_est_mod_rate_2008M07_2010M02.csv")

mod1 %>%
  .[mod == "Loan Controls" & trends == "No"] %>%
  .[time %between% as.Date(c("2008-07-01", "2011-12-01"))] %>%
  .[, .(sum(estimate))] %>%
  fwrite("CsvFiles/20-sum_ddd_est_mod_rate_2008M07_2011M12.csv")

mod1 %>%
  .[mod == "Loan Controls" & trends == "No"] %>%
  .[time %between% as.Date(c("2008-07-01", "2012-12-01"))] %>%
  .[, .(sum(estimate))] %>%
  fwrite("CsvFiles/20-sum_ddd_est_mod_rate_2008M07_2012M12.csv")


DT.num.mods <- mod1 %>%
  .[time %between% as.Date(c("2008-07-01", "2012-12-01"))] %>%
  .[, num.ca.mortgages.2007 := 5381874] %>%
  .[, .(num.mods = sum(estimate * num.ca.mortgages.2007),
        mod.rate = sum(estimate))]
print("DD Modification estiamtes for CA 2008-07-01 - 2012-12-01")
print(DT.num.mods)

DT.unconditional.mod.rates <-
  readRDS("../DataLL_2step/RdsFiles/10-DT_unconditional_mod_rates.rds")

non.ca.200807.200902.N.weigthed <- DT.unconditional.mod.rates %>%
  .[period %between% c("2008-07-01","2009-02-01") & CA == 0] %>%
  .[, weighted.mean(mod.rate, N)]

non.ca.200807.200902.hh2000.weighted <- DT.unconditional.mod.rates %>%
  .[period %between% c("2008-07-01","2009-02-01") & CA == 0] %>%
  .[, weighted.mean(mod.rate, hh2000)]

cfpl.mod.rate.change.200807.200902 <- mod1 %>%
  .[time %between% c("2008-07-01", "2009-02-01"),
    weighted.mean(estimate, w = obs)]


cfpl.mod.rate.change.est1 <- as.numeric(
  ((non.ca.200807.200902.N.weigthed + cfpl.mod.rate.change.200807.200902) - non.ca.200807.200902.N.weigthed) / non.ca.200807.200902.N.weigthed)

cfpl.mod.rate.change.est2 <- as.numeric(
((non.ca.200807.200902.hh2000.weighted + cfpl.mod.rate.change.200807.200902) - non.ca.200807.200902.hh2000.weighted) / non.ca.200807.200902.hh2000.weighted)

cfpl.mod.rate.change.est <- mean(
  c(cfpl.mod.rate.change.est1, cfpl.mod.rate.change.est2)
) * 100

print("The CFPL percentage change in the modification rate for 2008-07-01 to 2009-02-01 relative to non-CA zip3s is ")
print(cfpl.mod.rate.change.est)




mod2 <- felm(as.formula(f.full.formula), DT.mod,
             weights = DT.mod$hh2000) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "No")]

mod3 <- felm(as.formula(f.trends.formula), DT.mod,
             weights = DT.mod$hh2000) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "Yes")]

mod4 <- felm(as.formula(f.full.trends.formula), DT.mod,
             weights = DT.mod$hh2000) %>%
  f_CA.time_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "Yes")]

DT.dd.mod.out <- rbind(mod1, mod2, mod3, mod4) %>%
  setcolorder(c("mod", "trends", "time"))

saveRDS(DT.dd.mod.out, "RdsFiles/20-mod_rate_reg_2step_dd.rds")

