##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-01

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(parallel)})

##Preliminaries
source("00-pre.R")

## -- REO forc rate data -- ##
DT.reo.forc <- readRDS("../DataLL_2step/RdsFiles/10-2step_reo_forc_step1.rds")

##The observations for each month
DT.reo.forc.month.obs <- DT.reo.forc %>%
  .[, .(obs = sum(obs)), by = .(time = as.Date(period))]


## -- Get the dummies -- ##
f_get_dummies_trends(DT.reo.forc)

##  -- The formulas -- ##

## the base formula
f.base.formula <- paste0("reo.forc ~ ",
                         paste0(forc.high.CA.time, collapse = " + "), " + ",
                         paste0(forc.high.time, collapse = " + "), " + ",
                         paste0(CA.time, collapse = " + "),
                         " | zip3 + period | 0 | state")

## the full formula
f.full.formula <- paste0("reo.forc ~ ",
                         paste0(forc.high.CA.time, collapse = " + "), " + ",
                         paste0(forc.high.time, collapse = " + "), " + ",
                         paste0(CA.time, collapse = " + "), " + ",
                         paste0(lu.dummies, collapse = " + "),
                         " + bartik + bartik.qcew",
                         " | zip3 + period | 0 | state")

##the formulas with trends
f.trends.formula <- paste0("reo.forc ~ ",
                           paste0(forc.high.CA.time, collapse = " + "), " + ",
                           paste0(forc.high.time, collapse = " + "), " + ",
                           paste0(CA.time, collapse = " + "), " + ",
                           paste0(zip3.trends, collapse = " + "),
                           " | zip3 + period | 0 | state")

f.full.trends.formula <- paste0("reo.forc ~ ",
                                paste0(forc.high.CA.time, collapse = " + "), " + ",
                                paste0(forc.high.time, collapse = " + "), " + ",
                                paste0(CA.time, collapse = " + "), " + ",
                                paste0(zip3.trends, collapse = " + "), " + ",
                                paste0(lu.dummies, collapse = " + "),
                                " + bartik + bartik.qcew",
                                " | zip3 + period | 0 | state")


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
    merge(DT.reo.forc.month.obs, by = "time")

  return(out)

}

forc.high.cutoff.seq <-
  DT.reo.forc[!duplicated(zip3), quantile(forc.rf.pred, seq(0.5,0.9,by=0.01))] %>%
  as.numeric %>%
  ##To make sure that california has low foreclosure zip3 areas
  .[. > 6]

##To run the regression using the forc high cutoff
f_forc.high_reg <- function(forc.rf.pred.cutoff) {

  temp <- copy(DT.reo.forc) %>%
    ##create forc.high
    .[, forc.high := ifelse(forc.rf.pred >= forc.rf.pred.cutoff, 1L, 0L)]
  ##create high.forc * time dummies
  temp <- temp %>%
    .[, (forc.high.time) := NA_integer_] %>%
    .[, (forc.high.time) := .SD * forc.high, .SDcols = time.dummies] %>%
    ##Create the high.forc * CA * time dummies
    .[, (forc.high.CA.time) := NA_integer_] %>%
    .[, (forc.high.CA.time) := .SD * CA * forc.high,.SDcols = time.dummies]


  reg.base.pre.error <- felm(as.formula(f.base.formula), data = temp,
                             weights = temp$hh2000) %>%
    f_forc.high_tidy_reg %>%
    .[time < as.Date("2008-07-01"), sum((estimate ^ 2))]
  reg.base.pre.error <- reg.base.pre.error * 100

  return(reg.base.pre.error)

}

##Run the cross-validation procedure in parallel
cl <- makeCluster(detectCores(logical = FALSE))
clusterEvalQ(cl, {library(CLmisc); library(lfe); library(broom)})
clusterExport(cl, varlist = c("DT.reo.forc", "f_forc.high_tidy_reg",
                              "f.base.formula",
                              "forc.high.time",
                              "forc.high.CA.time", "time.dummies",
                              "DT.reo.forc.month.obs"))
forc.high.seq.errors <- parLapplyLB(cl, forc.high.cutoff.seq, f_forc.high_reg)
stopCluster(cl)

##Get the optimal cutoff according to the cross-validation procedure
forc.high.seq.errors.which.min <- which.min(forc.high.seq.errors) %>%
  .[1]
forc.high.cutoff <- forc.high.cutoff.seq[forc.high.seq.errors.which.min]

DT.reo.forc <- DT.reo.forc %>%
  .[, forc.high := as.integer(forc.rf.pred > c(forc.high.cutoff))] %>%
  .[, c(forc.high.CA.time) := lapply(.SD, function(x) x * CA * forc.high),
    .SDcols = time.dummies] %>%
  .[, c(forc.high.time) := lapply(.SD, function(x) x * forc.high),
    .SDcols = time.dummies] %>%
  .[, c(CA.time) := lapply(.SD, function(x) x * CA),
    .SDcols = time.dummies] %>%
  ##time trends
  .[, c(zip3.trends) := lapply(.SD, function(x) x * time.trend),
    .SDcols = zip3.dummies]

DT.forc.high.zip3 <- DT.reo.forc[!duplicated(zip3), .(zip3, forc.high)]

saveRDS(DT.forc.high.zip3, "RdsFiles/10-DT_forc_high_zip3.rds")


##some contorls
lu.dummies <- paste0("lu_", time.dummies)
DT.reo.forc <- DT.reo.forc %>%
  .[, c(lu.dummies) := lapply(.SD, function(x) x * total.unavailable),
    .SDcols = time.dummies]


## -- The models -- ##


mod1 <- felm(as.formula(f.base.formula), DT.reo.forc,
             weights = DT.reo.forc$hh2000)

##Add some F-stats
f_hypothesis <- function(start.date, end.date) {
  hypothesis <- seq(from = as.Date(start.date), to = as.Date(end.date),
                    by = "1 month") %>%
    gsub("-", "_", x = .) %>%
    paste0("forc.high_CA_period_", .) %>%
    paste0(., collapse = " + ") %>%
    paste0(., " = 0")
  return(hypothesis)
}
hypothesis0 <- f_hypothesis("2008-01-01", "2008-05-01")
hypothesis1 <- f_hypothesis("2008-07-01", "2009-02-01")
hypothesis2 <- f_hypothesis("2008-07-01", "2010-12-01")

car::linearHypothesis(mod1, hypothesis0, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/10-f_stat_reo_forc_rate_sum_2008M01_2008M05.csv")
car::linearHypothesis(mod1, hypothesis1, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/10-f_stat_reo_forc_rate_sum_2008M07_2009M02.csv")
car::linearHypothesis(mod1, hypothesis2, test = "F", vcov. = mod1$clustervcv) %>%
  broom::tidy(.) %>%
    setDT %>%
    fwrite("CsvFiles/10-f_stat_reo_forc_rate_sum_2008M07_2010M12.csv")

mod1 <- mod1 %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "No")]

mod1 %>%
  .[time %between% as.Date(c("2008-07-01", "2009-02-01"))] %>%
  .[, .(wtd.mean = weighted.mean(estimate, w = obs))] %>%
  fwrite("CsvFiles/10-avg_monthly_reo_forc_rate_reduction_2008M07_2009M02.csv")

DT.unconditional.reo.forc.rates <-
  readRDS("../DataLL_2step/RdsFiles/10-DT_unconditional_reo_forc_rates.rds") %>%
  merge(DT.forc.high.zip3, by = "zip3")


forc.high.non.ca.200807.200902.N.weigthed <- DT.unconditional.reo.forc.rates %>%
  .[period %between% c("2008-07-01","2009-02-01") & forc.high == 1 & CA == 0] %>%
  .[, weighted.mean(reo.forc.rate, N)]

forc.high.non.ca.200807.200902.hh2000.weighted <- DT.unconditional.reo.forc.rates %>%
  .[period %between% c("2008-07-01","2009-02-01") & forc.high == 1 & CA == 0] %>%
  .[, weighted.mean(reo.forc.rate, hh2000)]

##The the
cfpl.reo.forc.change.200807.200902 <- mod1 %>%
  .[time %between% as.Date(c("2008-07-01", "2009-02-01"))] %>%
  .[, .(wtd.mean = weighted.mean(estimate, w = obs))]

cfpl.reo.forc.rate.change.est1 <- as.numeric(
  ((forc.high.non.ca.200807.200902.N.weigthed + cfpl.reo.forc.change.200807.200902) - forc.high.non.ca.200807.200902.N.weigthed) / forc.high.non.ca.200807.200902.N.weigthed)

cfpl.reo.forc.rate.change.est2 <- as.numeric(
((forc.high.non.ca.200807.200902.hh2000.weighted + cfpl.reo.forc.change.200807.200902) - forc.high.non.ca.200807.200902.hh2000.weighted) / forc.high.non.ca.200807.200902.hh2000.weighted)

cfpl.reo.forc.rate.change.est <- mean(
  c(cfpl.reo.forc.rate.change.est1, cfpl.reo.forc.rate.change.est2)
) * 100

print("The CFPL percentage change in the reo foreclosure rate for 2008-07-01 to 2009-02-01 relative to forc high, non-CA zip3s is ")
print(cfpl.reo.forc.rate.change.est)


mod1 %>%
  .[time %between% as.Date(c("2008-07-01", "2010-12-01"))] %>%
  .[, .(wtd.mean = weighted.mean(estimate, w = obs))] %>%
  fwrite("CsvFiles/10-avg_monthly_reo_forc_rate_reduction_2008M07_2010M12.csv")


mod2 <- felm(as.formula(f.full.formula), DT.reo.forc,
             weights = DT.reo.forc$hh2000) %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "No")]

mod3 <- felm(as.formula(f.trends.formula), DT.reo.forc,
             weights = DT.reo.forc$hh2000) %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan Controls",
           trends = "Yes")]

mod4 <- felm(as.formula(f.full.trends.formula), DT.reo.forc,
             weights = DT.reo.forc$hh2000) %>%
  f_forc.high_tidy_reg %>%
  .[, `:=`(mod = "Loan and Macro Controls",
           trends = "Yes")]


DT.out <- rbind(mod1, mod2, mod3, mod4) %>%
  setcolorder(c("mod", "trends", "time"));

saveRDS(DT.out, "RdsFiles/10-reo_forc_rate_reg_2step.rds")



## the full formula
f.full.formula <- paste0("reo.forc ~ ",
                         ##paste0(forc.high.CA.time, collapse = " + "), " + ",
                         ##paste0(forc.high.time, collapse = " + "), " + ",
                         paste0(CA.time, collapse = " + "), " + ",
                         paste0(lu.dummies, collapse = " + "),
                         " + bartik + bartik.qcew",
                         " | zip3 + period | 0 | state")


