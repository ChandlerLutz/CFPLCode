##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-15

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())



source("../../SynthMult/SynthMult.R", chdir = TRUE)

county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds") %>%
  .[CA == 1] %>%
  .[, ln.zillow.forc := log(zillow.forc)] %>%
  .[, ln.zillow.forc := ln.zillow.forc - ln.zillow.forc[1], by = fips.code]

time.index <- county.panel[, unique(time.index)]

controls <- county.panel[forc.high == 0, unique(fips.code)]
treated <- county.panel[forc.high == 1, unique(fips.code)]

SynthCounty <- SynthMult$new(
  all.data = as.data.frame(county.panel),
  dependent.vars = c("ln.zillow.forc"),
  treated = treated,
  controls = controls,
  time.policy = 2008.497,
  sample = "13-County_Forc_CA_Only",
  time.index = time.index,
  predictors = c("ln.zillow.forc"),
  special.predictors = list(
    list("forc.rf.pred", 2007.000, "mean"),
    list("unemp.rate", 2007.000, "mean"),
    list("land.unavailable", 2007.000, "mean"),
    list("bartik", 2008.000, "mean"),
    list("IncomePerHousehold", 2007.000, "mean"),
    list("subprime.percent.2005", 2007.000, "mean"),
    list("non.occ.rate", 2007.000, "mean"),
    list("zillow.ret.2008Q12", 2007.000, "mean")
  ),
  y.axis.label = "Zillow Foreclosures",
  sample.average.label = "Sample Average",
  parallel = TRUE, ##Run in parallel,
  parallel.cores = 4,
  fast.compute = TRUE,
  plots = FALSE,
  compute.perm = TRUE,
  optim.method = "BFGS"
)

DT.results <- SynthCounty$synth.output %>%
  merge(county.panel, by = c("region", "region.number", "time.index")) %>%
  .[, .(fips.code, time, time.char = as.character(time), value, control, gap, hh2000)]

library(Hmisc)
##The aggregated results
DT.results.agg <- DT.results[, gap.cumsum := cumsum(gap), by = fips.code] %>%
  .[, .(gap.cumsum.percentile10th = wtd.quantile(gap.cumsum, w = hh2000, probs = 0.1),
        gap.cumsum.percentile25th = wtd.quantile(gap.cumsum, w = hh2000, probs = 0.25),
        gap.cumsum.percentile50th = wtd.quantile(gap.cumsum, w = hh2000, probs = 0.5),
        gap.cumsum.percentile75th = wtd.quantile(gap.cumsum, w = hh2000, probs = 0.75)),
    by = time] %>%
  melt(id.vars = "time", variable.factor = FALSE, variable.name = "gap.cumsum.percentile",
       value.name = "gap.cumsum") %>%
  .[, gap.cumsum.percentile := gsub("gap.cumsum.percentile", "", gap.cumsum.percentile)]


DT.perm <- SynthCounty$perm.output %>%
  merge(county.panel, by = c("region", "region.number", "time.index")) %>%
  .[, .(fips.code, time, time.char = as.character(time), value, control, gap, hh2000)] %>%
  .[, gap.cumsum := cumsum(gap), by = fips.code] %>%
  .[, .(perm.gap.cumsum.lower = wtd.quantile(gap.cumsum, probs = 0.05),
        perm.gap.cumsum.upper = wtd.quantile(gap.cumsum, probs = 0.95)),
    by = time]

DT.results.mean.all <- merge(DT.results.agg, DT.perm, by = "time")

saveRDS(DT.results.mean.all, "RdsFiles/13-synth_ca_only_forc_high.rds")


