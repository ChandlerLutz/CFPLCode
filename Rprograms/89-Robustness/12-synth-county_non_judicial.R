##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-03-05

##Clear the workspace
rm(list = ls()) ##Delete all objects
suppressWarnings(CLmisc::detach_all_packages())


source("../../SynthMult/SynthMult.R", chdir = TRUE)

judicial.forc.states <-
  readRDS("../../Data/_Data_Final_/10-Judicial_Foreclosure_States.rds")

county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds") %>%
  .[!(state.fips %in% judicial.forc.states[["state.fips"]])]

time.index <- county.panel[, unique(time.index)]

controls <- county.panel[CA == 0, unique(fips.code)]
treated <- county.panel[CA == 1, unique(fips.code)]

SynthCounty <- SynthMult$new(
  all.data = as.data.frame(county.panel),
  dependent.vars = c("zillow.forc"),
  treated = treated,
  controls = controls,
  time.policy = 2008.497,
  sample = "12-County_Forc_non_judicial",
  time.index = time.index,
  predictors = c("zillow.forc"),
  special.predictors = list(
    list("forc.rf.pred", 2007.000, "mean"),
    list("unemp.rate", 2007.000, "mean"),
    list("land.unavailable", 2007.000, "mean"),
    list("bartik", 2008.000, "mean"),
    list("IncomePerHousehold", 2007.000, "mean"),
    list("subprime.percent.2005", 2007.000, "mean"),
    list("non.occ.rate", 2007.000, "mean"),
    list("zillow.ret.2008Q12", 2007.000, "mean"),
    list("unemp.rate2007_zillow.ret.2008Q12", 2007.000, "mean")
  ),
  y.axis.label = "Zillow Foreclosures",
  sample.average.label = "Sample Average",
  parallel = TRUE, ##Run in parallel,
  parallel.cores = detectCores(logical = FALSE),
  fast.compute = TRUE,
  plots = FALSE,
  compute.perm = FALSE,
  optim.method = "BFGS"
)


hh2000 <- county.panel[!duplicated(region), .(region, hh2000)] %>%
  setkey(region)
out <- SynthCounty$synth.output %>%
  .[time.index >= 2008.497 & time.index < 2012,
    .(value = sum(value), control = sum(control)), by = region] %>%
  setkey(region) %>%
  merge(hh2000) %>%
  .[, .(value = sum(value / 10000 * hh2000),
        control = sum(control / 10000 * hh2000))]
forc.reduction <- out[, value - control] %>% round(3)
forc.reduction.percent <- out[, (value - control) / control * 100] %>% round(3)
sprintf("According to Synth Estimates, the CFPLs reduced forelocures when the control group only consists of non-judicial foreclosure states by %s homes or %s percent",
        forc.reduction, forc.reduction.percent) %>%
  print(.)
