##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-11-26

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(parallel); library(lfe)})

county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds") %>%
  .[, y := zillow.forc] %>%
  .[, time.char := as.character(time)] %>%
  .[, fips.code := as.character(fips.code)] %>%
  .[, forc.high := NULL] %>%
  .[, forc.high := NA_real_]


##Get the dummy variables for time
county.panel <- dummy_cols(county.panel, c("time.char"),
                           remove_first_dummy = FALSE)

time.char.names.old <- names(county.panel) %>%
  .[grepl("time.char_", x = .)]
time.char.names.new <- gsub("-", "_", x = time.char.names.old)
setnames(county.panel, time.char.names.old, time.char.names.new)

##The time dummy column names
time.dummies <- names(county.panel) %>%
  .[grepl("time.char_", x = .)]
fips.dummies <- names(county.panel) %>%
  .[grepl("fips.code_", x = .)]

time.dummies.inter <- time.dummies %>%
  .[. != "time.char_2008_06_01"]

##Get the interactions
##Add the DDD interaction
##A function to get the interactions with time
f_time_inter_char <- function(x, static = TRUE, ...) {
  return(paste0(paste0(x, "_"), time.dummies.inter, ...))
}

##The forc.high * CA * time  and the forc.high * time dummies
forc.high.CA.time <- f_time_inter_char("forc.high_CA")
forc.high.time <- f_time_inter_char("forc.high")
county.panel <- county.panel %>%
  .[, (forc.high.CA.time) := NA_integer_] %>%
  .[, (forc.high.time) := NA_integer_]

##Add all other interactions
base.vars <- c("CA", "forc.high")
control.vars <- c("zillow.ret.2008Q12", "land.unavailable",
          "non.occ.rate", "subprime.percent.2005",
          "IncomePerHousehold", "bartik", "bartik.qcew")

for (temp.var in c(base.vars, control.vars)) {
  temp.var.time <- f_time_inter_char(temp.var)
  county.panel <- county.panel %>%
    .[, (temp.var.time) := NA_real_] %>%
    .[, (temp.var.time) := .SD * get(temp.var), .SDcols = time.dummies.inter]
}

##Sanity checks
stopifnot(
  all(county.panel[time.index == 2008, bartik] ==
        county.panel[time.index == 2008, bartik_time.char_2008_01_01])
)


##A function to create the regression formulas
f_formula <- function(vars) {

  out <- paste0(
    "y ~ 0 + ",
    paste0(forc.high.CA.time, collapse = " + "), ##forc.high * CA * time
    " + "
  )

  for (temp.var in vars) {
    temp.var.reg <- f_time_inter_char(temp.var, collapse = " + ")
    out <- paste0(out, " + ", temp.var.reg)
    ##For the bartik or unemp, make sure that we add in for the dropped dummy
    if (temp.var == "bartik") {
      out <- paste0(out, " + bartik")
    } else if (temp.var == "bartik.qcew") {
      out <- paste0(out, " + bartik.qcew")
    } else if (temp.var == "unemp.rate") {
      out <- paste0(out, " + unemp.rate")
    }

  }
  ##out <- paste0(out, " + ", paste0(time.dummies.inter, collapse = " + "))
  out <- paste0(out, "| time.char + fips.code | 0 | state.fips")

  return(out)
}

##The formulas
f.base.formula <- f_formula(base.vars)
##remove the unemployment rate as a control
control.vars <- control.vars %>% .[. != "unemp.rate"]
f.full.formula <- f_formula(c(base.vars, control.vars))

##Use cross-validation to choose the high forc cutoff
##From the 50th percentile to the
forc.high.cutoff.seq <-
  county.panel[!duplicated(fips.code), quantile(forc.rf.pred, seq(0.5,0.9,by=0.01))] %>%
  as.numeric

##A function to tidy the DDD regression output
f_forc.high_tidy_reg <- function(felm.mod) {
  if (!("felm" %in% class(felm.mod)))
    stop("Error: lm.mod must be of class 'lm'")

  ##Tidy the model,
  out <- felm.mod %>%
    broom::tidy(.) %>%
    as.data.table %>%
    ##only get the DDD terms
    .[grepl("forc.high_CA_time.char_", term)] %>%
    ##Add a time column
    .[, time := gsub("forc.high_CA_time.char_", "", term)] %>%
    .[, time := as.Date(time, "%Y_%m_%d")]

  return(out)

}


##To run the regression using the forc high cutoff
f_forc.high_reg <- function(forc.rf.pred.cutoff) {

  temp <- copy(county.panel) %>%
    ##create forc.high
    .[, forc.high := ifelse(forc.rf.pred >= forc.rf.pred.cutoff, 1L, 0L)]
  ##create high.forc * time dummies
  temp <- temp %>%
    .[, (forc.high.time) := NA_integer_] %>%
    .[, (forc.high.time) := .SD * forc.high, .SDcols = time.dummies.inter] %>%
    ##Create the high.forc * CA * time dummies
    .[, (forc.high.CA.time) := NA_integer_] %>%
    .[, (forc.high.CA.time) := .SD * CA * forc.high,
      .SDcols = time.dummies.inter]

  reg.base.pre.error <- felm(as.formula(f.base.formula), data = temp,
                           weights = temp$hh2000) %>%
    f_forc.high_tidy_reg %>%
    .[time < as.Date("2008-07-01"), sum((estimate ^ 2))]

  return(reg.base.pre.error)

}

##Run the cross-validation procedure in parallel
cl <- makeCluster(detectCores(logical = FALSE))
clusterEvalQ(cl, {library(CLmisc); library(lfe); library(broom)})
clusterExport(cl, varlist = c("county.panel", "f_forc.high_tidy_reg",
                              "f.base.formula", "f.full.formula",
                              "forc.high.time",
                              "forc.high.CA.time", "time.dummies.inter"))
forc.high.seq.errors <- clusterMap(cl, f_forc.high_reg, forc.high.cutoff.seq,
                                   .scheduling = "dynamic")
stopCluster(cl)


##Get the optimal cutoff according to the cross-validation procedure
forc.high.seq.errors.which.min <- which.min(forc.high.seq.errors) %>%
  .[1]
forc.high.cutoff <- forc.high.cutoff.seq[forc.high.seq.errors.which.min]

f_ecdf <- ecdf(county.panel[!duplicated(fips.code), forc.rf.pred])
print(sprintf("The forc.high cutoff is as %s", forc.high.cutoff))
print(sprintf("The forc.high cutoff percentile is as %s", f_ecdf(forc.high.cutoff)))

saveRDS(forc.high.cutoff, "RdsFiles/10-forc_high_cutoff_ddd.rds")

##The county panel with the forc cutoff
county.panel <- county.panel %>%
  ##create forc.high
  .[, forc.high := ifelse(forc.rf.pred >= forc.high.cutoff, 1L, 0L)]

forc.high.inter <- f_time_inter_char("forc.high")
county.panel <- county.panel %>%
  ##Create forc.high * time dummies
  .[, (forc.high.time) := NA_integer_] %>%
  .[, (forc.high.time) := .SD * forc.high, .SDcols = time.dummies.inter] %>%
  ##Create the high.forc * CA * time dummies
  .[, (forc.high.CA.time) := NA_integer_] %>%
  .[, (forc.high.CA.time) := .SD * CA * forc.high,
    .SDcols = time.dummies.inter]


county.base <- felm(as.formula(f.base.formula), data = county.panel,
                 weights = county.panel$hh2000)
county.full <- felm(as.formula(f.full.formula), data = county.panel,
                    weights = county.panel$hh2000)

out <- data.table(type = c("base", "full"),
                  model = list(county.base, county.full)) %>%
  ##Broom Glance
  .[, glance := lapply(model, broom::glance)] %>%
  .[, glance := lapply(glance, as.data.table)] %>%
  ##Broom tidy
  .[, tidy := lapply(model, broom::tidy)] %>%
  .[, tidy := lapply(tidy, as.data.table)] %>%
  ##Only the ddd coefficients and add the time column for the coefficients
  .[, tidy.ddd := lapply(model, f_forc.high_tidy_reg)]


time.post <- seq(from = as.Date("2008-07-01"), to = as.Date("2011-12-01"),
                 by = "1 month") %>% as.character %>% gsub("-", "_", x = .)
coefs.test.post <- paste0("forc.high_CA_time.char_", time.post, collapse = " + ")
print("cumulative DDD 2008-07-01 to 2011-12-01:")
out[type == "full", tidy.ddd][[1]] %>%
  .[time >= as.Date("2008-07-01") & time <= as.Date("2011-12-01"), sum(estimate)] %>%
  print
print("cumulative DDD F-test 2008-07-01 to 2011-12-01:")
car::linearHypothesis(county.full,
                      coefs.test.post
                      ) %>% broom::tidy(.) %>% print(.)

DT.for.graphs <- out$tidy.ddd %>% setNames(out$type) %>%
  rbindlist(idcol = "type")


saveRDS(out, "RdsFiles/10-county_reg_base_full_all.rds")
saveRDS(out, "RdsFiles/10-county_reg_base_full_all_for_graphs.rds")


## -- For the Synth output -- ##

county.panel.ca <- county.panel %>%
  .[CA == 1]

synth.county <- readRDS("../20-SynthCountyState/output/10-County_Forc/10-County_Forc_zillow.forc.rds")
synth.output <- synth.county$synth.output %>%
  .[, .(fips.code = region, time.index, gap)] %>%
  setkey(fips.code, time.index)

##forc.high.time <- f_time_inter_char("forc.high")
county.panel.ca <- merge(county.panel.ca, synth.output,
                         all.x = TRUE, by = c("fips.code", "time.index")) %>%
  .[, y := gap] %>%
  .[, time.char := as.character(time)] %>%
  .[, (forc.high.time) := NA_integer_]


f_formula_synth <- function(vars = NULL) {

  out <- paste0(
    "y ~ 0 + ",
    paste0(forc.high.time, collapse = " + ") ##forc.high * time
  )

  if (!is.null(vars)) {
    out <- paste0(out, " + ")
    for (temp.var in vars) {
      out <- paste0(out, " + ", temp.var, ":CFPL")
      ##For the bartik or unemp, make sure that we add in for the dropped dummy
      if (temp.var == "bartik") {
        out <- paste0(out, " + bartik")
      } else if (temp.var == "bartik.qcew") {
        out <- paste0(out, " + bartik.qcew")
      } else if (temp.var == "unemp.rate") {
        out <- paste0(out, " + unemp.rate")
      }

    }
  }
  out <- paste0(out, " | time.char + fips.code | 0 | fips.code")

  return(out)
}

f_forc.high_tidy_reg <- function(felm.mod) {
  if (!("felm" %in% class(felm.mod)))
    stop("Error: lm.mod must be of class 'lm'")

  ##Tidy the model,
  out <- felm.mod %>%
    broom::tidy(.) %>%
    as.data.table %>%
    ##only get the DDD terms
    .[grepl("forc.high_time.char_", term)] %>%
    ##Add a time column
    .[, time := gsub("forc.high_time.char_", "", term)] %>%
    .[, time := as.Date(time, "%Y_%m_%d")]

  return(out)

}

##To run the regression using the forc high cutoff
f_forc.high_reg <- function(forc.rf.pred.cutoff) {

  temp <- copy(county.panel.ca) %>%
    ##create forc.high
    .[, forc.high := ifelse(forc.rf.pred >= forc.rf.pred.cutoff, 1L, 0L)]
  ##create high.forc * time dummies
  temp <- temp %>%
    .[, (forc.high.time) := NA_integer_] %>%
    .[, (forc.high.time) := .SD * forc.high, .SDcols = time.dummies.inter]

  reg.base.pre.error <- felm(as.formula(f.base.formula.synth), data = temp,
                           weights = temp$hh2000) %>%
    f_forc.high_tidy_reg %>%
    .[time < as.Date("2008-07-01"), sum((estimate ^ 2))]

  return(reg.base.pre.error)

}




f.base.formula.synth <- f_formula_synth()
f.full.formula.synth <- f_formula_synth(control.vars)

##Run the cross-validation procedure in parallel
cl <- makeCluster(detectCores(logical = FALSE))
clusterEvalQ(cl, {library(CLmisc); library(lfe); library(broom)})
clusterExport(cl, varlist = c("county.panel.ca", "f_forc.high_tidy_reg",
                              "f.base.formula.synth", "f.full.formula.synth",
                              "forc.high.time", "time.dummies.inter"))
forc.high.seq.errors <- clusterMap(cl, f_forc.high_reg, forc.high.cutoff.seq,
                                   .scheduling = "dynamic")
stopCluster(cl)

##Get the optimal cutoff according to the cross-validation procedure
forc.high.seq.errors.which.min <- which.min(forc.high.seq.errors) %>%
  .[1]
forc.high.cutoff <- forc.high.cutoff.seq[forc.high.seq.errors.which.min]

##The county panel with the forc cutoff
county.panel.ca <- county.panel.ca %>%
  ##create forc.high
  .[, forc.high := ifelse(forc.rf.pred >= forc.high.cutoff, 1L, 0L)]

forc.high.inter <- f_time_inter_char("forc.high")
county.panel.ca <- county.panel.ca %>%
  ##Create forc.high * time dummies
  .[, (forc.high.time) := NA_integer_] %>%
  .[, (forc.high.time) := .SD * forc.high, .SDcols = time.dummies.inter]


##Base regression
county.synth.base <- felm(as.formula(f.base.formula.synth), data = county.panel.ca,
                          weights = county.panel.ca$hh2000)
county.synth.full <- felm(as.formula(f.full.formula.synth), data = county.panel.ca,
                          weights = county.panel.ca$hh2000)


out.synth <- data.table(type = c("base", "full"),
                        model = list(county.synth.base, county.synth.full)) %>%
  ##Broom Glance
  .[, glance := lapply(model, broom::glance)] %>%
  .[, glance := lapply(glance, as.data.table)] %>%
  ##Broom tidy
  .[, tidy := lapply(model, broom::tidy)] %>%
  .[, tidy := lapply(tidy, as.data.table)] %>%
  ##Only the ddd coefficients and add the time column for the coefficients
  .[, tidy.ddd := lapply(tidy, function(DT) {
    DT %>%
      .[grepl("forc.high_time.char_", term)] %>%
      .[, time := gsub("forc.high_time.char_", "", term)] %>%
      .[, time := as.Date(time, "%Y_%m_%d")]
  })]

DT.for.graphs.synth <- out.synth$tidy.ddd %>% setNames(out$type) %>%
  rbindlist(idcol = "type")


saveRDS(out.synth, "RdsFiles/10-synth_county_ddd.rds")
saveRDS(DT.for.graphs.synth, "RdsFiles/10-synth_county_ddd_for_graphs.rds")



##Run the F-test
time.post <- seq(from = as.Date("2008-07-01"), to = as.Date("2011-12-01"),
                 by = "1 month") %>% as.character %>% gsub("-", "_", x = .)
coefs.test.post <- paste0("forc.high_time.char_", time.post, collapse = " + ")
print("Synth cumulative DDD 2008-07-01 to 2011-12-01:")
out.synth[type == "base", tidy.ddd][[1]] %>%
  .[time >= as.Date("2008-07-01") & time <= as.Date("2011-12-01"), sum(estimate)] %>%
  print
print("cumulative DDD F-test 2008-07-01 to 2011-12-01:")
car::linearHypothesis(county.synth.base,
                      coefs.test.post) %>%
  broom::tidy(.) %>% print(.)




## update the county data and the zip data
forc.high.cutoff <- readRDS("RdsFiles/10-forc_high_cutoff_ddd.rds")
county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds") %>%
  .[, forc.high := ifelse(forc.rf.pred >= (forc.high.cutoff), 1L, 0L)]
saveRDS(county.panel, "../../Data/_Data_Final_/30-county_panel.rds")
zip.panel <- readRDS("../../Data/_Data_Final_/40-zip_panel.rds") %>%
  .[, forc.high := ifelse(forc.rf.pred >= (forc.high.cutoff), 1L, 0L)]
saveRDS(zip.panel, "../../Data/_Data_Final_/40-zip_panel.rds")
