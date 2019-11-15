##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-15

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe)})

county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds") %>%
  .[, ln.zillow.forc := log(zillow.forc)] %>%
  .[, ln.zillow.forc := ln.zillow.forc - ln.zillow.forc[1], by = fips.code] %>%
  .[, y := ln.zillow.forc] %>%
  .[, time.char := as.character(time)] %>%
  .[, fips.code := as.character(fips.code)] %>%
  ##CA only
  .[CA == 1]

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

##The forc.high * time dummies
forc.high.time <- f_time_inter_char("forc.high")
county.panel <- county.panel %>%
  .[, (forc.high.time) := NA_integer_] %>%
  .[, (forc.high.time) := .SD * forc.high,
    .SDcols = time.dummies.inter]


##Add all other interactions
control.vars <- c("zillow.ret.2008Q12", "land.unavailable",
          "non.occ.rate", "subprime.percent.2005",
          "IncomePerHousehold", "bartik", "bartik.qcew")

f_formula <- function(vars = NULL) {

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

##The formulas
f.base.formula <- f_formula()
##remove the unemployment rate as a control
control.vars <- control.vars %>% .[. != "unemp.rate"]
f.full.formula <- f_formula(c(control.vars))

county.base <- felm(as.formula(f.base.formula), data = county.panel,
                 weights = county.panel$hh2000)
county.full <- felm(as.formula(f.full.formula), data = county.panel,
                    weights = county.panel$hh2000)


##A function to tidy the DD regression output
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

DT.for.graphs <- out[, tidy.ddd] %>% setNames(c("base", "full")) %>%
  rbindlist(idcol = "mod.type") %>%
  .[, type := ifelse(grepl("base", mod.type), "No Controls", "Full Model")]


ggplot(DT.for.graphs, aes(x = time, y = estimate, group = type)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2008-07-01")) +
  geom_ribbon(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error,
                  fill = type), alpha = 0.25) +
  geom_line(aes(color = type))


saveRDS(out, "RdsFiles/50-county_reg_base_full_ca_only.rds")
saveRDS(DT.for.graphs, "RdsFiles/50-county_reg_base_full_ca_only_for_graphs.rds")


