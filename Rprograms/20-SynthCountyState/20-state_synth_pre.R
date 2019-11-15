##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-20

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


state.month <- readRDS("../../Data/_Data_Final_/05-state_month.rds")
state.quarter <- readRDS("../../Data/_Data_Final_/05-state_quarter.rds")

vars.state <- list(
    c("forc", "Forc Starts (% of All Loans)", "Forc Starts (% of All Loans)",
      "quarter"),
    c("forc.prime", "Prime Forc Starts (% of Prime Loans)",
      "Prime Forc Starts (% of Prime Loans)", "quarter"),
    c("forc.subprime", "Subprime Forc Starts (% of Subprime Loans)",
      "Subprime Forc Starts (% of Subprime Loans)", "quarter"),
    c("State_Zillow_forc", "Zillow REO Forc per 10,000 Homes",
      "Zillow REO Forc per 10,000 Homes", "month"),
    c("mdri", "Growth in Mortgage Default Risk (MDRI)",
      "Mortgage Default Risk (MDRI)", "month"),
    c("fhfa_state_ret", "FHFA Returns",
      "FHFA HP Growth (%)", "quarter"),
    c("State_Zhvi_AllHomes_ret", "Zillow All Homes Returns",
      "Zillow All Homes HP Growth (%)", "month"),
    c("State_Zhvi_BottomTier_ret", "Zillow Bottom Tier Returns",
      "Zillow Bottom Tier HP Growth (%)", "month"),
    c("State_Zhvi_TopTier_ret", "Zillow Top Tier Returns",
      "Zillow Top Tier HP Growth (%)", "month")
)

##Set names for the vars.state and create matrices and dataframest
vars.state <- lapply(vars.state, function(x)
    setNames(x, c("var", "long.name", "table.name", "frequency")))
names(vars.state) <- vapply(vars.state, function(x) x[1], character(1))
vars.state.mat <- do.call("rbind", vars.state)
vars.state.df <- data.frame(vars.state.mat, stringsAsFactors = FALSE)

saveRDS(vars.state.df, "RdsFiles/30-state_vars_df.rds")
