##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-08

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); })

CW <- readRDS("data-raw/10-CW_county_to_zip3_2010.rds")

DT.zip3.state <- copy(CW) %>%
  .[, max.hh := max(housing.units), by = zip3] %>%
  .[housing.units == max.hh] %>%
  .[, state := substr(fips, 1, 2)] %>%
  .[!duplicated(paste0(state, zip3))] %>%
  .[, .(state, zip3)]

saveRDS(DT.zip3.state, "RdsFiles/10-DT_cw_zip3_to_state.rds")
