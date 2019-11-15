## _RunAll_.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-27

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())

##get the R files after a little cleaning
R.files.Rprograms <- list.files("Rprograms/", recursive = TRUE)
R.files.Rprograms <- paste0("Rprograms/", R.files.Rprograms)

R.files.forc.costs <- list.files(path = "R-loan_level_forc_costs/", recursive = TRUE)
R.files.forc.costs <- paste0("R-loan_level_forc_costs/", R.files.forc.costs)
R.files.2step <- list.files(path = "R-LL_forc_2step/", recursive = TRUE)
R.files.2step <- paste0("R-LL_forc_2step/", R.files.2step)

R.files <- c(R.files.Rprograms, R.files.forc.costs, R.files.2step)
R.files <- R.files[grepl(".R$", R.files)]

##remove some files we don't need
R.files <- R.files[!grepl("SynthMult\\/", R.files)]
R.files <- R.files[!grepl("_RunAll_.R", R.files)]

saveRDS(R.files, "CFPLML_scratch_All_Rfiles_to_run.rds")

##run all of the files
lapply(R.files, function(f) {
  print(f)
  source(f, chdir = TRUE)
  return(NULL)
})

## BBX programs
if (file.exists("CFPL_Border/data-raw/30-bbx_data_lake_tahoe_zip3_894_961.csv"))
    source("CFPL_Border/_RunAll_.R", chdir = TRUE)

library(CLmisc)
from.current.or.default.files <- list.files("CFPL_FromDefaultOrCurrent",
                                            full.names = TRUE,
                                            recursive = TRUE) %>%
  .[grepl(".R$", x = .)] %>%
  .[!grepl("scratch", x = .)]

if (file.exists("R_CFPL_Step1_Default_CsvFiles/010-delin0_to_delin0/010-delin0_to_delin0_2008-01-01.csv")) {

  lapply(from.current.or.default.files, function(x) {
    print(x)
    source(x, chdir = TRUE)
  })
}


if (file.exists("CFPL_OCC/data-raw/R_CFPL_OCC_CsvFiles/010-DT_est_forc_start.csv"))
    source("CFPL_OCC/10-plot_cfpl_occ.R", chdir = TRUE)


## Create the file output for tables and figures

if (!dir.exists("_FinalPlotsTables_")) dir.create("_FinalPlotsTables_")

if (!dir.exists("_FinalPlotsTables_/main")) dir.create("_FinalPlotsTables_/main")

if (!dir.exists("_FinalPlotsTables_/appendix")) dir.create("_FinalPlotsTables_/appendix")


##custom copy function
f_copy <- function(from, to) {
  if (!file.exists(from)) return(invisible())

  file.copy(from = from, to = to)
}

##Main Figures
f_copy("Rprograms/90-PlotsFinal/PlotsFinal/05-sand_states_plot.pdf",
       "_FinalPlotsTables_/main/figure-01.pdf")
f_copy("Rprograms/90-PlotsFinal/PlotsFinal/30-county_plots_grid.pdf",
       "_FinalPlotsTables_/main/figure-02.pdf")
f_copy("R-LL_forc_2step/90-PlotsFinal/PlotsFinal/10-reo_forc_rate_ll_ddd_est.pdf",
       to = "_FinalPlotsTables_/main/figure-03.pdf")
f_copy("CFPL_FromDefaultOrCurrent/R/PlotsFinal/010-p_trans_probs_distressed_to_forc.pdf",
       to = "_FinalPlotsTables_/main/figure-04.pdf")
f_copy("CFPL_Border/R/PlotsFinal/30-p_bbx_ca_border_all.pdf",
       to = "_FinalPlotsTables_/main/figure-05.pdf")
f_copy("CFPL_OCC/PlotsFinal/10-p_occ_vs_non_occ_ca_forc_start_mod.pdf",
       to = "_FinalPlotsTables_/main/figure-06.pdf")
f_copy("R-LL_forc_2step/90-PlotsFinal/PlotsFinal/10-mod_rate_ll_dd_est.pdf",
       to = "_FinalPlotsTables_/main/figure-07.pdf")
f_copy(
  "CFPL_FromDefaultOrCurrent/R/PlotsFinal/020-p_trans_probs_distressed_to_current.pdf",
  to = "_FinalPlotsTables_/main/figure-08.pdf")
f_copy("Rprograms/90-PlotsFinal/PlotsFinal/40-zip_plots_grid.pdf",
       to = "_FinalPlotsTables_/main/figure-09.pdf")
f_copy("CFPL_FromDefaultOrCurrent/R/PlotsFinal/030-p_trans_prob_strategic_default_current_straight_to_delin90.pdf",
       to = "_FinalPlotsTables_/main/figure-10.pdf")

##Main tables
f_copy("R-loan_level_forc_costs/TexFiles/10-forc_costs_reg_non_judicial.tex",
       to = "_FinalPlotsTables_/main/table-01.tex")
f_copy("R-loan_level_forc_costs/TexFiles/20-forc_duration_reg.tex",
       to = "_FinalPlotsTables_/main/table-02.tex")

##Appendix
f_copy("Rprograms/20-SynthCountyState/TexFiles/22-synth_state_est_output.tex",
       "_FinalPlotsTables_/appendix/B1-table.tex")
f_copy("Rprograms/90-PlotsFinal/PlotsFinal/06-synth_states_plot.pdf",
       "_FinalPlotsTables_/appendix/B1-figure.pdf")
f_copy("Rprograms/10-RandomForest/PlotsFinal/10-RF_variable_importance.pdf",
       "_FinalPlotsTables_/appendix/D-figure.pdf")
f_copy("Rprograms/90-PlotsFinal/PlotsFinal/25-p_ca_synth_map_of_largest_controls.pdf",
       "_FinalPlotsTables_/appendix/E1-figure.pdf")
f_copy("Rprograms/90-PlotsFinal/PlotsFinal/15-plot_CFPL_ddd_trends_bartik.pdf",
       "_FinalPlotsTables_/appendix/F1-figure.pdf")
f_copy("R-LL_forc_2step/90-PlotsFinal/PlotsFinal/10-forc_alt_ll_ddd_est.pdf",
       "_FinalPlotsTables_/appendix/G1-figure.pdf")
f_copy("CFPL_Border/R/PlotsFinal/50-p_tahoe_border_region.pdf",
       "_FinalPlotsTables_/appendix/H1-figure.pdf")
f_copy("CFPL_Border/R/PlotsFinal/51-p_az_ca_nv_border_region.pdf",
       "_FinalPlotsTables_/appendix/H2-figure.pdf")
f_copy("R-loan_level_forc_costs/TexFiles/11-forc_costs_reg_all_states.tex",
       "_FinalPlotsTables_/appendix/I1-table.tex")
f_copy("Rprograms/90-PlotsFinal/TexFiles/40-zip_reg.tex",
       "_FinalPlotsTables_/appendix/J1-table.tex")
f_copy("Rprograms/50-HmdaReg/TexFiles/10-hmda_reg_star_out.tex",
       "_FinalPlotsTables_/appendix/K1-table.tex")


