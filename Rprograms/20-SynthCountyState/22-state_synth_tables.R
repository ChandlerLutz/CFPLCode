##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-20

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())



suppressMessages({library(CLmisc); library(stargazer); library(starpolishr)})

source("../../SynthMult/SynthMult.R", chdir = TRUE)

vars.state.DT <- readRDS("RdsFiles/30-state_vars_df.rds") %>% as.data.table

state.synth <- list.files("output/31-synth_state/", full.names = TRUE) %>%
  .[grepl(".rds$", x = .)] %>%
  lapply(readRDS)

f_output <- function(synth.obj) {

  temp.var <- synth.obj$dependent.vars
  synth.output <- synth.obj$synth.output
  synth.perm <- synth.obj$perm.output
  synth.mspe <- synth.obj$mspe.pre.output[, mspe.pre]

  var.name <- vars.state.DT[var == temp.var, table.name]

  synth.gap.cum <- synth.output %>%
    .[time.index >= 2008.5 & time.index < 2012, lapply(.SD, sum),
      .SDcols = names(.) %in% c("value", "control", "gap")] %>%
    as.matrix %>%
    .[1, ]

  synth.perm.output <- synth.perm %>%
    .[time.index >= 2008.5 & time.index < 2012,
      .(gap = sum(gap)), by = region]

  f_ecdf <- ecdf(synth.perm.output$gap)

  gap.percentile <- f_ecdf(synth.gap.cum["gap"]) * 100

  ##Get everything as strings
  synth.gap.cum <- sprintf("%.02f", synth.gap.cum)
  gap.percentile <- sprintf("%.02f", gap.percentile)
  synth.mspe <- sprintf("%.02f", synth.mspe)

  return(c(var.name, synth.mspe, synth.gap.cum, gap.percentile))

}

output <- lapply(state.synth, f_output) %>%
  do.call("rbind", args = .) %>%
  .[c(3, 2, 4, 9, 5, 1, 6, 7, 8), ]

colnames(output) <- c("", "MSE", "CA", "Synth", "Gap", "GapPcntle")

## -- stargazer output -- ##
star1 <- stargazer(output[1:5, ], type = "latex",
                   title = "\\textbf{State-Level Synthetic Control Estimation Results}",
                   label = "tab:synth_state")
star2 <- stargazer(output[6:9, ], type = "latex")

star.all <- star_panel(star1, star2,
                       panel.names = c("Foreclosures and the MDRI", "House Price Growth"),
                       panel.label.fontface = "bold",
                       reg = FALSE) %>%
  sub("cccccc", "lccccc", x = .) %>%
  star_lhs_names(pattern = c("MSE", "CA", "Synth", "Gap", "GapPcntle"),
                 line1 = c("Pre", "", "", "Gap", "Gap"),
                 line2 = c("MSE", "CA", "Synth", "(CA - Synth)", "Pcntle")) %>%
  star_add_column_numbers(insert.after = 11) %>%
  star_insert_row(
    c("&  & \\multicolumn{4}{c}{CFPL Treatment Period} \\\\ \n",
      "\\cline{3-6} \\\\[-1.8ex]"),
    insert.after = 9) %>%
  star_notes_tex(note.type = "caption",
                 note = "The far left column lists the outcome variable, Pre-MSE in column (1) is the mean-squared error from the Synthetic control match during the pre-treatment period (2004M01-2008M06), the next two columns (2 \\& 3) show the change in the outcome variable for California and its Synthetic Control during the CFPL treatment period, and Gap in column (4) is the difference between of the change in the outcome variable for treated unit  (California) relative to its Synthetic Control. The MDRI over the treatment period is the cumulative sum in its index points over the treatment period. Column (5) shows the percentile of the Gap estimate relative to all placebo effects estimated via falsification tests where we iteratively apply the treatment to all other states. The Gap Percentile is then calculated by first estimating the empirical CDF from all placebo effects and then calculating the percentile of the Gap for California relative to the CDF of placebo effects. The treatment period ranges from 2008M07 to 2011M12. The variable descriptions and data sources are in the notes to figure \\ref{fig:sand_states} and in appendix \\ref{sec:appendix}.")


star_tex_write(star.all, file = "TexFiles/22-synth_state_est_output.tex",
               headers = TRUE)
