##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-07





##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(cowplot)})

p.bbx.tahoe <- readRDS("RdsFiles/10-p_bbx_diff_diff_tahoe.rds")
p.bbx.ca.border <- readRDS("RdsFiles/20-p_bbx_diff_diff_az_ca_nv.rds")

p.all <- plot_grid(
  p.bbx.tahoe +
    labs(y = "Monthly Probability of REO Foreclosure\n(Basis Points)"),
  p.bbx.ca.border +
    labs(y = "Monthly Probability of REO Foreclosure\n(Basis Points)"),
                   align = "hv", nrow = 2)
ggsave(filename = "PlotsFinal/30-p_bbx_ca_border_all.pdf",
       width = 9.2, height = 8.5, dpi = 600)
