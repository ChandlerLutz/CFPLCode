##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-02

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())

suppressMessages({library(CLmisc); library(cowplot); library(ggrepel)})

p.synth.cum.gap <- readRDS("RdsFiles/20-plot_synth_forc_county.rds") +
  ggtitle("1A: CFPL REO Foreclosure Synthetic Control Cumulative Gap per 10K homes") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        )

p.synth.map <- readRDS("RdsFiles/20-synth_county_ca_map.rds") +
  ggtitle("2: CFPL Synthetic Control Gap In REO Foreclosures per 10K Homes")

p.ddd <- readRDS("RdsFiles/10-plot_ddd_main.rds") +
  labs(title = "1B: CFPL REO Foreclosure Regression Triple Differences Monthly Estimates") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.95, .35))

p.synth.ddd <- readRDS("RdsFiles/10-plot_synth_ddd.rds") +
  ggtitle("1C: Synthetic Control CFPL REO Foreclosure Triple Differences Monthly Estimates") +
  theme(legend.position = c(.95, .35),
        axis.title.y = element_blank())

p.grid.left <- cowplot::plot_grid(p.synth.cum.gap, p.ddd, p.synth.ddd,
                                  align = "hv", nrow = 3,
                                  rel_heights = c(1.25, 1, 1))

p.grid <- cowplot::plot_grid(p.grid.left, p.synth.map, nrow = 1, ncol = 2,
                             rel_widths = c(1.1, 1))
cowplot::save_plot("PlotsFinal/30-county_plots_grid.pdf", p.grid,
                   base_height = 9.13, base_width = 16.9235)
