##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-01

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(cowplot)})

##The plot function
f_plot <- function(tidy.ddd) {

  p.out <- ggplot(tidy.ddd, aes(x = time)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = as.Date("2008-07-08"), color = "blue", linetype = 2,
               size = 1.1) +
    geom_vline(xintercept = as.Date("2009-06-15"), color = "blue", linetype = 2,
               size = 1.1) +
    geom_ribbon(aes(ymin = estimate - 2 * std.error,
                    ymax = estimate + 2 * std.error,
                    fill = model.type), alpha = 0.25) +
    geom_line(aes(y = estimate, color = model.type), size = 1.1) +
    ##The cowplot background
    background_grid(major = "xy", minor = "none", size.major = 0.1,
                    colour.major = "grey91") +
    ##The break dates
    scale_x_date(breaks = seq(from = as.Date("2007-01-01"), to = as.Date("2015-01-01"),
                              by = "1 year"), date_labels = "%Y") +
    labs(x = "Time in Months", y = "DDD Coefficient",
         title = "CFPL REO Foreclosure DDD Estimates") +
    theme(
      ##Remove legend title
      legend.title = element_blank(),
      ##Add box around legend
      legend.background = element_rect(color="black", size=.5, linetype="solid"),
      ##Increase legend text
      legend.text = element_text(size = 14),
      ##Move the legend
      legend.position = c(.95, .15),
      legend.justification = c("right", "top"),
      legend.margin = margin(6, 6, 6, 6),
      ##Remove left and right margins -- for use in multiplot
      ##see
      ##https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
      plot.margin = unit(c(6,0,6,0), "pt")
    )

  return(p.out)

}

##the regression output
reg.all <- readRDS("../25-CountyReg/RdsFiles/10-county_reg_base_full_all.rds")


tidy.ddd <- reg.all$tidy.ddd %>%
  setNames(reg.all[, type]) %>%
  rbindlist(idcol = "model") %>%
  .[, model.type := ifelse(grepl("base", model), "No Controls", "Full Model")] %>%
  .[, model.type := factor(model.type, levels = c("No Controls", "Full Model"))]

## -- Main -- ##

main.plot <- tidy.ddd %>%
  f_plot +
  ggtitle("2A: CFPL REO Foreclosure Regression DDD Estimates")
print(main.plot)

saveRDS(main.plot, "RdsFiles/10-plot_ddd_main.rds")

## -- Synth County -- ##

synth.reg.all <- readRDS("../25-CountyReg/RdsFiles/10-synth_county_ddd.rds")
synth.tidy.ddd <- synth.reg.all$tidy.ddd %>%
  setNames(c("base", "full")) %>%
  rbindlist(idcol = "model") %>%
  .[, model.type := ifelse(grepl("base", model), "No Controls", "Full Model")] %>%
  .[, model.type := factor(model.type, levels = c("No Controls", "Full Model"))]

synth.ddd.plot <- f_plot(synth.tidy.ddd) +
  ggtitle("3A: Synth CFPL REO Foreclosure DDD Estimates")
print(synth.ddd.plot)
saveRDS(synth.ddd.plot, "RdsFiles/10-plot_synth_ddd.rds")
