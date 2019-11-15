##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-02

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(ggalt); library(cowplot); library(latex2exp)})

##helper function to change the data
f_clean <- function(DT)
  DT <- DT[, mod := factor(mod, levels = c("Loan Controls", "Loan and Macro Controls"))]

##Reo forc
DT.est.reo.forc <- readRDS("../RdsFiles/10-reo_forc_rate_reg_2step.rds") %>%
  f_clean
##Mod DD
DT.est.mod.dd <- readRDS("../RdsFiles/20-mod_rate_reg_2step_dd.rds") %>%
  f_clean
##ForcAlt DD
DT.est.forc.alt.dd <-
  readRDS("../RdsFiles/30-forc_alt_rate_reg_2step_dd_forc_high_only.rds") %>%
  f_clean
##ForcAlt DDD
DT.est.forc.alt.ddd <- readRDS("../RdsFiles/30-forc_alt_reg_2step_ddd.rds") %>%
  f_clean


f_plot <- function(DT.plot) {

  DT.plot <- copy(DT.plot) %>%
    .[, estimate := estimate * 1000] %>%
    .[, std.error := std.error * 1000]

  p.out <- ggplot(DT.plot, aes(x = time, group = mod)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = as.Date("2008-07-08"), color = "blue", linetype = 2,
               size = 1.1) +
    geom_vline(xintercept = as.Date("2009-06-15"), color = "blue", linetype = 2,
               size = 1.1) +
    geom_ribbon(aes(ymin = estimate - 2 * std.error,
                    ymax = estimate + 2 * std.error,
                    fill = mod),
                ##stat = "stepribbon",
                alpha = 0.25) +
    geom_line(aes(y = estimate, color = mod), size = 1.1) +
    ##The cowplot background
    background_grid(major = "xy", minor = "none", size.major = 0.1,
                    colour.major = "grey91") +
    ##The break dates
    scale_x_date(breaks = seq(from = as.Date("2007-01-01"), to = as.Date("2015-01-01"),
                              by = "1 year"), date_labels = "%Y") +
    labs(x = "Time in Months") +
    theme(
      ##left adjsut title
      plot.title = element_text(hjust = 0),
      ##Remove legend title
      legend.title = element_blank(),
      ##Add box around legend
      legend.background = element_rect(color="black", size=.5, linetype="solid"),
      ##Increase legend text
      legend.text = element_text(size = 14),
      ##Move the legend
      legend.position = c(.95, .2),
      legend.justification = c("right", "top"),
      legend.margin = margin(6, 6, 6, 6),
      ##Remove left and right margins -- for use in multiplot
      ##see
      ##https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
      plot.margin = unit(c(6,0,6,0), "pt")
    )

  return(p.out)

}


p.ddd.reo.forc <- f_plot(DT.est.reo.forc[trends == "No"]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Probability of REO Foreclosure\n(Basis Points)",
       title = "A: CFPL Loan-Level Probability of REO Foreclosure Estimates",
       subtitle = "Triple Differences Monthly Estimates")
p.ddd.reo.forc

p.ddd.reo.forc.trends <- f_plot(DT.est.reo.forc[trends == "Yes"]) +
  labs(y = "Probability of REO Foreclosure\n(Basis Points)",
       title = "B: CFPL Loan-Level Probability of REO Foreclosure Estimates",
       subtitle = "Triple Differences Monthly Estimates -- With Zip3 Time Trends")

p.ddd.reo.forc.trends

p.ddd.reo.forc.all <- cowplot::plot_grid(p.ddd.reo.forc, p.ddd.reo.forc.trends,
                                         align = "hv", nrow = 2)

ggsave("PlotsFinal/10-reo_forc_rate_ll_ddd_est.pdf", width = 9.7, height = 8.4,
       dpi = 900)

## -- Mod Rate DD -- ##

p.dd.mod <- f_plot(DT.est.mod.dd[trends == "No"]) +
  theme(
    ##Move the legend
    legend.position = c(.95, .80)) +
  labs(
    y = ("Probability of Modification\n(Basis Points)"),
    title = "A: CFPL Loan-Level Probability of Modification Estimates",
    subtitle = "Difference-in-Differences Monthly Estimates"
  )


p.dd.mod.trends <- f_plot(DT.est.mod.dd[trends == "Yes"]) +
  theme(
    ##Move the legend
    legend.position = c(.95, .80)
  ) +
  labs(
    y = ("Probability of Modification\n(Basis Points)"),
    title = "B: CFPL Loan-Level Probability of Modification Estimates",
    subtitle = "Difference-in-Differences Monthly Estimates -- With Zip3 Time Trends"
  )

p.dd.mod.all <- cowplot::plot_grid(p.dd.mod, p.dd.mod.trends,
                                   align = "hv", nrow = 2)
ggsave("PlotsFinal/10-mod_rate_ll_dd_est.pdf", width = 9.7, height = 8.4,
       dpi = 900)

## -- Forc Alt DDD -- ##

p.ddd.forc.alt <- f_plot(DT.est.forc.alt.ddd[trends == "No"]) +
  theme(
    ##Move the legend
    legend.position = c(.35, .25)) +
  labs(
    y = "Probabilty of Foreclosure Alternate\n(Basis Points)",
    title = "A: CFPL Loan-Level Probability of Foreclosure Alternate",
    subtitle = "Triple Differences Monthly Estimates Regressions"
  )


p.ddd.forc.alt.trends <- f_plot(DT.est.forc.alt.ddd[trends == "Yes"]) +
  theme(
    ##Move the legend
    legend.position = c(.35, .25)
  ) +
  labs(
    y = "Probabilty of Foreclosure Alternate\n(Basis Points)",
    title = "B: CFPL Loan-Level Probability of Foreclosure Alternate",
    subtitle = "Triple Differences Monthly Estimates Regressions -- With Zip3 Time Trends"
  )

p.ddd.forc.alt.all <- cowplot::plot_grid(p.ddd.forc.alt, p.ddd.forc.alt.trends,
                                   align = "hv", nrow = 2)
ggsave("PlotsFinal/10-forc_alt_ll_ddd_est.pdf", width = 9.7, height = 8.4,
       dpi = 900)


