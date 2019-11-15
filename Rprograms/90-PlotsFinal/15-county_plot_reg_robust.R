##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-03

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
                    fill = type), alpha = 0.25) +
    geom_line(aes(y = estimate, color = type), size = 1.1) +
    ##The cowplot background
    background_grid(major = "xy", minor = "none", size.major = 0.1,
                    colour.major = "grey91") +
    ##The break dates
    scale_x_date(breaks = seq(from = as.Date("2007-01-01"), to = as.Date("2015-01-01"),
                              by = "1 year"), date_labels = "%Y") +
    labs(y = "DDD Coefficient",
         title = "CFPL REO Foreclosure DDD Estimates") +
    theme(
      ##Remove legend title
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      ##Add box around legend
      legend.background = element_rect(color="black", size=.5, linetype="solid"),
      ##Increase legend text
      legend.text = element_text(size = 14),
      ##Move the legend
      legend.position = c(.97, .4),
      legend.justification = c("right", "top"),
      legend.margin = margin(6, 6, 6, 6),
      ##Remove left and right margins -- for use in multiplot
      ##see
      ##https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
      plot.margin = unit(c(6,0,6,0), "pt")
    )

  return(p.out)

}


DT.trends <-
  readRDS("../25-CountyReg/RdsFiles/40-county_reg_base_full_all_trends_for_graphs.rds") %>%
  .[, type := factor(type, levels = c("No Controls", "Full Model"))]

p.trends.linear <- f_plot(DT.trends[trend.type == "linear"]) +
  labs(title = "1A: CFPL REO Foreclosures per 10K Homes Triple Differences Estimates",
       subtitle = "Monthly Estimates With County Linear Time Trends",
       y = "Foreclosures per 10K Homes\n(Triple Differences Coefficient)") +
  theme(plot.title = element_text(hjust = 0))

p.trends.quadratic <- f_plot(DT.trends[trend.type == "quadratric"]) +
  labs(title = "1B: CFPL REO Foreclosures per 10K Homes Triple Differences Estimates",
       subtitle = "Monthly Estimates With County Linear & Quadratic Time Trends",
       y = "Foreclosures per 10K Homes\n(Triple Differences Coefficient)") +
  theme(plot.title = element_text(hjust = 0),
        plot.margin = unit(c(6, 0, 6, 15), "pt"))

DT.bartik.qcew <- readRDS("../25-CountyReg/RdsFiles/30-county_reg_bartik_qcew_base_full_all_for_graphs.rds") %>%
  .[, type := factor(type, levels = c("No Controls", "Full Model"))]

p.bartik <- f_plot(DT.bartik.qcew) +
  labs(title = "2A: Falsification Test -- Bartik Triple Differences Monthly Estimates",
       y = "Bartik Shock\n(Triple Differences Coefficient)") +
  theme(plot.title = element_text(hjust = 0),
        legend.position = c(0.97, 0.9),
        axis.title.x = element_blank())

DT.bartik.hp.2008Q34 <- readRDS("../25-CountyReg/RdsFiles/36-county_reg_bartik_hp_ret2008Q34_control_base_full_all_for_graphs.rds")

p.bartik.hp.2008Q34 <- f_plot(DT.bartik.hp.2008Q34) +
  theme(legend.position = c(.99, .3)) +
  labs(title = "2B: CFPL REO Foreclosures per 10K Homes Triple Differences Estimates",
       subtitle = "Controlling for 2008Q3 and 2008Q4 Bartik x HP Growth Interactions",
       y = "Foreclosures per 10K Homes\n(Triple Differences Coefficient)"
       ) +
  theme(plot.title = element_text(hjust = 0),
        plot.margin = unit(c(6, 0, 6, 15), "pt"))

DT.synth.ca.only <-
  readRDS("../89-Robustness/RdsFiles/13-synth_ca_only_forc_high.rds")

p.synth.ca.only <- ggplot(DT.synth.ca.only[!duplicated(time)], aes(x = time)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = as.Date("2008-07-08"), color = "blue", linetype = 2,
             size = 1.1) +
  geom_vline(xintercept = as.Date("2009-06-15"), color = "blue", linetype = 2,
             size = 1.1) +
  geom_ribbon(mapping = aes(ymin = perm.gap.cumsum.lower, ymax = perm.gap.cumsum.upper),
              fill = "gray60", alpha = 0.25) +
  geom_line(data = DT.synth.ca.only,
            mapping = aes(x = time, y = gap.cumsum, linetype = gap.cumsum.percentile,
                          color = gap.cumsum.percentile)) +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  ##The break dates
  scale_x_date(breaks = seq(from = as.Date("2007-01-01"), to = as.Date("2015-01-01"),
                            by = "1 year"), date_labels = "%Y") +
  scale_y_continuous(breaks = c(-50, 0, 50)) +
  labs(title = "3A: California Only -- REO Foreclosure Cumulative Estimates",
       subtitle = "Synthetic Control Cumulative Gap Growth (Log Points)",
       y = "Log of Foreclosures\n(Cumulative Gap)") +
  theme(
    plot.title = element_text(hjust = 0),
    axis.title.x = element_blank(),
    ##Remove legend title
    ##legend.title = element_blank(),
    ##Add box around legend
    legend.background = element_rect(color="black", size=.5, linetype="solid"),
    ##Increase legend text
    legend.text = element_text(size = 14),
    ##Increase the legend size https://stackoverflow.com/a/41843082/1317443
    legend.key.size = unit(2,"line"),
    ##Move the legend
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6),
    ##Remove left and right margins -- for use in multiplot
    ##see
    ##https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
    plot.margin = unit(c(0,0,0,0), "pt")
  )


##Add the guide
g <- guide_legend(nrow = 1, byrow = TRUE, title = "Forc High Percentile")
p.synth.ca.only <- p.synth.ca.only + guides(color = g, linetype = g)

DT.ca.only.dd <- readRDS("../25-CountyReg/RdsFiles/50-county_reg_base_full_ca_only_for_graphs.rds") %>%
  .[, type := factor(type, levels = c("No Controls", "Full Model"))]
p.ca.only.dd <- f_plot(DT.ca.only.dd) +
  labs(title = "3B: California Only -- High versus Low Foreclosure Counties",
       subtitle = "Monthly REO Foreclosures Difference-in-Differences Estimates (Log Points)",
       y = "Log of Foreclosures\n(Diff-in-Diff Coefficient)") +
  theme(plot.title = element_text(hjust = 0),
        plot.margin = unit(c(6, 0, 6, 15), "pt"),
        legend.position = c(.2, .4))



p.all <- plot_grid(p.trends.linear, p.trends.quadratic, p.bartik,
                   p.bartik.hp.2008Q34, p.synth.ca.only, p.ca.only.dd,
                   align = "hv",
                   nrow = 3)
print(p.all)
ggsave("PlotsFinal/15-plot_CFPL_ddd_trends_bartik.pdf", plot = p.all,
       width = 16.9235, height = 9.13)
