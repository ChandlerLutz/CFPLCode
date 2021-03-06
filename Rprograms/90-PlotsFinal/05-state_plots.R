##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-08

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(scales); library(cowplot)})

vars.state <- list(
    c("forc", "Forc Starts (% of All Loans)", "Forc Starts (% of All Loans)",
      "quarter", "yes"),
    c("forc.prime", "Prime Forc Starts (% of Prime Loans)",
      "Prime Forc Starts (% of Prime Loans)", "quarter", "yes"),
    c("forc.subprime", "Subprime Forc Starts (% of Subprime Loans)",
      "Subprime Forc Starts (% of Subprime Loans)", "quarter", "yes"),
    c("State_Zillow_forc", "Zillow REO Forc per 10,000 Homes",
      "Zillow REO Forc per 10,000 Homes", "month", "yes"),
    c("mdri", "Growth in Mortgage Default Risk (MDRI)",
      "Growth in Mortgage Default Risk (MDRI; %)", "month", "yes"),
    c("fhfa_state_ret", "FHFA Returns",
      "FHFA HP Growth (%)", "quarter", "yes"),
    c("State_Zhvi_AllHomes_ret", "Zillow All Homes Returns",
      "Zillow All Homes HP Growth (%)", "month", "yes"),
    c("State_Zhvi_BottomTier_ret", "Zillow Bottom Tier Returns",
      "Zillow Bottom Tier HP Growth (%)", "month", "yes"),
    c("State_Zhvi_TopTier_ret", "Zillow Top Tier Returns",
      "Zillow Top Tier HP Growth (%)", "month", "yes")
)

## -- Determine the number of panels and add the panel names -- ##
##The number of panels graphs per panel specified by the user
graphs.per.panel <- 3
##Determine how many panels we with have
panels <- ceiling(length(vars.state) / graphs.per.panel)
##Create the panel labels
panel.labels <- expand.grid(panel = 1:panels, number = LETTERS[1:graphs.per.panel]) %>%
  as.data.table %>%
  .[order(panel)] %>%
  .[, paste0(panel, number, ": ")]


##add the panel.labels to the vars.state list
for (i in seq_along(vars.state)) {
    vars.state[[i]][length(vars.state[[i]]) + 1] <-
        paste0(panel.labels[i], vars.state[[i]][2])
}

##Set names for the vars.state and create matrices and dataframest
vars.state <- lapply(vars.state, function(x)
    setNames(x, c("var", "long.name", "table.name", "frequency", "plot", "plot.label")))
names(vars.state) <- vapply(vars.state, function(x) x[1], character(1))
vars.state.mat <- do.call("rbind", vars.state)
vars.state.df <- data.frame(vars.state.mat, stringsAsFactors = FALSE)

state.month <- readRDS("../../Data/_Data_Final_/05-state_month.rds") %>%
  .[state %in% c("AZ", "CA", "FL", "NV")]
state.quarter <- readRDS("../../Data/_Data_Final_/05-state_quarter.rds") %>%
  .[state %in% c("AZ", "CA", "FL", "NV")]

f_plot <- function(.var, plot.label) {

  ##M07/Q3
  sb1137.date <- as.Date("2008-07-01")

  if (.var %in% names(state.month)) {
    DT <- copy(state.month)
    ##M06
    cfpa.date <- as.Date("2009-06-15")
  } else {
    DT <- copy(state.quarter)
    ##Q2
    cfpa.date <- as.Date("2009-04-01")
  }

  DT %>%
    ##For the order of the states when plotted -- put CA on top
    .[, state := factor(state, levels = c("AZ", "FL", "NV", "CA"))] %>%
    .[, temp.var := get(.var)] %>%
    ##for the legend
    .[, treated := ifelse(CA == 1, "California", "AZ, FL, or NV")] %>%
    .[, treated := factor(treated, levels = c("California", "AZ, FL, or NV"))]

  ##The break dates
  break.dates <- seq(from = as.Date("2004-01-01"), to = as.Date("2015-01-01"),
                     by = "2 years")

  p.out <- ggplot(data = DT, aes(x = time, y = temp.var, group = state)) +
    geom_vline(xintercept = sb1137.date,
               color = "blue", linetype = 2,
               size = 1.1) +
    geom_vline(xintercept = cfpa.date,
               color = "blue", linetype = 2,
               size = 1.1) +
    geom_line(aes(color = treated, size = treated, alpha = treated)) +
    background_grid(major = "xy", minor = "none", size.major = 0.1,
                    colour.major = "grey91") +
    scale_alpha_manual(values = c(1, 0.75)) +
    scale_size_manual(values = c(1.1, 0.9)) +
    scale_color_manual(values = c("black", "#A020F0")) +
  ggtitle(plot.label) +
    scale_x_date(breaks = break.dates, date_labels = "%Y") +
    ##Update theme
    theme(
      ##Remove x and y labels
      axis.title.y = element_blank(), axis.title.x = element_blank(),
      ##Remove legend title
      legend.title = element_blank(),
      ##Increase legend text
      legend.text = element_text(size = 16),
      ##Add box around legend
      legend.background = element_rect(color="black", size=.5,
                                       linetype="solid"),
      ##Remove left and right margins -- for use in multiplot
      ##see https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
      plot.margin = unit(c(6,0,6,0), "pt")
    ) +
    ##make symbols in legend bigger. see
    ##http://stackoverflow.com/a/20416049/1317443
    guides(alpha = guide_legend(override.aes = list(size=1)))

  return(p.out)

}

plots.all <- Map(f_plot, vars.state.df$var, vars.state.df$plot.label)

##Get the legend for the shared legend
##see https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
legend.bottom <- get_legend(plots.all[[1]] + theme(legend.position = "bottom",
                                                   legend.justification = "center"))

##Remove the legends from all of the other figures
plots.all <- lapply(plots.all, function(p) p + theme(legend.position = "none"))


##The multiplot
plots.all <- plot_grid(plotlist = plots.all,
                   align = "hv",
                   hjust = -1,
                   nrow = panels)
##Add in the shared legend
plots.all <- plot_grid(plots.all, legend.bottom, ncol = 1,
                       rel_heights = c(1, 0.05))

save_plot("PlotsFinal/05-sand_states_plot.pdf", plots.all,
          base_height = 9, base_width = 16)
