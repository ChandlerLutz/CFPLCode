##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-16

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

source("../../SynthMult/SynthMult.R", chdir = TRUE)

DT.state.synth <- list.files("../20-SynthCountyState/output/31-synth_state/", full.names = TRUE) %>%
  .[grepl(".rds$", x = .)] %>%
  lapply(readRDS) %>%
  lapply(function(x) x$synth.output) %>%
  rbindlist %>%
  .[, .(variable, time.index, California = value, control)]

f_plot <- function(.var, plot.label) {

  DT.temp <- DT.state.synth[variable == c(.var)] %>%
    .[, variable := NULL] %>%
    melt(id.vars = c("time.index")) %>%
    .[variable == "control", variable := "Synthetic Control"]


  p <- ggplot(DT.temp, aes(x = time.index, y = value, group = variable)) +
    geom_vline(xintercept = 2008.5,
               color = "blue", linetype = 2,
               size = 1.1) +
    geom_vline(xintercept = 2009.49,
               color = "blue", linetype = 2,
               size = 1.1) +
    geom_line(aes(color = variable, size = variable, alpha = variable)) +
    ##Update the alphas
    scale_alpha_manual(values = c(1, 1)) +
    ##Update the linetypes
    scale_linetype_manual(values = c(1, 1)) +
    ##Update the size
    scale_size_manual(values = c(1.1, 1)) +
    ##Update the color
    scale_color_manual(values = c("black", "#A020F0")) +
    ##the break dates
    scale_x_continuous(breaks = seq(2004L, 2014L, by = 2L)) +
    ##the plot title
    ggtitle(plot.label) +
    ##the cowplot background grid
    background_grid(major = "xy", minor = "none", size.major = 0.1,
                    colour.major = "grey91") +
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
print(plots.all)

save_plot("PlotsFinal/06-synth_states_plot.pdf", plots.all,
          base_height = 9, base_width = 16)
