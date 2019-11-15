## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/plot_functions_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15

##A set of functions to create the plots

##a function to make sure the plots are receiving the
##right input for the treated unit and dependent variable
f.plot.check <- function(env = parent.frame()) {
  if (length(env$treated.unit) != 1) {
    stop("treated.unit needs to be of length 1")
  }
  if (length(env$dependent.var) != 1) {
    stop("dependent.var needs to be of length 1")
  }
  return(NULL)
}


## a function for some aesthetics for the plot
f.aes <- function(plot, policy.line = TRUE) {
  plot <- plot +
    theme_bw() +
    theme(legend.title = element_blank(),axis.title.x = element_blank(),
          legend.justification=c(0,0), legend.position=c(0,0),
          legend.text=element_text(size=16),
          legend.background =
            element_rect(fill="gray90", size=.5,linetype="dotted")
          )
  if (policy.line) {
    plot <- plot +
      geom_vline(xintercept = self$time.policy, colour="dark red",
                 linetype="longdash")
  }
  return(plot)

}

plot.gap.helper <- function(env = parent.frame()) {

  ## A function to plot just the gap that will be used in later functions
  force(env$treated.unit); force(env$dependent.var)
  synth.output <- env$synth.output %>%
    .[region %in% env$treated.unit & variable %in% env$dependent.var]

  gap.data <- synth.output %>%
    .[, .(region, time.index, gap)] %>%
    as.data.frame(.)

  p <- ggplot(gap.data, aes(x = time.index, y = gap)) +
    geom_line(aes(linetype = region))
  p <- private$f.aes(p) +
    labs(y = paste0("Gap in ", self$y.axis.label[env$dependent.var]))
  return(p)
}

create_plots <- function() {

  ##Map to create the plots using self$plot_all()
  plots.out <- Map(self$plot_all,
                   as.list(self$grid.outcomes$treated),
                   as.list(self$grid.outcomes$dependent.var)
                   )

  ##Add the plot names
  names(plots.out) <- self$grid.outcomes$id

  f.save <- function(temp.plot, temp.name) {
    ##create a directory if necessary
    if (!dir.exists("plots/")) dir.create("plots/")
    ##ggsave
    ggsave(filename = paste0("plots/", self$sample, "_",  temp.name, ".pdf"),
           plot = temp.plot, dpi = 600, width = 15, height = 8)
    return(NULL)
  }

  Map(f.save, plots.out, self$grid.outcomes$id)

  self$plots.all <- plots.out

  return(NULL)
}


plot_all <- function(treated.unit, dependent.var,
                     synth.output = self$synth.output,
                     perm.output = self$perm.output,
                     policy.line = TRUE) {

  ##Check to make sure that treated.unit and
  ##dependent.var of of length 1
  private$f.plot.check()

  ##Plot of the treated unit versus the sample average
  temp.treated.average <- self$plot_treated_average(treated.unit = treated.unit,
                                                    dependent.var = dependent.var,
                                                    synth.output, policy.line)

  ##Path plot
  temp.path <- self$plot_path(treated.unit = treated.unit, dependent.var = dependent.var,
                              synth.output, policy.line)

  ##gap plot
  temp.gap <- self$plot_gap(treated.unit = treated.unit, dependent.var = dependent.var,
                            synth.output, policy.line)

  ##permutation plot
  if (self$compute.perm) {
    temp.perm <- self$plot_permutation(treated.unit = treated.unit, dependent.var = dependent.var,
                                       perm.output = self$perm.output,
                                       synth.output, policy.line)
  } else {
    ##If no permutation test, use a blank panel
    ##to be passed to grid.extra
    temp.perm <- grid.rect(gp = gpar(col="white"))
  }

  return(grid.arrange(temp.treated.average, temp.path, temp.gap, temp.perm, ncol = 2))

}


plot_treated_average <- function(treated.unit,
                                 dependent.var,
                                 synth.output = self$synth.output,
                                 policy.line = TRUE) {

  ##Check to make sure that treated.unit and
  ##dependent.var of of length 1
  private$f.plot.check()

  sample.ave <- self$sample.average.df %>%
    .[, mget(c("time.index", dependent.var))] %>%
    setNames(., c("time.index", "value")) %>%
    .[, region := "Sample Average"]

  data <- self$Y.df %>%
    .[(region %in% c(treated.unit) & variable %in% c(dependent.var))] %>%
    .[, .(region, time.index, value)]

  data <- rbind(data, sample.ave) %>% as.data.frame

  p <- ggplot(data, aes(time.index, value)) +
    geom_line(aes(linetype = region))
  p <- private$f.aes(p) +
    labs(y = self$y.axis.label[dependent.var], title = "Path Plot")

  return(p)

}


plot_path <- function(treated.unit,
                      dependent.var,
                      synth.output = self$synth.output,
                      policy.line = TRUE) {

  ##Check to make sure that treated.unit and
  ##dependent.var of of length 1
  private$f.plot.check()

  ##The synth output for the treated and the control
  synth.output <- synth.output %>%
    .[(region %in% c(treated.unit) & variable %in% c(dependent.var))] %>%
    .[, .(region, time.index, value, control)] %>%
    setNames(., c("region", "time.index", treated.unit, "Synthetic")) %>%
    melt(id.vars = c("region", "time.index"), variable.factor = FALSE) %>%
    as.data.frame

  p <- ggplot(data = synth.output, aes(x = time.index, y = value)) +
    geom_line(aes(linetype = variable))
  p <- private$f.aes(p) +
    labs(y = self$y.axis.label[dependent.var], title = "Path Plot")
  return(p)
}



plot_gap <- function(treated.unit,
                     dependent.var,
                     synth.output = self$synth.output,
                     policy.line = TRUE) {

  ##Check to make sure that treated.unit and
  ##dependent.var of of length 1
  private$f.plot.check()

  ##Use the gap helper function
  p <- private$plot.gap.helper() +
    ggtitle("Gap Plot")

  ##if the permutation has been computed, plot the bootstrapped
  ##confidence intervals
  ci.data <- try(
    synth.output %>%
      .[(time.index >= self$time.policy & variable == c(dependent.var))] %>%
      .[, .(time.index, gap.ci.lower, gap.ci.upper)],
    silent = TRUE
  )
  if (!(inherits(ci.data, "try-error"))) {

    p <- p +
      geom_line(data = as.data.frame(ci.data),
                mapping = aes(x = time.index, y = gap.ci.lower),
                linetype = 2) +
      geom_line(data = as.data.frame(ci.data),
                mapping = aes(x = time.index, y = gap.ci.upper),
                linetype = 2)
  }



  return(p)

}

plot_permutation <- function(treated.unit,
                             dependent.var,
                             synth.output = self$synth.output,
                             perm.output = self$perm.output,
                             policy.line = TRUE) {

  ##Get the gap plot
  p <- private$plot.gap.helper()

  ##Add the permuation output
  p <- p +
    geom_line(data = as.data.frame(perm.output[variable == c(dependent.var)]),
              mapping = aes(x = time.index, y = gap, group = region),
              alpha = 0.25) +
    ggtitle("Permutation Plot")

  return(p)

}



