##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-01

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(cowplot); library(Hmisc); library(parallel); library(sf)})

set.seed(12345)

source("../../SynthMult/SynthMult.R", chdir = TRUE)

##The county panel
county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds")
hh2000 <- county.panel[!duplicated(fips.code), .(region = fips.code, hh2000)]

##The synth output
synth.county <-
  readRDS("../20-SynthCountyState/output/10-County_Forc/10-County_Forc_zillow.forc.rds")
synth.output <- synth.county$synth.output %>%
  merge(hh2000, all.x = TRUE, by = "region") %>%
  .[, cum.gap := cumsum(gap), by = region]
synth.perm <- synth.county$perm.output %>%
  merge(hh2000, all.x = TRUE, by = "region") %>%
  .[!is.na(hh2000)] %>%
  .[, cum.gap := cumsum(gap), by = region]

## -- Synth California percentiles -- ##
ca.synth.percentiles <- synth.output %>%
  .[, .(ca.cum.gap.10 = wtd.quantile(cum.gap, weights = hh2000, probs = 0.10),
        ca.cum.gap.25 = wtd.quantile(cum.gap, weights = hh2000, probs = 0.25),
        ca.cum.gap.50 = wtd.quantile(cum.gap, weights = hh2000, probs = 0.50),
        ca.cum.gap.75 = wtd.quantile(cum.gap, weights = hh2000, probs = 0.75),
        ca.cum.gap.90 = wtd.quantile(cum.gap, weights = hh2000, probs = 0.90)
        ),
    by = time.index]

##Bootstrapped Permutation test
N.regions <- uniqueN(synth.perm$region)
boot.indices <- lapply(1:2500, function(i) {
  sample(N.regions, size = N.regions, replace = TRUE)
})

f_boot <- function(index) {
  synth.perm %>%
    .[, .SD[index, ], by = time.index] %>%
    .[, .(perm.cum.gap.boot.05 = wtd.quantile(x = cum.gap, weights = hh2000, probs = 0.05),
          perm.cum.gap.boot.10 = wtd.quantile(x = cum.gap, weights = hh2000, probs = 0.10),
          perm.cum.gap.boot.lower = wtd.quantile(x = cum.gap, weights = hh2000, probs = 0.025),
          perm.cum.gap.boot.upper = wtd.quantile(x = cum.gap, weights = hh2000, probs = 0.975)),
      by = time.index]
}

##Run and save for ease of computation
##Comment out the following lines if will run multiple times
##Run in parallel

cl <- makeCluster(4)
clusterEvalQ(cl, {
  library(CLmisc); library(Hmisc)
})
clusterExport(cl, "synth.perm")
synth.perm.boot <- parLapplyLB(cl, boot.indices, f_boot)
stopCluster(cl)
synth.perm.boot <- rbindlist(synth.perm.boot) %>%
  .[, lapply(.SD, mean), by = time.index]
saveRDS(synth.perm.boot, "RdsFiles/20-synth_perm_boot.rds")

synth.perm.boot <- readRDS("RdsFiles/20-synth_perm_boot.rds")

ca.cum.gap <- copy(ca.synth.percentiles) %>%
  melt(id.vars = "time.index") %>%
  .[, variable := stringr::str_extract(variable, "[0-9]{2}")] %>%
  .[, variable := paste0(variable, "th")] %>%
  setnames("variable", "CAPercentile")

p.synth.out <- ggplot(synth.perm.boot, aes(x = time.index)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2008.497, color = "blue", linetype = 2,
             size = 1.1) +
  geom_vline(xintercept = 2009.414, color = "blue", linetype = 2,
               size = 1.1) +
  geom_ribbon(aes(ymin = perm.cum.gap.boot.lower, ymax = perm.cum.gap.boot.upper),
              fill = "gray60", alpha = 0.25) +
  geom_line(data = ca.cum.gap,
            aes(y = value, color = CAPercentile, linetype = CAPercentile),
            size = 1.1) +
  ##The cowplot background
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  ##The break dates
  scale_x_continuous(breaks = 2007:2015) +
  labs(x = "Time in Months", y = "Cumulative Gap",
       title = "1A: CFPL REO Foreclosure Synth Cumulative Gap") +
    theme(
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
g <- guide_legend(nrow = 1, byrow = TRUE, title = "California Percentile")
p.synth.out <- p.synth.out + guides(color = g, linetype = g)
##print
print(p.synth.out)
##save
saveRDS(p.synth.out, "RdsFiles/20-plot_synth_forc_county.rds")


## -- The synth plot estimates -- ##
library(sf); library(scales); library(ggrepel)

##See these issues for use with geom_text_repel
##https://github.com/slowkow/ggrepel/issues/89
##https://github.com/tidyverse/ggplot2/issues/2111

ca.sf <- readRDS("data-raw/CA_sf.rds") %>%
  dplyr::mutate(region = paste0(STATE, COUNTY))
ca.synth.output <- synth.output %>%
  .[time.index == 2011.915, .(region, cum.gap)]

perm.boot.2011M12.cutoff <- synth.perm.boot[time.index==2011.915,perm.cum.gap.boot.05]

ca.sf <- dplyr::left_join(ca.sf, ca.synth.output, by = "region") %>%
  ##For label in geom_text_repel()
  ##from https://github.com/tidyverse/ggplot2/issues/2111
  dplyr::mutate(
    CENTROID = lapply(geometry, st_centroid),
    COORDS = lapply(CENTROID, st_coordinates),
    COORDS_X = sapply(COORDS, function(x) x[1]),
    COORDS_Y = sapply(COORDS, function(x) x[2])
  )


ca.sf.perm.boot <- ca.sf %>% dplyr::filter(!is.na(cum.gap) & cum.gap < perm.boot.2011M12.cutoff)

saveRDS(ca.sf.perm.boot, "RdsFiles/20-ca_sf_perm_boot.rds")

p.map <- ggplot(ca.sf) +
  geom_sf(aes(fill = cum.gap)) +
  coord_sf(crs = st_crs(ca.sf), datum = NA) +
  ## scale_colour_gradient2(
  ##   low = muted("red"), mid = "white",
  ##   high = muted("blue"), midpoint = 0, space = "Lab",
  ##   na.value = "grey50", guide = "colourbar") +
  scale_fill_gradient2(
    low = "darkred",##limits = c(-25,25),
    mid = "grey80", high = muted("#132B43"),
    midpoint = 0, space = "rgb",
    na.value = "white", guide = "colourbar",
    labels = comma,
    breaks = c(-600, -300, 0, 300, 600)
  ) +
  geom_text_repel(data = ca.sf.perm.boot, aes(x = COORDS_X, y = COORDS_Y, label = NAME),
                  fontface="bold", size = 5) +
  labs(fill = "",
       title = "Panel 2: Synth Gap in REO Forelcosures per 10K Homes") +
  theme_void() +
  theme(
    title = element_text(face = "bold", size = 14),
    ##Add box around legend
    ##legend.background = element_rect(color="black", size=.5, linetype = "solid"),
    ##Increase legend text
    legend.text = element_text(size = 16),
    ##Move the legend
    legend.position = c(.75, .95),
    legend.justification = c("right", "top"),
    ##legend.margin = margin(6, 6, 6, 6),
    ##Increase the legend size https://stackoverflow.com/a/41843082/1317443
    legend.key.size = unit(2.5,"line"),
    ##Remove left and right margins -- for use in multiplot
    ##see
    ##https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
    plot.margin = unit(c(0,0,0,0), "pt")
  )

saveRDS(p.map, "RdsFiles/20-synth_county_ca_map.rds")

print("Foreclosure reduction in San Beranardino (06071)")
synth.output %>%
  .[region %in% c("06071") & time.index >= 2008.497 & time.index < 2012,
    .(CFPL.cum.gap = sum(gap), CFPL.control = sum(control)), by = region] %>%
  print(.)

temp <- synth.output %>%
  .[time.index >= 2008.497 & time.index < 2012,
    .(value = sum(value), control = sum(control), hh2000 = first(hh2000)),
    by = region] %>%
  .[, .(value = sum(value / 10000 * hh2000),
        control = sum(control / 10000 * hh2000))]
forc.reduction <- temp[, value - control] %>% round(3)
forc.reduction.percent <- temp[, (value - control) / control * 100] %>% round(3)
sprintf("According to Synth Estimates, the CFPLs reduced forelocures by %s homes or %s percent",
        forc.reduction, forc.reduction.percent) %>%
  print(.)


