##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-15


##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())

suppressMessages({library(CLmisc); library(lfe); library(cowplot); library(scales);
library(stargazer); library(starpolishr); library(ggrepel);
library(Hmisc); library(quantreg); library(spatstat)})

source("../../SynthMult/SynthMult.R", chdir = TRUE)

##crosswalk
cw.ca <- fread("data-raw/missouri_databridge_county_to_zip_2010.csv",
               skip = 1) %>%
  .[, .(fips.code = sprintf("%05.f", county),
        zip = sprintf("%05.f", `ZIP Census Tabulation Area`),
        hh2010 = `Total HUs, 2010 census`)] %>%
  ##only california
  .[substr(fips.code, 1, 2) == "06"] %>%
  setkey(fips.code)

cw.ca.max.hh <- copy(cw.ca) %>%
  .[, max.hh2010 := max(hh2010), by = zip] %>%
  .[hh2010 == max.hh2010] %>%
  .[, .(zip, fips.code)] %>%
  setkey(zip)

zip.panel <- readRDS("../../Data/_Data_Final_/40-zip_panel.rds")
zip.hh2000 <- zip.panel[!duplicated(zip), .(zip, hh2000)] %>%
  setkey(zip)

##cross-sectional zip code data
bartik <- zip.panel %>%
  .[CA == 1 & time.index %in% c(2009, 2010, 2011)] %>%
  .[, .(bartik = sum(bartik)), by = zip] %>%
  setkey(zip)

hp <- zip.panel %>%
  .[CA == 1 & time == as.Date("2008-06-01"),
    .(zip, hp = Zip_Zhvi_AllHomes / 1000)] %>%
  setkey(zip)

zip.cross <- zip.panel[CA == 1 & time == as.Date("2008-01-01")] %>%
  setkey(zip) %>%
  .[, .(zip, forc.high, IncomePerHousehold, subprime.percent.2005, non.occ.rate,
        land.unavailable)] %>%
  setkey(zip) %>%
  merge(bartik, all.x = TRUE, by = "zip") %>%
  merge(hp, all.x = TRUE, by = "zip")

##County data
county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds")
county.hh2000 <- county.panel[!duplicated(fips.code), .(fips.code, hh2000)] %>%
  setkey(fips.code)

##The Synhetic control foreclosure estimates
synth.forc.raw <- readRDS("../20-SynthCountyState/output/10-County_Forc/10-County_Forc_zillow.forc.rds")
synth.forc <- synth.forc.raw$synth.output %>%
   .[time.index >= 2008.497 & time.index < 2012,
     .(forc.gap = sum(gap)), by = .(fips.code = region)] %>%
  merge(county.hh2000, by = "fips.code") %>%
  setkey(forc.gap)

synth.forc.county.median <- synth.forc %>%
  .[, wtd.quantile(forc.gap, weights = hh2000, probs = 0.5)]
print("Median County-Level Synth Reduction in Foreclosures: ")
print(synth.forc.county.median)


##Get the synth foreclosures for each zip code
synth.forc.zip <- synth.forc %>%
  merge(cw.ca, all.y = TRUE, by = "fips.code") %>%
  .[!is.na(forc.gap)] %>%
  .[, w.weights := hh2010 / sum(hh2010), by = zip] %>%
  .[, .(forc.gap = weighted.mean(forc.gap, w = w.weights)), by = zip] %>%
  .[order(zip)] %>%
  setkey(zip)



##The zip code synthetic control
synth.zip.raw <- readRDS("../30-SynthZip/output/20-Zip_Ret/20-Zip_Ret_zillow.zip.ret.rds")

##Synth output
synth.zip <- synth.zip.raw$synth.output %>%
  .[time.index >= 2008.497 & time.index < 2012,
    .(ret.gap = sum(gap)), by = .(zip = region)] %>%
  setkey(zip) %>%
  merge(synth.forc.zip, all.x = TRUE, by = "zip") %>%
  merge(zip.hh2000, all.x = TRUE, by = "zip") %>%
  merge(cw.ca.max.hh, all.x = TRUE, by = "zip") %>%
  .[complete.cases(.)] %>%
  .[, zip3 := substr(zip, 1, 3)] %>%
  .[, zip4 := substr(zip, 1, 4)] %>%
  merge(zip.cross, all.x = TRUE, by = "zip") %>%
  .[, forc.quantile := dplyr::ntile(forc.gap, n = 5)] %>%
  setkey(forc.gap)

synth.county.ret <- synth.zip %>%
  .[, .(ret.gap.county = wtd.quantile(ret.gap, hh2000, probs = 0.5)),
    by = fips.code] %>%
  setkey(fips.code)


mod1.0 <- felm(ret.gap ~ forc.gap | 0 | 0 | zip3, synth.zip,
               weights = synth.zip$hh2000)
mod1.0.rq <- rq(ret.gap ~ forc.gap, synth.zip, tau = 0.5,
               weights = hh2000)
mod2.0 <- felm(ret.gap ~ forc.gap + bartik + IncomePerHousehold +
               hp |
               0 | 0 | zip3,synth.zip, weights = synth.zip$hh2000)
mod2.0.rq <- rq(ret.gap ~ forc.gap + bartik + IncomePerHousehold +
                    hp, tau = 0.5, synth.zip, weights = synth.zip$hh2000)



##The tex table
star.out <- stargazer(
  mod1.0, mod2.0, mod1.0.rq, mod2.0.rq,
  type = "latex",
  title = "\\textbf{Zip Code CFPL DDD Regressions -- Foreclosures and House Price Growth}",
  star.char = "", keep.stat = c("n", "rsq")) %>%
  gsub("Dependent variable:}}",
       "Dep Var}: Synth Gap in Housing Returns}",
       x = .)

##Delete the ret.gap
ret.gap.line <- which(grepl("ret.gap", x = star.out))
star.out <- star.out[-12]
##Delete the felm and quantile regression lines
model.lines <- which(grepl("textit\\{quantile\\}|textit\\{regression\\}",
                           x = star.out))
star.out <- star.out[-model.lines]
##Add in rows for the regression type
string <- c("\\\\[-1.8ex] & \\multicolumn{2}{c}{OLS} & \\multicolumn{2}{c}{Median Reg} \\\\", "\\cline{2-3} \\cline{4-5}")
star.out <- star_insert_row(
  star.out,
  string = string,
  insert.after = which(grepl("cline", x = star.out)))

##Update some other information
star.out <- star.out %>%
  star_rhs_names(pattern = c("forc.gap", "bartik",
                             "IncomePerHousehold", "hp"),
                 line1 = c("Synthetic Control",
                           "Bartik Shock", "Household Income",
                           "House Price (\\\\$000s)"),
                 line2 = c("Gap in Foreclosures",
                           "2009 - 2011", "in 2007 (\\$000s)",
                           "")) %>%
  star_notes_tex(note.type = "caption",
                 note = "Regressions are weighted by the number of households in 2000. For OLS, robust standard errors are clustered at the three digit zip code level. For quantile median regression estimates, standard errors are computed using a robust Huber sandwich esimtate as suggested by \\citet{koenkerhallock01}.")
star_tex_write(star.out, file = "TexFiles/40-zip_reg.tex", headers = TRUE)


## -- Plots -- ##
mod1.0.tidy <- broom::tidy(mod1.0) %>% as.data.table
mod1.0.est <- mod1.0.tidy[term == "forc.gap", estimate] %>% sprintf("%0.3f", .)
mod1.0.se <- mod1.0.tidy[term == "forc.gap", std.error] %>% sprintf("%0.3f", .)
mod1.0.est.med <- summary(mod1.0.rq)$coefficients[2,"Value"] %>%
                                   sprintf("%0.3f", .)
mod1.0.se.med <- summary(mod1.0.rq)$coefficients[2,"Std. Error"] %>%
                                   sprintf("%0.3f", .)

p1.text <- sprintf("OLS Slope: %s (%s)",
                   mod1.0.est, mod1.0.se)
p1.text.median <- sprintf("Median Slope %s (%s)",
                          mod1.0.est.med, mod1.0.se.med)
p1 <- ggplot(synth.zip, aes(x = forc.gap, y = ret.gap)) +
  geom_jitter(aes(size = hh2000), width = 30, shape = 21, fill = NA) +
  geom_smooth(method = "lm", mapping = aes(weight = hh2000),
              show.legend = FALSE, se = FALSE, size = 2, color = "blue") +
  stat_quantile(quantiles = c(0.5), color = "red", size = 2) +
  labs(
    x = "Synthetic Control Gap in REO Foreclosures",
    y = "Synthetic Control Gap in Housing Returns",
    title = "Panel 2A: CFPL Foreclosure and Housing Return DDD Estimates") +
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91") +
  theme(
    legend.position = "none",
##https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
    plot.margin = unit(c(6,6,6,6), "pt")
  ) +
  annotate("label", x = -15, y = c(-45, -54),
           label = c(p1.text, p1.text.median),
           color = c("blue", "red"),
           fontface = "bold", hjust = 0, size = 4.5, label.size = NA)

## -- The quantile plot -- ##

synth.zip <- synth.zip[, forc.quantile := as.character(forc.quantile)]

mod.quantile <- felm(ret.gap ~ 0 + forc.quantile | 0 | 0 | zip3,
                     synth.zip, weights = synth.zip$hh2000) %>%
  broom::tidy(.) %>%
  setDT %>%
  ##.[grepl("forc.gap$", x = term)] %>%
  setkey(term) %>%
  .[, Quantile := 1:.N] %>%
  .[, quantile2 := as.character(1:.N)] %>%
  .[, forc.quantile := as.character(1:.N)]

p2 <- ggplot(data = mod.quantile, aes(x = Quantile)) +
  geom_hline(yintercept = 0) +
  ##geom_col(aes(y = estimate, color = quantile2), fill = NA)
  geom_errorbar(mapping = aes(ymin = estimate - 2 * std.error,
                              ymax = estimate + 2 * std.error,
                    color = quantile2),
                width = 0.2, size = 2) +
  geom_point(aes(y = estimate, color = quantile2), size = 3) +
  labs(x = "REO Synth Forc Gap Quintile", y = "Mean Abnormal House Price Growth",
       title = "Panel 2B: Abnormal House Price Growth by CFPL REO Forc Reduction Quintiles") +
  theme(legend.position = "none") +
  background_grid(major = "xy", minor = "none", size.major = 0.1,
                  colour.major = "grey91")

cross.section.plots <- plot_grid(p1, p2, align = "hv", nrow = 2)

## -- Map -- ##
library(sf)
ca.sf.county <- readRDS("data-raw/CA_sf.rds") %>%
  dplyr::left_join(synth.forc, by = c("GEOID" = "fips.code")) %>%
  dplyr::left_join(synth.county.ret, by = c("GEOID" = "fips.code")) %>%
  dplyr::mutate(
    CENTROID = lapply(geometry, st_centroid),
    COORDS = lapply(CENTROID, st_coordinates),
    COORDS_X = sapply(COORDS, function(x) x[1]),
    COORDS_Y = sapply(COORDS, function(x) x[2])
  )

ca.sf.perm.boot <- readRDS("RdsFiles/20-ca_sf_perm_boot.rds")

p.map <- ggplot() +
  geom_sf(data = ca.sf.county, mapping = aes(fill = ret.gap.county)) +
  coord_sf(crs = st_crs(ca.sf.county), datum = NA) +
  scale_fill_gradient2(
    low = muted("#132B43"),##limits = c(-25,25),
    mid = "grey80", high = "darkred",
    midpoint = 0, space = "rgb",
    na.value = "white", guide = "colourbar",
    labels = comma ##,
    ##breaks = c(-5, 0, 5, 10, 15)
  ) +
  geom_text_repel(data = ca.sf.perm.boot,
                  aes(x = COORDS_X, y = COORDS_Y, label = NAME),
                  fontface = "bold", size = 5,
                  ## Strength of the repulsion force.
                  force = 1,
                  max.iter = 3e3) +
  labs(fill = "",
       title = "Panel 1: Median Synthetic Control Gap in Housing Returns") +
  theme_void() +
  theme(
    title = element_text(face = "bold", size = 14),
    ##Add box around legend
    ##legend.background = element_rect(color="black", size=.5, linetype = "solid"),
    ##Increase legend text
    legend.text = element_text(size = 14),
    ##Move the legend
    legend.position = c(.75, .95),
    legend.justification = c("right", "top"),
    ##legend.margin = margin(6, 6, 6, 6),
    ##Increase the legend size https://stackoverflow.com/a/41843082/1317443
    legend.key.size = unit(2,"line"),
    ##Remove left and right margins -- for use in multiplot
    ##see
    ##https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
    plot.margin = unit(c(0,0,0,0), "pt")
  )

plots.all <- plot_grid(p.map, cross.section.plots,
                       ncol = 2)
save_plot("PlotsFinal/40-zip_plots_grid.pdf", plots.all,
                   base_height = 9.13, base_width = 16.9235)

## -- ecdf -- ##
f_ecdf <- spatstat::ewcdf(
  x = synth.zip[["forc.gap"]],
  weights = (synth.zip[["hh2000"]] / sum(synth.zip[["hh2000"]]))
)

synth.zip[, forc.gap.quantile := f_ecdf(forc.gap)]

synth.forc.gap.quantiles <- synth.zip[, unique(forc.gap.quantile)] %>% sort

mod.rq.synth.forc.gap.quantiles <- rq(
  ret.gap ~ forc.gap,
  tau = synth.forc.gap.quantiles,
  data = synth.zip, weights = hh2000) %>%
  broom::tidy(.) %>%
  as.data.table %>%
  .[term == "forc.gap"] %>%
  setnames("tau", "forc.gap.quantile") %>%
  setkey(forc.gap.quantile)

synth.zip <- setkey(synth.zip, forc.gap.quantile) %>%
  merge(mod.rq.synth.forc.gap.quantiles) %>%
  .[, weights := (hh2000 * hp) / sum(hh2000 * hp)]

print("Increase in house prices due to the CFPLs (percent):")
print(synth.zip[, sum(forc.gap * estimate * weights)])
print("increase in housing wealth due to the CFPLs ($billions)")
print(synth.zip[, sum(estimate / 100 * forc.gap * hh2000 * hp)] / 10e5)
