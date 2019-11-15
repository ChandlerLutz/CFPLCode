##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-11-07

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(cowplot); library(Hmisc); library(parallel); library(sf); library(ggrepel)})

set.seed(12345)

source("c:/Dropbox/CFPLSML/SynthMult/SynthMult.R", chdir = TRUE)

DT.county.names <- fread("data-raw/missouri_databridge_county_to_zip_2010.csv",
                         skip = 1) %>%
  .[!duplicated(county), .(county.fips.code = sprintf("%05.f", county),
                           county.name = cntyname)]


##The county panel
county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds")
hh2000 <- county.panel[!duplicated(fips.code), .(region = fips.code, hh2000)]

ca.county.income <- county.panel %>%
  .[CA == 1 & !duplicated(fips.code),
    .(ca.county.fips = fips.code, IncomePerHousehold)]



##The synth output
synth.county <-  readRDS("../20-SynthCountyState/output/10-County_Forc/10-County_Forc_zillow.forc.rds")


DT.synth.controls <- lapply(synth.county$region.weights, as.data.table) %>%
  rbindlist(idcol = "ca.county.fips") %>%
  .[order(ca.county.fips, w.weights)] %>%
  .[, tail(.SD, 2), by = ca.county.fips] %>%
  merge(DT.county.names, by.x = "region", by.y = "county.fips.code", all.x = TRUE) %>%
  .[order(ca.county.fips, -w.weights)] %>%
  .[, .(control.county.names = paste(county.name, collapse = "\n")),
    by = ca.county.fips]


DT.ca.county.sf <- readRDS("data-raw/CA_sf.rds") %>%
  as.data.table %>%
  .[, ca.county.fips := paste0(STATE, COUNTY)] %>%
  merge(ca.county.income, by = "ca.county.fips", all.x = TRUE) %>%
  merge(DT.synth.controls, by = "ca.county.fips", all.x = TRUE) %>%
  .[!is.na(control.county.names),
    IncomePerHousehold.ntile := dplyr::ntile(IncomePerHousehold, 4)] %>%
  .[, plot.label := paste0(NAME, ":\n", control.county.names)]




DT.ca.sf <- DT.ca.county.sf$geometry %>% st_union

f_plot <- function(IncomePerHousehold.ntile.temp) {
  DT <- DT.ca.county.sf %>%
    .[IncomePerHousehold.ntile == c(IncomePerHousehold.ntile.temp)]

  plot.title <- paste0("Household Income Quartile: ", IncomePerHousehold.ntile.temp)
  if (IncomePerHousehold.ntile.temp == 4) {
    plot.title <- paste0(plot.title, " (Highest Income)")
  } else if (IncomePerHousehold.ntile.temp == 1) {
    plot.title <- paste0(plot.title, " (Lowest Income)")
  } else {
    plot.title <- paste0(plot.title, " (Middle Income)")
  }



  ggplot() +
    geom_sf(data = DT.ca.sf, fill = NA, color = "gray70") +
    geom_sf(data = DT, color = "gray70", fill = "gray90") +
    geom_label_repel(data = DT,
                     mapping = aes(label = plot.label, geometry = geometry),
                     stat = "sf_coordinates", size = 3, seed = 12345, alpha = 0.1,
                     min.segment.length = 0.25, force = 10,
                     max.iter = 15000) +
    geom_label_repel(data = DT,
                     mapping = aes(label = plot.label, geometry = geometry),
                     stat = "sf_coordinates", size = 3, fill = NA,
                     min.segment.length = 0.25, force = 10,
                     seed = 12345, max.iter = 15000) +
    theme_void() +
    coord_sf(datum = NA) +
    ggtitle(plot.title) +
    theme(plot.margin = unit(c(0,0,0,40), "pt"),
          plot.title = element_text(face = "bold"))

}


plots.all <- lapply(sort(unique(DT.ca.county.sf$IncomePerHousehold.ntile)),
                    f_plot) %>%
  plot_grid(plotlist = ., nrow = 2, align = "hv")

ggsave("PlotsFinal/25-p_ca_synth_map_of_largest_controls.pdf",
       plot = plots.all, width = 11, height = 12, dpi = 300)


