##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-08-06




##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(sf)})

DT.sf.zip3 <- readRDS("../data-raw-shp/35-zip3_2000_shapefile.rds") %>%
  setDT %>%
  ##.[, geometry := st_transform(geometry, crs = 2163)] %>%
  ##Lake tahoe region
  .[GEOID %chin% c("894", "895", "897", "961")]

DT.sf.state <- readRDS("../data-raw-shp/50-state_shp.rds") %>%
  setDT %>%
  .[STUSPS %chin% c("CA", "NV")] ## %>%
  ## .[, geometry := st_transform(geometry, crs = 2163)]

##Plot for tahoe
p.tahoe <- ggplot(data = DT.sf.state$geometry) +
  geom_sf() +
  geom_sf(data = DT.sf.zip3, mapping = aes(fill = GEOID)) +
  guides(fill = guide_legend(title = "Zip3")) +
  coord_sf(datum = NA)##  +
  ## ggtitle("Lake Tahoe California and Nevada Border Region")

saveRDS(p.tahoe, "RdsFiles/50-p_tahoe_border_region.rds")

ggsave(filename = "PlotsFinal/50-p_tahoe_border_region.pdf",
       plot = p.tahoe, width = 7, height = 7)




