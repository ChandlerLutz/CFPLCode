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
  .[, geometry := st_transform(geometry, crs = 2163)]

DT.sf.state <- readRDS("../data-raw-shp/50-state_shp.rds") %>%
  setDT %>%
  .[STUSPS %chin% c("AZ", "CA", "NV")] %>%
  .[, geometry := st_transform(geometry, crs = 2163)]


##the union of sf and az
sf.az.nv <- DT.sf.state[STUSPS %chin% c("AZ", "NV")]$geometry %>%
  st_union(.)

DT.sf.zip3 <- DT.sf.zip3 %>%
  .[, touches.ca := st_intersects(geometry,
                                  DT.sf.state[STUSPS == "CA"]$geometry,
                                  sparse = FALSE)[, 1]] %>%
  .[, touches.az.nv := st_intersects(geometry, sf.az.nv,
                                     sparse = FALSE)[, 1]]

nv.zip3.touches.ca <- DT.sf.zip3[touches.ca == TRUE & GEOID < "900", GEOID]
ca.zip3.touches.az.nv <- DT.sf.zip3[touches.az.nv == TRUE & GEOID >= "900", GEOID]

DT.sf.zip3 <- DT.sf.zip3[GEOID %chin% c(nv.zip3.touches.ca, ca.zip3.touches.az.nv)] %>%
  ##remove las vegas
  .[!(GEOID %chin% c("889", "891"))] %>%
  ##remove oregon, montanna, etc
  .[GEOID < "976"] %>%
  .[, geometry := st_transform(geometry, crs = 4269)]

DT.sf.state <- DT.sf.state %>%
  .[, geometry := st_transform(geometry, crs = 4269)]

p.az.ca.nv.border <- ggplot(DT.sf.state$geometry) +
  geom_sf() +
  geom_sf(aes(fill = GEOID), data = DT.sf.zip3) +
  guides(fill = guide_legend(title = "Zip3")) +
  coord_sf(datum = NA)##  +
  ## ggtitle("Arizona, California, and Nevada Border Region")

ggsave(filename = "PlotsFinal/51-p_az_ca_nv_border_region.pdf",
       plot = p.az.ca.nv.border, width = 7, height = 7)

