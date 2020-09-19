library(gt)
library(tidyverse)
library(ggmap)
library(sbtools)
library(dataRetrieval)
library(sf)

huc_poly <- st_read('wbdhu12_a_us_september2019.gdb') # https://waterdata.usgs.gov/blog/beyond-basic-mapping/

class(huc_poly)
str(huc_poly)
st_geometry(huc_poly)
st_bbox(huc_poly)
st_crs(huc_poly)

# Lu should use st_intersects or st_intersection to get the area of intersection between a HUC and a census area.

# try to plot one state ---------------------------------------------------

md_only <- huc_poly %>% dplyr::filter(STATES=="MD")
md <- md_only %>% as.data.frame()


plot(st_geometry(head(huc_poly,50)))
plot(st_geometry(md_only))

md_only %>% arrange(desc(Shape_Area))


# pull in water quality ---------------------------------------------------

water <- read_csv("Water_FINAL2.csv",trim_ws = TRUE,guess_max = 2000000)
md_water <- water %>% dplyr::filter(state=="MD")

HUCbyyr <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% 
  mutate(yr=lubridate::year(Date)) %>% mutate(month=lubridate::month(Date)) %>%
  group_by(yr,HUC12_) %>% summarise(UniquePoints=n_distinct(Latitude,Longitude)) %>% ungroup() %>%
  left_join(huc_poly %>% as.data.frame() %>% select(HUC12,NAME,STATES,AREASQKM),by=c("HUC12_"="HUC12"))

HUCbyyr %>% dplyr::filter(yr>2010) %>%
  pivot_wider(names_from = yr,values_from=UniquePoints,values_fill=0) %>%
  arrange(desc(AREASQKM)) %>% View()

HUC_summary <- HUCbyyr %>% dplyr::filter(yr>=2010) %>% group_by(HUC12_) %>% mutate(max=max(UniquePoints)) %>% ungroup() %>%
  pivot_wider(names_from = yr,values_from=UniquePoints,values_fill=0) %>%
  arrange(desc(AREASQKM))

HUC_summary %>% group_by(max) %>% count() %>% ungroup() %>%
  ggplot() + geom_bar(aes(x=max,y=n),stat='identity') + scale_y_log10(name="HUCs") + scale_x_continuous(name="maximum lat/lon pairs (across years)") +
  ggtitle("Distribution of maximum Lat/Lon pairs across all HUCs","Data: 2010-2020")

HUC_summary %>% mutate(pointspersq=max/AREASQKM) %>% arrange(pointspersq) %>% View()

mdmap <- map_data("county",region=c("Maryland")) %>% dplyr::filter(subregion=='anne arundel')

ggplot() +
  geom_polygon(data = mdmap, aes(x=long, y = lat,group = group),colour='white',fill='lightgrey') +
  geom_point(data=mdwater %>%
               distinct(Longitude,Latitude,Database),
             aes(x=Longitude,y=Latitude),colour='black',size=1.5) +
  theme(legend.position="none") +
  ggtitle(paste0(mdwater %>% select(HUCNAME_) %>% slice(1) %>% paste0(collapse='')," - ",mdwater %>% select(STATE_) %>% slice(1) %>% paste0(collapse='')))

HUCmap <- function(HUC){
  mdwater <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=lubridate::year(Date)) %>%
    dplyr::filter(HUC12_==HUC)
  # ggmap
  sbbox <- make_bbox(lon=mdwater$Longitude,lat=mdwater$Latitude,f=0.1)
  sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google") #https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  ggmap(sq_map) + geom_point(data = mdwater, mapping = aes(x = Longitude, y = Latitude), color = "red") +
    ggtitle(label = paste0(mdwater %>% select(HUCNAME_) %>% slice(1) %>% paste0(collapse='')),subtitle=paste0(mdwater %>% select(COUNTY_) %>% slice(1) %>% paste0(collapse=''),", ",mdwater %>% select(STATE_) %>% slice(1) %>% paste0(collapse='')))
}

HUCmap("020600040203") # small area lot of points over years
HUCmap("020801010000") # large area lot of points over years
HUCmap("020600010000") # large area lot of points over years

