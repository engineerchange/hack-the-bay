# takes a few minutes to import all packages and data; ignore warnings
source("import.R")


# database look-see -------------------------------------------------------

cmc <- water %>% dplyr::filter(Database=='CMC') %>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=year(Date))
cbp <- water %>% dplyr::filter(Database=='CBP') %>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=year(Date))

cmc_byyr <- cmc %>% group_by(HUC12_,yr) %>% summarise(cmc_ct=n()) %>% ungroup() %>% 
  mutate(HUC12_=as.character(HUC12_)) %>%
  mutate(HUC12_=case_when(
    substr(HUC12_,1,1) == "0" ~ substr(HUC12_,2,nchar(HUC12_)),
    TRUE ~ HUC12_
  ))
cbp_byyr <- cbp %>% group_by(HUC12,yr) %>% summarise(cbp_ct=n()) %>% ungroup() %>% mutate(HUC12=as.character(HUC12))

huc_byyr <- cmc_byyr %>% full_join(cbp_byyr,by=c("yr","HUC12_"="HUC12"))

graphicdata <- function(YEAR){
  huc <- huc %>% mutate(HUC12=case_when(
    substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
    TRUE ~ HUC12
  ))
  huc_both <- huc_byyr %>% filter(yr==YEAR,cmc_ct>0,cbp_ct>0)
  huc_cmc <- huc_byyr %>% filter(yr==YEAR,cmc_ct>0) %>% filter(HUC12_ %notin% huc_both$HUC12_)
  huc_cbp <- huc_byyr %>% filter(yr==YEAR,cbp_ct>0) %>% filter(HUC12_ %notin% huc_both$HUC12_)
  huc_null <- huc_byyr %>% filter(!(yr==YEAR&(cmc_ct>0|cbp_ct>0)))
  
  huc_null <<-  huc %>%
    dplyr::filter(HUC12 %in% huc_null$HUC12_)
  
  huc_both <<- huc %>% 
    dplyr::filter(HUC12 %in% huc_both$HUC12_)
  
  huc_cmc <<- huc %>% 
    dplyr::filter(HUC12 %in% huc_cmc$HUC12_)
  
  huc_cbp <<- huc %>%
    dplyr::filter(HUC12 %in% huc_cbp$HUC12_)
}

graphicplot <- function(){
  ggplot() +
    #geom_sf(data = usa,fill='gray90') +
    geom_sf(data=huc_null,fill='white') +
    geom_sf(data=huc_both,fill='purple') +
    geom_sf(data=huc_cmc,fill='green') +
    geom_sf(data=huc_cbp,fill='red') +
    theme_bw() +
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(size=22,hjust = 0.5)) 
}

graphicdata(2016);graphicplot() + ggtitle("2016") -> gg1
graphicdata(2017);graphicplot() + ggtitle("2017") -> gg2
graphicdata(2018);graphicplot() + ggtitle("2018") -> gg3
graphicdata(2019);graphicplot() + ggtitle("2019") -> gg4

library(patchwork)

# this visual can take a few mins to run and may need to be re-run to produce correctly
gg2 + gg3 + gg4 + plot_annotation(caption = "Purple = both; Red = CBP; Green = CMC") +
  plot_layout(nrow = 1)
  

# normal plot hucs ---------------------------------------------------------------



# plot(st_geometry(huc_null),col='white',lwd=2)
# plot(st_geometry(huc_both_2019),col='purple',lwd=2,add=TRUE)
# plot(st_geometry(huc_cmc_2019),col='green',lwd=2,add=TRUE)
# plot(st_geometry(huc_cbp_2019),col='red',lwd=2,add=TRUE)

# more stuff

# library(maps)
# usa = st_as_sf(map('state', plot = FALSE, fill = TRUE)) %>%
#   filter(ID %in% c("new york","delaware","new jersey","pennsylvania","maryland","district of columbia","virginia","west virginia"))


