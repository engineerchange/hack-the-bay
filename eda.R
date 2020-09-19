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

huc_both_2019 <- huc_byyr %>% filter(yr==2019,cmc_ct>50,cbp_ct>50)
huc_cmc_2019 <- huc_byyr %>% filter(yr==2019,cmc_ct>50) %>% filter(HUC12_ %notin% huc_both_2019$HUC12_)
huc_cbp_2019 <- huc_byyr %>% filter(yr==2019,cbp_ct>50) %>% filter(HUC12_ %notin% huc_both_2019$HUC12_)
huc_null <- huc_byyr %>% filter(!(yr==2019&(cmc_ct>50|cbp_ct>50)))



# plot hucs ---------------------------------------------------------------

huc_null <- huc %>% mutate(HUC12=case_when(
  substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
  TRUE ~ HUC12
)) %>%
  dplyr::filter(HUC12 %in% huc_null$HUC12_)
huc_both_2019 <- huc %>% mutate(HUC12=case_when(
  substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
  TRUE ~ HUC12
)) %>%
  dplyr::filter(HUC12 %in% huc_both_2019$HUC12_)

huc_cmc_2019 <- huc %>% mutate(HUC12=case_when(
  substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
  TRUE ~ HUC12
)) %>%
  dplyr::filter(HUC12 %in% huc_cmc_2019$HUC12_)

huc_cbp_2019 <- huc %>% mutate(HUC12=case_when(
  substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
  TRUE ~ HUC12
)) %>%
  dplyr::filter(HUC12 %in% huc_cbp_2019$HUC12_)

# plot(st_geometry(huc_null),col='white',lwd=2)
# plot(st_geometry(huc_both_2019),col='purple',lwd=2,add=TRUE)
# plot(st_geometry(huc_cmc_2019),col='green',lwd=2,add=TRUE)
# plot(st_geometry(huc_cbp_2019),col='red',lwd=2,add=TRUE)

library(maps)
usa = st_as_sf(map('state', plot = FALSE, fill = TRUE)) %>%
  filter(ID %in% c("new york","delaware","new jersey","pennsylvania","maryland","district of columbia","virginia","west virginia"))

ggplot() +
  geom_sf(data = usa,fill='gray90') +
  geom_sf(data=huc_null,fill='white') +
  geom_sf(data=huc_both_2019,fill='purple') +
  geom_sf(data=huc_cmc_2019,fill='green') +
  geom_sf(data=huc_cbp_2019,fill='red') +
  theme_bw()






