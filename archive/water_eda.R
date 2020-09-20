library(gt)
library(tidyverse)
water <- read_csv("Water_FINAL2.csv",trim_ws = TRUE,guess_max = 2000000)

water %>% group_by(GroupCode,StationCode) %>% count()

water %>% dplyr::filter(is.na(StationCode)) %>% group_by(Date) %>% count() %>% ungroup() %>% ggplot() + geom_point(aes(x=Date,y=n))

usamap <- map_data("county",region=c("Delaware","Maryland","Pennsylvania","Virginia","New York","West Virginia","district of columbia"))

ggplot() +
  geom_polygon(data = usamap, aes(x=long, y = lat,group = group),colour='white',fill='lightgrey') +
  geom_point(data=water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=lubridate::year(Date)) %>%
  #filter(yr==2005) %>%
  #filter(Parameter=="WTEMP") %>%
  distinct(Longitude,Latitude,Database),
  aes(x=Longitude,y=Latitude),colour='black',size=0.5) +
  #aes(x=Longitude,y=Latitude,colour=Database),size=0.25) +
  theme(legend.position="none")

water_countybyyr <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=lubridate::year(Date)) %>%
  group_by(STATE_,COUNTY_,yr) %>% count() %>% ungroup() %>% arrange(STATE_,COUNTY_,yr) %>% dplyr::filter(yr>2004) %>%
  pivot_wider(names_from=yr,values_from=n,values_fill=0)


up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"

t1 <- water_countybyyr %>% colnames() %>% as_tibble() %>% dplyr::filter(grepl("2",value)) %>% pull()
water_countybyyr %>%
  gt() %>%
  gt::fmt_number(vars(t1),decimals = 0) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2018`),
      rows = ifelse(`2018`==0,FALSE,`2018`>=`2017`*1.25)),
    fn=function(x){paste0(x," ",up_arrow)}
  )

water_countybyyr <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=lubridate::year(Date)) %>%
  group_by(STATE_,COUNTY_,StationCode,yr) %>% count() %>% ungroup() %>% group_by(STATE_,COUNTY_,yr) %>% count() %>%
  ungroup() %>%
  pivot_wider(names_from=yr,values_from=n,values_fill=0)

water_countybyyr %>%
  gt() %>%
  gt::fmt_number(vars(t1),decimals = 0) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2019`),
      rows = ifelse(`2019`==0,FALSE,`2019`>=`2018`*1.25)),
    fn=function(x){paste0(x," ",up_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2018`),
      rows = ifelse(`2018`==0,FALSE,`2018`>=`2017`*1.25)),
    fn=function(x){paste0(x," ",up_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2017`),
      rows = ifelse(`2017`==0,FALSE,`2017`>=`2016`*1.25)),
    fn=function(x){paste0(x," ",up_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2016`),
      rows = ifelse(`2016`==0,FALSE,`2016`>=`2015`*1.25)),
    fn=function(x){paste0(x," ",up_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2015`),
      rows = ifelse(`2015`==0,FALSE,`2015`>=`2014`*1.25)),
    fn=function(x){paste0(x," ",up_arrow)}
  ) %>%  # down arrows
  text_transform(
    locations = cells_body(
      columns = vars(`2019`),
      rows = ifelse(`2019`==0,FALSE,`2019`<=`2018`*0.75)),
    fn=function(x){paste0(x," ",down_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2018`),
      rows = ifelse(`2018`==0,FALSE,`2018`<=`2017`*0.75)),
    fn=function(x){paste0(x," ",down_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2017`),
      rows = ifelse(`2017`==0,FALSE,`2017`<=`2016`*0.75)),
    fn=function(x){paste0(x," ",down_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2016`),
      rows = ifelse(`2016`==0,FALSE,`2016`<=`2015`*0.75)),
    fn=function(x){paste0(x," ",down_arrow)}
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars(`2015`),
      rows = ifelse(`2015`==0,FALSE,`2015`<=`2014`*0.75)),
    fn=function(x){paste0(x," ",down_arrow)}
  )

# heat map

heat_data <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% 
  mutate(yr=lubridate::year(Date)) %>% mutate(month=lubridate::month(Date)) %>%
  mutate(STATECOUNTY=paste0(STATE_,"-",COUNTY_)) %>%
  group_by(STATECOUNTY,yr,month) %>% count() %>% ungroup() %>%
  mutate(yrmonth=paste0(yr,"-",str_pad(month,width=2,pad="0")))

t1 <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% 
  mutate(yr=lubridate::year(Date)) %>% mutate(month=lubridate::month(Date)) %>%
  mutate(STATECOUNTY=paste0(STATE_,"-",COUNTY_)) %>%
  group_by(STATECOUNTY,yr,month) %>% count() %>%
  group_by(STATECOUNTY) %>% count() %>% arrange(desc(n)) %>% tail(0.5*154) %>% head(0.25*154)



heat_data <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% 
  mutate(yr=lubridate::year(Date)) %>% mutate(month=lubridate::month(Date)) %>%
  mutate(STATECOUNTY=paste0(STATE_,"-",COUNTY_)) %>%
  group_by(STATECOUNTY,yr,month) %>% summarise(n=n_distinct(StationCode)) %>% ungroup() %>%
  dplyr::filter(STATECOUNTY %in% t1$STATECOUNTY) %>%
  mutate(yrmonth=paste0(yr,"-",str_pad(month,width=2,pad="0")))

heat_data %>% dplyr::filter(yr==2019) %>%
  ggplot(aes(x=yrmonth,y=STATECOUNTY,fill=n)) +
  geom_tile()


# understand distro of type by state -----------------------------------------------------------

param <- water %>% group_by(Parameter) %>% count() %>% arrange(desc(n)) %>% head(15)

ridge_data <- water %>% mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% 
  mutate(yr=lubridate::year(Date)) %>% mutate(month=lubridate::month(Date)) %>%
  dplyr::filter(Parameter %in% param$Parameter) %>%
  group_by(state,Date,Parameter) %>% count() %>% ungroup()

ridge_data %>%
  ggplot(aes(x = Date, y = Parameter)) + ggridges::geom_density_ridges()


# compare their eda with mine ---------------------------------------------
water <- read_csv("Water_FINAL2.csv",trim_ws = TRUE,guess_max = 2000000)
p1 <- water %>% 
  dplyr::filter(Database=="CMC") %>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% 
  mutate(yr=lubridate::year(Date)) %>% mutate(month=lubridate::month(Date)) %>%
  group_by(yr) %>% count() %>%
  ggplot() + geom_bar(aes(x=yr,y=n),stat='identity') +
  scale_y_continuous(labels=scales::comma) +
  ggtitle("CMC Water Quality Data Over Time (By Year)")

p2 <- water %>% 
  dplyr::filter(Database=="CBP") %>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% 
  mutate(yr=lubridate::year(Date)) %>% mutate(month=lubridate::month(Date)) %>%
  group_by(yr) %>% count() %>%
  ggplot() + geom_bar(aes(x=yr,y=n),stat='identity') +
  scale_y_continuous(labels=scales::comma) +
  ggtitle("CBP Water Quality Data Over Time (By Year)")

library(patchwork);p1+p2


