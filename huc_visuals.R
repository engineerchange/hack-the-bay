# takes a few minutes to import all packages and data; ignore warnings
source("import.R")
options(scipen=9999)



# databases ---------------------------------------------------------------

cmc <- water %>% dplyr::filter(Database=='CMC') %>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=year(Date))
cbp <- water %>% dplyr::filter(Database=='CBP') %>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y")) %>% mutate(yr=year(Date))

cmc <- cmc %>% mutate(par = case_when(
  grepl("^[A-Za-z]{1,}\\.",Parameter) ~ str_extract(Parameter,"^[A-Za-z]{1,}\\.") %>% str_extract(.,"^[A-Za-z]{1,}"),
  grepl("^[A-Za-z0-9]{1,}\\.",Parameter) ~ str_extract(Parameter,"^[A-Za-z0-9]{1,}\\.") %>% str_extract(.,"^[A-Za-z0-9]{1,}"),
  grepl("^[A-Za-z]{1,}$",Parameter) ~ Parameter,
  TRUE ~ Parameter
))

cmc %>% group_by(par) %>% summarise(nd=n_distinct(HUC12_)) %>% as.data.frame()

cbp <- cbp %>% mutate(par = case_when(
  grepl("^[A-Za-z]{1,}\\.",Parameter) ~ str_extract(Parameter,"^[A-Za-z]{1,}\\.") %>% str_extract(.,"^[A-Za-z]{1,}"),
  grepl("^[A-Za-z0-9]{1,}\\.",Parameter) ~ str_extract(Parameter,"^[A-Za-z0-9]{1,}\\.") %>% str_extract(.,"^[A-Za-z0-9]{1,}"),
  grepl("^[A-Za-z]{1,}$",Parameter) ~ Parameter,
  TRUE ~ Parameter
))

cbp %>% group_by(par) %>% summarise(nd=n_distinct(HUC12)) %>% as.data.frame()


# some quick tell by year -------------------------------------------------

cmc %>% group_by(yr) %>% summarise(distinct_huc=n_distinct(HUC12_),total_points=n()) %>% ungroup() -> s1;s1 %>% filter(yr>=2010) %>%
  gt() %>% gt::tab_header(title='CMC, by year')
cbp %>% group_by(yr) %>% summarise(distinct_huc=n_distinct(HUC12),total_points=n()) %>% ungroup() -> s2;s2 %>% filter(yr>=2010) %>%
  gt() %>% gt::tab_header(title='CBP, by year')

bth <- cmc %>% mutate(HUC12_=as.character(HUC12_)) %>% 
  mutate(HUC12_=case_when(
    substr(HUC12_,1,1) == "0" ~ substr(HUC12_,2,nchar(HUC12_)),
    TRUE ~ HUC12_
  )) %>% group_by(HUC12_,yr) %>% summarise(cmc_ct=n()) %>% ungroup() %>%
  full_join(cbp %>% mutate(HUC12=as.character(HUC12)) %>% group_by(HUC12,yr) %>% summarise(cbp_ct=n()) %>% ungroup(),by=c("yr","HUC12_"="HUC12")) %>%
  filter(yr>=2010) %>%
  group_by(yr) %>% summarise(total_points=sum(cmc_ct,cbp_ct,na.rm=T),distinct_huc=n_distinct(HUC12_)) %>% ungroup()

s1 %>% rename("CMC_distinct_huc"="distinct_huc","CMC_total_points"="total_points") %>% 
  left_join(s2 %>% rename("CBP_distinct_huc"="distinct_huc","CBP_total_points"="total_points"),by=c("yr")) %>% filter(yr>=2010) %>%
  left_join(bth %>% rename("BOTH_distinct_huc"="distinct_huc","BOTH_total_points"="total_points"),by=c("yr")) %>%
  gt() %>% gt::cols_label("CBP_total_points"="Total Points","CMC_total_points"="Total Points",
                          "CBP_distinct_huc"="Distinct HUCs","CMC_distinct_huc"="Distinct HUCs",
                          "BOTH_distinct_huc"="Distinct HUCs","BOTH_total_points"="Total Points",
                          "yr"="Year") %>%
  gt::tab_spanner(
    label = "CMC",
    columns = vars(CMC_distinct_huc,CMC_total_points)
  ) %>%
  gt::tab_spanner(
    label = "CBP",
    columns = vars(CBP_distinct_huc,CBP_total_points)
  ) %>%
  gt::tab_spanner(
    label = "All",
    columns = vars(BOTH_distinct_huc,BOTH_total_points)
  ) %>%
  gt::fmt_number(vars(CBP_total_points,CMC_total_points,BOTH_total_points),use_seps = TRUE,decimals=0) %>%
  gt::tab_header(title='Comparing Water Quality Data Across Databases') %>%
  gt::tab_source_note(source_note ='Data as of 7/2020; the year 2020 has reduced data points due to partial year data and COVID-19.')


# parameter usage by year -------------------------------------------------

s1_par <- cmc %>% group_by(par,yr) %>% summarise(distinct_huc=n_distinct(HUC12_),total_points=n()) %>% ungroup()
s1_par <- s1_par %>% rename("CMC_distinct_huc"="distinct_huc","CMC_total_points"="total_points") %>% filter(yr>=2010) %>%
  select(-CMC_total_points) %>%
  pivot_wider(names_from=par,values_from=CMC_distinct_huc)
s1_par[is.na(s1_par)]<-0
s1_par %>% gt() %>% gt::tab_header(title='CMC',subtitle = 'Parameters, by distinct HUCs')

s1_par <- cmc %>% group_by(par,yr) %>% summarise(distinct_huc=n_distinct(HUC12_),total_points=n()) %>% ungroup()
s1_par <- s1_par %>% rename("CMC_distinct_huc"="distinct_huc","CMC_total_points"="total_points") %>% filter(yr>=2010) %>%
  select(-CMC_distinct_huc) %>%
  pivot_wider(names_from=par,values_from=CMC_total_points)
s1_par[is.na(s1_par)]<-0
s1_par %>% gt() %>% gt::tab_header(title='CMC',subtitle = 'Parameters, by total points') %>%
  gt::fmt_number(colnames(s1_par)[2:length(s1_par)],use_seps=TRUE,decimals = 0)

s2_par <- cbp %>% group_by(par,yr) %>% summarise(distinct_huc=n_distinct(HUC12),total_points=n()) %>% ungroup()
s2_par <- s2_par %>% rename("CBP_distinct_huc"="distinct_huc","CBP_total_points"="total_points") %>% filter(yr>=2010) %>%
  select(-CBP_total_points) %>%
  pivot_wider(names_from=par,values_from=CBP_distinct_huc)
s2_par[is.na(s2_par)]<-0
s2_par %>% gt() %>% gt::tab_header(title='CBP',subtitle = 'Parameters, by total points') %>%
  gt::fmt_number(colnames(s2_par)[2:length(s2_par)],use_seps=TRUE,decimals = 0)

s2_par <- cbp %>% group_by(par,yr) %>% summarise(distinct_huc=n_distinct(HUC12),total_points=n()) %>% ungroup()
s2_par <- s2_par %>% rename("CBP_distinct_huc"="distinct_huc","CBP_total_points"="total_points") %>% filter(yr>=2010) %>%
  select(-CBP_distinct_huc) %>%
  pivot_wider(names_from=par,values_from=CBP_total_points)
s2_par[is.na(s2_par)]<-0
s2_par %>% gt() %>% gt::tab_header(title='CBP',subtitle = 'Parameters, by total points') %>%
  gt::fmt_number(colnames(s2_par)[2:length(s2_par)],use_seps=TRUE,decimals = 0)

# strictly cbp data gaps --------------------------------------------------

tfilters <- function(t1){
  t_low <<- t1 %>% filter(npar<=5)
  t_lowmid <<- t1 %>% filter(npar>5&npar<=8)
  t_highmid <<- t1 %>% filter(npar>8&npar<=12)
  t_high <<- t1 %>% filter(npar>12)
}

tyr <- function(YEAR){
t1 <<- cbp %>% filter(yr==YEAR) %>% 
  group_by(HUC12) %>% summarise(npar=n_distinct(par)) %>% ungroup()
# t1 %>% group_by(npar) %>% count() %>% ungroup() %>%
#   ggplot() + geom_bar(aes(x=npar,y=n),stat='identity') +
#   scale_x_continuous(breaks=c(seq(1,25)))
}

tplot <- function(t1){
ggplot() + theme_bw() + geom_sf(data=huc %>% mutate(HUC12=case_when(
  substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
  TRUE ~ HUC12
)) %>% filter(HUC12 %in% t_low$HUC12),fill='lightyellow') +
  geom_sf(data=huc %>% mutate(HUC12=case_when(
    substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
    TRUE ~ HUC12
  )) %>% filter(HUC12 %in% t_lowmid$HUC12),fill='yellow') +
  geom_sf(data=huc %>% mutate(HUC12=case_when(
    substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
    TRUE ~ HUC12
  )) %>% filter(HUC12 %in% t_highmid$HUC12),fill='orange') +
  geom_sf(data=huc %>% mutate(HUC12=case_when(
    substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
    TRUE ~ HUC12
  )) %>% filter(HUC12 %in% t_high$HUC12),fill='red') +
    scale_x_continuous(limits=c(min(min(cmc$Longitude),min(cbp$Longitude)),max(max(cmc$Longitude),max(cbp$Longitude)))) +
    scale_y_continuous(limits=c(min(min(cmc$Latitude),min(cbp$Latitude)),max(max(cmc$Latitude),max(cbp$Latitude))))
}

library(patchwork)
tyr(2016);tfilters(t1);tplot(t1) + ggtitle("2016") -> gg1
tyr(2017);tfilters(t1);tplot(t1) + ggtitle("2017") -> gg2
tyr(2018);tfilters(t1);tplot(t1) + ggtitle("2018") -> gg3
tyr(2019);tfilters(t1);tplot(t1) + ggtitle("2019") -> gg4
gg1 + gg2 + gg3 + gg4 + plot_annotation(title='CBP',caption = "Parameters: Yellow = 6-8, Orange = 9-12, Red = >12") +
  plot_layout(nrow = 1)

# strictly cmc data gaps --------------------------------------------------

tyr_cmc <- function(YEAR){
  t1 <<- cmc %>% filter(yr==YEAR) %>% 
    group_by(HUC12_) %>% summarise(npar=n_distinct(par)) %>% ungroup() %>% rename('HUC12'='HUC12_') %>%
    mutate(HUC12=case_when(
      substr(HUC12,1,1) == "0" ~ substr(HUC12,2,nchar(HUC12)),
      TRUE ~ HUC12
    ))
  # t1 %>% group_by(npar) %>% count() %>% ungroup() %>%
  #   ggplot() + geom_bar(aes(x=npar,y=n),stat='identity') +
  #   scale_x_continuous(breaks=c(seq(1,25)))
}



tyr_cmc(2016);tfilters(t1);tplot(t1) + ggtitle("2016") -> gg1
tyr_cmc(2017);tfilters(t1);tplot(t1) + ggtitle("2017") -> gg2
tyr_cmc(2018);tfilters(t1);tplot(t1) + ggtitle("2018") -> gg3
tyr_cmc(2019);tfilters(t1);tplot(t1) + ggtitle("2019") -> gg4
gg1 + gg2 + gg3 + gg4 + plot_annotation(title='CMC',caption = "Parameters: Light Yellow < 5, Yellow = 6-8, Orange = 9-12, Red = >12") +
  plot_layout(nrow = 1)


# look at parameters change in DC only -------------------------------------------

library(maps)
dc = st_as_sf(map('state', plot = FALSE, fill = TRUE)) %>%
filter(ID %in% c("district of columbia"))

dcdat <- cmc %>% filter(state=='DC') %>% filter(yr>=2017) %>% filter(par %in% c('DO','WT','CO','PH')) %>%
  filter(Latitude>38,-76.94>Longitude)
  
dcplot <- function(PAR,parameter,COL){
  dcdat %>% filter(par==PAR) %>%
    ggplot() + theme_bw() +
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(size=16,hjust = 0.5),
          legend.position="none") +
    geom_sf(data=dc,fill='gray98') +
    geom_point(aes(x=Longitude,y=Latitude),pch=21,fill=COL,colour='black',size=3) +
    ggtitle(paste0(parameter))
}

dcplot('DO','Dissolved Oxygen','red') + dcplot('WT','Water Temperature','blue') + dcplot('PH','pH','purple') + dcplot('CO','Conductivity','green') +
  plot_annotation(caption = "Data from 2017-2020 from CMC database.")

dcdatgrp <- dcdat %>% group_by(yr,par) %>% count() %>% ungroup() %>% mutate(CO=0) %>% pivot_wider(names_from=par,values_from=n)
dcdatgrp[is.na(dcdatgrp)]<-0

dcdatgrp %>%
  gt() %>% gt::tab_header(title='Growth in 4 parameters in DC',subtitle = 'Date range: 2017 - 2020') %>%
  gt::cols_label("DO"="Dissolved Oxygen","CO"='Connectivity',"PH"="pH","WT"='Water Temperature','yr'='Year')

dcdat <- cmc %>% filter(state=='DC') %>% filter(yr>=2017) %>%
  filter(Latitude>38,-76.94>Longitude)

# ‘#E6194B’,  ‘#FFE119’, ‘#4363D8’,  ‘#911EB4’,  ‘#F032E6’, ‘#FABEBE’, ‘#008080’,  ‘#9A6324’, ‘#FFFAC8’, ‘#800000’, ‘#AAFFC3’


(dcplot('AT','Air\nTemperature','#E6194B') + dcplot('DO','Dissolved\nOxygen','#FFE119') + dcplot('ECOLI','E. Coli','#4363D8') + plot_layout(nrow = 1)) /
(dcplot('NO3N','Nitrate\nNitrogen','#911EB4') + dcplot('OP','Orthophosphate\nPhosphorus','#F032E6') + dcplot('PH','pH','#FABEBE') + dcplot('SA','Salinity','#008080') + plot_layout(nrow = 1)) /
(dcplot('TD','Total Depth','#9A6324') + dcplot('WC','Turbidity','#FFFAC8') + dcplot('WT','Water\nTemperature','#AAFFC3') + plot_layout(nrow = 1)) +
  plot_annotation(caption = "Data from 2017-2020 from CMC database.") + theme(plot.margin = margin(5.5, 5.5, 5.5, 35))

dcdatgrp <- dcdat %>% group_by(yr,par) %>% count() %>% ungroup() %>% arrange(par) %>% mutate(CO=0) %>% pivot_wider(names_from=par,values_from=n)
dcdatgrp[is.na(dcdatgrp)]<-0

dcdatgrp %>% select(-CO) %>% arrange(yr) %>%
  gt() %>% gt::tab_header(title='Growth in all parameters in DC (CMC)',subtitle = 'Date range: 2017 - 2020') %>% 
  gt::cols_label('yr'='Year',
                 "AT"="Air\nTemperature","DO"="Dissolved\nOxygen","ECOLI"="E. Coli",
                 "NO3N"="Nitrate\nNitrogen","OP"="Orthophosphate\nPhosphorus","PH"="pH","SA"="Salinity",
                 "TD"="Total Depth","WC"="Turbidity","WT"="Water\nTemperature") %>%
  gt::tab_source_note(source_note ='Data as of 7/2020. The year 2020 has reduced data points due to partial year data and COVID-19.')
