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

s1 %>% rename("CMC_distinct_huc"="distinct_huc","CMC_total_points"="total_points") %>% 
  left_join(s2 %>% rename("CBP_distinct_huc"="distinct_huc","CBP_total_points"="total_points"),by=c("yr")) %>% filter(yr>=2010) %>%
  gt() %>% gt::cols_label("CBP_total_points"="Total Points","CMC_total_points"="Total Points",
                          "CBP_distinct_huc"="Distinct HUCs","CMC_distinct_huc"="Distinct HUCs") %>%
  gt::tab_spanner(
    label = "CMC",
    columns = vars(CMC_distinct_huc,CMC_total_points)
  ) %>%
  gt::tab_spanner(
    label = "CBP",
    columns = vars(CBP_distinct_huc,CBP_total_points)
  ) %>%
  gt::fmt_number(vars(CBP_total_points,CMC_total_points),use_seps = TRUE,decimals=0) %>%
  gt::tab_header(title='Comparing Water Quality Data Across Databases')


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
  )) %>% filter(HUC12 %in% t_high$HUC12),fill='red')
}

library(patchwork)
tyr(2016);tfilters(t1);tplot(t1) + ggtitle("2016") -> gg1
tyr(2017);tfilters(t1);tplot(t1) + ggtitle("2017") -> gg2
tyr(2018);tfilters(t1);tplot(t1) + ggtitle("2018") -> gg3
tyr(2019);tfilters(t1);tplot(t1) + ggtitle("2019") -> gg4
(gg1 + gg2) / (gg3 + gg4) + plot_annotation(title='CBP',caption = "Parameters: Yellow = 5-8, Orange = 8-12, Red = >12") 

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
(gg1 + gg2) / (gg3 + gg4) + plot_annotation(title='CMC',caption = "Parameters: Light Yellow = <5, Yellow = 5-8, Orange = 8-12, Red = >12") 











