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

bth <- cmc %>% mutate(HUC12_=as.character(HUC12_)) %>% group_by(HUC12_,yr) %>% summarise(cmc_ct=n()) %>% ungroup() %>%
  full_join(cbp %>% mutate(HUC12=as.character(HUC12)) %>% group_by(HUC12,yr) %>% summarise(cbp_ct=n()) %>% ungroup(),by=c("yr","HUC12_"="HUC12")) %>%
  filter(yr>=2010) %>%
  group_by(yr) %>% summarise(total_points=sum(cmc_ct,cbp_ct,na.rm=T),distinct_huc=n_distinct(HUC12_)) %>% ungroup()

s1 %>% rename("CMC_distinct_huc"="distinct_huc","CMC_total_points"="total_points") %>% 
  left_join(s2 %>% rename("CBP_distinct_huc"="distinct_huc","CBP_total_points"="total_points"),by=c("yr")) %>% filter(yr>=2010) %>%
  left_join(bth %>% rename("BOTH_distinct_huc"="distinct_huc","BOTH_total_points"="total_points"),by=c("yr")) %>%
  gt() %>% gt::cols_label("CBP_total_points"="Total Points","CMC_total_points"="Total Points",
                          "CBP_distinct_huc"="Distinct HUCs","CMC_distinct_huc"="Distinct HUCs",
                          "BOTH_distinct_huc"="Distinct HUCs","BOTH_total_points"="Total Points") %>%
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
  gt::tab_header(title='Comparing Water Quality Data Across Databases')


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











