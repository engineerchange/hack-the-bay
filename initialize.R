# initialization

# get water data from water quality data link here: https://github.com/Hack-the-Bay/hack-the-bay (expect Water_FINAL.csv)
# save to data/

library(tidyverse)


# clean up some initial ugliness in the data ------------------------------
# change to rds to improve read time

water <- read_csv("data/Water_FINAL.csv",trim_ws = TRUE,guess_max = 2000000)
water %>% rename("row"="X1","line"="Unnamed: 0") -> water
water %>% write_rds("data/Water_FINAL2.rds")

# download 'wbdhu12_a_us_september2019.gdb' and store in data/