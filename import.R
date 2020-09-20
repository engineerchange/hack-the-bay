library(tidyverse)
library(gt)
library(ggmap)
library(sbtools)
library(dataRetrieval)
library(sf)
library(lubridate)

if(!exists("water")){
  water <- read_rds("data/Water_FINAL2.rds") # see initialize.R to create this file
  huc <- st_read('data/wbdhu12_a_us_september2019.gdb')
}

# utilities

`%notin%` <- Negate(`%in%`)

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}