## ------------------------------------------------------
## Compute 3-years moving avg of Ms from the keyrun (at keyrun resolution, by quarter)
## after shift and aggregate to match the assessment resolution 
## ------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)

rm(list=ls())

m.sms <- read_excel("Calculation_M_final.xlsx", range="H2:M198")
colnames(m.sms) <- c("year","quarter","a0","a1","a2","a3")
m.sms <- m.sms %>%
    mutate(a0=as.numeric(a0)) %>%
    gather("age","m",a0:a3) %>%
    mutate(quarter = as.numeric(substring(quarter,2,2))) %>%
    mutate(age = as.numeric(substring(age,2,2)))

## shift year,season,age for the assessment
m.sms <- m.sms %>%
    mutate(year2 = ifelse(quarter %in% 1:2, year-1, year)) %>%
    mutate(season = ifelse(quarter %in% 1:2, quarter+2,
                    ifelse(quarter %in% 3:4, quarter-2, NA))) %>%
    mutate(age2 = ifelse(quarter %in% 1:2, age-1, age)) %>%
    filter(age2 >= 0)

## calculate 3-year smoothing window
m.sms <- m.sms %>%
    group_by(quarter,age) %>%
    mutate(m.smooth3=frollmean(m,3, na.rm=T)) %>%
    ungroup()
m.sms %>% head(20)
m.sms %>% tail(10)


## aggregate over 2 seasons
m.ass <- m.sms %>%
    mutate(season2 = ifelse(season %in% 1:2,1,
                     ifelse(season %in% 3:4,2, NA))) %>%
    group_by(year2,season2,age2) %>%
    summarise(m.smooth3 = sum(m.smooth3))
## fill in age3 in S2 (assumed =age2)
m.ass <- m.ass %>%
    bind_rows(m.ass %>%
              filter(age2==2 & season2==2) %>%
              mutate(age2=age2+1))
m.ass %>% head(20)
m.ass %>% tail(20)

m.ass %>% spread(age2,m.smooth3) %>% tail(10)
m.ass %>% spread(age2,m.smooth3) %>% head(10)
