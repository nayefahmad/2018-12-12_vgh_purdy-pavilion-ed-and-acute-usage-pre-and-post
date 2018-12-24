

#*************************************************************************
# DE-SEASONALIZING NUM ED VISITS DATA
# 2018-12-24
# Nayef 
#*************************************************************************

library(forecast)
library(fpp)
library(ggplot2)
library(tidyverse)
library(here)
library(janitor)
library(TSA)


# rm(list = ls())
source(here("src", 
            "stl.as.df_function.R"))


# 1) input dataset: -------------------
df1.orig.data <- read_csv(here("data", 
                               "2018-12-24_vgh_purdy-pavilion-intervention.csv")) %>% 
      clean_names()


str(df1.orig.data)


# 2.1) create pre-intervention ts: -------------------
ts1.pre.intervention <- 
      df1.orig.data %>% 
      filter(is_post_intervention == 0) %>% 
      pull(ed_visits) %>% 
      ts(start = c(2015, 1), 
         frequency = 12)

ts1.pre.intervention

# 2.2) create POST-intervention ts: -------------------
ts2.post.intervention <- 
      df1.orig.data %>% 
      filter(is_post_intervention == 1) %>% 
      pull(ed_visits) %>% 
      ts(start = c(2017, 1), 
         frequency = 12)

ts2.post.intervention



# 3) Spectral analysis of pre- series: ---------------
# https://ms.mcmaster.ca/~bolker/eeid/2010/Ecology/Spectral.pdf

?spectrum
?periodogram

spectrum(ts1.pre.intervention)  # raw periodogram; difficult to interpret 


p1.pre.periodogram <- periodogram(ts1.pre.intervention) 

str(p1.pre.periodogram)

# data frame with frequencies: 
df2.1.pre.frequencies <- 
      data.frame(freq = p1.pre.periodogram$freq, 
                 spec = p1.pre.periodogram$spec) %>% 
      mutate(period = 1/freq) %>% 
      arrange(desc(spec)) %>% print



