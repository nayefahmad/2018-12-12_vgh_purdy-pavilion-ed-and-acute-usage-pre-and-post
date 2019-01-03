

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
library(ggpubr)
library(broom)


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






# 3) Fit models to pre-intervention series: -------------
# > 3.1) model 1: trend only --------------

m1.pre.trend <- tslm(ts1.pre.intervention ~ trend) 
summary(m1.pre.trend)  # no significant trend 

# plot data and trend: 
p1.data.and.trend <- 
      data.frame(data = as.numeric(ts1.pre.intervention), 
                 trend = predict(m1.pre.trend), 
                 period = 1:24) %>% 
      gather(key = "key", 
             value = "val", 
             -period) %>% 
      ggplot(aes(x = period, 
                 y = val, 
                 group = key, 
                 colour = key)) + 
      geom_line() + 
      theme(legend.position = "none"); p1.data.and.trend



# > 3.2) model 2: approximate the seasonal pattern using Fourier terms -------

m2.fourier <- tslm(ts1.pre.intervention ~ trend + fourier(ts1.pre.intervention,2))
summary(m2.fourier)

# save coefficients: 
df2.coeffients.from.m2 <- tidy(m2.fourier)


# what does the sum of all these terms look like? 
sum.of.fouriers <- fourier(ts1.pre.intervention, 2) %>%
      as.data.frame() %>% 
      apply(MARGIN = 1, 
            FUN = sum)

# >> plot sum of fourier terms: 
p2.fourier.terms <- 
      data.frame(period = rep(1:24), 
                 value = sum.of.fouriers) %>% 
      ggplot(aes(x = period, 
                 y = value)) +
      geom_hline(yintercept = 0, 
                 col = "grey60") + 
      geom_line(col = "coral2"); p2.fourier.terms

# >> compare with original series: 
ggarrange(p1.data.and.trend, 
          p2.fourier.terms, 
          nrow = 2)


# now let's add in the final trend + fourier series: 
p3.final.series <- 
      data.frame(data = as.numeric(ts1.pre.intervention), 
                 predicted.with.fourier = predict(m2.fourier), 
                 period = 1:24) %>% 
      gather(key = "key", 
             value = "value", 
             -period) %>%  
      
      ggplot(aes(x = period, 
                 y = value, 
                 group = key, 
                 col = key)) + 
      geom_line() + 
      theme(legend.position = "bottom"); p3.final.series


# 3.3) decomposition into trend/season/remainder: -----

# first let's create the trend series from model m2: 
pre.trend.m2 <- 
      df2.coeffients.from.m2$estimate[1] +  # intercept 
      df2.coeffients.from.m2$estimate[2] * seq_along(ts1.pre.intervention)


df3.pre.decomposed <- 
      cbind(data = ts1.pre.intervention, 
            trend = pre.trend.m2,                            # from model m2.fourier
            season = ts1.pre.intervention - pre.trend.m2 - resid(m2.fourier),  # from model m2.fourier
            remainder = resid(m2.fourier))               # from model m2.fourier

df3.pre.decomposed
str(df3.pre.decomposed)  
# note that this is not a df, so we can't use $ to subset columns. 
# e.g. to get teh season component, we write: 
# df2.decomposed[,'season']

# plot decomposed series: 
autoplot(df3.pre.decomposed, facets = TRUE)


# plot all components: 
autoplot(df3.pre.decomposed[, 'trend'], 
         series = "Trend") + 
      
      autolayer(df3.pre.decomposed[, 'season'], 
                series = "Seasonal") + 
      
      autolayer(df3.pre.decomposed[, 'remainder'], 
                series = "Remainder") + 
      
      autolayer(ts1.pre.intervention, 
                series = "Raw data") + 
      
      
      labs(title = "ED Visits: De-seasonalized trend pre-intervention", 
           subtitle = "Jan 2015 to Dec 2016")





