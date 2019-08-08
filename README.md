De-seasonalizing daily ED visit data and running segmented regression
analysis
================
Nayef Ahmad
2018-12-24

``` r
source(here("src", 
            "stl.as.df_function.R"))
```

## Data

``` r
# 1) input dataset: -------------------
options(readr.default_locale=readr::locale(tz="America/Los_Angeles"))

df1.orig.data <- read_csv(here("data", 
                               "2018-12-24_vgh_purdy-pavilion-intervention.csv")) %>% 
      clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   Period = col_double(),
    ##   `Calendar Month` = col_character(),
    ##   `ED visits` = col_double(),
    ##   is_post_intervention = col_double()
    ## )

``` r
str(df1.orig.data)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 48 obs. of  4 variables:
    ##  $ period              : num  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ calendar_month      : chr  "January" "February" "March" "April" ...
    ##  $ ed_visits           : num  14 5 7 19 10 3 4 6 9 11 ...
    ##  $ is_post_intervention: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   Period = col_double(),
    ##   ..   `Calendar Month` = col_character(),
    ##   ..   `ED visits` = col_double(),
    ##   ..   is_post_intervention = col_double()
    ##   .. )

``` r
# 2.1) create pre-intervention ts: -------------------
ts1.pre.intervention <- 
      df1.orig.data %>% 
      filter(is_post_intervention == 0) %>% 
      pull(ed_visits) %>% 
      ts(start = c(2015, 1), 
         frequency = 12)

ts1.pre.intervention
```

    ##      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    ## 2015  14   5   7  19  10   3   4   6   9  11  12   7
    ## 2016  12  15  16  16  14  11  10  11  16  13  15  17

``` r
# 2.2) create POST-intervention ts: -------------------
ts2.post.intervention <- 
      df1.orig.data %>% 
      filter(is_post_intervention == 1) %>% 
      pull(ed_visits) %>% 
      ts(start = c(2017, 1), 
         frequency = 12)

ts2.post.intervention
```

    ##      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    ## 2017   2   4   4  11   7   6   8   3   4   5   4   5
    ## 2018   4   1   6   8   6   3   5   3   4   9   6  10

## Fit models to pre-intervention time series

### Model 1: trend only

``` r
# 3) Fit models to pre-intervention series: -------------
# > 3.1) model 1: trend only --------------

m1.pre.trend <- tslm(ts1.pre.intervention ~ trend) 
summary(m1.pre.trend)  # no significant trend 
```

    ## 
    ## Call:
    ## tslm(formula = ts1.pre.intervention ~ trend)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4391 -2.7685  0.4228  2.1201 10.1565 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.6522     1.6506   4.636 0.000128 ***
    ## trend         0.2978     0.1155   2.578 0.017155 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.917 on 22 degrees of freedom
    ## Multiple R-squared:  0.232,  Adjusted R-squared:  0.1971 
    ## F-statistic: 6.647 on 1 and 22 DF,  p-value: 0.01715

``` r
# plot data and trend: 
p1.data.and.trend <- 
      data.frame(data = as.numeric(ts1.pre.intervention), 
                 trend = as.numeric(m1.pre.trend$fitted.values), 
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
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Model 2: approximate the seasonal pattern using Fourier terms

``` r
# > 3.2) model 2: approximate the seasonal pattern using Fourier terms -------

m2.fourier <- tslm(ts1.pre.intervention ~ trend + fourier(ts1.pre.intervention,2))
summary(m2.fourier)
```

    ## 
    ## Call:
    ## tslm(formula = ts1.pre.intervention ~ trend + fourier(ts1.pre.intervention, 
    ##     2))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8731 -0.7867  0.0421  0.6656  6.9093 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                             6.7550     1.4709   4.592 0.000226
    ## trend                                   0.3696     0.1054   3.506 0.002523
    ## fourier(ts1.pre.intervention, 2)S1-12   2.5981     1.0047   2.586 0.018643
    ## fourier(ts1.pre.intervention, 2)C1-12   1.2129     0.9305   1.304 0.208800
    ## fourier(ts1.pre.intervention, 2)S2-12  -1.7414     0.9423  -1.848 0.081103
    ## fourier(ts1.pre.intervention, 2)C2-12  -1.4113     0.9305  -1.517 0.146696
    ##                                          
    ## (Intercept)                           ***
    ## trend                                 ** 
    ## fourier(ts1.pre.intervention, 2)S1-12 *  
    ## fourier(ts1.pre.intervention, 2)C1-12    
    ## fourier(ts1.pre.intervention, 2)S2-12 .  
    ## fourier(ts1.pre.intervention, 2)C2-12    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.202 on 18 degrees of freedom
    ## Multiple R-squared:  0.5801, Adjusted R-squared:  0.4635 
    ## F-statistic: 4.973 on 5 and 18 DF,  p-value: 0.004919

``` r
# save coefficients: 
df2.coeffients.from.m2 <- tidy(m2.fourier)
```

## Examining the Fourier terms

``` r
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
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# >> compare with original series: 
ggarrange(p1.data.and.trend, 
          p2.fourier.terms, 
          nrow = 2)
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# now let's add in the final trend + fourier series: 
p3.final.series <- 
      data.frame(data = as.numeric(ts1.pre.intervention), 
                 predicted.with.fourier = as.numeric(m2.fourier$fitted.values), 
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
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

## Decomposition into trend/season/remainder

``` r
# > 3.3) decomposition into trend/season/remainder: -----

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
```

    ##          data     trend     season   remainder
    ## Jan 2015   14  7.124615  0.1357250  6.73966038
    ## Feb 2015    5  7.494213  2.0539811 -4.54819450
    ## Mar 2015    7  7.863812  4.0093229 -4.87313502
    ## Apr 2015   19  8.233411  3.8572519  6.90933726
    ## May 2015   10  8.603010  1.0510672  0.34592328
    ## Jun 2015    3  8.972608 -2.6241984 -3.34840986
    ## Jul 2015    4  9.342207 -4.5631942 -0.77901282
    ## Aug 2015    6  9.711806 -3.6589195 -0.05288618
    ## Sep 2015    9 10.081404 -1.1867921  0.10538768
    ## Oct 2015   11 10.451003  0.5702173 -0.02122049
    ## Nov 2015   12 10.820602  0.5538713  0.62552682
    ## Dec 2015    7 11.190201 -0.1983324 -3.99186827
    ## Jan 2016   12 11.559799  0.1357250  0.30447566
    ## Feb 2016   15 11.929398  2.0539811  1.01662079
    ## Mar 2016   16 12.298997  4.0093229 -0.30831974
    ## Apr 2016   16 12.668596  3.8572519 -0.52584745
    ## May 2016   14 13.038194  1.0510672 -0.08926143
    ## Jun 2016   11 13.407793 -2.6241984  0.21640543
    ## Jul 2016   10 13.777392 -4.5631942  0.78580247
    ## Aug 2016   11 14.146990 -3.6589195  0.51192910
    ## Sep 2016   16 14.516589 -1.1867921  2.67020296
    ## Oct 2016   13 14.886188  0.5702173 -2.45640520
    ## Nov 2016   15 15.255787  0.5538713 -0.80965789
    ## Dec 2016   17 15.625385 -0.1983324  1.57294702

``` r
str(df3.pre.decomposed)  
```

    ##  Time-Series [1:24, 1:4] from 2015 to 2017: 14 5 7 19 10 3 4 6 9 11 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr [1:4] "data" "trend" "season" "remainder"

``` r
# note that this is not a df, so we can't use $ to subset columns. 
# e.g. to get teh season component, we write: 
# df2.decomposed[,'season']

# plot decomposed series: 
autoplot(df3.pre.decomposed, facets = TRUE)
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# plot all components: 
autoplot(df3.pre.decomposed[, 'trend'], 
         series = "Trend") + 
      
      autolayer(df3.pre.decomposed[, 'season'], 
                series = "Seasonal") + 
      
      autolayer(df3.pre.decomposed[, 'remainder'], 
                series = "Remainder") + 
      
      autolayer(ts1.pre.intervention, 
                series = "Raw data") + 
      
      geom_hline(yintercept = 0) + 
      
      
      labs(title = "ED Visits: De-seasonalized trend pre-intervention", 
           subtitle = "Jan 2015 to Dec 2016")
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

## Fit models to post-intervention time series

### Model 1: trend only

``` r
# 4) Fit models to post-intervention series: -------------
# > 4.1) model 1: trend only --------------

m3.post.trend <- tslm(ts2.post.intervention ~ trend) 
summary(m3.post.trend)  # no significant trend 
```

    ## 
    ## Call:
    ## tslm(formula = ts2.post.intervention ~ trend)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4142 -1.4681 -0.4951  1.2806  6.1249 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.65942    1.06287   4.384 0.000236 ***
    ## trend        0.05391    0.07439   0.725 0.476227    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.523 on 22 degrees of freedom
    ## Multiple R-squared:  0.02332,    Adjusted R-squared:  -0.02107 
    ## F-statistic: 0.5253 on 1 and 22 DF,  p-value: 0.4762

``` r
# plot data and trend: 
p4.data.and.trend <- 
      data.frame(data = as.numeric(ts2.post.intervention), 
                 trend = as.numeric(m3.post.trend$fitted.values), 
                 period = 1:24) %>% 
      gather(key = "key", 
             value = "val", 
             -period) %>% 
      ggplot(aes(x = period, 
                 y = val, 
                 group = key, 
                 colour = key)) + 
      geom_line() + 
      theme(legend.position = "none"); p4.data.and.trend
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Model 2: approximate the seasonal pattern using Fourier terms

``` r
# > 4.2) model 2: approximate the seasonal pattern using Fourier terms -------

m4.post.fourier <- tslm(ts2.post.intervention ~ trend + fourier(ts2.post.intervention,2))
summary(m4.post.fourier)
```

    ## 
    ## Call:
    ## tslm(formula = ts2.post.intervention ~ trend + fourier(ts2.post.intervention, 
    ##     2))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4397 -0.9871 -0.3297  0.5282  4.3450 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value
    ## (Intercept)                             4.83775    1.04898   4.612
    ## trend                                   0.03965    0.07518   0.527
    ## fourier(ts2.post.intervention, 2)S1-12  0.43664    0.71648   0.609
    ## fourier(ts2.post.intervention, 2)C1-12 -0.51133    0.66354  -0.771
    ## fourier(ts2.post.intervention, 2)S2-12 -1.80772    0.67200  -2.690
    ## fourier(ts2.post.intervention, 2)C2-12  0.37702    0.66354   0.568
    ##                                        Pr(>|t|)    
    ## (Intercept)                            0.000217 ***
    ## trend                                  0.604367    
    ## fourier(ts2.post.intervention, 2)S1-12 0.549860    
    ## fourier(ts2.post.intervention, 2)C1-12 0.450928    
    ## fourier(ts2.post.intervention, 2)S2-12 0.014964 *  
    ## fourier(ts2.post.intervention, 2)C2-12 0.576920    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.284 on 18 degrees of freedom
    ## Multiple R-squared:  0.345,  Adjusted R-squared:  0.1631 
    ## F-statistic: 1.896 on 5 and 18 DF,  p-value: 0.145

``` r
# save coefficients: 
df4.coeffients.from.m4 <- tidy(m4.post.fourier)


# what does the sum of all these terms look like? 
sum.of.fouriers2 <- fourier(ts2.post.intervention, 2) %>%
      as.data.frame() %>% 
      apply(MARGIN = 1, 
            FUN = sum)

# >> plot sum of fourier terms: 
p5.fourier.terms <- 
      data.frame(period = rep(1:24), 
                 value = sum.of.fouriers2) %>% 
      ggplot(aes(x = period, 
                 y = value)) +
      geom_hline(yintercept = 0, 
                 col = "grey60") + 
      geom_line(col = "coral2"); p5.fourier.terms
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# compare p5 with p2 ==> same seasonal pattern detected


# >> compare with original series: 
ggarrange(p4.data.and.trend, 
          p5.fourier.terms, 
          nrow = 2)
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# now let's add in the final trend + fourier series: 
p6.post.final.series <- 
      data.frame(data = as.numeric(ts2.post.intervention), 
                 predicted.with.fourier = as.numeric(m4.post.fourier$fitted.values), 
                 period = 1:24) %>% 
      gather(key = "key", 
             value = "value", 
             -period) %>%  
      
      ggplot(aes(x = period, 
                 y = value, 
                 group = key, 
                 col = key)) + 
      geom_line() + 
      theme(legend.position = "bottom"); p6.post.final.series
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

## Decomposition into trend/season/remainder

``` r
# > 4.3) decomposition into trend/season/remainder: -----

# first let's create the trend series from model m2: 
post.trend.m4 <- 
      df4.coeffients.from.m4$estimate[1] +  # intercept 
      df4.coeffients.from.m4$estimate[2] * seq_along(ts2.post.intervention)


df5.post.decomposed <- 
      cbind(data = ts2.post.intervention, 
            trend = post.trend.m4,                            # from model m2.fourier
            season = ts2.post.intervention - post.trend.m4 - resid(m4.post.fourier),  # from model m2.fourier
            remainder = resid(m4.post.fourier))               # from model m2.fourier

df5.post.decomposed
```

    ##          data    trend      season   remainder
    ## Jan 2017    2 4.877396 -1.60152926 -1.27586650
    ## Feb 2017    4 4.917043 -1.63156684  0.71452433
    ## Mar 2017    4 4.956689  0.05961888 -1.01630813
    ## Apr 2017   11 4.996336  2.01082751  3.99283649
    ## May 2017    7 5.035983  2.41518798 -0.45117073
    ## Jun 2017    6 5.075629  0.88835450  0.03601601
    ## Jul 2017    8 5.115276 -1.15251058  4.03723435
    ## Aug 2017    3 5.154923 -1.87651285 -0.27841013
    ## Sep 2017    4 5.194570 -0.81365872 -0.38091100
    ## Oct 2017    5 5.234216  0.74321234 -0.97742881
    ## Nov 2017    4 5.273863  1.09289170 -2.36675492
    ## Dec 2017    5 5.313510 -0.13431466 -0.17919530
    ## Jan 2018    4 5.353157 -1.60152926  0.24837255
    ## Feb 2018    1 5.392803 -1.63156684 -2.76123661
    ## Mar 2018    6 5.432450  0.05961888  0.50793092
    ## Apr 2018    8 5.472097  2.01082751  0.51707555
    ## May 2018    6 5.511744  2.41518798 -1.92693167
    ## Jun 2018    3 5.551390  0.88835450 -3.43974494
    ## Jul 2018    5 5.591037 -1.15251058  0.56147341
    ## Aug 2018    3 5.630684 -1.87651285 -0.75417108
    ## Sep 2018    4 5.670331 -0.81365872 -0.85667194
    ## Oct 2018    9 5.709977  0.74321234  2.54681025
    ## Nov 2018    6 5.749624  1.09289170 -0.84251586
    ## Dec 2018   10 5.789271 -0.13431466  4.34504376

``` r
str(df5.post.decomposed)  
```

    ##  Time-Series [1:24, 1:4] from 2017 to 2019: 2 4 4 11 7 6 8 3 4 5 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr [1:4] "data" "trend" "season" "remainder"

``` r
# note that this is not a df, so we can't use $ to subset columns. 
# e.g. to get teh season component, we write: 
# df2.decomposed[,'season']

# plot decomposed series: 
autoplot(df5.post.decomposed, facets = TRUE)
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# plot all components: 
autoplot(df5.post.decomposed[, 'trend'], 
         series = "Trend") + 
      
      autolayer(df5.post.decomposed[, 'season'], 
                series = "Seasonal") + 
      
      autolayer(df5.post.decomposed[, 'remainder'], 
                series = "Remainder") + 
      
      autolayer(ts2.post.intervention, 
                series = "Raw data") + 
      
      geom_hline(yintercept = 0) + 
      
      
      labs(title = "ED Visits: De-seasonalized trend post-intervention", 
           subtitle = "Jan 2017 to Aug 2018")
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

## Df with pre- and post-intervention de-seasonalized series

``` r
# 5) df with pre- and post-intervention de-seasonalized series: -------

df6.trends.pre.and.post <- 
      data.frame(trend.value = c(ts1.pre.intervention - df3.pre.decomposed[, "season"], 
                                 ts2.post.intervention -  df5.post.decomposed[, "season"]),
                 timeperiod = 1:48,
                 post.intervention = c(rep(0, 24), 
                                       rep(1, 24)) %>% as.factor,
                 time.after.intervention = c(rep(0, 24), 
                                             1:24))

df6.trends.pre.and.post
```

    ##    trend.value timeperiod post.intervention time.after.intervention
    ## 1    13.864275          1                 0                       0
    ## 2     2.946019          2                 0                       0
    ## 3     2.990677          3                 0                       0
    ## 4    15.142748          4                 0                       0
    ## 5     8.948933          5                 0                       0
    ## 6     5.624198          6                 0                       0
    ## 7     8.563194          7                 0                       0
    ## 8     9.658920          8                 0                       0
    ## 9    10.186792          9                 0                       0
    ## 10   10.429783         10                 0                       0
    ## 11   11.446129         11                 0                       0
    ## 12    7.198332         12                 0                       0
    ## 13   11.864275         13                 0                       0
    ## 14   12.946019         14                 0                       0
    ## 15   11.990677         15                 0                       0
    ## 16   12.142748         16                 0                       0
    ## 17   12.948933         17                 0                       0
    ## 18   13.624198         18                 0                       0
    ## 19   14.563194         19                 0                       0
    ## 20   14.658920         20                 0                       0
    ## 21   17.186792         21                 0                       0
    ## 22   12.429783         22                 0                       0
    ## 23   14.446129         23                 0                       0
    ## 24   17.198332         24                 0                       0
    ## 25    3.601529         25                 1                       1
    ## 26    5.631567         26                 1                       2
    ## 27    3.940381         27                 1                       3
    ## 28    8.989172         28                 1                       4
    ## 29    4.584812         29                 1                       5
    ## 30    5.111645         30                 1                       6
    ## 31    9.152511         31                 1                       7
    ## 32    4.876513         32                 1                       8
    ## 33    4.813659         33                 1                       9
    ## 34    4.256788         34                 1                      10
    ## 35    2.907108         35                 1                      11
    ## 36    5.134315         36                 1                      12
    ## 37    5.601529         37                 1                      13
    ## 38    2.631567         38                 1                      14
    ## 39    5.940381         39                 1                      15
    ## 40    5.989172         40                 1                      16
    ## 41    3.584812         41                 1                      17
    ## 42    2.111645         42                 1                      18
    ## 43    6.152511         43                 1                      19
    ## 44    4.876513         44                 1                      20
    ## 45    4.813659         45                 1                      21
    ## 46    8.256788         46                 1                      22
    ## 47    4.907108         47                 1                      23
    ## 48   10.134315         48                 1                      24

``` r
# 5.1) plot pre- and post- trends: 
p7.pre.post.trends <- 
      df6.trends.pre.and.post %>% 
      ggplot(aes(x = timeperiod, 
                 y = trend.value, 
                 group = post.intervention, 
                 colour = post.intervention)) + 
      
      geom_line() + 
      geom_point() + 
      stat_smooth(method = "lm") + 
      
      geom_vline(xintercept = 24,
                 colour = "grey50") + 
      
      geom_vline(xintercept = 25,
                 colour = "grey50") + 
      
      scale_y_continuous(limits = c(0, 20)) + 
      
      theme_minimal(base_size = 14) + 
      theme(panel.border = element_rect(fill = NA)) + 
      
      labs(title = "VGH Purdy Pavilion Evaluation", 
           subtitle = "ED visits (de-seasonalized) pre- and post- Jan 2017"); p7.pre.post.trends
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Segmented regression analysis

``` r
# 6) segmented regression analysis: ----------

m5.segmented.regression <- lm(trend.value ~ timeperiod + 
                                    post.intervention + 
                                    time.after.intervention, 
                              data = df6.trends.pre.and.post)


summary(m5.segmented.regression)
```

    ## 
    ## Call:
    ## lm(formula = trend.value ~ timeperiod + post.intervention + time.after.intervention, 
    ##     data = df6.trends.pre.and.post)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8731 -0.8869 -0.0711  0.5775  6.9093 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               6.75502    1.06002   6.373 9.55e-08 ***
    ## timeperiod                0.36960    0.07419   4.982 1.02e-05 ***
    ## post.intervention1      -10.78764    1.45437  -7.417 2.81e-09 ***
    ## time.after.intervention  -0.32995    0.10491  -3.145  0.00297 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.516 on 44 degrees of freedom
    ## Multiple R-squared:  0.6819, Adjusted R-squared:  0.6602 
    ## F-statistic: 31.44 on 3 and 44 DF,  p-value: 5.117e-11

``` r
# > 6.1) diagnostics: -------------
par(mfrow = c(2,2))
plot(m5.segmented.regression)
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
par(mfrow = c(1,1))

resid(m5.segmented.regression) %>% density() %>% plot
```

![](2018-12-24_interrupted-time-series_ed-visits-data_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
# > 6.2) interpretation of segmented regression: --------
df7.coeff.from.segmented.regression <-  tidy(m5.segmented.regression)

df7.1.predicted.values <- augment(m5.segmented.regression) %>% 
      select(.fitted) %>% 
      rename(fitted.values = .fitted)

df8.coefficient.confints <- confint(m5.segmented.regression,
                                    level = 0.95) 

# remove rownames: 
rownames(df8.coefficient.confints) <- NULL

# final summary of coefficients:
df9.coefficients.with.CIs <- 
      cbind(df7.coeff.from.segmented.regression, 
            df8.coefficient.confints) %>% 
      rename(ci.lower = `2.5 %`, 
             ci.upper = `97.5 %`)


# 6.3) counterfactual estimates (long-term effect of intervention): ----------------

df10.counterfactuals <- 
      df6.trends.pre.and.post %>% 
      
      filter(post.intervention == 1) %>% 
      mutate(predicted.diff.from.counterfactual = 
                   
                   # point estimate off difference from counterfactual: Note:
                   # see p302 of paper "Segmented regression analysis of
                   # interrupted time series studies in medication use research"
                   
                   # coefficient beta2 + beta3 * time after intervention is the
                   # estimate of the long-term effect
                   
                   
                   df9.coefficients.with.CIs$estimate[3] +   
                   df9.coefficients.with.CIs$estimate[4] * time.after.intervention,
            
            
            
            lower.predicted.diff.from.counterfactual = 
      
                   # smallest possible level & trend CHANGES from pre-intervention 
                   df9.coefficients.with.CIs$ci.upper[3] +   
                   df9.coefficients.with.CIs$ci.upper[4] * time.after.intervention, 
             
             
             upper.predicted.diff.from.counterfactual = 
                   
                   # LARGEST possible level & trend CHANGES from pre-intervention 
                   df9.coefficients.with.CIs$ci.lower[3] +   
                   df9.coefficients.with.CIs$ci.lower[4] * time.after.intervention
             
             )


# long-term effect of the intervention over following 24 months: 
df11.long.term.effects <- data.frame(
      estimate.long.term.effect = sum(df10.counterfactuals$predicted.diff.from.counterfactual),   
      lower.long.term.effect = sum(df10.counterfactuals$lower.predicted.diff.from.counterfactual),  
      upper.long.term.effect = sum(df10.counterfactuals$upper.predicted.diff.from.counterfactual)
)

df11.long.term.effects
```

    ##   estimate.long.term.effect lower.long.term.effect upper.long.term.effect
    ## 1                 -357.8889                -224.11              -491.6677

## Model interpretation

``` r
# > 6.4) MODEL INTERPRETATION: ---------------- 
# pre-intervention y-intercept: 6.8 ED visits 
# pre-intervention slope: +0.37 ED visits per month 

# immediate effect of intervention: change of -10.8 ED visits (95% CI: [-13.7, -7.9]) 

# longer-term effect of intervention: 
# reduction of 357 ED visits over 24 months (95% CI: [-224, -492]) 




# 7) Write outputs: --------------
write_csv(cbind(df6.trends.pre.and.post,
                df7.1.predicted.values),
          here("results",
               "dst",
               "2019-01-23_data-for-segmented-regression-analysis.csv"))


ggsave(here("results",
            "dst",
            "2019-01-04_data-for-segmented-regression-analysis.pdf"),
       p7.pre.post.trends,
       width = 10)
```

    ## Saving 10 x 5 in image

``` r
write_csv(df9.coefficients.with.CIs,
          here("results",
               "dst",
               "2019-01-07_segmented-regression-model-coefficients.csv"))


write_csv(df10.counterfactuals,
          here("results",
               "dst",
               "2019-01-07_counterfactual-estimates-for-long-term-effect-of-intervention.csv"))
```
