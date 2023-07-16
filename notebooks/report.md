Statistical Methods for Discrete Response, Time Series, and Panel Data
(W271): Lab 2
================

# C02 Emissions

## Part 1

### Introduction

$CO_2$ levels have been recorded at the Mauna Loa observatory for over
40 years. At this time the data seem to show an alarming trend of
increasing $CO_2$ levels year over year. This is alarming because $CO_2$
contributes to the “greenhouse effect”, where certain gasses collect in
the Earth’s atmosphere and trap heat from leaving the Earth. As $CO_2$
levels increase we expect the Earth’s temperature to increase with it.
While the exact effects of the change this will have on the Earth’s
environment remain to be seen, expected changes include but are not
limited to the following: - Heat waves - Drought - Rising sea levels
With the data at hand, it is imperative that we discover whether we have
enough evidence to show that this recent rise in $CO_2$ levels is the
result of a larger trend or could be explained by natural variation. If
this trend is confirmed then it could pave the way to future research on
ways to measure and address the adverse effects and causes of this rise
in $CO_2$. This report will look into the existence of this larger trend
of rising $CO_2$ levels and, if it exists, will also report on the
magnitude of the rise as well as project future $CO_2$ levels.

![](report_files/figure-gfm/plot%20the%20keeling%20curve-1.png)<!-- -->

### CO2 Data

#### Mauna Loa Site

As stated above, the data we will be using for this analysis is the
$CO_2$ measurements from a laboratory at Mauna Loa, Hawaii. While there
are other laboratories that collect $CO_2$ measurements, the Mauna Loa
site has been collected $CO_2$ longer than any other site in the world
which will give us the most data to work with as we conduct this
analysis. The Mauna Loa site is also unique in that it is representative
of air for the entire Northern Hemisphere due to its altitude and is not
usually affected by nearby vegetation as the site is surrounded be lava
flows.

The Mauna Loa data is frequently used because of the amount *and*
quality of the data collected. Specifically, this dataset contains
accurate and robust measurements of the number of $CO_2$ molecules per
million in a cubic meter of *dry* air. The term **concentration** may be
used for familiarity but it should be stated that this is not the
preferred term as the concentration of $CO_2$ may be affected be a
number of factors unrelated to how much $CO_2$ is actually in the
world’s atmosphere at a given moment.

This site measures the concentration of $CO_2$ by funnelling air through
a cold chamber (to eliminate the effect of humidity) and then measuring
how much infrared radiation is absorbed by the $CO_2$ in the chamber.
Because $CO_2$ naturally absorbs infrared radiation, a higher density of
$CO_2$ molecules will absorb more radiation. The researchers at the
Mauna Loa site take great care to continually calibrate their equipment
multiple times a day. In addition, the researchers are careful to
account for any outside factors that may effect measurements such as the
diurnal wind flow patterns present on Mauna Loa. Altogether, we can be
confident that the data recorded at Mauna Loa is representative of
global $CO_2$ concentrations.

#### Data Introduction

TODO: Fill this out with the data EDA

### Polynomial Time Trend Models

``` r
co2_ts <- as_tsibble(co2)
data.1997 <- data.frame(index = 1:nrow(co2_ts), month = factor(month(co2_ts$index)), value = co2_ts$value)
```

``` r
model.linear <- lm(value ~ index, data = data.1997)
checkresiduals(model.linear)
```

![](report_files/figure-gfm/linear%20model-1.png)<!-- -->

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 10
    ## 
    ## data:  Residuals
    ## LM test = 452.64, df = 10, p-value < 2.2e-16

While the residuals do appear to follow a normal distribution, it is
clear that a purely linear model does a poor job at modeling the
seasonality of the data. There is also still clearly a trend in the
remaining residuals which a linear model fails to capture. Overall, the
a linear model does capture some of the trend but would not be
sufficient to eliminate it entirely.

``` r
model.quadratic <- lm(value ~ I(index^2), data = data.1997)
checkresiduals(model.quadratic)
```

![](report_files/figure-gfm/quadratic%20model-1.png)<!-- -->

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 10
    ## 
    ## data:  Residuals
    ## LM test = 454.87, df = 10, p-value < 2.2e-16

A quadratic model does not seem to fare much better than a linear model.
In addition to not fully capturing the trend and the seasonality as the
linear model did, the quadratic model’s residuals also appear less
normally distributed.

There is not much evidence to support that a logarithmic transformation
is necessary. This is supported by the fact that the seasonality factor
is not multiplicative and by the fact that the overall trend does not
appear to be exponential.

``` r
model.polynomial <- lm(value ~ I(index^2) + index + month, data = data.1997)
summary(model.polynomial)
```

    ## 
    ## Call:
    ## lm(formula = value ~ I(index^2) + index + month, data = data.1997)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.99478 -0.54468 -0.06017  0.47265  1.95480 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)  3.147e+02  1.494e-01 2105.894  < 2e-16 ***
    ## I(index^2)   8.865e-05  2.050e-06   43.242  < 2e-16 ***
    ## index        6.763e-02  9.929e-04   68.114  < 2e-16 ***
    ## month2       6.642e-01  1.640e-01    4.051 5.99e-05 ***
    ## month3       1.407e+00  1.640e-01    8.582  < 2e-16 ***
    ## month4       2.538e+00  1.640e-01   15.480  < 2e-16 ***
    ## month5       3.017e+00  1.640e-01   18.400  < 2e-16 ***
    ## month6       2.354e+00  1.640e-01   14.357  < 2e-16 ***
    ## month7       8.331e-01  1.640e-01    5.081 5.50e-07 ***
    ## month8      -1.235e+00  1.640e-01   -7.531 2.75e-13 ***
    ## month9      -3.059e+00  1.640e-01  -18.659  < 2e-16 ***
    ## month10     -3.243e+00  1.640e-01  -19.777  < 2e-16 ***
    ## month11     -2.054e+00  1.640e-01  -12.526  < 2e-16 ***
    ## month12     -9.374e-01  1.640e-01   -5.717 1.97e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.724 on 454 degrees of freedom
    ## Multiple R-squared:  0.9977, Adjusted R-squared:  0.9977 
    ## F-statistic: 1.531e+04 on 13 and 454 DF,  p-value: < 2.2e-16

``` r
checkresiduals(model.polynomial)
```

![](report_files/figure-gfm/polynomial%20model-1.png)<!-- -->

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 17
    ## 
    ## data:  Residuals
    ## LM test = 398.58, df = 17, p-value < 2.2e-16

While the use of monthly dummy variables does not entirely capture the
seasonality aspect of the data it is a marked improvement over the
linear and quadratic models. And while the residuals do not appear to be
a white noise process they do appear much more detrended than the other
two models with a fairly normal distribution.

``` r
data.1997.max_month <- max(data.1997$index)
data.future.max_month <- data.1997.max_month + (12 * (2020 - 1997))
data.future <- data.frame(index = (data.1997.max_month + 1):data.future.max_month, month = factor(rep(1:12)))
model.polynomial.forecast <- predict(model.polynomial, data.future, interval = "prediction", level = 0.95)
model.polynomial.predicted <- cbind(data.future, model.polynomial.forecast)

ggplot() +
  geom_line(aes(x = index, y = value), color = 'black', data = data.1997) +
  geom_ribbon(aes(x = index, ymin = lwr, ymax = upr), color = 'blue', alpha = 0.1, data = model.polynomial.predicted) +
  geom_line(aes(x = index, y = fit), color = 'blue', data = model.polynomial.predicted) +
  labs(
    title = TeX(r'(Monthly Mean $CO_2$ with Forecasted Values)'),
    subtitle = 'Forecasted levels (with 95% CI) in blue',
    x = 'Months since Jan 1959',
    y = TeX(r'($CO_2$ parts per million)')
  )
```

![](report_files/figure-gfm/forecasting%20using%20the%20polynomial%20model-1.png)<!-- -->

Visually the model appears to do a fairly decent job. While the 95%
confidence interval does appear somewhat small for a forecast so far
into the future, the predicted values do seem to follow the pattern of
the historical data reliably.

### ARIMA Model

``` r
# Perform the Phillips Perron test
pp.test(co2, alternative = "stationary")
```

    ## 
    ##  Phillips-Perron Unit Root Test
    ## 
    ## data:  co2
    ## Dickey-Fuller Z(alpha) = -92.68, Truncation lag parameter = 5, p-value
    ## = 0.01
    ## alternative hypothesis: stationary

``` r
model.arima <- co2 %>% auto.arima(ic="aicc", seasonal = FALSE)
model.arima
```

    ## Series: . 
    ## ARIMA(2,1,1) with drift 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1   drift
    ##       1.5519  -0.8550  -0.9272  0.1055
    ## s.e.  0.0239   0.0237   0.0129  0.0070
    ## 
    ## sigma^2 = 0.3808:  log likelihood = -436.73
    ## AIC=883.47   AICc=883.6   BIC=904.2

``` r
checkresiduals(model.arima)
```

![](report_files/figure-gfm/Create%20ARIMA%20model-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(2,1,1) with drift
    ## Q* = 660.32, df = 21, p-value < 2.2e-16
    ## 
    ## Model df: 3.   Total lags used: 24

The model chosen was the model with the lowest AICc found by the
Hyndman-Khandakar algorithm. The AICc score was chosen because it is
generally accepted by the community to be a strong indicator of quality
models. As the Hyndman-Khandakar algorithm iterates over many models, we
can be confident that the model chosen is the best given the
limitations.

This non-seasonal ARIMA model does an extremely good job of capturing
the stochasticity of the time series, but obviously, does not capture
the seasonality as intended. The residuals appear to be a white noise
process with a *mostly* gaussian distribution. The ACF plot clearly
shows the results of not capturing the seasonality. Interestingly, the
Phillips-Peron test indicates that the data are already are stationary;
however, the evidence shows one level of differencing leads to a better
model.

``` r
model.arima.predicted <- forecast(model.arima, h = 12 * 25, level = c(95))
model.arima.predicted %>%
  autoplot() +
  labs(
    title = TeX(r'(Monthly Mean $CO_2$ with Forecasted Values)'),
    subtitle = 'Forecasted levels (with 95% CI) in blue',
    x = 'Month',
    y = TeX(r'($CO_2$ parts per million)')
  )
```

![](report_files/figure-gfm/forecasting%20using%20the%20ARIMA%20model-1.png)<!-- -->

The most obvious aspect of the ARIMA predictions is the flattening of
the predictions after a few years. This is to be expected with long
forecasts using ARIMA models and could be mitigated using seasonal
parameters. Because of this limitation, it would appear that the
polynomial model had stronger forecasting. It is also noteworthy that,
because of the quadratic parameter in the polynomial model, the
polynomial forecasts also predict a much faster rise in $CO_2$ levels
than the ARIMA forecasts with expect the increase in $CO_2$ levels to
remain steady.

### Forecasting CO2 Growth

``` r
# Return the date that the given series first hit x num
first.date <- function(series, num) {
  return(dates[which(series > num)[1]])
}
forecast.1997 <- forecast(model.arima, h = 12 * 117, level = c(95))
dates <- rownames(data.frame(forecast.1997))

# When CO2 is expected to hit 420ppm
prediction.420.lower <- first.date(forecast.1997$lower, 420)
prediction.420.expected <- first.date(forecast.1997$mean, 420)
prediction.420.upper <- first.date(forecast.1997$upper, 420)

# When CO2 is expected to hit 500ppm
prediction.500.lower <- first.date(forecast.1997$lower, 500)
prediction.500.expected <- first.date(forecast.1997$mean, 500)
prediction.500.upper <- first.date(forecast.1997$upper, 500)

# Expected CO2 levels in the year 2100
prediction.2100.index <- match("Jan 2100", dates)
prediction.2100.lower <- forecast.1997$lower[prediction.2100.index]
prediction.2100.expected <- forecast.1997$mean[prediction.2100.index]
prediction.2100.upper <- forecast.1997$upper[prediction.2100.index]
```

#### When CO2 is expected to hit 420ppm and 500ppm

| PPM | Best Case Scenario | Expected | Worst Case Scenario |
|-----|--------------------|----------|---------------------|
| 420 | Jul 2048           | Dec 2041 | Jan 2036            |
| 500 | Jun 2114           | Mar 2105 | Aug 2096            |

#### Expected CO2 levels in the year 2100

| Best Case Scenario | Expected | Worst Case Scenario |
|--------------------|----------|---------------------|
| 482.48             | 493.51   | 504.53              |

#### Discussion

These predictions are based upon the ARIMA model because the confidence
band appeared more realistic than the confidence interval created by the
polynomial model. The ARIMA model not using seasonal parameter would
affect the results but because of the small magnitude of the seasonal
variation it would likely not affect results by very much. Rising $CO_2$
levels would be primarily due to the trend and not the seasonality of
the time series which would be capturing relatively similarly in both an
ARIMA and SARIMA model by the drift parameter.

Note: The best and worst case scenarios are based upon the 95%
confidence interval.

## Part 2

### Introduction

In follow-up to our 1997 report we wish to continue investigating the
trend of rising $CO_2$ levels and whether or not it is likely caused by
a larger trend or stochastic effects.

In April of 2019 the Mauna Loa laboratory updated their equipment to
measure $CO_2$ with a new technique called Cavity Ring-Down Spectroscopy
(CRDS) in contrast to the prior infrared absorption technique. As such,
all data from April 2019 onwards will contain measurements using the new
method. Additionally, due to eruptions at the Mauna Loa site in 2022,
data from December 2022 onwards are from a site at the Maunakea
laboratory.

### Data Update

``` r
co2_url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_weekly_mlo.csv"
co2_month_url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.csv"

co2_present <- read.csv(co2_url, comment.char = "#") %>%
  mutate(index = yearweek(lubridate::make_datetime(year, month, day))) %>%
  mutate(average = ifelse(average == -999.99, NA, average)) %>%
  fill(average) %>%
  tsibble(index = index)
co2_present_month <- read.csv(co2_month_url, comment.char = "#") %>%
  mutate(index = yearmonth(lubridate::make_datetime(year, month))) %>%
  tsibble(index = index)
```

### Model Comparisons

#### Polynomial

``` r
# Generate newdata to predict on
data.1997.max_month <- max(data.1997$index)
data.future.max_month <- data.1997.max_month + (12 * (2023 - 1997))
data.future <- data.frame(index = (data.1997.max_month + 1):data.future.max_month)

# Create forecasts
model.linear.forecast <- predict(model.linear, data.future, interval = "prediction", level = 0.99)
model.linear.predicted <- cbind(data.future, model.linear.forecast) %>%
  mutate(value = fit, index = yearmonth(seq(as.Date("1998/1/1"), as.Date("2023/12/1"), by = "month"))) %>%
  select(value, index) %>%
  head(-6) # remove the last 6 months


ggplot() +
  geom_line(aes(x = index, y = average), color = 'black', data = co2_present_month) +
  geom_line(aes(x = index, y = value), color = 'steelblue', data = model.linear.predicted) +
  labs(
    title = TeX(r'(Monthly Mean $CO_2$ with Forecasted Values)'),
    subtitle = 'Forecasted levels in blue',
    x = 'Month',
    y = TeX(r'($CO_2$ parts per million)')
  )
```

![](report_files/figure-gfm/compare%20polynomial%20model%20forecast-1.png)<!-- -->

#### ARIMA

``` r
model.arima.predicted <- data.frame(forecast(model.arima, h = (12 * 25) + 6, level = c(0)))
model.arima.predicted <- data.frame(
  index = yearmonth(rownames(model.arima.predicted)),
  average = model.arima.predicted$Point.Forecast
)

ggplot() +
  geom_line(aes(x = index, y = average), color = 'steelblue', data = model.arima.predicted) +
  geom_line(aes(x = index, y = average), color = 'black', data = co2_present_month) +
  labs(
    title = TeX(r'(Monthly Mean $CO_2$ with Forecasted Values)'),
    subtitle = 'Forecasted levels in blue',
    x = 'Month',
    y = TeX(r'($CO_2$ parts per million)')
  )
```

![](report_files/figure-gfm/compare%20ARIMA%20model%20forecast-1.png)<!-- -->

#### Evaluation

TBD once someone knows the correct tests

#### Discussion

The ARIMA and linear models differ for around the first 2-3 years at
which point the ARIMA model converges to a line resembling that of the
linear model. The ARIMA model clearly matches the Keeling curve better
than the linear model up until that point; at which point, they perform
similarly.

Both models do not capture the growing rate emissions which is a
critical part of the trend. The polynomial model fared much better as it
did consider that the amount of $CO_2$ being put into the air annually
is growing year over year. Because of this, if one were to plot the
forecast of the polynomial model they would find that it is much more
accurate than the linear or the ARIMA model.

The predictions from the previous report indicated that $CO_2$ would
cross 420ppm for the first time in Dec 2041. Whereas in reality, $CO_2$
levels crossed 420ppm for the first time in April 2022. As discussed,
the reason for this error in forecasting is primarily due to the lack of
a quadratic term and seasonal terms.

### Training on Modern Data

#### Training

``` r
co2_present.seasonal <- ts(co2_present$average, freq=52, start=decimal_date(ymd("1974-05-19")))

co2_present.nonseasonal <- seasadj(stl(co2_present.seasonal, "period"))

# Just to show that it's deseasonalized
co2_present.seasonal %>% autoplot()
```

![](report_files/figure-gfm/deseasonalize%20the%20weekly%20data-1.png)<!-- -->

``` r
co2_present.nonseasonal %>% autoplot()
```

![](report_files/figure-gfm/deseasonalize%20the%20weekly%20data-2.png)<!-- -->

``` r
test_length <- 104L # Two years
co2_present.seasonal.train <- head(co2_present.seasonal, length(co2_present.seasonal) - test_length)
co2_present.seasonal.test <- tail(co2_present.seasonal, test_length + 1)
co2_present.nonseasonal.train <- head(co2_present.nonseasonal, length(co2_present.nonseasonal) - test_length)
co2_present.nonseasonal.test <- tail(co2_present.nonseasonal, test_length + 1)
```

``` r
model.seasonal <- co2_present.seasonal.train %>% auto.arima(ic="aicc", seasonal=FALSE)
model.nonseasonal <- co2_present.nonseasonal.train %>% auto.arima(ic="aicc", seasonal=FALSE)

checkresiduals(model.seasonal)
```

![](report_files/figure-gfm/train%20ARIMA%20models%20on%20SA%20and%20NSA%20models-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(5,1,3) with drift
    ## Q* = 592.77, df = 96, p-value < 2.2e-16
    ## 
    ## Model df: 8.   Total lags used: 104

``` r
checkresiduals(model.nonseasonal)
```

![](report_files/figure-gfm/train%20ARIMA%20models%20on%20SA%20and%20NSA%20models-2.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(2,1,2) with drift
    ## Q* = 560.46, df = 100, p-value < 2.2e-16
    ## 
    ## Model df: 4.   Total lags used: 104

``` r
model.seasonal.forecast <- model.seasonal %>% forecast()
model.seasonal.forecast %>% autoplot() +
  autolayer(co2_present.seasonal.test, color = "black") +
  xlim(decimal_date(ymd("2020-01-01")), decimal_date(ymd("2023-08-01"))) +
  ylim(405, 430) +
  labs(
    title = TeX(r'(Weekly Mean $CO_2$ with Forecasted Values)'),
    subtitle = 'Non-Seasonally Adjusted Data',
    x = 'Month',
    y = TeX(r'($CO_2$ parts per million)')
  )
```

    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.

    ## Warning: Removed 2373 rows containing missing values (`geom_line()`).

    ## Warning: Removed 6 rows containing missing values (`geom_line()`).

![](report_files/figure-gfm/compare%20SA%20model%20to%20realized%20data-1.png)<!-- -->

``` r
model.nonseasonal.forecast <- model.nonseasonal %>% forecast()
model.nonseasonal.forecast %>% autoplot() +
  autolayer(co2_present.nonseasonal.test, color = "black") +
  xlim(decimal_date(ymd("2020-01-01")), decimal_date(ymd("2023-08-01"))) +
  ylim(405, 430) +
  labs(
    title = TeX(r'(Weekly Mean $CO_2$ with Forecasted Values)'),
    subtitle = 'Seasonally Adjusted Data',
    x = 'Month',
    y = TeX(r'($CO_2$ parts per million)')
  )
```

    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.

    ## Warning: Removed 2373 rows containing missing values (`geom_line()`).

    ## Warning: Removed 6 rows containing missing values (`geom_line()`).

![](report_files/figure-gfm/compare%20NSA%20model%20to%20realized%20data-1.png)<!-- -->

``` r
data.present.train <- data.frame(
  index = 1:length(co2_present.nonseasonal.train),
  value = co2_present.nonseasonal.train
)
data.present.test <- data.frame(
  index = (length(co2_present.nonseasonal.train) + 1):(length(co2_present.nonseasonal.train) + length(co2_present.nonseasonal.test)),
  value = co2_present.nonseasonal.test
)

model.polynomial.present <- lm(value ~ I(index^2) + index, data = data.present.train)
checkresiduals(model.polynomial.present)
```

![](report_files/figure-gfm/train%20polynomial%20model-1.png)<!-- -->

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 10
    ## 
    ## data:  Residuals
    ## LM test = 2032.6, df = 10, p-value < 2.2e-16

``` r
model.polynomial.present.forecast <- predict(model.polynomial.present, data.present.test,
                                             interval = "prediction", level = 0.95)
model.polynomial.present.predicted <- cbind(data.present.test, model.polynomial.present.forecast)

ggplot() +
  geom_line(aes(x = index, y = value), color = 'black', data = data.present.train) +
  geom_line(aes(x = index, y = value), color = 'black', data = data.present.test) +
  geom_ribbon(aes(x = index, ymin = lwr, ymax = upr),
              alpha = 0.5, data = model.polynomial.present.predicted) +
  geom_line(aes(x = index, y = fit), color = 'steelblue', data = model.polynomial.present.predicted) +
  xlim(2200, 2565) +
  ylim(405, 430) +
  labs(
    title = TeX(r'(Monthly Mean $CO_2$ with Forecasted Values)'),
    subtitle = 'Forecasted levels (with 95% CI) in blue',
    x = 'Weeks since May 5th, 1974',
    y = TeX(r'($CO_2$ parts per million)')
  )
```

    ## Warning: Removed 2199 rows containing missing values (`geom_line()`).

    ## Warning: Removed 1 row containing missing values (`geom_line()`).
    ## Removed 1 row containing missing values (`geom_line()`).

![](report_files/figure-gfm/forecasting%20using%20the%20current%20polynomial%20model-1.png)<!-- -->
\#### Discussion

TODO: Leaving for now because the deseasonalization went poorly

### Forecasting Future CO2 Growth

``` r
# Return the date that the given series first hit x num
first.date <- function(series, num) {
  return(dates[which(series > num)[1]])
}
forecast.2022 <- forecast(model.seasonal, h = 52 * 150, level = c(95))
dates <- rownames(data.frame(forecast.2022))

# When CO2 is expected to hit 420ppm
prediction_present.420.lower <- first.date(forecast.2022$lower, 420)
prediction_present.420.expected <- first.date(forecast.2022$mean, 420)
prediction_present.420.upper <- first.date(forecast.2022$upper, 420)

# When CO2 is expected to hit 500ppm
prediction_present.500.lower <- first.date(forecast.2022$lower, 500)
prediction_present.500.expected <- first.date(forecast.2022$mean, 500)
prediction_present.500.upper <- first.date(forecast.2022$upper, 500)

# Expected CO2 levels in the year 2122 (first week)
prediction_present.2122.index <- match("2122.013", dates)
prediction_present.2122.lower <- forecast.2022$lower[prediction_present.2122.index]
prediction_present.2122.expected <- forecast.2022$mean[prediction_present.2122.index]
prediction_present.2122.upper <- forecast.2022$upper[prediction_present.2122.index]

prediction_present.500.lower
```

    ## [1] "2145.417"

#### When CO2 is expected to hit 420ppm and 500ppm

| PPM | Best Case Scenario | Expected | Worst Case Scenario |
|-----|--------------------|----------|---------------------|
| 420 | 2072.013           | 2023.917 | 2021.897            |
| 500 | 2145.417           | 2070.013 | 2040.551            |

#### Expected CO2 levels in the year 2122

| Best Case Scenario | Expected | Worst Case Scenario |
|--------------------|----------|---------------------|
| 472.42             | 590.27   | 708.12              |

#### Discussion

Clearly, the best case scenario is far more optimistic than we could
realistically expect. In fact, the confidence intervals for the ARIMA
estimates are much wider than we could hope for leading to us not being
confident in these predictions. Much of this is due to the seasonality
and nonstationarity of the data which both violate the assumptions of
the ARIMA model.

Even still, the 420ppm estimate was not too far off with the true date
of $CO_2$ concentrations reaching 420ppm being between the expected and
worse case scenarios. With this in mind, it would not be too surprising
if the true date that $CO_2$ levels reach 500ppm was near the expected
estimate if current trends continue.
