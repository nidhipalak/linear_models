Cross Validation
================
Nidhi Patel
12/9/2020

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.

``` r
library(p8105.datasets)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 6,
  out.width = "90%")

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Simulate data

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )
```

Look at data

``` r
nonlin_df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()
```

<img src="cross_vaild_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

## Cross Validation by hand

Get training and testing datasets

``` r
train_df = sample_n(nonlin_df, size = 80)
test_df = anti_join(nonlin_df, train_df, by = "id") #anti_join takes the remaining data, according to the ID and makes the dataset with it
```

Fit three models

``` r
lin_mod = lm(y ~ x, data = train_df)
smooth_mod = gam(y ~ s(x), data = train_df)
#gam is general additive model coming out of the mgcv package. saying y is a smooth fn of x
wiggly_mod = gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
# k tells us hoe specific we want that model to be. we are setting parameters (sp) to something VERY low to make it not smooth at all
```

Can I see what we just did

``` r
train_df %>% 
  add_predictions(wiggly_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_vaild_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

Wiggly chases too much. Smooth model does do best in terms of prediction
accuracy.

Can do multiple predictions at the same time:

``` r
train_df %>% 
  gather_predictions(lin_mod, smooth_mod, wiggly_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red") +
  facet_grid(. ~ model)
```

<img src="cross_vaild_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

Look at prediction accuracy. Look at RMSE at testing data set.

``` r
rmse(lin_mod, test_df)
```

    ## [1] 0.8500625

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.3651755

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.4171986

Linear: very high, does not fit model at all. Wiggly: still high, not as
bad. Smooth: the best choice. Lowest RMSE.

## Cross Validation using `modelr`

``` r
cv_df = 
  crossv_mc(nonlin_df, 100) # we want 100 cross validations
```

What is happening here? This makes a list of 100 cross validations. We
can pull the test and training df from each and make it a tibble

``` r
cv_df %>% pull(train) %>% .[[1]] %>% as.tibble()
```

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## # A tibble: 79 x 3
    ##       id      x       y
    ##    <int>  <dbl>   <dbl>
    ##  1     1 0.612  -0.0652
    ##  2     2 0.495   0.439 
    ##  3     6 0.382   1.69  
    ##  4     8 0.0458  0.0638
    ##  5     9 0.0328  0.127 
    ##  6    10 0.155   0.852 
    ##  7    12 0.149   0.700 
    ##  8    13 0.896  -3.02  
    ##  9    14 0.657  -0.234 
    ## 10    15 0.593   0.276 
    ## # … with 69 more rows

``` r
cv_df %>% pull(test) %>% .[[1]] %>% as.tibble()
```

    ## # A tibble: 21 x 3
    ##       id     x       y
    ##    <int> <dbl>   <dbl>
    ##  1     3 0.548  0.552 
    ##  2     4 0.242  0.976 
    ##  3     5 0.891 -2.64  
    ##  4     7 0.862 -1.55  
    ##  5    11 0.606  0.0177
    ##  6    16 0.305  1.40  
    ##  7    18 0.537  0.527 
    ##  8    30 0.809 -1.72  
    ##  9    46 0.491  0.802 
    ## 10    47 0.560  0.566 
    ## # … with 11 more rows

We will convert all training and testing data sets and convert to
tibble.

``` r
cv_df = cv_df %>% 
mutate(train = map(train, as_tibble),
       test = map(test, as_tibble))
```

Let’s try to fit models and get RMSEs for them

``` r
cv_df = cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(y ~ x, data = .x)),
    smooth_mod = map(.x = train, ~gam(y ~ s(x), data = .x)),
    wiggly_mod = map(.x = train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))) %>% 
# i not have lists containing model info for each of the cross validations. USE map2 for TWO vars going into the map
  mutate(
    rmse_linear = map2(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_wiggly = map2(.x = wiggly_mod, .y = test, ~rmse(model = .x, data = .y))
  ) %>% 
   unnest(rmse_linear, rmse_smooth, rmse_wiggly)
```

    ## Warning: unnest() has a new interface. See ?unnest for details.
    ## Try `df %>% unnest(c(rmse_linear, rmse_smooth, rmse_wiggly))`, with `mutate()` if needed

``` r
  #pull(rmse_linear) # this is shows all the rsme for the linear models we are cross validating.
```

What doe these results dae about the model choices?

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_violin()
```

<img src="cross_vaild_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />
Among the three models we’re using, the smooth model is doing the best.

Comput averages:

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  group_by(model) %>% 
  summarize(avg_rmse = mean(rmse))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 3 x 2
    ##   model  avg_rmse
    ##   <chr>     <dbl>
    ## 1 linear    0.825
    ## 2 smooth    0.332
    ## 3 wiggly    0.419

## Try a real dataset

import data

``` r
child_growth = read_csv("./data/nepalese_children.csv") %>% 
  # do a change point model with this:
  mutate(
    weight_cp = (weight > 7) * (weight - 7)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   age = col_double(),
    ##   sex = col_double(),
    ##   weight = col_double(),
    ##   height = col_double(),
    ##   armc = col_double()
    ## )

weight vs arm circumference

``` r
child_growth %>% 
  ggplot(aes(x = weight, y = armc)) +
  geom_point(alpha = 0.3)
```

<img src="cross_vaild_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" />

We will do two models: 1. Straight linear model 2. linear model with a
change point, where the slope changes after some point. PEICE WISE
LINEAR MODEL\!\!\!\!

``` r
linear_mod = lm(armc ~ weight, data = child_growth)
pwlin_mod = lm(armc ~ weight + weight_cp, data = child_growth)
smooth_mod = gam(armc ~ s(weight), data = child_growth)
```

Get child df, gather predicitons and make a plot

``` r
child_growth %>% 
  gather_predictions(linear_mod, pwlin_mod, smooth_mod) %>% 
  ggplot(aes(x = weight, y = armc)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred), color = "red") +
  facet_grid(. ~ model)
```

<img src="cross_vaild_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" />

Now we want to cross validate again. Try to understand model fit using
cross validation

``` r
cv_df = 
  crossv_mc(child_growth, 100) %>% 
  mutate(train = map(train, as_tibble),
       test = map(test, as_tibble)) # this is necessary bc we are fitting the gam model. Would not have to do this mutate step othewise. 
```

``` r
cv_df = cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(armc ~ weight, data = .x)),
    pwlin_mod = map(.x = train, ~lm(armc ~ weight + weight_cp, data = .x)),
    smooth_mod = map(.x = train, ~gam(armc ~ s(weight), data = .x))) %>% 
  mutate(
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_pwlin = map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))
  )
```

Plot it

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_violin()
```

<img src="cross_vaild_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" />

Which to pick? Jeff likes piece wise bc the interpretation is much more
clear than in smooth. Smooth model is more correct, but prob harder to
interpret.
