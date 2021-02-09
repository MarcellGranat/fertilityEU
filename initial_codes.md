fertilityEU
================
Marcell Granat
2021 02 08

``` r
plot_NUTS2 <- function(df, viridis_c = T, ...) {
p <- df %>% 
  {merge(eurostat::get_eurostat_geospatial(nuts_level = 2), .)} %>% 
  ggplot(aes(fill = values)) +
  geom_sf(color = "black") +
  theme(
    text  = element_text(family = "Impact"),
    axis.text = element_blank()   
    
  ) +
  xlim(c(-30, 44)) +
  ylim(c(35, 75))

if (viridis_c) {
  p <- p + scale_fill_viridis_c(option = "magma", ...,
                                guide = guide_colorbar(frame.colour = "black", 
                                                       ticks.colour = "black"), 
                                na.value = "white")
}
p
}
```

``` r
dat <- search_eurostat("NUTS 2") %>% 
  select(1, 2)
```

``` r
dat %>% 
  filter(str_detect(title, "birth")) %>% 
  .[1:3,] %>% 
  apply(1, FUN = function(x) {
    title = x[1]; code = x[2]
    get_eurostat(code) %>% 
      mutate(code = code, title = title)
  })
```

``` r
search_eurostat("NUTS 2") %>% 
  filter(str_detect(title, "Fert"))
```

    ## # A tibble: 4 x 8
    ##   title code  type  `last update of~ `last table str~ `data start` `data end`
    ##   <chr> <chr> <chr> <chr>            <chr>            <chr>        <chr>     
    ## 1 Fert~ demo~ data~ 09.10.2020       08.02.2021       1990         2018      
    ## 2 Fert~ demo~ data~ 09.10.2020       08.02.2021       1990         2018      
    ## 3 Fert~ demo~ data~ 09.10.2020       08.02.2021       1990         2018      
    ## 4 Fert~ demo~ data~ 09.10.2020       08.02.2021       1990         2018      
    ## # ... with 1 more variable: values <chr>

``` r
fertility_byage <- get_eurostat("demo_r_frate2", time_format = "num")
```

``` r
fertility_byage <- fertility_byage %>% 
  filter(age != "TOTAL") %>% 
  transmute(age = age, geo = geo, time, values)
```

``` r
fr_agegroups_wide <- fertility_byage %>% 
  mutate(age = str_remove(age, "Y"),
         age = str_remove(age, "10-"),
         age = str_remove(age, "_GE"),
         age = as.numeric(age),
         age_group = paste0((age %/% 5)*5, "-", (age %/% 5 + 1)*5 - 1),
         age_group = ifelse(age_group == "50-54", "50-", age_group)
         ) %>% 
  group_by(age_group, time, geo) %>% 
  summarise(values = sum(values)) %>% 
  ungroup() %>%
  pivot_wider(names_from = "age_group", values_from = "values") %>% 
  select(-"10-14", -"50-") %>% 
  na.omit() %>% 
  arrange(time, geo)
```

``` r
fr_age_comps <- fr_agegroups_wide %>% 
  select(-time, -geo) %>% 
  na.omit() %>% 
  {princomp(scale(.))}

fr_age_comps %>% 
  .$loadings %>% 
  unclass() %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-1) %>% 
  mutate(
    name = str_remove(name, 'Comp.'),
    name = factor(str_c('# ', name), levels = str_c('# ', 1:n_distinct(name)))
  ) %>% 
  ggplot +
  aes(rowname, value, fill = value < 0) +
  geom_hline(yintercept = 0) +
  geom_col(color = 'black') +
  coord_flip() +
  scale_fill_viridis_d(guide = F, option = "magma", begin = .4, end= .7, direction = -1) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~name, ncol = 3) +
  labs(x = NULL, y = NULL, title = 'Főkomponensek')
```

![](initial_codes_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
fr_age_comps %>% 
  .$score %>% 
  .[,1] %>% 
  {cbind(values =., na.omit(fr_agegroups_wide))} %>% 
  select(1:3) %>% 
  pivot_wider(names_from = geo, values_from = values) %>% 
  arrange(time) %>% 
  {apply(.[, -1], 2, FUN = function(x) {x <- na.omit(x); (tail(x, 1) - head(x, 1))/length(x)})} %>% 
  {tibble(FID = names(.), values = .)} %>% 
  plot_NUTS2() +
  labs(fill = "1. főkomponensben lezajló változás")
```

![](initial_codes_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
fr_age_comps %>% 
  .$score %>% 
  .[,1] %>% 
  {cbind(values =., na.omit(fr_agegroups_wide))} %>% 
  select(1:3) %>% 
  filter(time == 2017) %>% 
  plot_NUTS2() +
  labs(fill = "1. főkomponens értékei (2017)")
```

![](initial_codes_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->