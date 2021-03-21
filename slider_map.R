library(tidyverse)

dat <- rbind(read_csv("C:/school/szem_8/TDK-fertility/fertilityEU/eurostat_datasets/demo_r_frate2_1_Data.csv", locale = readr::locale(encoding = "UTF-8"))%>% 
               transmute(time = TIME, geo = GEO, variable = "Fertility", value = Value),
             read_csv("C:/school/szem_8/TDK-fertility/fertilityEU/eurostat_datasets/edat_lfse_16_1_Data.csv", locale = readr::locale(encoding = "UTF-8")) %>% 
               filter(SEX == "Total") %>% 
               transmute(time = TIME, geo = GEO, 
                         variable = "Early leavers from education and training (%)", value = Value)) %>% 
  filter(value != ":") %>% 
  mutate(
    value = as.numeric(value)
  )

eu_map <- eurostat::get_eurostat_geospatial(nuts_level = 2)

used_dat <- dat %>% 
  filter(variable == 'Fertility') %>% 
  {
    merge(expand.grid(time = unique(.$time), geo = unique(.$geo)), ., all.x = T)
  }

plotly::ggplotly(
used_dat %>% 
  {merge(eu_map, ., all.x = T)} %>% 
  ggplot(aes(fill = as.numeric(value), frame = time)) +
  geom_sf(color = "black", size = .1) +
  scale_fill_viridis_c(na.value = "white") +
  theme_minimal() +
  xlim(c(-30, 44)) +
  ylim(c(35, 75)) +
  labs(
    fill = 'Fertility'
  )
)
