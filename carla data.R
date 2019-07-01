library(tidyverse)
library(lubridate)
data=read_csv("carla_data.csv") %>%
  mutate(date = mdy(date)) %>%
  mutate(yday = yday(date))

bad_sites = c("beringer stanley ranch",
              "domaine chandon mt veeder",
              "gallo",
              "gemstone yountville",
              "mondavi oak knoll",
              "shifflet",
              "sterling calistoga",
              "vyborny_napa",
              "walsh")

'%!in%' <- function(x,y)!('%in%'(x,y))

data_small = data %>%
  select(-pm_index, -ascospore, -severity, -bot_incidence, -bot_severity ) %>% 
  filter(site %!in% bad_sites) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(max_inc = ifelse(incidence == max(incidence), 1, NA)) %>%
  mutate(max_date = ifelse(max_inc == 1, yday, NA)) %>%
  fill(max_date, .direction = "up") %>%
  filter(yday <= max_date)

saveRDS(object = data_small, file = "data/carla_pm_incidence_data.rds")
saveRDS(object = data, file = "data/carla_all_data.rds")

ggplot(data_small, aes(yday, incidence,color=region, group = site))+
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")

ggsave(filename = "figures/incidence_time_yday_small.png", width = 8, height=5 )


  