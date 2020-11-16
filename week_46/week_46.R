## _____________________________
##
## Script name: 
##
## Purpose of script:
##
## Author: Vinay Singh
##
## Date Created: 
## Email: vinaytweets@gmail.com
##
## _____________________________
##
## Notes: 
##
## _____________________________

## Loading Libraries

require(tidyverse)
require(lubridate)
require(tidytuesdayR)
require(gganimate)
require(gifski)
require(scales)
require(ggrepel)

## _____________________________

## preparing data ----
setwd("~/Dropbox/03 R/tidyTuesday")

tuesday <- tidytuesdayR::tt_load(2020, week = 46)

mobile <- tuesday$mobile
landline <- tuesday$landline

mobile <- mobile %>% 
  mutate(year = as.integer(year))


# label certain countries

countries <- c("Australia", "New Zealand", "Fiji",
               "Germany", "United Kingdon", "France",
               "India", "China", "United Arab Emirate", "Qatar",
               "United States", "Canda", "Mexico", "Brazil",
               "Libya", "Kenya", "Nigeria", "Egypt")


y_limits <- c(NA, 1.5e+02)
x_limits <- c(80000, NA)
 
p <- mobile %>%
  drop_na() %>%
  filter(year <=  2010) %>%
  ggplot(aes(gdp_per_cap, mobile_subs, color= continent)) + 
  geom_point(aes(size = total_pop), alpha = 1/2) + 
  facet_wrap(~continent) +
  theme_minimal(base_size = 15) + 
  labs(x = "GDP per capita (constant 2011 international $)",
       y = "Mobile Subscriptions (per 100 people)") + #
  scale_x_continuous(labels = scales::comma) + 
  scale_size(range  = c(1, 12)) +
  geom_label_repel(data = mobile %>%
                     filter(year <= 2010) %>%
                     filter(entity %in% countries),
                     aes(label = entity),
                   segment.size = 0.2,
                   segment.color = "grey50",
                   color     = "black",
                   ylim      = y_limits,
                   xlim      = x_limits) +
  theme(strip.background   = element_rect(fill = "grey90", color = NA),
        strip.text         = element_text(size = 12, color = "indianred", 
                                          face = "bold"),
        axis.title         = element_text(size = 15, face = "bold"),
        panel.grid.minor   = element_blank(),
        panel.background   = element_rect(fill = "grey95", color = NA),
        plot.background    = element_rect(fill = "grey95", color = "black"),
        plot.caption       = element_text(color = "grey50"),
        plot.title         = element_text(size = 20, face = "bold")) +
  guides(colour     = FALSE,
         size       = FALSE)


  #transition_states(year) +
p + transition_time(year) +
  labs(title = "Year: {frame_time}")

  ease_aes("linear") 
  enter_fade() + 
  exit_fade()


  


