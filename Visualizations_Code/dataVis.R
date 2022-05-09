library(tidyverse)
require(maps)
library(colorBlindness)

wiid <- read_csv("wiid.csv")

glimpse(wiid)

wiid_select <- wiid %>%
  select(country, c3, c2, year, gini_reported, region_un, region_un_sub, region_wb, eu,
         oecd, incomegroup, mean_usd, median_usd, gdp_ppp_pc_usd2011, population)

glimpse(wiid_select)

wiid_select$incomegroup <- factor(wiid$incomegroup , 
                           levels = c("High income", "Upper middle income", 
                                      "Lower middle income", "Low income"),
                           ordered = TRUE)

colorBlindness::cvdPlot(plot)


## 1st plot: World Map with Most recent Gini Scores by Country
world_map <- map_data("world")

world_map <- world_map %>%
  filter(region != "Antarctica") %>%
  mutate(region = case_when(
    region == "Greenland" ~ "Denmark",
    TRUE ~ region))

wiid_most_recent <- wiid_select %>%
  group_by(country) %>%
  drop_na(gini_reported) %>%
  mutate(most_recent = max(year)) %>%
  filter(year == most_recent) %>%
  mutate(mean_gini = mean(gini_reported)) %>%
  rename(region = country) %>%
  mutate(region = case_when(
    region == "Bahamas, The" ~ "Bahamas",
    region == "Congo, Democratic Republic of the" ~ "Democratic Republic of the Congo",
    region == "Congo, Republic of the" ~ "Republic of Congo",
    region == "Cote d'Ivoire" ~ "Ivory Coast",
    region == "Czechia" ~ "Czech Republic",
    region == "Eswatini" ~ "Swaziland",
    region == "Gambia, The" ~ "Gambia",
    region == "Korea, Republic of" ~ "South Korea",
    region == "Macedonia, former Yugoslav Republic of" ~ "North Macedonia",
    region == "Micronesia, Federated States of" ~ "Micronesia",
    region == "Serbia and Montenegro" ~ "Serbia",
    region == "Taiwan (China)" ~ "Taiwan",
    region == "Trinidad and Tobago" ~ "Trinidad",
    region == "United Kingdom" ~ "UK",
    region == "West Bank and Gaza" ~ "Palestine",
    region == "United States" ~ "USA",
    TRUE ~ region)
  ) 

non_match <- anti_join(wiid_most_recent, world_map, by = "region")

wiid_world_map <- left_join(world_map, wiid_most_recent, by = "region")

wiid_world_map %>%
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = mean_gini, color = ""), color = "white") +
  labs(title = "Most Recent Gini Coefficients by Country",
       fill = "Gini Coefficient") +
  scale_fill_gradientn(colors = c("#32CF16", "#F9F214", "#E20000")) +
  theme_void() +
  guides(fill = guide_colorbar(title.vjust = 0.7)) +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5))


## 2nd Plot: Mean Gini Indices over time by UN regions
wiid_select %>%
  filter(year > 1959,
         year < 2011) %>%
  group_by(year, region_un) %>%
  summarize(mean_gini = mean(gini_reported, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_gini, color = region_un)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(size = 0, span = 0.7, alpha = 0.1) +
  stat_smooth(geom = "line", size = 0.8) +
  labs(title = "Gini Coefficient of UN Regions between 1960 - 2010",
       y = "Gini Coefficient",
       color = "UN Regions") +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank())


## 3rd Plot: Mean Income against Gini Coefficient
wiid_select %>%
  filter(year > 1999,
         !is.na(mean_usd),
         !is.na(gini_reported)) %>%
  ggplot(aes(x = mean_usd, y = gini_reported, color = region_un)) +
  geom_point(size = 1.3, alpha = 0.8) +
  geom_smooth(aes(x = mean_usd, y = gini_reported), inherit.aes = FALSE,
              color = "#CF0E0E", size = 0.8) +
  labs(title = "Mean Income against Gini Coefficient",
       subtitle = "Only Gini Coefficients from after 1999 considered",
       x = "Mean Income (in $)",
       y = "Gini Coefficient",
       size = "Population",
       color = "UN Region") +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")


## 4th Plot: Most recent Gini coeffienct of G20 members and countries with the highest and lowest gini coefficient
top_5_names <- wiid_most_recent %>%
  filter(year > 2010) %>%
  group_by(region) %>%
  summarize(first(region), mean_gini = mean(gini_reported, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(mean_gini)) %>%
  select(region) %>%
  slice(1:5)

bottom_5_names <- wiid_most_recent %>%
  filter(year > 2010) %>%
  group_by(region) %>%
  summarize(first(region), mean_gini = mean(gini_reported, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(mean_gini) %>%
  select(region) %>%
  slice(1:5)

top_5_names <- pull(top_5_names, region)
bottom_5_names <- pull(bottom_5_names, region)
g20 <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany",
"India", "Indonesia", "Italy", "Japan", "South Korea", "Mexico", "Russia",
"Saudi Arabia", "South Africa", "Turkey", "UK", "USA", "EU")

top_bottom_5_oecd <- wiid_most_recent %>%
  filter(year > 2010,
         region %in% top_5_names | region %in% bottom_5_names | region %in% g20) %>%
  mutate(status = case_when(
    region %in% top_5_names & region %in% g20 ~ "G20 Member and Highest 5",
    region %in% bottom_5_names & region %in% g20 ~ "G20 Member and Lowest 5",
    region %in% top_5_names ~ "Highest 5",
    region %in% bottom_5_names ~ "Lowest 5",
    region %in% g20 ~ "G20 Member"
  )) %>%
  group_by(region) %>%
  summarize(status, mean_gini = mean(gini_reported, na.rm = TRUE)) %>%
  distinct()

top_bottom_5_oecd %>%
  ggplot(aes(x = reorder(region, mean_gini), y = mean_gini, fill = status)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Gini Coefficient of G20 Countries\n and the 5 Countries with the Highest and Lowest Gini Coefficient",
       subtitle = "Only Gini Coefficients from after 2009 considered",
       y = "Gini Coefficient",
       fill = "Status") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        legend.position = "bottom")
