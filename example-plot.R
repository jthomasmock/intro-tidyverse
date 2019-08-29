library(tidyverse)
library(readxl)
library(bbplot)
df_raw <- read_excel("data/SeriesReport-20190706143733_bba3d4.xlsx", 
                                                 skip = 3)
df_clean <- df_raw %>% 
  janitor::clean_names() %>% 
  pivot_long(annual_1994:annual_2018, names_to = "year", values_to = "percent") %>% 
  filter(str_detect(series_id, "FMUP137885")) %>% 
  mutate(year = str_extract(year, "[:digit:]+"),
         year = as.integer(year),
         percent = percent/100) %>% 
  mutate(code = str_remove(series_id, "FMUP13788"),
         code = factor(code, 
                       levels = c("51", "52", "53"),
                       labels = c("All", "Men", "Women"))) 

label_df <- df_clean %>% 
  filter(year == max(year)) %>% 
  mutate(label = as.character(code))

plot1 <- df_clean %>% 
  ggplot(aes(x = year, y = percent, group = code, color = code)) +
  geom_line(aes(size = 1)) +
  geom_text(data = label_df,
           aes(x = year + 0.4, y = percent, label = label, color = label, 
               hjust = 0, size = 4)) +
  geom_point(data = label_df, 
             aes(x = year, y = percent, color = label, size = 3)) +
  scale_x_continuous(breaks = seq(1995, 2015, 5),
                     limits = c(1994, 2022)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Year",
       y = "",
       title = "Labor Force Participation (1994-2018) by Gender",
       caption = "Data Source: BLS") +
  ggthemes::theme_gdocs() +
  bbplot::bbc_style() +
  theme(legend.position="none")
  
plot1

ggsave("plot1.png", plot1, height = 12, width = 10, units = "in", dpi = "retina")
