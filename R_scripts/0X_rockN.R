#### Nitrogen content of rocks ###
## Merge rock N & del15N with sample attributes
## Rock samples from Alaska Geologic Materials Center

## Inputs:
# Materials Center sample metadata
# Rock N

library(tidyverse)
library(here)

## Data ##
rocks <- read.csv(here("data", "rockN", "RockN_15N.csv"))

### Plots ###
## Boxplots by rock type
rockN.pl <- rocks %>% filter(!is.na(N_mgkg)) %>%
                      ggplot(aes(x = rock_type, y = N_mgkg)) +
                        geom_boxplot() +
                        ylab("Nitrogen (mg/kg)") +
                        theme_bw() +
                        theme(panel.grid = element_blank(),
                              panel.border = element_rect(color = "black", size = 2),
                              axis.text = element_text(size = 20),
                              axis.title.y = element_text(size = 20),
                              axis.title.x = element_blank()
                          )

ggsave(rockN.pl, path = "plots", file = "prelim_rockN.pdf", width = 9, height = 8, units = "in")

rock15N.pl <- rocks %>% filter(!is.na(N_mgkg)) %>%
  ggplot(aes(x = rock_type, y = d15N)) +
  geom_boxplot() +
  ylab(expression(delta^{"15"}*N~"(‰)")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_blank()
  )
  
ggsave(rock15N.pl, path = "plots", file = "prelim_rock15N.pdf", width = 9, height = 8, units = "in", encoding="MacRoman")

## By rock attributes
# All rock types on faceted plots
# This is a manual hack. Will fix later when full dataset is in.
labels <- c(a = "felsic", 
            b = "mafic", 
            c = "mafic/ultramafic", 
            d = "low-mid", 
            e = "mid",
            f = "mid-high",
            g = "high",
            h = "shallow marine",
            i = "slope, deep water",
            j = "deep water")

rockN_types.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange") %>%
                            ggplot(aes(x = rock_cats, y = N_mgkg)) +
                              geom_point(size = 2, position = position_jitter(width = 0.15)) +
                              ylab("Nitrogen (mg/kg)") +
                              scale_x_discrete(labels = labels) +
                              facet_wrap(~rock_type, scales = "free_x") +
                              theme_bw() +
                              theme(panel.grid = element_blank(),
                                panel.border = element_rect(color = "black", size = 2),
                                axis.text = element_text(size = 20),
                                axis.title.y = element_text(size = 20),
                                axis.title.x = element_blank(),
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                strip.background = element_blank(),
                                strip.text = element_text(size = 20)
  )

ggsave(rockN_types.pl, path = "plots", file = "prelim_rockN_types.pdf", width = 9, height = 8, units = "in", encoding="MacRoman")

rock15N_types.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange") %>%
  ggplot(aes(x = rock_cats, y = d15N)) +
  geom_point(size = 2, position = position_jitter(width = 0.15)) +
  ylab(expression(delta^{"15"}*N~"(‰)")) +
  scale_x_discrete(labels = labels) +
  facet_wrap(~rock_type, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 20)
  )
                    
ggsave(rock15N_types.pl, path = "plots", file = "prelim_rock15N_types.pdf", width = 9, height = 8, units = "in", encoding="MacRoman")

## Separate panels for Yesim talk
# bulk N
rockN_types_ig.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange" & rock_type == "igneous") %>%
  ggplot(aes(x = rock_cats, y = N_mgkg)) +
  geom_point(color = "#f0aa48", size = 3, position = position_jitter(width = 0.15)) +
  ylab("Nitrogen (mg/kg)") +
  scale_x_discrete(labels = c("felsic", "mafic", "mafic/ultramafic")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio=2/1
  )

ggsave(rockN_types_ig.pl, path = "plots", file = "ig_N.pdf", width = 3.5, height = 8, units = "in", encoding="MacRoman")
                
rockN_types_sed.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange" & rock_type == "sedimentary") %>%
  ggplot(aes(x = rock_cats, y = N_mgkg)) +
  geom_point(color = "#bbd998", size = 3, position = position_jitter(width = 0.15)) +
  ylab("Nitrogen (mg/kg)") +
  scale_x_discrete(labels = c("shallow marine", "slope, deep water", "deep water")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio=2/1,
        plot.margin = margin(0, 0.25, 0.25, 1, unit = "in")
  )

ggsave(rockN_types_sed.pl, path = "plots", file = "sed_N.pdf", width = 3.8, height = 8, units = "in", encoding="MacRoman")    

rockN_types_meta.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange" & rock_type == "metamorphic") %>%
  ggplot(aes(x = rock_cats, y = N_mgkg)) +
  geom_point(color = "#be99d1", size = 3, position = position_jitter(width = 0.15)) +
  ylab("Nitrogen (mg/kg)") +
  scale_x_discrete(labels = c("low-mid", "mid", "mid-high", "high")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio=2/1,
        plot.margin = margin(0, 0.25, 0.25, 0.5, unit = "in")
  )

ggsave(rockN_types_meta.pl, path = "plots", file = "meta_N.pdf", width = 3.3, height = 8, units = "in", encoding="MacRoman")    

# 15N
rock15N_types_ig.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange" & rock_type == "igneous") %>%
  ggplot(aes(x = rock_cats, y = d15N)) +
  geom_point(color = "#f0aa48", size = 3, position = position_jitter(width = 0.15)) +
  ylab(expression(delta^{"15"}*N~"(‰)")) +
  scale_x_discrete(labels = c("felsic", "mafic", "mafic/ultramafic")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio=2/1
  )

ggsave(rock15N_types_ig.pl, path = "plots", file = "ig_15N.pdf", width = 3.6, height = 8, units = "in", encoding="MacRoman")

rock15N_types_sed.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange" & rock_type == "sedimentary") %>%
  ggplot(aes(x = rock_cats, y = d15N)) +
  geom_point(color = "#bbd998", size = 3, position = position_jitter(width = 0.15)) +
  ylab(expression(delta^{"15"}*N~"(‰)")) +
  scale_x_discrete(labels = c("shallow marine", "slope, deep water", "deep water")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio=2/1,
        plot.margin = margin(0, 0.25, 0.25, 1, unit = "in")
  )

ggsave(rock15N_types_sed.pl, path = "plots", file = "sed_15N.pdf", width = 3.8, height = 8, units = "in", encoding="MacRoman")    

rock15N_types_meta.pl <- rocks %>% filter(!is.na(N_mgkg) & rock_type != "melange" & rock_type == "metamorphic") %>%
  ggplot(aes(x = rock_cats, y = d15N)) +
  geom_point(color = "#be99d1", size = 3, position = position_jitter(width = 0.15)) +
  ylab(expression(delta^{"15"}*N~"(‰)")) +
  scale_x_discrete(labels = c("low-mid", "mid", "mid-high", "high")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio=2/1,
        plot.margin = margin(0, 0.25, 0.25, 0.5, unit = "in")
  )

ggsave(rock15N_types_meta.pl, path = "plots", file = "meta_15N.pdf", width = 3.3, height = 8, units = "in", encoding="MacRoman")    
