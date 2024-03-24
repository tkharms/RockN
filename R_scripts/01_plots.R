### Plot N content & del15N of rocks as a function of geologic attributes ###
## Rock samples from Alaska Geologic Materials Center

## Inputs:
# Rock N w/ metadata: "rockNmeta.csv"

library(tidyverse)
library(here)

## Data ##
Nmeta <- read.csv(here("data", "rockNmeta.csv"))

### Plots ###
## Boxplots by rock type (ig, meta, sed)
rockN.pl <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                      filter(!is.na(type)) %>%
                      ggplot(aes(x = type, y = N_mgkg)) +
                        geom_boxplot() +
                        ylab("Nitrogen (mg/kg)") +
                        scale_x_discrete(labels = c("igneous", "metamorphic", "sedimentary")) +
                        theme_bw() +
                        theme(panel.grid = element_blank(),
                              panel.border = element_rect(color = "black", size = 2),
                              axis.text = element_text(size = 20),
                              axis.title.y = element_text(size = 20),
                              axis.title.x = element_blank()
                          )

ggsave(rockN.pl, path = "plots", file = "N_igmetased.pdf", width = 9, height = 8, units = "in")

rock15N.pl <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                        filter(!is.na(type)) %>%
                        ggplot(aes(x = type, y = del15N)) +
                          geom_boxplot() +
                          ylab(expression(delta^{"15"}*N~"(‰)")) +
                          scale_x_discrete(labels = c("igneous", "metamorphic", "sedimentary")) +
                          theme_bw() +
                        theme(panel.grid = element_blank(),
                              panel.border = element_rect(color = "black", size = 2),
                              axis.text = element_text(size = 20),
                              axis.title.y = element_text(size = 20),
                              axis.title.x = element_blank()
                          )
  
ggsave(rock15N.pl, path = "plots", file = "N15_igmetased.pdf", width = 9, height = 8, units = "in", encoding="MacRoman")

## Violin plots

rockN.pl2 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                       filter(!is.na(type)) %>%
                       ggplot(aes(x = type, y = N_mgkg)) +
                       geom_violin(alpha = 0.5) +
                       geom_point(position = position_jitter(seed = 1, width = 0.2)) +
                          ylab("Nitrogen (mg/kg)") +
                          scale_x_discrete(labels = c("igneous", "metamorphic", "sedimentary")) +
                       theme_bw() +
                       theme(panel.grid = element_blank(),
                          panel.border = element_rect(color = "black", size = 2),
                          axis.text = element_text(size = 20),
                          axis.title.y = element_text(size = 20),
                          axis.title.x = element_blank()
  )

ggsave(rockN.pl2, path = "plots", file = "N_igmetased_vio.pdf", width = 9, height = 8, units = "in")

rock15N.pl2 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                         filter(!is.na(type)) %>%
                         filter(uncertainty15N_pmil < 2) %>%
                         ggplot(aes(x = type, y = del15N)) +
                          geom_violin(alpha = 0.5) +
                          geom_point(position = position_jitter(seed = 1, width = 0.2)) +
                             ylab(expression(delta^{"15"}*N~"(‰)")) +
                             scale_x_discrete(labels = c("igneous", "metamorphic", "sedimentary")) +
                          theme_bw() +
                          theme(panel.grid = element_blank(),
                                panel.border = element_rect(color = "black", size = 2),
                                axis.text = element_text(size = 20),
                                axis.title.y = element_text(size = 20),
                                axis.title.x = element_blank()
                                    )

ggsave(rock15N.pl2, path = "plots", file = "N15_igmetased_vio.pdf", width = 9, height = 8, units = "in", encoding="MacRoman")

## By subtypes ##
## Boxplots
rockN.pl3 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                         filter(!is.na(type_b)) %>%
                         ggplot(aes(x = type_b, y = N_mgkg)) +
                           geom_boxplot() +
                           ylab("Nitrogen (mg/kg)") +
                           theme_bw() +
                           theme(panel.grid = element_blank(),
                                 panel.border = element_rect(color = "black", size = 2),
                                 axis.text = element_text(size = 15),
                                 axis.text.x = element_text(angle = 45, hjust = 1),
                                 axis.title.y = element_text(size = 20),
                                 axis.title.x = element_blank()
                                   )

ggsave(rockN.pl3, path = "plots", file = "N_subtypes.pdf", width = 15, height = 8, units = "in")

rock15N.pl3 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                            filter(!is.na(type_b)) %>%
                            filter(uncertainty15N_pmil < 2) %>%
                            ggplot(aes(x = type_b, y = del15N)) +
                               geom_boxplot() +
                               ylab(expression(delta^{"15"}*N~"(‰)")) +
                               theme_bw() +
                               theme(panel.grid = element_blank(),
                                    panel.border = element_rect(color = "black", size = 2),
                                    axis.text = element_text(size = 20),
                                    axis.text.x = element_text(angle = 45, hjust = 1),
                                    axis.title.y = element_text(size = 20),
                                    axis.title.x = element_blank()
                                         )

ggsave(rock15N.pl3, path = "plots", file = "N15_subtypes.pdf", width = 15, height = 8, units = "in", encoding="MacRoman")

## Types abbreviated from state layer ##
## Boxplots
rockN.pl4 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                          filter(!is.na(Statelayer_rocktype)) %>%
                          ggplot(aes(x = Statelayer_rocktype, y = N_mgkg)) +
                             geom_boxplot() +
                             ylab("Nitrogen (mg/kg)") +
                             theme_bw() +
                             theme(panel.grid = element_blank(),
                                   panel.border = element_rect(color = "black", size = 2),
                                   axis.text = element_text(size = 15),
                                   axis.text.x = element_text(angle = 45, hjust = 1),
                                   axis.title.y = element_text(size = 20),
                                   axis.title.x = element_blank()
                                       )

ggsave(rockN.pl4, path = "plots", file = "N_statetypes.pdf", width = 15, height = 8, units = "in")

rock15N.pl4 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                            filter(!is.na(Statelayer_rocktype)) %>%
                            filter(uncertainty15N_pmil < 2) %>%
                            ggplot(aes(x = Statelayer_rocktype, y = del15N)) +
                               geom_boxplot() +
                               ylab(expression(delta^{"15"}*N~"(‰)")) +
                               theme_bw() +
                               theme(panel.grid = element_blank(),
                                     panel.border = element_rect(color = "black", size = 2),
                                     axis.text = element_text(size = 20),
                                     axis.text.x = element_text(angle = 45, hjust = 1),
                                     axis.title.y = element_text(size = 20),
                                     axis.title.x = element_blank()
                                         )

ggsave(rock15N.pl4, path = "plots", file = "N15_statetypes.pdf", width = 15, height = 8, units = "in", encoding="MacRoman")

## facet by ig/meta/sed
rockN.pl5 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                          filter(!is.na(Statelayer_rocktype)) %>%
                          ggplot(aes(x = Statelayer_rocktype, y = N_mgkg)) +
                             geom_boxplot() +
                             facet_wrap(~type, scales = "free_x", labeller = as_labeller(type_labs)) +
                             ylab("Nitrogen (mg/kg)") +
                             theme_bw() +
                             theme(panel.grid = element_blank(),
                                   panel.border = element_rect(color = "black", size = 2),
                                   axis.text = element_text(size = 20),
                                   axis.text.x = element_text(angle = 45, hjust = 1),
                                   axis.title.y = element_text(size = 20),
                                   axis.title.x = element_blank(),
                                   strip.background = element_blank(),
                                   strip.text = element_text(size = 20)
                                       )


ggsave(rockN.pl5, path = "plots", file = "N_state_types.pdf", width = 15, height = 8, units = "in", encoding="MacRoman")

rock15N.pl5 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                          filter(!is.na(Statelayer_rocktype)) %>%
                          filter(uncertainty15N_pmil < 2) %>%
                          ggplot(aes(x = Statelayer_rocktype, y = del15N)) +
                          geom_boxplot() +
                             facet_wrap(~type, scales = "free_x", labeller = as_labeller(type_labs)) +
                             ylab(expression(delta^{"15"}*N~"(‰)")) +
                             theme_bw() +
                             theme(panel.grid = element_blank(),
                                   panel.border = element_rect(color = "black", size = 2),
                                   axis.text = element_text(size = 20),
                                   axis.text.x = element_text(angle = 45, hjust = 1),
                                   axis.title.y = element_text(size = 20),
                                   axis.title.x = element_blank(),
                                   strip.background = element_blank(),
                                   strip.text = element_text(size = 20)
                                       )

ggsave(rock15N.pl5, path = "plots", file = "N_state_types.pdf", width = 15, height = 8, units = "in", encoding="MacRoman")

## Deformation ##
deform_order <- c("none",
                  "low",
                  "low-moderate",
                  "moderate",
                  "moderate-high")

rockN.pl6 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                       filter(!is.na(deform)) %>%
                       ggplot(aes(x = deform, y = N_mgkg)) +
                           geom_boxplot() +
                           ylab("Nitrogen (mg/kg)") +
                           scale_x_discrete(limits = deform_order) +
                           theme_bw() +
                           theme(panel.grid = element_blank(),
                                panel.border = element_rect(color = "black", size = 2),
                                axis.text = element_text(size = 20),
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                axis.title.y = element_text(size = 20),
                                axis.title.x = element_blank(),
                                strip.background = element_blank(),
                                strip.text = element_text(size = 20))
             
ggsave(rockN.pl6, path = "plots", file = "N_deform.pdf", width = 15, height = 8, units = "in", encoding="MacRoman")

rockN.pl6 <- Nmeta %>% filter(!is.na(N_mgkg)) %>%
                          filter(!is.na(deform)) %>%
                          filter(uncertainty15N_pmil < 2) %>%
                          ggplot(aes(x = deform, y = del15N)) +
                             geom_boxplot() +
                             ylab(expression(delta^{"15"}*N~"(‰)")) +
                             scale_x_discrete(limits = deform_order) +
                             theme_bw() +
                             theme(panel.grid = element_blank(),
                                   panel.border = element_rect(color = "black", size = 2),
                                   axis.text = element_text(size = 20),
                                   axis.text.x = element_text(angle = 45, hjust = 1),
                                   axis.title.y = element_text(size = 20),
                                   axis.title.x = element_blank(),
                                   strip.background = element_blank(),
                                   strip.text = element_text(size = 20)
                                       )


ggsave(rock15N.pl6, path = "plots", file = "N15_deform.pdf", width = 15, height = 8, units = "in", encoding="MacRoman")

### Scatterplots of continuous attributes ###
pairs(~N_mgkg + del15N + mafic, data = Nmeta)



######################
### Old 2023 plots ###
######################
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
