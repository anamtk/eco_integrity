#CWM traits for CO Front Range Birds
#Ana Miller-ter Kuile
#October 28, 2024

#this script takes results from Latif et al. 2020 on IMBCR
#bird data from CO Front Range and merges them with AVONET 
#trait data to look at relationships between trait-based responses
#to covaraites (canopy gap percent)


#Data from this paper, downloaded from Dryad:
#https://doi.org/10.1002/eap.2555

# Load libraries ----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  'readxl', 'patchwork',
                  'mFD', 'ape', "FD", 'tripack')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())

# Load data ---------------------------------------------------------------

#trait data already compiled for this paper
traits <- read.csv(here('data',
                        'worked_example',
                        'COFrontRange_birdtraits.csv'))

birds <- readRDS(here('data',
                      'worked_example',
                      'canopy_gap_communities.RDS'))

species <- read.csv(here('data',
                         'worked_example',
                         "Spp_list.csv"))

#to get distribution of canopy gaps from the data
#we're looking at the "landscape data" from this data list
load(here('data',
          'worked_example',
          'Data_compiled.RData'))


# Create scenarios of landscape configuration -----------------------------

hist(landscape_data$PACC10)


# Look at CWM of traits ---------------------------------------------------

#link bird data to traits
birds2 <- birds %>%
  left_join(traits, by = "Species")

#get the categorical variables 
cat_birds <- birds2 %>%
  dplyr::select(Species, occupancy, gap, community,
                PIPO_specialist, Habitat, 
                Trophic.Niche, Primary.Lifestyle) %>%
  mutate(assemblage = case_when(gap < 0 ~ "Closed",
                                gap > 9 ~ 'Open',
                                TRUE ~ "Intermediate")) %>%
  mutate(assemblage = factor(assemblage, levels = c("Closed",
                                                    'Intermediate',
                                                    'Open')))

#from the categorical variables, select
#trophic niche and get abundance by canopy calss
niche <- cat_birds %>%
  filter(occupancy > 0) %>%
  group_by(assemblage, community, Trophic.Niche) %>%
  summarise(abund = sum(occupancy)) %>%
  ungroup() %>%
  group_by(assemblage) %>%
  mutate(sum = sum(abund)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(prop = abund/sum) %>%
  ungroup()

# Plot them ---------------------------------------------------------------

(niche1 <- niche %>%
  filter(Trophic.Niche == "Granivore") %>%
  ggplot(aes(x = assemblage, y = abund, fill = assemblage)) +
  geom_boxplot() +
  labs(x = "Canopy",
       y = "CWM abundance of seed dispersers") +
  scale_fill_manual(values = c('#b35806', '#fee0b6', '#542788')) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        panel.grid = element_blank()))

birds3 <- birds2 %>% 
  filter(Trophic.Niche == "Granivore") %>%
  filter(occupancy > 0) %>%
  group_by(Species, gap) %>%
  summarise(CWM_Mass = weighted.mean(Mass, occupancy, na.rm = T)) %>%
  filter(!is.na(CWM_Mass)) %>%
  mutate(assemblage = case_when(gap < 0 ~ "Closed",
                                gap > 9 ~ 'Open',
                                TRUE ~ "Intermediate")) %>%
  mutate(assemblage = factor(assemblage, levels = c("Closed",
                                                    'Intermediate',
                                                    'Open')))

(niche2 <- birds2 %>% 
  filter(Trophic.Niche == "Granivore") %>%
  group_by(Species, gap) %>%
  summarise(CWM_Mass = weighted.mean(Mass, occupancy, na.rm = T)) %>%
  filter(!is.na(CWM_Mass)) %>%
    mutate(assemblage = case_when(gap < 0 ~ "Closed",
                                  gap > 9 ~ 'Open',
                                  TRUE ~ "Intermediate")) %>%
    mutate(assemblage = factor(assemblage, levels = c("Closed",
                                                      'Intermediate',
                                                      'Open'))) %>%
  ggplot(aes(x = CWM_Mass, fill = assemblage)) +
  geom_density(position = "dodge", alpha = 0.6) +
  labs(x = "CWM species mass (g)",
       y = "Proportion of seed-dispersing species") +
  scale_fill_manual(values = c('#b35806', '#fee0b6', '#542788')) +
    facet_grid(assemblage~.) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        strip.background = element_rect(color = 'white', 
                                        fill = "white"),
        strip.text  = element_text(size = 10)))
  

niche1 + niche2

ggsave(here('pictures', 'worked_example_figure.jpeg'),
       plot = last_plot(),
       width = 6.5,
       height = 4,
       units = c('in'))

ggsave(here('pictures', 'worked_example_figure.pdf'),
       plot = last_plot(),
       width = 6.5,
       height = 4,
       units = c('in'))



# Example birds -----------------------------------------------------------

birds2 %>%
  #filter(Species %in% c("LEGO", "PISI")) %>%
  filter(Trophic.Niche == "Granivore") %>%
  distinct(Species, Mass) %>%
  arrange(Mass)

species %>%
  filter(BirdCode %in% c("LEGO", "PISI", "AMGO", "DEJU",
                         "HOFI", "HOSP", "CAFI", "LASP",
                         "RECR"))
#small species examples: lesser goldfinch, pine siskin

species %>%
  filter(BirdCode %in% c("BTPI", "ROPI"))
#large species: BTPI, ROPI: Rock Pigeon, Band-tailed pigeon

species %>%
  filter(BirdCode %in% c("MODO", "ACWO", "RWBL"))
#medium species: MODO, ACWO: Mourning dove, Acorn woodpecker

species %>%
  filter(BirdCode %in% birds3$Species) %>%
  dplyr::select(common_name)


# MGMT scenarios ----------------------------------------------------------

#scenario 1: no mgmt, current distribution
#High canopy cover in general:
scenario_df <- as.data.frame(cbind(no_change = (1-landscape_data$PACC10),
                                   fire = (1- landscape_data$PACC10) - 
                                     runif(length(landscape_data$PACC10), min = 0.6, max = 1),
                                   restoration = (1-landscape_data$PACC10))) %>%
  mutate(fire = case_when(fire > 1 ~ 1,
                          fire < 0 ~ 0,
                          TRUE ~ fire)) %>%
  mutate(restoration = case_when(restoration > 0.99 ~ restoration,
                                 restoration < 0.99 & restoration > 0 ~ 
                                   restoration - runif(n(), min =0.1, max =  0.7))) %>%
  mutate(restoration = case_when(restoration > 1 ~ 1,
                          restoration < 0 ~ 0,
                          TRUE ~ restoration))

# New facet label names for scenario variable
scen.labs <- c("No change", "High severity fire", "Restoration")
names(scen.labs) <- c("no_change", "fire", 'restoration')

(scenario_df %>%
  pivot_longer(no_change:restoration,
               names_to = "scenario",
               values_to = "canopy") %>%
  mutate(scenario = factor(scenario, levels = c("no_change",
                                                "fire",
                                                "restoration"))) %>%
  ggplot() +
  geom_density(aes(x = canopy), color = "black", fill = '#525252') +
  facet_grid(scenario~.,
             scales = "free_y",
             labeller = labeller(scenario = scen.labs)) +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(color = 'white', 
                                        fill = "white"),
        strip.text  = element_text(size = 10),
        legend.position = "none"))

(noplot <- scenario_df %>%
  ggplot() +
  geom_density(aes(x = no_change*100),
                 color = "black", 
                 fill = '#525252') +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
    xlim(0, 100) +
  labs(x = "Canopy cover (%)",
       y = ""))


ggsave(here('pictures', 'no_mgmt_worked_example.pdf'),
       plot = noplot,
       width = 3,
       height = 2,
       units = c('in'))


(fireplot <- scenario_df %>%
  ggplot() +
  geom_density(aes(x = fire*100),
                 color = "black", 
                 fill = '#525252') +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
        ) +
    xlim(0, 100) +
  labs(x = "Canopy cover (%)",
       y = ""))

ggsave(here('pictures', 'fire_worked_example.pdf'),
       plot = fireplot,
       width = 3,
       height = 2,
       units = c('in'))

(restorationplot <- scenario_df %>%
  ggplot() +
  geom_density(aes(x = restoration*100),
                 color = "black", 
                 fill = '#525252') +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "Canopy cover (%)",
       y = ""))

ggsave(here('pictures', 'restoration_worked_example.pdf'),
       plot = restorationplot,
       width = 3,
       height = 2,
       units = c('in'))


