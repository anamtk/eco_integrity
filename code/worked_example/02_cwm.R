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

# Look at CWM of traits ---------------------------------------------------

#link bird data to traits
birds2 <- birds %>%
  left_join(traits, by = "Species")

#get the categorical variables 
cat_birds <- birds2 %>%
  dplyr::select(Species, occupancy, gap, community,
                PIPO_specialist, Habitat, 
                Trophic.Niche, Primary.Lifestyle) %>%
  mutate(assemblage = case_when(gap == -1 ~ "Closed",
                                gap == 0 ~ "Average",
                                gap == 6 ~ 'Open')) %>%
  mutate(assemblage = factor(assemblage, levels = c("Closed",
                                                    'Average',
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
       y = "CWM abundance of seed-eaters") +
  scale_fill_manual(values = c('#b35806', '#fee0b6', '#542788')) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)))

birds3 <- birds2 %>% 
  filter(Trophic.Niche == "Granivore") %>%
  filter(occupancy > 0) %>%
  group_by(Species, gap) %>%
  summarise(CWM_Mass = weighted.mean(Mass, occupancy, na.rm = T)) %>%
  filter(!is.na(CWM_Mass)) %>%
  mutate(assemblage = case_when(gap == -1 ~ "Closed",
                                gap == 0 ~ "Average",
                                gap == 6 ~ 'Open')) %>%
  mutate(assemblage = factor(assemblage, levels = c("Closed",
                                                    'Average',
                                                    'Open')))

(niche2 <- birds2 %>% 
  filter(Trophic.Niche == "Granivore") %>%
  group_by(Species, gap) %>%
  summarise(CWM_Mass = weighted.mean(Mass, occupancy, na.rm = T)) %>%
  filter(!is.na(CWM_Mass)) %>%
  mutate(assemblage = case_when(gap == -1 ~ "Closed",
                                gap == 0 ~ "Average",
                                gap == 6 ~ 'Open')) %>%
  mutate(assemblage = factor(assemblage, levels = c("Closed",
                                                    'Average',
                                                    'Open'))) %>%
  ggplot(aes(x = CWM_Mass, fill = assemblage)) +
  geom_density(position = "dodge", alpha = 0.6) +
  labs(x = "CWM species mass (g)",
       y = "Proportion of seed-eating species") +
  scale_fill_manual(values = c('#b35806', '#fee0b6', '#542788')) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)))
  

niche1 + niche2

ggsave(here('pictures', 'worked_example_figure.jpeg'),
       plot = last_plot(),
       width = 6.5,
       height = 4,
       units = c('in'))


# Example birds -----------------------------------------------------------

birds2 %>%
  filter(Species %in% c("LEGO", "PISI")) %>%
  distinct(Species, Mass)

species %>%
  filter(BirdCode %in% c("LEGO", "PISI"))
#small species examples: lesser goldfinch, pine siskin

species %>%
  filter(BirdCode %in% c("BTPI", "ROPI"))
#large species: BTPI, ROPI: Rock Pigeon, Band-tailed pigeon

species %>%
  filter(BirdCode %in% c("MODO", "ACWO"))
#medium species: MODO, ACWO: Mourning dove, Acorn woodpecker

species %>%
  filter(BirdCode %in% birds3$Species) %>%
  dplyr::select(common_name)
