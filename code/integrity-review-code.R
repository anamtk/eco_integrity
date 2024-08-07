
# load libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(viridis)
library(RColorBrewer)
library(tidyverse)
library(here)
library(patchwork)

theme_set(theme_bw())
#import data
dat <- read.csv(here('data',
                     'Ecological integrity literature review - Sheet1.csv'))
## fix differences in data entry
dat2 <- dat %>%
  rename('about_integrity' = 'Is.the.paper.actually.about.ecological.integrity.',
         'integrity_calculated' = 'Is.integrity.actually.calculated.',
         'include_animals' = 'Does.metric.include.animals.',
         'animal_communities' = 'Does.metric.include.animal.communities.') %>%
  mutate(about_integrity = case_when(about_integrity %in% c("No", 'no', 'No - see notes',
                                                            'No- but see notes', "Maybe") ~ "No",
                                     about_integrity %in% c("Yes", 'yes', 'Yes-maybe', 'Yes- but note terms',
                                                            'Yes- but indirectly', "Yes- but see notes") ~ "Yes")) %>%
  mutate(integrity_calculated = case_when(integrity_calculated %in% c("-", "No", "Maybe","", 
                                                                      "no", "No- concept paper with examples?", 
                                                                      "No-  \"validation\" paper" , "No- \"validation\" paper",                              
                                                                      "No-\"validation\" paper","No- \"metric development\"- see notes",                 
                                                                       "No- concept paper with examples- see notes" ,           
                                                                       "No- \"validation paper\"- see notes" ,                  
                                                                       "No- \"review\" paper" ,                                 
                                                                       "No- concept paper with examples from a National Forest") ~ "No",
                                          integrity_calculated %in% c("yes","Yes","Yes/maybe","Yes/Maybe",
                                                                      "Yes- but see notes","Yes- note similar to above but link assumed") ~ "Yes")) %>%
  mutate(include_animals = case_when(include_animals %in% c("no", "No", "-", "No- see notes",
                                                            "", "Maybe") ~ "No", 
                                     include_animals %in% c("Yes", "yes") ~ "Yes")) %>%
  mutate(animal_communities = case_when(animal_communities %in% c("-", "no",
                                                                  "No", "no (?)", "") ~ "No",
                                        animal_communities %in% c("yes", "Yes", "yes??") ~ "Yes"))


# simplify data frame
dat2 <- dat2[,c(6:8,10:11)]

# change chacater to integer
dat2[dat2=="Yes"] <- 1
dat2[dat2=="No"] <- 0
dat2[is.na(dat2)] <- 0

dat2 <- dat2 %>%
  mutate_if(is.character, as.numeric)

# get rid of nestedness
dat2[,4][dat2[,5] == 1] <-0
dat2[,3][dat2[,4] == 1] <-0
dat2[,2][dat2[,3] == 1] <-0

# change to long format
dat3 <- dat2 %>%
  pivot_longer(cols = 2:5,
               names_to = "variable",
               values_to = "value")

# summarize by year
dat4 <- dat3 %>% 
  group_by(Year, variable) %>% 
  summarise(sum=sum(value, na.rm = T)) %>%
  mutate(variable = factor(variable, levels = c('about_integrity',
                                                'integrity_calculated',
                                                'include_animals',
                                                'animal_communities')))


# plot
(a <- dat4 %>%
    arrange(variable) %>%
    ggplot(aes(x = Year,
                y = sum,
                fill = variable)) +
  geom_col(position = "stack") +
  theme_bw() +
  scale_fill_brewer(palette = "Oranges",
                    labels = c('about_integrity' = "Is the paper about ecological integrity?",
                               'integrity_calculated' = "Is ecological integrity calculated?",
                               'include_animals' = "Does the integrity metric include animals?",
                               'animal_communities' = "Does the integrity metric include animal communities?")) +
  ylab("Number of papers") + xlab(NULL) +
  theme(legend.position = c(0.3, 0.82),
        legend.title = element_blank(),
        axis.text = element_text(size=8, color="black"),
        axis.title = element_text(size=8, color="black"),
        legend.text = element_text(size=8, color="black"),
        panel.grid = element_blank()))
#a


# output to hi res
# tiff("integrity-litreview-11302023.tif",
#      res=500, width=6, height=4, units="in", compression="lzw")
# a
# dev.off()

ggsave(a,
       filename = here('pictures',
                       'R',
                       'studies_by_year.jpg'),
       width = 6,
       height = 4,
       units = 'in')


# Another option ----------------------------------------------------------

dat5 <- dat2 %>%
  dplyr::select(about_integrity, integrity_calculated,
                include_animals, animal_communities) %>%
  pivot_longer(1:4,
               names_to = "var",
               values_to = "value") %>%
  mutate(value = case_when(value == "No" ~ 0,
                           value == "Yes" ~ 1)) %>%
  group_by(var) %>%
  summarise(sum = sum(value)) %>%
  ungroup() %>% 
  add_row(var = "total", sum = 279) %>%
  mutate(var = factor(var, levels = c("total", 
                                      "about_integrity",
                                      "integrity_calculated", 
                                      "include_animals",
                                      "animal_communities")),
         varID = as.numeric(var)) %>%
  arrange(varID) %>%
  mutate(diff = sum - lead(sum)) %>%
  mutate(diff = case_when(var == "animal_communities" ~ sum,
                          TRUE ~ diff)) %>%
  #for graphing
  mutate(y = 1) 

(b <- dat5 %>%
    arrange(var) %>%
    ggplot(aes(x = y, y = diff, fill= var)) +
    geom_col(position = "fill") +
    theme_bw() +
    scale_fill_brewer(palette = "Oranges",
                      labels = c('total' = "Total papers in literature review (n = 278)",
                                 'about_integrity' = "Is the paper about ecological integrity?",
                                 'integrity_calculated' = "Is ecological integrity calculated?",
                                 'include_animals' = "Does the integrity metric include animals?",
                                 'animal_communities' = "Does the integrity metric include animal communities?")) +
    ylab("Proportion of papers") + xlab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    xlim(0.5, 2.25) +
    theme(legend.position = c(0.3, 0.82),
          legend.title = element_blank(),
          axis.text = element_text(size=8, color="black"),
          axis.title = element_text(size=8, color="black"),
          legend.text = element_text(size=8, color="black"),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    coord_flip())

ggsave(b,
       filename = here('pictures',
                       'R',
                       'studies_total.jpg'),
       width = 6,
       height = 4.5,
       units = 'in')

# Other explorations ------------------------------------------------------

unique(dat$Environment)
unique(dat$Animal.groups)
unique(dat$Function.Metric)

#MAIN FIGURES
#Environmental bias
#types of traits
(env_plot <- dat %>%
  filter(Environment != "") %>%
  group_by(Environment) %>%
  tally() %>%
  ggplot(aes(x = reorder(Environment, n), y = n)) +
  geom_bar(stat = "identity", fill = "#998ec3",color = "black") +
  labs(x = "Environment", y = "Number of studies") +
    scale_x_discrete(labels = c("aquatic" = "Aquatic",
                                "terrestrial" = "Terrestrial",
                                'multiple' = "Multiple*",
                                'marine' = "Marine",
                                "riparian" = "Riparian")) +
    coord_flip() +
  theme(axis.text = element_text(size=8, color="black"),
        axis.title = element_text(size=8, color="black"),
        legend.text = element_text(size=8, color="black"),
        panel.grid = element_blank()))

traits <- dat %>%
  separate_longer_delim(Function.notes, delim = ",") %>%
  dplyr::select(Function.notes) %>%
  filter(Function.notes != "") %>%
  mutate(Function.notes2 = case_when(Function.notes %in% c('feeding guilds',
                                                          'trophic position',
                                                          'trophic strategies',
                                                          'litter decompostion',
                                                          'trophic groups',
                                                          'foraging',
                                                          'feeding behaviors',
                                                          'feeding behaviours',
                                                          'feeding guild') ~ "Feeding",
                                    Function.notes %in% c('habitat', ' habitat', 'microhabitat',
                                                          'foraging locations', 
                                                          'habitat generalism',
                                                          'habitat functions', 
                                                          'habitat assoications',
                                                          'habitat associations',
                                                          'habitat use') ~ "Habitat",
                                    Function.notes %in% c('behavior', 'nesting',
                                                          'migration') ~ "Behavior",
                                    Function.notes %in% c('mobiility', 'morphology',
                                                          'body size') ~ "Morphology",
                                    Function.notes %in% c("pollution sensitivity", "sensitivity") ~ "Human sensitivity",
                                    Function.notes %in% c("functional diversity") ~ "Multiple\u2020"))

(trait_plot <- traits %>% 
  group_by(Function.notes2) %>%
  tally() %>%
  ggplot(aes(x = reorder(Function.notes2, n), y = n)) +
  geom_bar(stat = "identity", fill = "#998ec3", color = "black") +
    coord_flip() +
  labs(x = "Functional trait category", y = "Number of studies")+
    theme(axis.text = element_text(size=8, color="black"),
          axis.title = element_text(size=8, color="black"),
          legend.text = element_text(size=8, color="black"),
          panel.grid = element_blank()))

b <- env_plot + trait_plot +
  plot_annotation(tag_levels = "A")

ggsave(b,
       filename = here('pictures',
                       'R',
                       'study_content.jpg'),
       width = 6,
       height = 2.5,
       units = 'in')

# Supplementary plots -----------------------------------------------------

#animal groups
(animal_plot <- dat %>%
    filter(Animal.groups != "") %>%
    group_by(Animal.groups) %>%
    tally() %>%
    ggplot(aes(x = reorder(Animal.groups, n), y = n)) +
    geom_bar(stat = "identity", fill = "#998ec3", , color="black") +
    labs(x = "Animal group", y = "Number of studies") +
    coord_flip() +
    theme(axis.text = element_text(size=8, color="black"),
          axis.title = element_text(size=8, color="black"),
          legend.text = element_text(size=8, color="black"),
          panel.grid = element_blank()))

(metric_plot <- dat %>%
    filter(Function.Metric != "") %>%
    group_by(Function.Metric) %>%
    tally() %>%
    ggplot(aes(x = reorder(Function.Metric, -n), y = n)) +
    geom_bar(stat = "identity", fill = "#998ec3", , color="black") +
    labs(x = "Was a functional metric calculated?", y = "Number of studies"))
# Geography ---------------------------------------------------------------

dat_geo <- dat %>%
  mutate(Continent = case_when(Geography %in% c("eastern canada bogs",
                                                "USA", "northeastern USA swamps",
                                                'quebec urban ish', 'North America',
                                                'eastern canada', 'chesapeake bay',
                                                "N. America",
                                                'mid-atlantic states', 'great lakes coastal wetlands',
                                                'N. America- regional', 'N. America- forested',
                                                'N. America- estuary', "N. America- riparian",
                                                'N. America- wetland', "N. America- coastal",
                                                'prairies in alberta canada', 'forests in ohio',
                                                'streams in colorado basin') ~ "North America",
                               Geography %in% c('streams in zimbabwe', 'streams in ethiopian highlands',
                                                'nigeria', 'streams in ethiopia',
                                                "Africa", "Africa- riparian", 
                                                "Africa- terrestrial",
                                                'South Africa') ~ "Africa",
                               Geography %in% c("Asia", "south korea", "rivers in china",
                                                "Asia- aquatic") ~ "Asia",
                               Geography %in% c("Europe", "switzerland alpine areas",
                                                'streams in europe', 'streams in spain',
                                                'germany', 'scandanavia', 'Europe- agricultural',
                                                'Europe- terrestrial', "Europe- forested", 
                                                "Europe- aquatic", 'north portugal',
                                                'freshwater in turkey',
                                                'italy', 'german north sea') ~ "Europe",
                               Geography %in% c("pelagic ocean", 
                                                'aquatic') ~ "Aquatic/Oceanic",
                               Geography %in% c("Brazilian Atlantic forest", 
                                                'northern brazil', 'South America',
                                               'S. America- riparian',
                                                'S. America- forested',
                                               'temperate grasslands in uruguay') ~ "South America",
                               Geography == 'Central America' ~ "Central America",
                               Geography %in% c('freshwater streams in new south wales australia',
                                                'Australia', 'New Zealand', 
                                                'Oceana', 'Oceana- intertidal',
                                                "Australia- riparian", "australia",
                                                'Oceana- coastal') ~ "Oceana",
                               Geography == "Global" ~ "Global",
                               Geography == "The Americas" ~ "Americas",
                               TRUE ~ NA_character_))

(geography_plot <- dat_geo %>%
  filter(Geography != "") %>%
  group_by(Continent) %>%
  tally() %>%
  ggplot(aes(x = reorder(Continent, n), y = n)) +
    geom_bar(stat = 'identity', fill = "#998ec3", color="black") +
  labs(x = "Continent", y = "Number of studies") +
    coord_flip()+
    theme(axis.text = element_text(size=8, color="black"),
          axis.title = element_text(size=8, color="black"),
          legend.text = element_text(size=8, color="black"),
          panel.grid = element_blank()))

metric_plot + (animal_plot / geography_plot) +
  plot_annotation(tag_levels = "A")

ggsave(filename = here('pictures',
                       'R',
                       'supp_review_fig.jpg'),
       width = 7,
       height = 4,
       units = 'in')
