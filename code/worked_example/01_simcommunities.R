#Trait data for birds for CO Front Range
#Ana Miller-ter Kuile
#August 30, 2024

#this script takes results from Latif et al. 2020 on IMBCR
#bird data from CO Front Range and merges them with AVONET 
#trait data to look at relationships between trait-based responses
#to covaraites (canopy cover)


#Data from this paper, downloaded from Dryad:
#https://doi.org/10.1002/eap.2555

#using mFD package, which requires three data objects:
#1. species x traits dataframe: a dataframe with rownames
## corresponding to species and columns for each trait of 
## interest in the analysis
#2. matrix of assemblages x species with rownames corresponding
## to assemblages (e.g, environments, sites) and column
## names for each species
#3. A dataframe of fruit trait categories with pre-defined 
## category types and two columns: `trait_name` and 
## `trait_type`

# Load libraries ----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  'readxl', 'patchwork',
                  'mFD', 'ape', "FD", 'tripack', 'TPD',
                  'fundiversity')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())

# Load data ---------------------------------------------------------------

#median and 95% BCI estimates from model for each 
#species covariate effect
bird_responses <- read.csv(here('data',
                                'worked_example',
                                "Avian_model_parameter_estimates.csv"))

#AVONET traits
avo_eb <- read.csv(here('data',
                        'worked_example',
                        'AVONET_eBird.csv'))

#to get distribution of canopy gaps from the data
load(here('data',
          'worked_example',
          'Data_compiled.RData'))

species <- read.csv(here('data',
                         'worked_example',
                         "Spp_list.csv"))


# Compile trait data ------------------------------------------------------

remove_cols <- c("Total.individuals",
                 "Female",
                 "Male", "Unknown",
                 "Complete.measures",
                 "Beak.Length_Culmen","Beak.Depth" ,
                 'Beak.Width',"Beak.Length_Nares",
                 "Tarsus.Length","Wing.Length",
                 "Kipps.Distance", "Secondary1",
                 "Tail.Length", "Mass.Source",
                 "Mass.Refs.Other", "Inference",
                 "Traits.inferred", "Reference.species")

#for mFD package, this needs to have just traits as 
#columns and the rownames are the species
species1 <- species %>%
  dplyr::select(BirdCode, common_name, species,
                PIPO_specialist) %>%
  mutate(species = case_when(species == "Polioptila caerulea " ~ "Polioptila caerulea",
                             species == "Regulus calendula" ~ 'Corthylio calendula',
                             TRUE ~ species)) %>%
  left_join(avo_eb, by = c("species" = "Species2")) %>%
  dplyr::select(-matches(remove_cols))

#1. the species x trait dataframe
bird_traits <- species1 %>%
  dplyr::select(BirdCode, PIPO_specialist,
                Hand.Wing.Index:Primary.Lifestyle) %>%
  mutate(Trophic.Level = case_when(Trophic.Level == "Herbivore" ~ 1,
                                   Trophic.Level == "Omnivore" ~ 2,
                                   Trophic.Level == "Carnivore" ~ 3)) %>%
  column_to_rownames(var = 'BirdCode') %>%
  mutate(PIPO_specialist = factor(PIPO_specialist, levels = c("FALSE", "TRUE")),
         Habitat = factor(Habitat, levels = c("Human Modified", "Forest",
                                              "Grassland", "Shrubland",
                                              "Rock", "Wetland", "Riverine", 
                                              "Woodland")),
         Trophic.Niche = factor(Trophic.Niche, levels = c("Granivore",
                                                          "Omnivore", "Invertivore",
                                                          "Nectarivore", 
                                                          "Aquatic predator",
                                                          'Frugivore')),
         Primary.Lifestyle = factor(Primary.Lifestyle, levels = c("Terrestrial",
                                                                  "Aerial", "Generalist",
                                                                  "Insessorial")),
         Habitat.Density = factor(Habitat.Density, ordered = T,
                                  levels = c('1', '2', '3')),
         Migration = factor(Migration, ordered = T,
                            levels = c('1', '2', '3')),
         Trophic.Level = factor(Trophic.Level, ordered = T,
                                levels = c('1', '2', '3')))

bird_traits2 <- bird_traits %>%
  rownames_to_column(var= "Species")

write.csv(bird_traits2, here('data.clean',
                             'traits_COFrontRange',
                             'COFrontRange_birdtraits.csv'),
          row.names = F)

# Generate "communities' of different canopy gaps -------------------------

#grid level occupancy = "beta" parameters
grid_effects <- bird_responses %>%
  filter(Species != "Community") %>%
  filter(Parameter %in% c("beta0", "beta.PACC10", "beta.PACC40",
                          'beta.PAROpn'))

#canopy gap extent I think is a %, and the range for the data
#unstandardized is mean 10 (lower 0, upper 45.8)

bird_cangaps <- grid_effects %>%
  dplyr::select(-l95, -u95) %>%
  filter(Parameter %in% c("beta0", 'beta.PACC10')) %>%
  pivot_wider(names_from = "Parameter",
              values_from = 'median')


community_sim_fun <- function(cangap){
  
  df <- bird_cangaps %>%
    rowwise() %>%
    mutate(b0 = rnorm(n(), mean = beta0, sd = 1),
           b = rnorm(n(), mean = beta.PACC10, sd = 1),
           logis_occ = b0 + b*cangap,
           occupancy = plogis(logis_occ)) %>%
    ungroup() %>%
    dplyr::select(Species, occupancy) %>%
    #cut off if <5% chance of occupancy
    mutate(occupancy = case_when(occupancy < 0.25 ~ 0,
                                 TRUE ~ occupancy)) %>%
    mutate(gap = cangap)
  
  return(df)
  
}

community_sim_fun(cangap = -1)

#observed canopy gap values in the dataframe
#from the OG paper
gaps <- landscape_data$PACC10
meangaps <- mean(gaps)
sdgaps <- sd(gaps)

#get the middle value of -1 and 6, corresponding to 0 and 0.69% gaps
#0.3433=scaled*sdgaps + mean
#0.3433-meangaps = scaled*sdgaps
#(0.3433-meangaps)/sdgaps = scaled
#(0.3433-meangaps)/sdgaps
#middle of those two values would be 2.44 then
#scaled values to simluate communities at
mingap <- (0-meangaps)/sdgaps #-1.01
middlegap <- (0.5-meangaps)/sdgaps #4.02
maxgap <- (1-meangaps)/sdgaps #9.04

#observed data min and max values
#gapscale <- scale(gaps)
# min(gapscale) #-1
# max(gapscale) #6
# min(gaps)
# max(gaps)
#used for standardizing data, 
#primarily by centering (subtracting the mean) 
#and scaling (dividing by the standard deviation). 
#scaled = (t - mean)/sd
#unscaled t*sd + mean YEP

#min, mean, max, -1, 0, 6

gap_meta <- c(rep(mingap, 25), rep(middlegap, 25), 
              rep(maxgap, 25))

comm_list <- lapply(gap_meta, community_sim_fun)

#compile into one dataframe
comm_df <- bind_rows(comm_list,.id = "community")

saveRDS(comm_df, here('data',
                      'worked_example',
                      'canopy_gap_communities.RDS'))
