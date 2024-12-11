#use landscape dataset:
#simulate species occupancies in the "real" dataset with
#marginal effect of canopy gaps

#simulate some different scenarios of canopy gaps, thinking about
#realistic representations, maybe of what "was" 
#and also what restoration/fire mitigation restoration might do

bird_cangaps <- grid_effects %>%
  dplyr::select(-l95, -u95) %>%
  filter(Parameter %in% c("beta0", 'beta.PACC10')) %>%
  pivot_wider(names_from = "Parameter",
              values_from = 'median')

gapdf <- landscape_data %>%
  dplyr::select(Grid, Year, PACC10)

ggplot(gapdf, aes(x = PACC10, fill = Year)) +
  geom_histogram()

df2 <- gapdf %>%
  cross_join(bird_cangaps) %>%
  rowwise() %>%
  mutate(occupancy = plogis(beta0 + beta.PACC10*PACC10)) %>%
  ungroup() %>%    
  #cut off if <25% chance of occupancy
  # mutate(occupancy = case_when(occupancy < 0.25 ~ 0,
  #                              TRUE ~ occupancy)) %>%
  dplyr::select(Grid, Year, beta.PACC10,PACC10, Species, occupancy)

df3 <- df2 %>%
  left_join(traits, by = "Species")

niche <- df3 %>%
  filter(Trophic.Niche == "Granivore") %>%
  filter(occupancy > 0) %>%
  group_by(Grid, PACC10) %>%
  summarise(cwm_abund = sum(occupancy))

ggplot(niche,aes(x = PACC10, y = cwm_abund)) +
  geom_point()
  
summarise(CWM_Mass = weighted.mean(Mass, occupancy, na.rm = T))

