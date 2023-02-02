# Ana Miller-ter Kuile
# February 1, 2023
# Ecological Integrity Indicators

#this script makes visuals for the indicators used by NPS and Parks
# Canada in ecological integrity assessments

# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", "patchwork")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())
# Load data ---------------------------------------------------------------

nps <- read.csv(here("data",
                     "NPS_vital_signs.csv"))

canada <- read.csv(here("data",
                        "parks_canada_aquatic.csv"))



# Explore -----------------------------------------------------------------

colnames(nps)

#make a summary of metric number
(nps_plot <- nps %>%
  group_by(Type) %>%
  tally(name = "total_metric_categories") %>%
    ggplot(aes(x = Type, y = total_metric_categories)) +
    geom_bar(stat = "identity") +
    labs(x = "Metric type", y = "Total metrics in category",
         title = "National Park Service Metrics") +
    ylim(0, 15) +
    theme(axis.title.y = element_blank()))

#and again for canada
(canada_plot <- canada %>%
  group_by(Type) %>%
  tally(name = "total_metric_categories") %>%
  ggplot(aes(x = Type, y = total_metric_categories)) +
  geom_bar(stat = "identity") +
    labs(x = "Metric type", y = "Total metrics by category",
         title = "Parks Canada Metrics") +
    scale_y_continuous(breaks = c(0, 5, 10, 15)) +
    ylim(0, 15))

#plot together
canada_plot + nps_plot

#get frequency with which those metrics are observed
(nps_freq_plot <- nps %>%
  group_by(Type) %>%
  summarise(avg_num_parks = mean(Count),
            sd = sd(Count),
            total = n(),
            se = sd/sqrt(total)) %>%
    ggplot(aes(x = Type, y = avg_num_parks)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = avg_num_parks - se,
                      ymax = avg_num_parks + se), width = 0.2))
           
