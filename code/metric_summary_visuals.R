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

fs <- read.csv(here("data",
                    "USFS_metrics.csv"))

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
    ylim(0, 30) +
    theme(axis.title.y = element_blank()))

#and again for canada
(canada_plot <- canada %>%
  group_by(Type) %>%
  tally(name = "total_metric_categories") %>%
  ggplot(aes(x = Type, y = total_metric_categories)) +
  geom_bar(stat = "identity") +
    labs(x = "Metric type", y = "Total metrics by category",
         title = "Parks Canada Metrics")  +
    ylim(0, 30))

(usfs_plot <- fs %>%
    group_by(Type) %>%
    tally(name = 'total_metric_categories') %>%
    ggplot(aes(x = Type, 
               y = total_metric_categories)) +
    geom_bar(stat = "identity", 
             position = position_dodge(preserve = "single")) +
    labs(x = "Metric type", y = "Total metrics in category",
         title = "USFS Metrics") +
    ylim(0, 30) +
    theme(axis.title.y = element_blank()) )


#plot together
canada_plot + nps_plot + usfs_plot

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
                      ymax = avg_num_parks + se), width = 0.2) +
    labs(x = "Metric type", y = "Average number of parks surveyed for metric", title = "NPS Metric Survey Frequency"))

(nps_nonaquatic <- nps %>%
    filter(Habitat != "aquatic") %>%
    group_by(Type) %>%
    tally(name = "total_metric_categories") %>%
    ggplot(aes(x = Type, y = total_metric_categories)) +
    geom_bar(stat = "identity") +
    labs(x = "Metric type", y = "Total metrics in category",
         title = "Non-aquatic National Park Service Metrics") +
    ylim(0, 15))          

(usfs_plot2 <- fs %>%
  group_by(Type, Integrity_Category) %>%
  tally(name = 'total_metric_categories') %>%
  ggplot(aes(x = Type, 
             y = total_metric_categories, 
             fill = Integrity_Category)) +
  geom_bar(stat = "identity", 
           position = position_dodge(preserve = "single"),
           color = "black") +
    scale_fill_manual(values = c('#8dd3c7',
                                 '#ffffb3',
                                 '#bebada',
                                 '#fb8072')) +
  labs(x = "Metric type", y = "Total metrics in category",
       title = "USFS Metrics by Integrity Category") +
    ylim(0, 15))    

