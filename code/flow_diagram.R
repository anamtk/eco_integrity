library(tidyverse)
library(here)
#library(viridis)
#library(patchwork)
#library(hrbrthemes)
suppressPackageStartupMessages(library(circlize))

#dev.off()
circos.clear()

row1 <- c(0, 1, 1)
row2 <- c(1, 0, 1)
row3 <- c(1, 1, 0)

data2 <- rbind(row1, row2, row3)

colnames(data2) <- c("Monitoring Technology", 'Data Availability', 'Computation Capacity')
rownames(data2) <- colnames(data2)

data2_long <- as.data.frame(data2) %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  mutate(value = case_when(rowname == key ~0,
                           TRUE ~ value))

circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:3)]

mycolor <- c('#b35806', '#fee0b6', '#542788')

jpeg(here("pictures",
                "R", 
                "flow_diagram.jpeg"),
     width = 900,
     height = 900)

# Base plot
chordDiagram(
  x = data2_long, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)



dev.off()
