# initial look at ABCD 5.1 ph_y_saiq table
# http://dx.doi.org/10.15154/z563‑zd24

# this was administered year 3 and 4 and are questions that I added to ABCD!
#

rm(list=ls())
library(tidyverse)
library(readr)
library(reshape2)
library(patchwork)

source('./setupPaths_5.1.R')

ph_y_saiq <- read_csv(file.path(releaseDir, "physical-health/ph_y_saiq.csv"))
ph_y_saiq <- as.data.frame(lapply(ph_y_saiq, as.factor))

#count occurrences of the different levels of sai_lmusic_hrs_day_y for each event
dcast(ph_y_saiq, eventname ~ sai_lmusic_hrs_day_y, length, value.var = "sai_lmusic_hrs_day_y") #value.var irrelevant for 'length' but it warns otherwise
dcast(ph_y_saiq, sai_lmusic_hrs_day_y ~ eventname, length, value.var = "sai_lmusic_hrs_day_y")

#summary for each event
by(ph_y_saiq, ph_y_saiq$eventname, summary)
# a more useful version showing full summary per column
by(select(ph_y_saiq,-c(src_subject_id, sai_readmusic_admin_y)), ph_y_saiq$eventname, function(x) sapply(x, summary))

# not so useful and wonder where I even got this!
ph_y_saiq %>%
  group_by(eventname) %>%
  select(-c(src_subject_id, sai_readmusic_admin_y)) %>%
  dplyr::summarise(across(everything(), ~list(summary(.))), .groups = 'drop')

# plot
plots_list <- list()

df = select(ph_y_saiq,-c(src_subject_id, sai_readmusic_admin_y))

# Loop through each column (except the grouping factor) and plot histograms across events
for (col in names(df)[names(df) != "eventname"]) {
  p <- ggplot(df, aes_string(x = col, fill = "eventname")) +
    #geom_histogram(aes(y=(..count..)/sum(..count..)), position = "identity", stat="count", alpha = 0.5, bins = 30) +
    geom_histogram(data = subset(df, eventname == levels(df$eventname)[1]), aes(y=(..count..)/sum(..count..)),
                   stat="count", alpha = 0.5, position = "identity", bins = 30) +
    geom_histogram(data = subset(df, eventname == levels(df$eventname)[2]), aes(y=(..count..)/sum(..count..)),
                   stat="count", fill = NA, color = "black", position = "identity", bins = 30, linetype = "solid", size=.2) +
    labs(title = col) + guides(fill="none") + theme_classic()
    #theme_minimal() +
    #scale_fill_brewer(palette = "Set1") # Adjust color palette as needed

  plots_list[[col]] <- p
  p
}

# Combine all plots
combined_plot <- wrap_plots(plots_list, ncol = 3) # Adjust the number of columns as needed

# Print the combined plot
combined_plot

#ggsave("ph_y_saiq.png", device="png", path=resultDirRoot, units="in", height=11, width=8.5, dpi=400)
