#a script for creating a heatmap to show similarity between the Semitic languages for all features

require(tidyverse)
library(ggplot2)
library(forcats)
library(wesanderson)
library(RColorBrewer)

semitic<-read.csv("Semitic_ordered.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))

# make long format
semitic[] <- lapply(semitic, as.character)

sem <- semitic %>%
  pivot_longer(8:ncol(semitic), names_to = "feature") %>% 
  group_by(feature) %>% 
  mutate(number_of_levels = length(unique(value))) %>% 
  group_by(feature) %>% 
  mutate(values = paste0(feature, value))

n_non_NA <- sem %>%
  filter(!is.na(value)) %>%  
  group_by(feature) %>% 
  count() %>% 
  summarize(potential_matches = n * (n-1) / 2)

matches <- sem %>%
  group_by(feature, value) %>%
  summarize(matches_by_state = n() * (n()-1) / 2, .groups = "drop_last") %>%
  filter(!is.na(value)) %>% 
  summarize(matches = sum(matches_by_state)) %>% 
  left_join(n_non_NA, by="feature") %>% 
  mutate(
    proportion_match = matches / potential_matches,
    proportion_differ = 1 - matches / potential_matches,
  )

state_counts <- sem %>%
  group_by(feature, value) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(value)) %>% 
  count()

# color <- rep(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 14)
# color<- c("#B8C4F9", "#354279", "#6D86F7", "#595E78", "#566BC4", "#5261FF") #blues
#color<- c("#F5D925", "#E2B36B", "#D5CE5A", "#F5CB25", "#D6D36B", "#F4E85F") #yellows
color<- c("#F1BE6C", "#DF9065", "#F17C6C", "#FFFF80", "#BE5F1B", "#DF8B46") #desert

# color <- c("#00798c", "#cc5566", "#edae49", "#66a182", "#2e4057", "#8d96a3", "#992299") 
#color <- c("#49a0b7", "#ffc411", "#cc5566", "#306580", "#dd7009", "#999999")

# Collect unique state names for each feature
states <- sem %>% 
  filter(!is.na(value)) %>% 
  group_by(feature) %>% 
  summarise(state_name = unique(value), .groups="drop")

# Collect colors for the state names
state_colors = states %>% 
  group_by(feature) %>% 
  summarise(c = color[1:n()], .groups="drop")

# Names list of colors for each name
long_color <- as.character(state_colors$c)
names(long_color) <- as.character(states$state_name)

sm <- ggplot() +
  geom_tile(data=sem, mapping=aes(x = fct_inorder(language), feature, fill=value)) +
  theme(
    legend.position="none", panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5), 
    axis.text.y = element_text(size = 3.3)
  ) +
  scale_fill_manual(values=long_color, na.value = "white") +
  labs(x = "Languages ordered by time", y = "Feature") +
  geom_text(data=matches, mapping = aes(x=27.5, y=feature, label=round(proportion_match, 2), hjust="inward"), size=1.5) +
  geom_text(mapping=aes(label="Feature agreement", x=27.5, y=53, hjust="inward", vjust="inward"), size=1.45)

print(paste("Mean agreement:", mean(matches$proportion_match), sep=" "))
print(paste("Standard deviation:", var(matches$proportion_match)**0.5, sep=" "))

ggsave("sem.pdf", sm, width = 15, height = 10, units = c("cm"), dpi = 400)

