###### make maps of a few features for discussion section ############
library(tidyverse)
library(ggdist)
library(cowplot)
library(gridExtra)


### WALS v.2020.2 from https://github.com/cldf-datasets/wals/releases ###
#########################################################################

wals.df <- read.csv('~/Downloads/wals-2020.2/cldf/values.csv') %>%
  select(Language_ID, Code_ID) %>%
  inner_join(read.csv('~/Downloads/wals-2020.2/cldf/languages.csv'),
             by = c('Language_ID' = 'ID')) %>%
  rename(Language = Name) %>%
  inner_join(read.csv('~/Downloads/wals-2020.2/cldf/codes.csv'),
             by = c('Code_ID' = 'ID')) %>%
  rename(Value = Name)


##############################################################
#read anea data
anea<- read.csv("data/raw/anea_diss.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n")) #import anea dataset

##############################################################
#load projection for maps
source('https://gitlab.uzh.ch/-/snippets/45/raw/master/ggworld2.R')

##############################################################
#Get WALS Data
##############################################################

##############################################################
#Possessive classes
##############################################################

possclass.df <- filter(wals.df, Parameter_ID %in% '59A') %>%
  distinct(Glottocode, .keep_all = T) %>% 
  rename(PossClass = Value) 

poss.class <- select(possclass.df, Language, Glottocode, Latitude, Longitude, PossClass)

poss.class$PossClass[poss.class$PossClass == "No possessive classification"] <- "none" 
poss.class$PossClass[poss.class$PossClass == "Two classes"] <- "2" 
poss.class$PossClass[poss.class$PossClass == "Three to five classes"] <- "3-5" 
poss.class$PossClass[poss.class$PossClass == "More than five classes"] <- ">5" 


##############################################################
#Noun Phrase Conjunction
##############################################################

npconj.df <- filter(wals.df, Parameter_ID %in% '63A') %>%
  distinct(Glottocode, .keep_all = T) %>% 
  rename(Conjunction = Value) 

npconj <- select(npconj.df, Language, Glottocode, Latitude, Longitude, Conjunction)

npconj$Conjunction[npconj$Conjunction == "'And' different from 'with'"] <- "with/and different" 
npconj$Conjunction[npconj$Conjunction == "'And' identical to 'with'"] <- "with/and identical" 


##############################################################
#Optative
##############################################################

optative.df <- filter(wals.df, Parameter_ID %in% '73A') %>%
  distinct(Glottocode, .keep_all = T) %>% 
  rename(Optative = Value) 

optative <- select(optative.df, Language, Glottocode, Latitude, Longitude, Optative)

optative$Optative[optative$Optative == "Inflectional optative absent"] <- "absent" 
optative$Optative[optative$Optative == "Inflectional optative present"] <- "present" 

##############################################################
#Systems of Gender Assynment
##############################################################

genderass.df <- filter(wals.df, Parameter_ID %in% '32A') %>%
  distinct(Glottocode, .keep_all = T) %>% 
  rename(Gender = Value) 

genass <- select(genderass.df, Language, Glottocode, Latitude, Longitude, Gender)

genass$Gender[genass$Gender == "No gender"] <- "no gender" 
genass$Gender[genass$Gender == "Semantic and formal"] <- "semantic+formal" 
genass$Gender[genass$Gender == "Semantic"] <- "semantic" 

##############################################################
#combine with anea data
##############################################################
wdata<- purrr::reduce(list(poss.class, npconj, optative, genass), dplyr::full_join)
wdata$area <- "universal"
diss_data <- bind_rows(anea, wdata) %>% distinct(Language, .keep_all = T)
diss_data<- diss_data %>% drop_na(Longitude)

#make long format
mapd_data <- diss_data %>% pivot_longer(6:ncol(diss_data), names_to = "feature")


##############################################################
#PLOT
#color pallette
cl <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")

##############################################################
possclass<- mapd_data %>% filter(feature %in% c("PossClass")) 
gg.poss <- project_data(df = possclass)

pos <- gg.poss$base_plot +
  geom_sf(data = gg.poss$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Possessive Classification")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")

##############################################################
conjunction<- mapd_data %>% filter(feature %in% c("Conjunction")) 
gg.conj <- project_data(df = conjunction)

con <- gg.conj$base_plot +
  geom_sf(data = gg.conj$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Noun Phrase Conjunction")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")

##############################################################
op<- mapd_data %>% filter(feature %in% c("Optative")) 
gg.op <- project_data(df = op)

opt <- gg.op$base_plot +
  geom_sf(data = gg.op$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Optative")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")


##############################################################
gend<- mapd_data %>% filter(feature %in% c("Gender")) 
gg.gend <- project_data(df = gend)

gen <- gg.gend$base_plot +
  geom_sf(data = gg.gend$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5)
  ) +
  labs(title = "Systems of Gender Assignment")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")



##############################################################
#put all maps together
mapdiss<- grid.arrange(pos, con, gen, opt)
ggsave("mapdiss.pdf", mapdiss, width = 15, height = 10, units = c("cm"), dpi = 600)


