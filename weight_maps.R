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

### Autotyp v.1.1.0 from https://github.com/autotyp/autotyp-data ###
####################################################################

load('~/Downloads/autotyp-data-master/data/autotyp.RData')


##############################################################

##############################################################
#load projection for maps
source('https://gitlab.uzh.ch/-/snippets/45/raw/master/ggworld2.R')

##########################################################################################################################################################################################
#figure for most weighed feature in sBayes
##########################################################################################################################################################################################
##############################################################
#read anea data
aneasb<- read.csv("anea_sb.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n")) #import anea dataset

##############################################################
#Get WALS Data
##############################################################

##############################################################
#Coding of Nominal Plurality
##############################################################

nplural.df <- filter(wals.df, Parameter_ID %in% '33A') %>%
  distinct(Glottocode, .keep_all = T) %>% 
  rename(NPlural = Value) 

nplural <- select(nplural.df, Language, Glottocode, Latitude, Longitude, NPlural)

nplural$NPlural[nplural$NPlural == "No plural"] <- "none" 
nplural$NPlural[nplural$NPlural == "Mixed morphological plural"] <- "mixed" 
nplural$NPlural[nplural$NPlural == "Plural complete reduplication"] <- "reduplication" 
nplural$NPlural[nplural$NPlural == "Plural suffix"] <- "suffix" 
nplural$NPlural[nplural$NPlural == "Plural prefix"] <- "prefix" 
nplural$NPlural[nplural$NPlural == "Plural stem change"] <- "stem change" 
nplural$NPlural[nplural$NPlural == "Plural word"] <- "plural word" 
nplural$NPlural[nplural$NPlural == "Plural clitic"] <- "clitic" 
nplural$NPlural[nplural$NPlural == "Plural tone"] <- "tone" 

##############################################################
#Negative Morphemes
##############################################################

negmorph.df <- filter(wals.df, Parameter_ID %in% '112A') %>%
  distinct(Glottocode, .keep_all = T) %>% 
  rename(NegMorph = Value) 

negmorph <- select(negmorph.df, Language, Glottocode, Latitude, Longitude, NegMorph)

negmorph$NegMorph[negmorph$NegMorph == "Negative particle"] <- "particle" 
negmorph$NegMorph[negmorph$NegMorph == "Negative word, unclear if verb or particle"] <- "word" 
negmorph$NegMorph[negmorph$NegMorph == "Negative affix"] <- "affix" 
negmorph$NegMorph[negmorph$NegMorph == "Negative auxiliary verb"] <- "auxiliary verb" 
negmorph$NegMorph[negmorph$NegMorph == "Variation between negative word and affix"] <- "word or affix" 
negmorph$NegMorph[negmorph$NegMorph == "Double negation"] <- "double negation" 

##############################################################
#Pronominal and Adnominal Demonstratives
##############################################################

pronad.df <- filter(wals.df, Parameter_ID %in% '42A') %>%
  distinct(Glottocode, .keep_all = T) %>% 
  rename(PronAdDem = Value) 

pronad <- select(pronad.df, Language, Glottocode, Latitude, Longitude, PronAdDem)


##############################################################
#VInfCat
##############################################################

##get from AUTOYP
infcat.df<- MaximallyInflectedVerbSynthesis %>%
  filter(!is.na(VerbInflectionMaxCategoryCount)) %>%
  mutate(Synthesis_Binary = cut(VerbInflectionMaxCategoryCount,
                                quantile(VerbInflectionMaxCategoryCount,
                                         probs = c(0, 1/2, 1)),
                                right = F, labels = c('low', 'high')),
         Synthesis_Ternary = cut(VerbInflectionMaxCategoryCount,
                                 quantile(VerbInflectionMaxCategoryCount,
                                          probs = c(0, 1/3, 2/3, 1)),
                                 right = F, labels = c('low', 'medium', 'high'))
  ) %>%
  mutate(Synthesis_Binary = ifelse(
    VerbInflectionMaxCategoryCount == max(VerbInflectionMaxCategoryCount), 'high',
    paste(Synthesis_Binary)),
    Synthesis_Ternary = ifelse(
      VerbInflectionMaxCategoryCount == max(VerbInflectionMaxCategoryCount), 'high',           paste(Synthesis_Ternary))
  ) %>%
  mutate(Synthesis_Binary = Synthesis_Binary %in% 'high') %>%
  inner_join(Register)

infcat <- select(infcat.df, Language, Glottocode, Latitude, Longitude, VerbInflectionMaxCategoryCount)
colnames(infcat)[which(colnames(infcat) == 'VerbInflectionMaxCategoryCount')] <- 'VInfCat'

infcat$area <- "universal"

#combine with anea
infan <- select(aneasb, Language, Glottocode, Latitude, Longitude, area, VInfCat)
vinfcat <- bind_rows(infan, infcat) %>% distinct(Language, .keep_all = T)
vinfcat<- vinfcat %>% drop_na(Longitude)

#make long format
vinfcat <- vinfcat %>% pivot_longer(6:ncol(vinfcat), names_to = "feature")

##############################################################
#combine with anea data
##############################################################
wdata<- purrr::reduce(list(nplural, negmorph, pronad), dplyr::full_join)
wdata$area <- "universal"
aneasbb <- select(aneasb, -VInfCat)
diss_s_data <- bind_rows(aneasbb, wdata) %>% distinct(Language, .keep_all = T)
diss_s_data<- diss_s_data %>% drop_na(Longitude)

#make long format
map_s_data <- diss_s_data %>% pivot_longer(6:ncol(diss_s_data), names_to = "feature")


##############################################################
#PLOT
#color pallette
cl <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#9561e2", "#6574cd", "#4dc0b5", "#ffed4a", "#f6993f")

##############################################################
nplural<- map_s_data %>% filter(feature %in% c("NPlural")) 
gg.npl <- project_data(df = nplural)

npl <- gg.npl$base_plot +
  geom_sf(data = gg.npl$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5),
        plot.margin = margin(0,5,0,0.1, "cm")) +
  labs(title = "Noun Plural")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")


############inset###############
clr <- c("#E69F00", "#56B4E9", "#CC79A7", "#9561e2", "#4dc0b5", "#ffed4a", "#f6993f")

gg.npl.inset <- project_data(df = nplural,
                             xmin = 20,
                             xmax = 60,
                             ymin = 25,
                             ymax = 50)


npl.inset<- gg.npl.inset$base_plot +
  geom_sf(data = gg.npl.inset$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = clr, name = NULL
  ) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
        ) +
  guides(shape = "none", color = "none", title = "Noun Plural") 

mapnpl<- ggdraw() +
  draw_plot(npl) +
  draw_plot(npl.inset, x = 0.65, y = 0.35, width = 0.35, height = 0.35)


ggsave("mapnpl.pdf", width = 17, height = 10, units = c("cm"), dpi = 600)


##############################################################
neg<- map_s_data %>% filter(feature %in% c("NegMorph")) 
gg.neg <- project_data(df = neg)

negm <- gg.neg$base_plot +
  geom_sf(data = gg.neg$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5),
        plot.margin = margin(0,5,0,0, "cm")) +
  labs(title = "Negative Morpheme")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")

############inset###############
cle <- c( "#56B4E9", "#009E73", "#CC79A7", "#9561e2", "#6574cd")
gg.negm.inset <- project_data(df = neg,
                             xmin = 20,
                             xmax = 60,
                             ymin = 25,
                             ymax = 50)


negm.inset<- gg.negm.inset$base_plot +
  geom_sf(data = gg.negm.inset$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cle, name = NULL
  ) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none", title = "Negative Morpheme") 

mapneg<- ggdraw() +
  draw_plot(negm) +
  draw_plot(negm.inset, x = 0.65, y = 0.35, width = 0.35, height = 0.35)


ggsave("mapneg.pdf", width = 17, height = 10, units = c("cm"), dpi = 600)


##############################################################
gg.inf <- project_data(df = vinfcat)

inf<- gg.inf$base_plot + 
  geom_sf(data = gg.inf$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_colour_gradient2(breaks = c(0,6,16), labels = c("0", "median", "16"),
                         midpoint = median(gg.inf$data$value),
                         low = 'blue', mid = 'gray', high = 'red', name = NULL) + 
  scale_shape_manual(values=c(17, 16), name= NULL) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 10, family= "serif"), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5),
        plot.margin = margin(0,5,0,0, "cm")) +
    labs(title = "Inflectional Categories")+
  guides(color= guide_colorbar(barwidth = 4, barheight = 0.6, override.aes = list(size = 1), order = 1),
         shape = "none")

############inset###############
gg.inf.inset <- project_data(df = vinfcat,
                              xmin = 20,
                              xmax = 60,
                              ymin = 25,
                              ymax = 50)


inf.inset <- gg.inf.inset$base_plot +
  geom_sf(data = gg.inf.inset$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_colour_gradient2(breaks = c(0,6,16), labels = c("0", "median", "16"),
                         midpoint = median(gg.inf$data$value),
                         low = 'blue', mid = 'grey' , high = 'red', name = NULL) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none", title = "Inflectional Categories") 

mapinf<- ggdraw() +
  draw_plot(inf) +
  draw_plot(inf.inset, x = 0.65, y = 0.35, width = 0.35, height = 0.35)


ggsave("mapinf.pdf", width = 17, height = 10, units = c("cm"), dpi = 600)

##############################################################
pro<- map_s_data %>% filter(feature %in% c("PronAdDem")) 
gg.pro <- project_data(df = pro)

prodem <- gg.pro$base_plot +
  geom_sf(data = gg.pro$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), legend.direction = "horizontal", legend.position = "bottom", legend.text = element_text(size = 8), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5),
        plot.margin = margin(0,5,0,0, "cm")) +
  labs(title = "Pronominal and Adnominal Demonstratives")+
  guides(color= guide_legend(keywidth = 0.2, keyheight = 0.5, override.aes = list(size = 4)),
         shape = "none")

############inset###############
gg.prodem.inset <- project_data(df = pro,
                              xmin = 20,
                              xmax = 60,
                              ymin = 25,
                              ymax = 50)


prodem.inset<- gg.prodem.inset$base_plot +
  geom_sf(data = gg.prodem.inset$data, aes(color = value, shape= area),
          alpha = .5,
          size = 1
  ) +
  scale_shape_manual(values=c(17, 16), name= NULL) +
  scale_color_manual(na.translate=FALSE, values = cl, name = NULL
  ) +
  theme(panel.grid = element_blank(), plot.title = element_text(family = "Helvetica", face = "bold", size = (8), hjust = 0.5), panel.border = element_rect(colour = "gray", fill=NA, size=1)
  ) +
  guides(shape = "none", color = "none", title = "Pronominal and Adnominal Demonstratives") 

mapprodem<- ggdraw() +
  draw_plot(prodem) +
  draw_plot(prodem.inset, x = 0.65, y = 0.35, width = 0.35, height = 0.35)


ggsave("mapprodem.pdf", width = 17, height = 10, units = c("cm"), dpi = 600)

##############################################################
#put all maps together
mapsb<- grid.arrange(mapnpl, mapneg, mapinf, mapprodem)
ggsave("mapsb.pdf", mapsb, width = 21, height = 15, units = c("cm"), dpi = 600)

