---
  title: "SI"
output:
  pdf_document: default
html_document: default
---
  
  # Supplementary Materials 
  
## Packages
library(brms)
library(dplyr)
library(tidyverse)
require(lingtypology)
library(plotrix)
library(TTR)
library(reshape2)
library(tidybayes)
library(tidyverse)
library(bayestestR)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(bayesplot)
library(ggpubr)
library(plotrix)
library(testthat)
library(cmdstanr)

## Data
#The data for the languages of the Ancient Near East sample were collected through grammar mining. The oldest language in the sample is Sumerian, with written attestation starting at around 3200 BCE, followed by Ancient Egyptian closely thereafter. The youngest languages in the sample are Epigraphic South Arabian and Arabic, ending around 800 AD. The sample contains 35 distinct language varieties. These 35 language varieties either represent a whole language, a distinct local dialect of a language, or a distinct diachronic period of a language. All together, the 35 distinct varieties come from 17 languages: Sumerian, Elamite, Hurrian (isolates); Egyptian, Coptic (Afro-Asiatic); Akkadian, Eblaite (East Semitic); Amorite, Ugaritic, Phoenician, Hebrew, Aramaic (Northwest Semitic); Ancient North Arabian, Epigraphic South Arabian, Arabic (Central Semitic); Luwian, Hittite (Indo-European). 
#A sample of 100 languages representing the worldwide distribution was also assembled. The languages of the worldwide distribution are the 100 languages with the least amount of missing data. The sample was checked to make sure of balanced areal and family distributions. The data for these languages was downloaded from WALS and Autotyp. The data from WALS was accessed through the lingtypology R package version 1.1.7 (Moroz 2017), the data from AUTOTYP was taken from AUTOTYP version 0.1.2.
#A sample of Semitic languages is necessary for the inheritance prior of the sBayes analysis. This data was accessed in a similar way to the universal sample, and supplemented by grammar mining where data was missing.
## Data import 

anea<- read.csv("data/anea_data.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))
semitic<- read.csv("data/semitic_data.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))
universal<- read.csv("data/universal_data.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))


### Regressionn Analysis ###
#We use logistic and categorical regression for the binary and multi-level variables respectively to quantify the extent to which the area (within the Ancient Near East versus outside the Ancient Near East) influences the odds of each value of the morphological features. 
#While the frequentist framework only allows for the rejection of a null effect, it cannot quantify the evidence for a null effect. Because we are interested both in the evidence for the influence of area on morphology as well as the absence of influence of area on morphology, we fit the regression models in a Bayesian framework. The models estimate the probability distributions of the effects of the area on morphology. In order to do so, we used the R package brms (Bürkner 2018). 
#We used a normal distribution prior. For the group-level (“random”) effects we use a student_t (3,0,2.5) prior.  

# create dataset for regression analysis
regression_data<- rbind(anea, universal)
#write.csv(data, file = "regression_data.csv")
#regression_data<- read.csv("data/regression_data.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))

variables <- colnames(regression_data)[7:86]
results.list <- list()
ex.prior <- c(prior_string("normal(0,1)", class="b"))

options(brms.backend = "cmdstanr")

#regression model
for(u in 1:length(variables)){
  print(u)
  brmsformula <- as.formula(paste(variables[u], '~ area + (1|family)'))
  fit <- brm(
    formula= brmsformula,
    family= categorical(link="logit"),
    prior= ex.prior, 
    data= regression_data,
    iter = 5000,
    cores=4,
    control = list(adapt_delta = 0.9999999))
  results.list[[u]] <- fit
}

#saveRDS(results.list, 'data/regression_results.rds')


#The results are reported in the log odds - when the area coefficient is centered around 0 it indicates no influence of the area on the morphology (null-effect). When the area coefficient is positive, that means that the variable in question is more likely in the Ancient Near East than elsewhere, and when it is negative, the variable is more rare in the Ancient Near East compared to the universal distribution. Therefore, the density curves show how probable each value is for each morphological feature in the Ancient Near East compared to the universal distribution.  


##plot

#We extract all the posterior draws from the area coefficient, i.e. indirectly quantifying the probability to observe the associated feature in the area. 
#The coefficient column says which variable and category it is.

#regression_data<- read.csv("data/regression_data.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))
#results<- readRDS("data/regression_results.rds")
results<- results.list
area_draws <- data.frame(post_draw = NA, coefficient = NA) #a vector which is to be expanded with the draws
data <-  as.data.frame(regression_data)

for(u in 7:86){ #loop through all the columns with variables analyzed
  print(u)
  fit <- results[[u -6]] #choose the corresponding fitted model in the results list
  categories <-gsub(' ','', sort(unique(data[,u]))[-1]) #determine the categories of a variable,
  categories <- gsub ('-', '', categories)
  #take all but the first (alphabetically ordered).
  mcmc_samples <- prepare_predictions(fit) #unpack brms stuff, get the posterior draws
  for(k in categories){ #loop through the categories and 
    coef_name <- paste('b_mu', k, '_areaanea', sep = '') #here you get the name of the cartegory + brms syntax
    #i.e. "b_mu..._areanea"
    draw <- mcmc_samples$dpars[[paste('mu', k, sep = '')]][["fe"]]$b[,coef_name] #get the associated area coefficient samples
    area_draws <- rbind(area_draws, data.frame(post_draw = draw, coefficient = paste(colnames(data)[u], k))) #append on the draws vector
  }
}


# "prop" is the proportion of draws larger than zero. If it is small, this indicates evidence for a *negative* coefficient. 
# If it is close to one, it indicates that there is evidence for a *positive* coefficient. In order to gauge it's 
# significance, we subtract "prop" from 1 where it is larger than 0.5. That has been done in the "null_prob" column.
area_draws <- tibble(area_draws) %>% 
  filter(!is.na(coefficient)) %>% 
  add_count(coefficient) %>% 
  group_by(coefficient) %>%  
  mutate(prop = length(which(post_draw > 0))/n) %>% 
  mutate(null_prob = case_when(prop > 0.5 ~ 1- prop, 
                               TRUE ~ prop))

null_probability_order <- area_draws  %>% 
  arrange(prop) %>% 
  distinct(prop)

#print(null_probability_order)

plot_order <- null_probability_order$coefficient
area_draws_order <- arrange(mutate(area_draws, 
                                   coefficient = factor(coefficient, levels = plot_order)), coefficient)


#plot the distributions as a bar plot
regression_plot <- ggplot(area_draws_order,aes(y=coefficient)) +
  stat_interval(aes(x = post_draw), point_interval = median_hdi, 
                .width = c(0.5, .90), size=0.8) + 
  scale_color_brewer(name="Credible Interval", palette = 1) +
  geom_vline(xintercept = 0) + theme_bw() +
  labs(y= "Feature states ordered by mean regression coefficient", x= "Posterior")+
  theme(plot.title = element_text(size = 80,face = "bold"),
        axis.text=element_text(size=2),
        #axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        #axis.title=element_text(size=10,face="bold"),
        legend.text = element_text(size = 8),
        legend.key.width = unit(3, "line"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())

#ggsave("Figures/anea_regression.pdf",
#       plot = regression_plot,
#       width = 15,
#       height = 20,
#       dpi = 300,
#       units = "cm")
