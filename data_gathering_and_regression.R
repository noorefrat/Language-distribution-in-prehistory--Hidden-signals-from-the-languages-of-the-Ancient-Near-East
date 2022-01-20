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

##Import data for the Ancient Near East

#load Ancient Near East data
anea<- read.csv("anea.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))
semitic<- read.csv("Semitic.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))

##change names of duplicate languages, to not crash the recoding code
semitic[semitic == "Amharic"] <- "AmharicS"
semitic[semitic == "Egyptian Arabic"] <- "Egyptian ArabicS"
semitic[semitic == "Modern Hebrew"] <- "Modern HebrewS"



### Worldwide Language Sample Selection 

#A sample of 100 languages representing the worldwide distribution was also assembled. The languages of the worldwide distribution are the 100 languages with the least amount of missing data. The sample was checked to make sure of balanced areal and family distributions. The data for these languages was downloaded from WALS and Autotyp and accessed through the lingtypology R package (Moroz 2017).


#get data for worldwide sample

#get AUTOTYP data
autotyp <- autotyp.feature(c("NP_structure","Clause_linkage", "Numeral_classifiers", "Alignment_per_language", "Synthesis")) 
autotyp<- select(autotyp, Glottocode, NPMarking, NPAgrCat, NPHeadlessness, ClausePosition, NumClass.n, AlignmentCaseDominantSAP, VInflCatandAgrFmtvMax.binned3)
colnames(autotyp)[which(colnames(autotyp) == 'Glottocode')] <- 'glottocode'
autotyp <- autotyp %>% distinct(glottocode, .keep_all = T)

#get WALS data
f_names <- c("24a","27a","30a","32a","33a","37a","38a","41a","42a","43a","44a","46a",
             "47a","48a","52a","53a","54a","57a","59a","63a","64a","65a","66a","67a",
             "70a","71a","73a","81a","84a","85a","86a","87a","88a","89a","90a","91a",
             "92a","93a","101a","102a","106a","107a","112a","143a")
wals <- wals.feature(f_names)
colnames(wals)[4:47] <- c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",  "Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives", "Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",  "Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals", "Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction", "Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense", "The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb", "Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun", "Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun", "Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Expression.of.Pronominal.Subjects", "Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb")
wals <- wals %>% distinct(language, .keep_all = T)

#get metadata
autotypmeta<- autotyp.feature("Register")
autotypmeta<- select(autotypmeta, Glottocode, Area, MajorBranch)
colnames(autotypmeta)[which(colnames(autotypmeta) == 'Glottocode')] <- 'glottocode'
colnames(autotypmeta)[which(colnames(autotypmeta) == 'Area')] <- 'area'
colnames(autotypmeta)[which(colnames(autotypmeta) == 'MajorBranch')] <- 'family'


#combine datasets
autotyp <- merge(autotyp, autotypmeta, by= "glottocode", all.x = T)
wals <- merge(wals, autotypmeta, by= "glottocode", all.x = T)
reference <- merge(wals, autotyp, all = T, by = c('glottocode', 'area', 'family'))
reference <- relocate(reference, language)
reference<- select(reference, -wals.code)
non_feature_columns <- -c(1:6)

#language threshold
feature_names<- c("Locus.of.Marking.in.Possessive.Noun.Phrases",	"Reduplication", "Gender.n", "Systems.of.Gender.Assignment",  "Nominal.Plurality",	"Definite.Articles",	"Indefinite.Articles",	"Distance.Contrasts.in.Demonstratives", "Pronominal.and.Adnominal.Demonstratives",	"Third.Person.Pronouns.and.Demonstratives",  "Gender.Distinctions.in.Independent.Personal.Pronouns",	"Indefinite.Pronouns",	"Intensifiers.and.Reflexive.Pronouns", "Person.Marking.on.Adpositions",	"Comitatives.and.Instrumentals",	"Ordinal.Numerals",	"Distributive.Numerals", "Position.of.Pronominal.Possessive.Affixes",	"Possessive.Classification",	"Noun.Phrase.Conjunction", "Nominal.and.Verbal.Conjunction",	"Perfective.Imperfective.Aspect",	"The.Past.Tense",	"The.Future.Tense", "The.Morphological.Imperative",	"The.Prohibitive",	"Optative",	"WordOrderSOV", "Order.of.Object..Oblique..and.Verb", "Order.of.Adposition.and.Noun.Phrase",	"Order.of.Genitive.and.Noun",	"Order.of.Adjective.and.Noun", "Order.of.Demonstrative.and.Noun",	"Order.of.Numeral.and.Noun",	"Order.of.Relative.Clause.and.Noun", "Order.of.Degree.Word.and.Adjective",	"Position.of.Polar.Question.Particles",	"Position.of.Interrogative.Phrases.in.Content.Questions",	"Expression.of.Pronominal.Subjects", "Verbal.Person.Marking",	"Reciprocal.Constructions",	"Passive.Constructions", "Negative.Morphemes",	"Order.of.Negative.Morpheme.and.Verb", "NPMarking", "NPAgrCat", "NPHeadlessness", "ClausePosition", "NumClass.n", "AlignmentCaseDominantSAP", "VInflCatandAgrFmtvMax.binned3", "glottocode", "language", "family", "longitude", "latitude")
reference.data <- reference[,colnames(reference) %in% feature_names]
reference.data$feature.sum<- rowSums(1*!is.na(as.matrix(reference.data[,non_feature_columns])))
reference.data <- reference.data %>% group_by(language) %>% 
  mutate(max_features = case_when(feature.sum == max(feature.sum) ~ 1, TRUE ~ 0)) %>%  
  filter(max_features == 1)
reference.data <- reference.data[-which(duplicated(reference.data$language)),]
#table(rowSums(1*!is.na(as.matrix(reference.data[,non_feature_columns]))))

#make a 100 language sample of the languages with most data
reference.data.order <- reference.data  %>% arrange(desc(feature.sum)) #order by amount of data
top.100 <- reference.data.order[1:100,] #keep top 100 languages with the most data
lang.samp <- top.100 %>% arrange(language)
lang.samp <- select(lang.samp, -feature.sum, -max_features)
lang.samp$area <- "universal"
lang.samp <- relocate(lang.samp, area, .after = family)
lang.samp<- as.data.frame(lang.samp)


#Combine Ancient Near East Data with worldwide data
combined_data<- rbind(anea, lang.samp, semitic)
combined_data[combined_data == ""] <- NA


### Feature Recoding

###recode WALS features with 'mixed' category
#rename data to match names in recoding-patterns
wals_source_data<- combined_data
colnames(wals_source_data)[7:50]<- c("52A Comitatives and Instrumentals", "63A Noun Phrase Conjunction", "64A Nominal and Verbal Conjunction", "37A Definite Articles", "38A Indefinite Articles", "101A Expression of Pronominal Subjects", "46A Indefinite Pronouns", "43A Third Person Pronouns and Demonstratives", "41A Distance Contrasts in Demonstratives",
                                     "42A Pronominal and Adnominal Demonstratives", "47A Intensifiers and Reflexive Pronouns", "44A Gender Distinctions in Independent Personal Pronouns", "32A Systems of Gender Assignment", "30A Number of Genders", 
                                     "112A Negative Morphemes", "33A Coding of Nominal Plurality", "53A Ordinal Numerals", "54A Distributive Numerals", "87A Order of Adjective and Noun", "85A Order of Adposition and Noun Phrase", "91A Order of Degree Word and Adjective",
                                     "88A Order of Demonstrative and Noun", "86A Order of Genitive and Noun", "143A Order of Negative Morpheme and Verb", "89A Order of Numeral and Noun", "84A Order of Object Oblique and Verb", 
                                     "90A Order of Relative Clause and Noun", "107A Passive Constructions", "106A Reciprocal Constructions", "73A The Optative", "71A The Prohibitive", "70A The Morphological Imperative", "48A Person Marking on Adpositions",
                                     "93A Position of Interrogative Phrases in Content Questions", "92A Position of Polar Question Particles", "59A Possessive Classification", "57A Position of Pronominal Possessive Affixes", "24A Locus of Marking in Possessive Noun Phrases",
                                     "27A Reduplication", "67A The Future Tense", "66A The Past Tense", "65A Perfective/Imperfective Aspect", "102A Verbal Person Marking", "81A Order of Subject Object and Verb")

#clean up data
expect_false(any(duplicated(wals_source_data)))
#expect_false(any(duplicated(wals_source_data$language)))
names(wals_source_data)<- gsub("X","", names(wals_source_data))
names(wals_source_data)<- gsub("\\."," ", names(wals_source_data))

wals_source_info <- wals_source_data[,1:6]
wals_source_data <- wals_source_data[,-c(2:6)]


# load the list of recodings
recode_patterns <- read.csv("recode_patterns_mixed.csv", stringsAsFactors=FALSE)
names(recode_patterns)[1]<-paste("wals.fname")

# extract the features that we don't need to recode
retained_wals_features <- filter(recode_patterns, is.na(recode.pattern))$wals.fname
retained_wals_features <- retained_wals_features[retained_wals_features %in% names(wals_source_data)]# Discard unused wals features

recode_patterns <- filter(recode_patterns, !is.na(recode.pattern))
recode_patterns <- recode_patterns[recode_patterns$wals.fname %in% names(wals_source_data),]

expect_false(any(duplicated(recode_patterns$new.fname)), info=paste0(
  "Duplicated feature names:\n", 
  paste0("  ", recode_patterns$new.fname[duplicated(recode_patterns$new.fname)], collapse="\n")
))

recoded_wals_fnames <- c(retained_wals_features, recode_patterns$new.fname)

#expect_false(any(duplicated(retained_wals_features)), info=paste0(
#  "Duplicated retained feature names:\n", 
#  paste0("  ", retained_wals_features[duplicated(retained_wals_features)], collapse="\n")
#))

expect_true(all(retained_wals_features %in% names(wals_source_data)), info=paste0(
  "Feature not found in WALS:\n", 
  paste0("  ", setdiff(retained_wals_features, names(wals_source_data)), collapse="\n")
))

# recode all the patterns
wals_recoded <- rowwise(recode_patterns) %>% do({
  cat("Processing ", .$new.fname, "(", .$wals.fname, " recoded as ", .$recode.pattern, ")\n", sep="")
  
  # check that the original variable is present in wals
  expect_true(.$wals.fname %in% names(wals_source_data)[-1])
  original_data <- as.character(wals_source_data[[.$wals.fname]])
  
  # make a table of original values
  expected_levels <- unlist(strsplit(.$wals.levels, "\n"))
  expected_levels <- data.frame(
    i = as.integer(gsub("^([0-9])+.+$", "\\1", expected_levels)),
    level = gsub("^[0-9]+\\.? +", "", expected_levels),
    stringsAsFactors=FALSE
  )
  
  expect_true(all(!is.na(expected_levels$i)))
  expect_true(all(!is.na(expected_levels$level)))
  
  # make sure that the WALS values are what we have in the table
  #expect_true(setequal(expected_levels$level, na.omit(original_data)), info=
  #              paste0("Expected:\n", paste0("  ", (expected_levels$level), collapse="\n"), "\n",
  #                     "Got:\n",  paste0("  ", (unique(original_data)), collapse="\n"))
  #)
  
  # parse the recoding pattern
  
  recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
  recoding_groups <- strsplit(recoding_groups, "/") %>% lapply(as.integer)
  
  # sanity checks
  expect_true(length(recoding_groups)>1) # must have at least 2 recoding groups
  expect_true(all(!is.na(unlist(recoding_groups)))) # can't have NA's
  expect_true(all(unlist(recoding_groups) %in% expected_levels$i)) # must correspond to wals values
  expect_false(any(duplicated(unlist(recoding_groups)))) # can't have any duplicates
  
  # build the recodign table
  recoded_levels <- unlist(strsplit(.$new.levels, "\n"))
  expect_true(length(recoding_groups)==length(recoded_levels)) # must have at least 2 recoding groups
  
  recoded_levels <- bind_rows(mapply(recoded_levels, recoding_groups, FUN=function(value, ii) {
    data.frame(i = ii, new_level=as.character(value), stringsAsFactors=FALSE)
  }, SIMPLIFY=FALSE))
  
  level_table <- full_join(expected_levels, recoded_levels, by="i")
  
  # sanity checks
  expect_true(all(!is.na(level_table$level)))
  
  # recode the data
  recoded_data <- level_table$new_level[match(original_data, level_table$level)]
  
  
  data.frame(
    feature = .$new.fname, 
    language = wals_source_data$language, 
    value = recoded_data,  
    stringsAsFactors=FALSE)  
}) %>%
  pivot_wider(names_from = feature, values_from = value)

#  add the non-recoded variables
retained_data <- wals_source_data[c("language", retained_wals_features)]
wals_recoded <- full_join(wals_recoded, retained_data, by=c(language="language"))

# and merge it with the language list
wals_recoded <- full_join(wals_source_info, wals_recoded, by = "language")

# check that all variable names are present
#expect_true(dplyr::setequal(names(wals_recoded)[-c(1:ncol(wals_source_info))], recoded_wals_fnames))


###recode AUTOTYP features with 'mixed' category
np_structure <- combined_data[,-c(7:50)]

# load the mapping
NPMarking_mapping <- read_csv("np_marking_feature_mapping.csv") %>%
  set_names(c("NPMarking", "Feature"))
NPAgrCat_mapping <- read_csv("NPAgrCat_feature_mapping.csv") %>%
  set_names(c("NPAgrCat", "Feature"))

# recode the NPMarking variable
NPMarking_recoded <- select(np_structure, language, NPMarking) %>%
  # remove all entries where NPMarking is NA
  filter(!is.na(NPMarking)) %>%
  # bring in some language information
  #left_join(select(register, LID, Glottocode, Language), by = "LID") %>%
  # recode the variable using the mapping table
  left_join(NPMarking_mapping, by = "NPMarking") %>%
  # drop NPMarking as we don't need it anymore
  select(-NPMarking) %>%
  # unique features only
  distinct() %>%
  # pivot feature values to columns
  mutate(value = TRUE) %>%
  pivot_wider(names_from = Feature, values_fill = FALSE)
NPMarking_recoded<- select(NPMarking_recoded, -"NA")

# recode the NPAgrCat variable
NPAgrCat_recoded <- select(np_structure, language, NPAgrCat) %>%
  # remove all entries where NPAgrCat is NA
  filter(!is.na(NPAgrCat)) %>%
  # bring in some language information
  #left_join(select(register, LID, Glottocode, Language), by = "LID") %>%
  # recode the variable using the mapping table
  left_join(NPAgrCat_mapping, by = "NPAgrCat") %>%
  # drop NPAgrCat as we don't need it anymore
  select(-NPAgrCat) %>%
  # unique features only
  distinct() %>%
  # pivot feature values to columns
  mutate(value = TRUE) %>%
  pivot_wider(names_from = Feature, values_fill = FALSE)
NPAgrCat_recoded<- select(NPAgrCat_recoded, -"NA")

# combime both recoded features
autotyp_recoded <- full_join(NPMarking_recoded, NPAgrCat_recoded, by=c(language="language"))

#  add the non-recoded variables
retained_autotyp_features <- select(np_structure, -NPMarking, -NPAgrCat)
autotyp_recoded <- full_join(autotyp_recoded, retained_autotyp_features, by=c(language="language"))
autotyp_recoded %>% select(language, glottocode, family, area, latitude, longitude, everything())
#autotyp_recoded<- select(autotyp_recoded, -"NA")

##combine recoded WALS with recoded AUTOTYP

recoded_data<- merge(wals_recoded, autotyp_recoded, by= c("language", "glottocode", "family", "area", "longitude", "latitude"))
#write.csv(recoded_data, file = "recoded.anea.uni.not.clean.csv")


## Data import and clean up
#clean up data from special characters so it can pass through the regression function
data<-recoded_data
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., ",", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "'", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., ":", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "-", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "/", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "\\+", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "\\[", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "\\]", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "\\(", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "\\)", "")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "=", "v")))
data <- data %>% mutate_at(vars(Reciprocal.Presence:VInflCatandAgrFmtvMax.binned3), list(~ stringr::str_replace_all(., "≠", "x")))

#change feature names to short names
colnames(data) <- c("language", "glottocode","family", "area", "longitude", "latitude", "Rec.P", "RecRef.I", "RecRef.D", "ContQInitial",	"ContQNonInitial", "NumNoun", "NounNum", "DegWAdj",	"AdjDegW", "GenN",	"NGen", "AdjN",	"NAdj",	"PossAff.P",	"PossPre",	"PossSuff",	"ComInst",	"NPconj",	"NVconj",	"DefArt",	"IndefArt",	"PronomS",	"IndefPron",	"PronDem",	"DemDis",	"PronAdnDem",	"IntRef",	"GenPron",	"Gender",	"Gender.n",	"NegMorph",	"NPlural",	"OrdNum",	"DistNum", "AdpN",	"DemN",	"NegV",	"OXV",	"RelN",	"Passive",	"Optative",	"Prohibitive",	"Imperative",	"AdpPM",	"PolarQ",	"PossClass",	"PossLocus",	"Reduplication",	"Future",	"Past",	"Aspect",	"Vper",	"SOV",	"Head.driven.agr",	"Construct.state",	"Juxtaposition",	"Governed",	"Mod.headed.poss.agr",	"Linker",	"Incorporation",	"Dep.driven.agr",	"Pronominal.agr",	"Anti.const.state.agr",	"Mod.governed.adp", "External.driven.agr",	"External.Poss",	"Bare.noun",	"Nominalizer",	"Agr.Number",	"Agr.Gender",	"Agr.Case",	"Agr.State", "Agr.none",	"Agr.Person",	"Agr.Role",	"NPHeadlessness",	"ClausePosition",	"NumClass.n",	"Alignment", "VInflCat")

#write.csv(data, file = "recoded.anea.semitic.uni.clean.csv")


## Separate back into three datasets, and create the dataset for the Regression
anea<- data %>% filter (area %in% "anea")
semitic<- data %>% filter (area %in% "semitic")
universal<- data %>% filter (area %in% "universal")

#change names of duplicate languages back in the Semitic dataset
semitic[semitic == "AmharicS"] <- "Amharic"
semitic[semitic == "Egyptian ArabicS"] <- "Egyptian Arabic"
semitic[semitic == "Modern HebrewS"] <- "Modern Hebrew"

#write.csv(anea, file = "recoded.anea.clean.csv")
#write.csv(semitic, file = "recoded.semitic.clean.csv")
#write.csv(universal, file = "recoded.universal.clean.csv")

# create dataset for regression analysis
data<- rbind(anea, universal)
#write.csv(data, file = "regression.data.csv")

### Regressionn Analysis ###
#We use logistic and categorical regression for the binary and multi-level variables respectively to quantify the extent to which the area (within the Ancient Near East versus outside the Ancient Near East) influences the odds of each value of the morphological features. 
#While the frequentist framework only allows for the rejection of a null effect, it cannot quantify the evidence for a null effect. Because we are interested both in the evidence for the influence of area on morphology as well as the absence of influence of area on morphology, we fit the regression models in a Bayesian framework. The models estimate the probability distributions of the effects of the area on morphology. In order to do so, we used the R package brms (Bürkner 2018). 
#We used a normal distribution prior. For the group-level (“random”) effects we use a student_t (3,0,2.5) prior.  

#data<- read.csv("recoded.anea.uni.clean.csv", encoding="UTF-8", na.strings=c(""," ","NA","\n"))

variables <- colnames(data)[7:86]
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
    data= data,
    iter = 5000,
    cores=4,
    control = list(adapt_delta = 0.9999999))
  results.list[[u]] <- fit
}

saveRDS(results.list, 'regression_results_prior_1.rds')


#The results are reported in the log odds - when the area coefficient is centered around 0 it indicates no influence of the area on the morphology (null-effect). When the area coefficient is positive, that means that the variable in question is more likely in the Ancient Near East than elsewhere, and when it is negative, the variable is more rare in the Ancient Near East compared to the universal distribution. Therefore, the density curves show how probable each value is for each morphological feature in the Ancient Near East compared to the universal distribution.  


##plot

#We have for all but the baseline category an area coefficient in the model. 
#It refers to the (logit transformed) probability to see the corresponding category in the area.
#So you might have a coefficient called No_Nouns_area. We are going to extract
#the samples from the posterior distribution of the coefficient.
#Important to know is that if the model finds that overall the feature is not much more prevalent 
#in the area than outside, the samples will all be around zero, and spread evenly on both sides of zero.

#What the code does is to take all the samples of coefficients related to area from all the models.
#It goes through the models one by one, takes the categories, extracts the samples
#and puts them into the longer vector "area_draws".
#So at the end, you have sort of a mega-coefficient: all the coefficients related to category probabilities
#in the areas are collected and you can see it as the overall bias towards having distinct features, I guess.


#the code collects all the posterior draws from the area coefficient,
#i.e. indirectly quantifying the probability to observe the associated 
#feature in the area. The coefficient column says which variable and category it is.
data<- read.csv("regression.data.csv")
results<- readRDS("regression_results_prior_1.rds")
results<- results.list
area_draws <- data.frame(post_draw = NA, coefficient = NA) #a vector which is to be expanded with the draws
data <-  as.data.frame(data)

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
# significance, you can just subtract "prop" from 1 where it is larger than 0.5. That has been done in the "null_prob" column.
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

ggsave("anea_regression.pdf",
       plot = regression_plot,
       width = 15,
       height = 20,
       dpi = 300,
       units = "cm")


### sBayes
#sBayes (Ranacher et al., 2021) is a software for finding contact areas in space based on linguistic features. Contact areas comprise of geographically proximate languages, which are linguistically similar and whose similarity cannot be explained by the confounding effects of either universal preference or inheritance. 
#Through a Markov Chain Monte Carlo (MCMC) approach sBayes samples from the posterior distribution of a model. In the warm-up phase, multiple independent chains explore the parameter space in parallel. After the warm-up phase, sBayes moves to the chain with the highest likelihood where it starts sampling from the posterior.


#### Data coding
#sBayes requires as input a matrix of discrete independent categorical features in the format of a CSV file. For inheritance sBayes takes a non-hierarchical family relation where each unique entry is considered a family. Languages with the same entry are treated as belonging to the same family. If the family column for a language is left empty, inheritance is not modeled for this language. For our model we chose the Major-Branch level of relatedness as the family. We chose that because a division according to Family or Stock would be too broad for our data, having most of the languages in the Ancient Near East sample belonging to the Semitic Stock.


##sBayes requires a specific data format
#create anea data set for sbayes
colnames(anea)[which(colnames(anea) == 'glottocode')] <- 'id'
colnames(anea)[which(colnames(anea) == 'language')] <- 'name'
colnames(anea)[which(colnames(anea) == 'latitude')] <- 'y'
colnames(anea)[which(colnames(anea) == 'longitude')] <- 'x'
Sanea<- relocate(anea, id)
Sanea<- select(anea, -area)
Sanea[Sanea == "NA"] <- ""
write.csv(Sanea, file = "Sanea.csv")

#create Semitic data set for sbayes
colnames(Ssemitic)[which(colnames(Ssemitic) == 'glottocode')] <- 'id'
colnames(Ssemitic)[which(colnames(Ssemitic) == 'language')] <- 'name'
colnames(Ssemitic)[which(colnames(Ssemitic) == 'latitude')] <- 'y'
colnames(Ssemitic)[which(colnames(Ssemitic) == 'longitude')] <- 'x'
Ssemitic<- relocate(Ssemitic, id)
Ssemitic<- select(anea, -area)
Ssemitic[Ssemitic == "NA"] <- ""
write.csv(Ssemitic, file = "Ssemitic.csv")

#create universal data set for sbayes
colnames(universal)[which(colnames(universal) == 'glottocode')] <- 'id'
colnames(universal)[which(colnames(universal) == 'language')] <- 'name'
colnames(universal)[which(colnames(universal) == 'latitude')] <- 'y'
colnames(universal)[which(colnames(universal) == 'longitude')] <- 'x'
Suniversal<- relocate(universal, id)
Suniversal<- select(anea, -area)
Suniversal[Suniversal == "NA"] <- ""
write.csv(Suniversal, file = "Suniversal.csv")