# Das System muss Analysen als vergleichbar erkennen können. 
# Vergleichbare Analysen im Sinne dieses Textes sind alle Analysen, die
# (I.) mit dem gleichen LOINC Kode kodiert wurden, ODER 
# (II.) deren LOINC Kode sich nur durch die Methode unterscheidet, ODER
# (III.) deren LOINC Kodes durch eine Umrechnung der Einheiten ineinander 
# überführen lassen.

library(tidyverse)
library(readxl)

loinc <-  read.csv("data/LOINC/2.74/LoincTable/Loinc.csv") %>%
  filter(CLASSTYPE == 1) %>%  # https://simplifier.net/hl7-oo/laboratory-tests-and-panels-uv-ips-duplicate-3
  filter(!CLASS %in% c("CHALSKIN", "H&P.HX.LAB", "H&P.HX", "NR STATS")) %>%
  filter(!CLASS %in% c("ABXBACT")) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(COMPONENT,'Bacteria identified')) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(CLASS, "PATH\\.PROTOCOLS\\.")) %>%
  filter(STATUS == "ACTIVE") %>%
  filter(ORDER_OBS != "Order") # nur Codes zur Darstellung


loinc_de <- read.csv(
  "data/LOINC/2.74/AccessoryFiles/LinguisticVariants/deDE15LinguisticVariant.csv") %>%
  inner_join(loinc, by='LOINC_NUM', suffix=c('.de', ''))


systems <- loinc %>%
  count(SYSTEM)

properties <- loinc_de %>%
  count(PROPERTY, PROPERTY.de, SCALE_TYP)

similarSystem <- tribble(
  ~SYSTEM_GROUP, ~SYSTEM,
  'Ser/Plas', c('Ser/Plas', 'Ser', 'Plas', 'Ser/Plas/Bld')
)%>%
  unnest(SYSTEM)

similarProperty <- tribble(
  ~PROPERTY_GROUP, ~PROPERTY,
  'S/MCnc', c('SCnc', 'MCnc', 'LsCnc', 'SCncSq'),
  'NCnc', c('NCnc', 'LnCnc'),) %>%
  unnest(PROPERTY)

similarLoinc  <- loinc_de %>%
  left_join(similarProperty, by="PROPERTY") %>%
  left_join(similarSystem, by="SYSTEM") %>%
  mutate(SIMILAR_PROP = if_else(is.na(SYSTEM_GROUP), 
                                SYSTEM,
                                SYSTEM_GROUP)) %>%
  group_by(COMPONENT, TIME_ASPCT, SYSTEM_GROUP, SCALE_TYP, SIMILAR_PROP) %>%
  summarise(LOINC_NUM = list(LOINC_NUM), 
            LONG_COMMON_NAME = list(LONG_COMMON_NAME.de),
            PROPERTY = list(PROPERTY),
            CLASS = list(CLASS),
            n=n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  mutate(GROUP_UID = 1:n()) %>%
  unnest(cols = c(LOINC_NUM, LONG_COMMON_NAME, PROPERTY, CLASS)) %>%
  select(GROUP_UID, LONG_COMMON_NAME, LOINC_NUM, COMPONENT, TIME_ASPCT, 
         SYSTEM_GROUP, SCALE_TYP, PROPERTY, SIMILAR_PROP, CLASS)


# Tests ---
similarLoinc  %>%
  count(LOINC_NUM) %>%
  filter(n>1)

dir.create('res')
write_excel_csv2(similarLoinc, file="res/similarLoincTerms.csv")
