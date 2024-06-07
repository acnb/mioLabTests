library(tidyverse)
library(readxl)

version <- "2.74"

loinc <-  read.csv(paste0("data/LOINC/", version, "/LoincTable/Loinc.csv")) %>%
  filter(CLASSTYPE == 1) %>%  # https://simplifier.net/hl7-oo/laboratory-tests-and-panels-uv-ips-duplicate-3
  filter(!CLASS %in% c("CHALSKIN", "H&P.HX.LAB", "H&P.HX", "NR STATS")) %>%
  filter(!CLASS %in% c("ABXBACT")) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(COMPONENT,'Bacteria identified')) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(CLASS, "PATH\\.PROTOCOLS\\.")) %>%
  filter(STATUS == "ACTIVE") %>%
  filter(ORDER_OBS != "Order") %>% # nur Codes zur Darstellung
  filter(CLASS != "DRUGDOSE") # no dosage on laboratory report


loinc_de <- read.csv(
  paste0("data/LOINC/",  version,
         "/AccessoryFiles/LinguisticVariants/deDE15LinguisticVariant.csv")) %>%
  inner_join(loinc, by='LOINC_NUM', suffix=c('.de', ''))