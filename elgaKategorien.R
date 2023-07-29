# Können die Kategorien in ELGA für die deutsche MIO verwendet werden?
# nach Mögichkeit soll algorithmisch vorgegangen werden und keine manuellen
# Enstcheidungen geftroffen werden.
# Achtung, quick und dirty

library(tidyverse)
library(readxl)
library(data.tree)


addAllChildren <- function(data, parent){
  pn <- parent$name[1]
  
  message(pn)
  
  headCode <- data %>%
    filter(unique_display == pn) %>%
    pull('code')
  
  children <- data %>% 
    filter(parent == headCode)
  if (nrow(children) > 0){
    apply(children, 1, function(child){
      ch <- parent$AddChild(child['unique_display'])
      addAllChildren(data, ch)
    })
  }
  parent
}

elga_lab <- read_excel("data/ELGA/ValueSet-elga-laborparameter.1.propcsv.xlsx", skip = 2)
elga_structure <- read_excel("data/ELGA/ValueSet-elga-laborstruktur.1.propcsv.xlsx")

loinc <-  read.csv("data/LOINC/2.74/LoincTable/Loinc.csv") %>%
  filter(CLASSTYPE == 1) %>%  # https://simplifier.net/hl7-oo/laboratory-tests-and-panels-uv-ips-duplicate-3
  filter(!CLASS %in% c("CHALSKIN", "H&P.HX.LAB", "H&P.HX", "NR STATS")) %>%
  filter(!str_detect(CLASS, "PATH\\.PROTOCOLS\\.")) %>%
  filter(STATUS == "ACTIVE") %>%
  filter(ORDER_OBS != "Order") %>% # nur Codes zur Darstellung
  select(LOINC_NUM, COMPONENT, SYSTEM) 

loinc_de <- read.csv(
  "data/LOINC/2.74/AccessoryFiles/LinguisticVariants/deDE15LinguisticVariant.csv") %>%
  filter(LOINC_NUM %in% loinc$LOINC_NUM)


# gleiche Component und System (Analyt und Material) ----
# sollten im Laborbefund an gleicher Stelle angeordnet sein

at_fields <- elga_lab %>%
  select(code, display, parent) %>%
  inner_join(loinc, by=c('code' = 'LOINC_NUM'))

de_fields <- loinc_de %>%
  filter(!LOINC_NUM %in% at_fields$code) %>% # schon in ELGA enthalten
  select(LOINC_NUM, LONG_COMMON_NAME, CLASS) %>%
  inner_join(loinc, by="LOINC_NUM")

combined <- de_fields %>%
  inner_join(at_fields, by=c("COMPONENT", "SYSTEM"))

# gleiche Component und ähnliches System (Analyt und Material) ----
# sollten im Laborbefund an gleicher Stelle angeordnet sein

similar_systems <-tribble(
  ~SYSTEM_GROUP, ~SYSTEM,
  'Blut', c("Ser", "Ser/Plas", "Bld", "Plas", "Ser/Plas/Bld", 
            "Ser/Bld", "Ser+Bld"),
  
) %>%
  unnest(cols = c(SYSTEM))

de_fields_sim_sys <- de_fields %>%
  filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
  inner_join(similar_systems, by="SYSTEM")

at_fields_sim_sys <- elga_lab %>%
  select(code, display, parent) %>%
  inner_join(loinc, by=c('code' = 'LOINC_NUM')) %>%
  inner_join(similar_systems, by="SYSTEM")
  
combined_sim_sys <- de_fields_sim_sys %>%
  inner_join(at_fields_sim_sys, by=c("COMPONENT", "SYSTEM_GROUP"))

# bei Mikrobio Codes reicht die gleiche Componenente aus ----


combined_MiBi <- de_fields %>%
  filter(!LOINC_NUM %in% at_fields$code) %>% # schon in ELGA enthalten
  filter(!LOINC_NUM %in% combined$LOINC_NUM) %>% # schon zugeordnet
  filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
  filter(CLASS == "Mikrobiologie") %>%
  filter(COMPONENT != "Microscopic observation") %>%
  inner_join(at_fields %>% mutate(SYSTEM = NULL) %>% distinct(), 
             by=c("COMPONENT")) 


# bei Allergiediagnotik reicht das Allergen und Antikörperklasse aus

de_fields_allergie <- de_fields %>%
  filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_MiBi$LOINC_NUM) %>%
  filter(CLASS == "Allergie") %>%
  mutate(COMPONENT_ALLERGIE = str_remove(COMPONENT, ".RAST class"))

at_fields_allergie <- elga_lab %>%
  select(code, display, parent) %>%
  inner_join(loinc, by=c('code' = 'LOINC_NUM')) %>%
  mutate(COMPONENT_ALLERGIE = str_remove(COMPONENT, ".RAST class"))

combined_allergie <- de_fields_allergie %>%
  inner_join(at_fields_allergie, by="COMPONENT_ALLERGIE")

# Reste ... ----

combined_reste <- de_fields %>%
  filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_MiBi$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_allergie$LOINC_NUM) %>%
  mutate(parent = "") %>%
  # Virologie
  mutate(parent = if_else(CLASS == "Mikrobiologie" & 
                   str_detect(COMPONENT, 'virus'), '10780', parent)) %>% 
  # IgG Allergene
  mutate(parent = if_else(CLASS == "Allergie" & 
                            str_detect(COMPONENT, 'IgG'), '12090', parent)) %>%
  #klin. Chemie (ungenau?)
  mutate(parent = if_else(CLASS == "Klinische Chemie (keine Funktionstests)" & 
                            SYSTEM %in% similar_systems$SYSTEM, 
                          '05180', parent)) %>% 
  # Urindiagnostik (ungenau?)
  mutate(parent = if_else(CLASS == "Klinische Chemie (keine Funktionstests)" & 
                            SYSTEM == "Urine", 
                          '13750', parent)) %>% 
  filter(parent != '')

# Test, was lässt sich nicht zuordnen

not_combined <- de_fields %>%
  filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_MiBi$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_allergie$LOINC_NUM) %>%
  filter(!LOINC_NUM %in% combined_reste$LOINC_NUM) 

cl_nc <- not_combined %>%
  count(CLASS)

# Darstellung ----

head <- Node$new("head")

de_lab <- bind_rows(combined, combined_sim_sys, combined_MiBi,
                    combined_allergie, combined_reste) %>%
  transmute(parent = parent, code = LOINC_NUM, display = LONG_COMMON_NAME,
            unique_display = str_c('DE', display, code, sep = ' - '))

multiple_parents_de <- de_lab %>%
  group_by(code, unique_display) %>%
  summarise(nd = n_distinct(parent)) %>%
  filter(nd != 1)

de_lab <- de_lab %>%
  filter(!code %in% multiple_parents_de$code) %>%
  unique()


all_lab <- elga_lab %>%
  select(parent, code, display) %>%
  mutate(parent = replace_na(parent, 'head'), 
         unique_display = str_c(display, code, sep = ' - ')) %>%
  bind_rows(de_lab) %>%
  add_row(display = 'head', code = 'head', unique_display = 'head')


res <- addAllChildren(all_lab, head)

capture.output(print(res, pruneMethod = 'dist'), 
               file="res/elga_mit_de.txt")

capture.output(print(res, pruneMethod = NULL), 
               file="res/elgaLong.txt")


codes_und_kategorien <- all_lab %>%
  left_join(all_lab, by =c('parent' = 'code'), suffix = c('', '.y')) %>%
  arrange(parent) %>%
  transmute(kategorie = display.y, code, unique_display)


write_excel_csv2(codes_und_kategorien, file = 'rse/codeUndKategorien.csv')