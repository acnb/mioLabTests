# Können die Kategorien in ELGA für die deutsche MIO verwendet werden?
# nach Mögichkeit soll algorithmisch vorgegangen werden und keine manuellen
# Enstcheidungen geftroffen werden.
# Achtung, quick und dirty

library(tidyverse)
library(readxl)
library(data.tree)

source("readLoincTables.R")

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

source("adjustElga.R")

# gleiche Component und System (Analyt und Material) ----
# sollten im Laborbefund an gleicher Stelle angeordnet sein

# at_fields <- elga_lab %>%
#   select(code, display, parent) %>%
#   inner_join(loinc, by=c('code' = 'LOINC_NUM'))
# 
# de_fields <- loinc_de %>%
#   filter(!LOINC_NUM %in% at_fields$code) %>% # schon in ELGA enthalten
#   select(LOINC_NUM, LONG_COMMON_NAME, CLASS) %>%
#   inner_join(loinc, by="LOINC_NUM")
# 
# combined <- de_fields %>%
#   inner_join(at_fields, by=c("COMPONENT", "SYSTEM"))

# gleiche Component und ähnliches System (Analyt und Material) ----
# sollten im Laborbefund an gleicher Stelle angeordnet sein

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

POCTparent <- c(200, 02060, 02070, 02080, 02090, 02100, 02110, 
                02120, 02130, 13730, 13730)

# de_fields_sim_sys <- de_fields %>%
#   filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
#   inner_join(similar_systems, by="SYSTEM")
# 
# at_fields_sim_sys <- elga_lab %>%
#   select(code, display, parent) %>%
#   inner_join(loinc, by=c('code' = 'LOINC_NUM')) %>%
#   inner_join(similar_systems, by="SYSTEM")
#   
# combined_sim_sys <- de_fields_sim_sys %>%
#   inner_join(at_fields_sim_sys, by=c("COMPONENT", "SYSTEM_GROUP"))

# bei Mikrobio Codes reicht die gleiche Componenente aus ----

manualCategories <- loinc %>%
  mutate(manualParent = case_when(
    str_detect(COMPONENT, '^Hemoglobin ') & CLASS == "HEM/BC" ~ "03050",
    LOINC_NUM == "777-3" ~ "03010",
    LOINC_NUM == "30364-4" ~ "03010", # FC Lymphos zu BB
    LOINC_NUM == "30365-1" ~ "03010",  # FC Lymphos zu BB
    LOINC_NUM == "31590-3" ~ "11370",  # Ribosomale-AK ql.IF zu Kollagneosen
    LOINC_NUM == "55315-6" ~ "10800",  # Alle Hefen zu Mykologie
    LOINC_NUM == "63484-0" ~ "11390",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "25139-7" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "25994-5" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "25122-3" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "22707-4" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26574-4" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26919-1" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26576-9" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26577-7" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26578-5" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "25086-0" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26580-1" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26581-9" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "25989-5" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "26601-5" ~ "05220", # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "54356-1" ~ "13750",  # F-Aktin-AK zu Lebererkrankung 
    LOINC_NUM == "72523-4" ~ "11450",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "54153-2" ~ "11370",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "53007-1" ~ "11380",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "53008-9" ~ "11380",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "10885-2" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "11225-0" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "56970-7" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "56974-9" ~ "15770",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "14252-1" ~ "11390",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "18253-5" ~ "06330",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "25133-0" ~ "05220",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "63333-9" ~ "11430",  # F-Aktin-AK zu Lebererkrankung
    LOINC_NUM == "43189-0" ~ "11430"  # F-Aktin-AK zu Lebererkrankung
  )) %>%
  select(LOINC_NUM, manualParent)


# categories from ELGA
categoryStep0  <- loinc %>%
  left_join(similarSystem, by="SYSTEM") %>%
  left_join(similarProperty, by="PROPERTY") %>%
  left_join(elgaLabAdjusted %>% select(code, parent),
            by=c("LOINC_NUM" = "code")) %>%
  left_join(manualCategories, join_by("LOINC_NUM")) %>%
  mutate(SYSTEM_GROUP = if_else(is.na(SYSTEM_GROUP), 
                                SYSTEM,
                                SYSTEM_GROUP)) %>%
  mutate(PROPERTY_GROUP = if_else(is.na(PROPERTY_GROUP), 
                                  PROPERTY,
                                  PROPERTY_GROUP)) %>%
  mutate(categoryDeterminedBy = if_else(is.na(parent),
                                        NA_character_,
                                        'ELGA')) %>%
  mutate(categoryDeterminedBy = if_else(is.na(manualParent),
                                        categoryDeterminedBy,
                                        'manual')) %>%
  mutate(parent = if_else(is.na(manualParent),
                          parent, manualParent)) %>%
  mutate(SYSTEM_GROUP = if_else(is.na(SYSTEM_GROUP), 
                                SYSTEM,
                                SYSTEM_GROUP)) %>%
  mutate(isPOCT = parent %in% POCTparent) %>%
  mutate(analyte1Part = str_remove(COMPONENT, "\\..+"),
         methodPart1 = str_remove(METHOD_TYP, "\\..+")) %>%
  select(-manualParent)
  




# categories for the same analytes with differnt unit
prepareCategoryStep1 <- categoryStep0 %>%
  group_by(COMPONENT, SYSTEM, TIME_ASPCT, METHOD_TYP, SCALE_TYP, 
           PROPERTY_GROUP) %>%
  summarise(parentList = list(unique(parent)), uid = cur_group_id(),
            nParents = length(unique(na.omit(parent))),
            LOINC_NUM = list(LOINC_NUM[!is.na(parent)])) %>%
  ungroup()

errorsStep1 <- prepareCategoryStep1 %>%
  filter(nParents > 1) %>%
  unnest(LOINC_NUM) %>%
  left_join(elga_lab, by=c("LOINC_NUM" = 'code')) %>%
  left_join(elga_lab %>% transmute(code, category=display),
            by=c("parent" = 'code'))
  
newCategoriesStep1 <- prepareCategoryStep1 %>%
  filter(nParents == 1) %>%
  unnest(parentList) %>%
  filter(!is.na(parentList)) %>%
  transmute(COMPONENT, SYSTEM, TIME_ASPCT, METHOD_TYP, SCALE_TYP, 
            PROPERTY_GROUP, newParent = parentList)
            
categoryStep1 <- categoryStep0 %>%
  left_join(newCategoriesStep1, 
            by = join_by(COMPONENT, SYSTEM, TIME_ASPCT, METHOD_TYP, 
                         SCALE_TYP, PROPERTY_GROUP)) %>%
  mutate(categoryDeterminedBy = if_else(is.na(parent) & !is.na(newParent),
                                       'step1',
                                       categoryDeterminedBy),
         parent = if_else(is.na(parent) & !is.na(newParent),
                          newParent,
                          parent)) %>%
  select(-newParent)



# categories for the same analytes with different unit regardless of method
# only POCT can have a different category

prepareCategoryStep2a <- categoryStep1 %>%
  group_by(COMPONENT, SYSTEM, TIME_ASPCT, SCALE_TYP, 
           PROPERTY_GROUP, methodPart1) %>%
  summarise(parentList = list(unique(parent)), uid = cur_group_id(),
            hasNA = anyNA(parent),
            nParents = length(unique(na.omit(parent))),
            LOINC_NUM = list(LOINC_NUM),
            contaisPOCT = any(isPOCT)) %>%
  ungroup()

errorsStep2a <- prepareCategoryStep2a %>%
  select(LOINC_NUM, nParents, parentList, uid, hasNA) %>%
  filter(nParents > 1 & hasNA) %>%
  unnest(LOINC_NUM) %>%
  left_join(elga_lab, by=c("LOINC_NUM" = 'code')) %>%
  left_join(elga_lab %>% transmute(code, category=display),
            by=c("parent" = 'code')) %>%
  left_join(loinc, by = join_by(LOINC_NUM))

newCategoriesStep2a <- prepareCategoryStep2a %>%
  filter(nParents == 1 & contaisPOCT)  %>%
  unnest(parentList) %>%
  filter(!is.na(parentList)) %>%
  transmute(COMPONENT, SYSTEM, TIME_ASPCT, methodPart1, SCALE_TYP, 
            PROPERTY_GROUP, newParent = parentList)

categoryStep2a <- categoryStep1 %>%
  left_join(newCategoriesStep2a, 
            by = join_by(COMPONENT, SYSTEM, TIME_ASPCT, methodPart1, 
                         SCALE_TYP, PROPERTY_GROUP)) %>%
  mutate(categoryDeterminedBy = if_else(is.na(parent) & !is.na(newParent),
                                        'step2a',
                                        categoryDeterminedBy),
         parent = if_else(is.na(parent) & !is.na(newParent),
                          newParent,
                          parent)) %>%
  mutate(isPOCT = parent %in% POCTparent) %>%
  select(-newParent)


prepareCategoryStep2b <- categoryStep2a %>%
  filter(!isPOCT) %>%
  filter(METHOD_TYP != "Screen") %>%
  filter(!(SCALE_TYP == "Nom" & CLASS %in% c("MICRO", "CYTO", "PATH"))) %>%
  group_by(COMPONENT, SYSTEM, TIME_ASPCT, SCALE_TYP, 
           PROPERTY_GROUP) %>%
  summarise(parentList = list(unique(parent)), uid = cur_group_id(),
            hasNA = anyNA(parent),
            nParents = length(unique(na.omit(parent))),
            LOINC_NUM = list(LOINC_NUM)) %>%
  ungroup()

errorsStep2b <- prepareCategoryStep2b %>%
  select(LOINC_NUM, nParents, parentList, uid, hasNA) %>%
  filter(nParents > 1 & hasNA) %>%
  unnest(LOINC_NUM) %>%
  left_join(elga_lab, by=c("LOINC_NUM" = 'code')) %>%
  left_join(elga_lab %>% transmute(code, category=display),
            by=c("parent" = 'code')) %>%
  left_join(loinc, by = join_by(LOINC_NUM)) 

newCategoriesStep2b <- prepareCategoryStep2b %>%
  filter(nParents == 1)  %>%
  unnest(parentList) %>%
  filter(!is.na(parentList)) %>%
  transmute(COMPONENT, SYSTEM, TIME_ASPCT, SCALE_TYP, 
            PROPERTY_GROUP, newParent = parentList)

categoryStep2b <- categoryStep2a %>%
  left_join(newCategoriesStep2b, 
            by = join_by(COMPONENT, SYSTEM, TIME_ASPCT,  
                         SCALE_TYP, PROPERTY_GROUP)) %>%
  mutate(categoryDeterminedBy = if_else(is.na(parent) & !is.na(newParent),
                                        'step2b',
                                        categoryDeterminedBy),
         parent = if_else(is.na(parent) & !is.na(newParent),
                          newParent,
                          parent)) %>%
  mutate(isPOCT = parent %in% POCTparent) %>%
  select(-newParent)

# same category for same measurand !

prepareCategoryStep3 <- categoryStep2b %>%
  filter(!isPOCT) %>%
  filter(METHOD_TYP != "Screen") %>%
  filter(!(SCALE_TYP == "Nom" & CLASS %in% c("MICRO", "CYTO", "PATH"))) %>%
  group_by(COMPONENT, SYSTEM_GROUP) %>%
  summarise(parentList = list(unique(parent)), uid = cur_group_id(),
            hasNA = anyNA(parent),
            nParents = length(unique(na.omit(parent))),
            LOINC_NUM = list(LOINC_NUM)) %>%
  ungroup()

errorsStep3 <- prepareCategoryStep3 %>%
  select(LOINC_NUM, nParents, parentList, uid, hasNA) %>%
  filter(nParents > 1 & hasNA) %>%
  unnest(LOINC_NUM) %>%
  left_join(elga_lab, by=c("LOINC_NUM" = 'code')) %>%
  left_join(elga_lab %>% transmute(code, category=display),
            by=c("parent" = 'code')) %>%
  left_join(loinc, by = join_by(LOINC_NUM)) %>%
  select(-starts_with("designation"))

newCategoriesStep3 <- prepareCategoryStep3 %>%
  filter(nParents == 1)  %>%
  unnest(parentList) %>%
  filter(!is.na(parentList)) %>%
  transmute(COMPONENT, SYSTEM_GROUP, newParent = parentList)

categoryStep3 <- categoryStep2b %>%
  left_join(newCategoriesStep3, 
            by = join_by(COMPONENT, SYSTEM_GROUP)) %>%
  mutate(categoryDeterminedBy = if_else(is.na(parent) & !is.na(newParent),
                                        'step3',
                                        categoryDeterminedBy),
         parent = if_else(is.na(parent) & !is.na(newParent),
                          newParent,
                          parent)) %>%
  mutate(isPOCT = parent %in% POCTparent) %>%
  select(-newParent)
###
  
  
#   mutate(parentList = if_else(!is.na(categoryDeterminedBy) & 
#                                 length(as.list(unique(na.omit(parent)))) > 0, 
#                               as.list(unique(parent)),
#                               parentList),
#          .by=c(COMPONENT, SYSTEM, TIME_ASPCT, METHOD_TYP, SCALE_TYP, 
#                PROPERTY_GROUP))
# 
# 
# #%>%
#   summarise()
#   group_by(SYSTEM_GROUP, analyte1Part) %>%
#   mutate(nParent = n_distinct(parent, na.rm = TRUE),
#          GROUP_ID = cur_group_id())
# 
# 
# multipleCategories <- relatedLOINC %>%
#   filter(nParent > 1) %>%
#   left_join(elga_lab %>% select(code, display),
#             by=c("parent" = "code")) %>%
#   arrange(GROUP_ID)
# 
#   
#   
# group_by(COMPONENT, TIME_ASPCT, SYSTEM_GROUP, SCALE_TYP) %>%
#   summarise(LOINC_NUM = list(LOINC_NUM), 
#             LONG_COMMON_NAME = list(LONG_COMMON_NAME),
#             PROPERTY = list(PROPERTY),
#             CLASS = list(CLASS),
#             n=n()) %>%
#   ungroup() %>%
#   filter(n > 1) %>%
#   mutate(GROUP_UID = 1:n()) %>%
#   unnest(cols = c(LOINC_NUM, LONG_COMMON_NAME, PROPERTY, CLASS)) %>%
#   select(GROUP_UID, LONG_COMMON_NAME, LOINC_NUM, COMPONENT, TIME_ASPCT, 
#          SYSTEM_GROUP, SCALE_TYP, PROPERTY, SIMILAR_PROP, CLASS)
# 
# 
# combined_MiBi <- de_fields %>%
#   filter(!LOINC_NUM %in% at_fields$code) %>% # schon in ELGA enthalten
#   filter(!LOINC_NUM %in% combined$LOINC_NUM) %>% # schon zugeordnet
#   filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
#   filter(CLASS == "Mikrobiologie") %>%
#   filter(COMPONENT != "Microscopic observation") %>%
#   inner_join(at_fields %>% mutate(SYSTEM = NULL) %>% distinct(), 
#              by=c("COMPONENT")) 
# 
# 
# # bei Allergiediagnotik reicht das Allergen und Antikörperklasse aus
# 
# de_fields_allergie <- de_fields %>%
#   filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_MiBi$LOINC_NUM) %>%
#   filter(CLASS == "Allergie") %>%
#   mutate(COMPONENT_ALLERGIE = str_remove(COMPONENT, ".RAST class"))
# 
# at_fields_allergie <- elga_lab %>%
#   select(code, display, parent) %>%
#   inner_join(loinc, by=c('code' = 'LOINC_NUM')) %>%
#   mutate(COMPONENT_ALLERGIE = str_remove(COMPONENT, ".RAST class"))
# 
# combined_allergie <- de_fields_allergie %>%
#   inner_join(at_fields_allergie, by="COMPONENT_ALLERGIE")
# 
# # Reste ... ----
# 
# combined_reste <- de_fields %>%
#   filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_MiBi$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_allergie$LOINC_NUM) %>%
#   mutate(parent = "") %>%
#   # Virologie
#   mutate(parent = if_else(CLASS == "Mikrobiologie" & 
#                    str_detect(COMPONENT, 'virus'), '10780', parent)) %>% 
#   # IgG Allergene
#   mutate(parent = if_else(CLASS == "Allergie" & 
#                             str_detect(COMPONENT, 'IgG'), '12090', parent)) %>%
#   #klin. Chemie (ungenau?)
#   mutate(parent = if_else(CLASS == "Klinische Chemie (keine Funktionstests)" & 
#                             SYSTEM %in% similar_systems$SYSTEM, 
#                           '05180', parent)) %>% 
#   # Urindiagnostik (ungenau?)
#   mutate(parent = if_else(CLASS == "Klinische Chemie (keine Funktionstests)" & 
#                             SYSTEM == "Urine", 
#                           '13750', parent)) %>% 
#   filter(parent != '')
# 
# # Test, was lässt sich nicht zuordnen
# 
# not_combined <- de_fields %>%
#   filter(!LOINC_NUM %in% combined$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_sim_sys$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_MiBi$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_allergie$LOINC_NUM) %>%
#   filter(!LOINC_NUM %in% combined_reste$LOINC_NUM) 
# 
# cl_nc <- not_combined %>%
#   count(CLASS)
# 
# # Darstellung ----
# 
# head <- Node$new("head")
# 
# de_lab <- categoryStep3 %>%
#   transmute(parent = parent, code = LOINC_NUM, display = LONG_COMMON_NAME,
#             unique_display = str_c('DE', display, code, sep = ' - '))
# 
# multiple_parents_de <- de_lab %>%
#   group_by(code, unique_display) %>%
#   summarise(nd = n_distinct(parent)) %>%
#   filter(nd != 1)
# 
# de_lab <- de_lab %>%
#   filter(!code %in% multiple_parents_de$code) %>%
#   unique()
# 
# 
# all_lab <- elga_lab %>%
#   select(parent, code, display) %>%
#   mutate(parent = replace_na(parent, 'head'), 
#          unique_display = str_c(display, code, sep = ' - ')) %>%
#   bind_rows(de_lab) %>%
#   add_row(display = 'head', code = 'head', unique_display = 'head')
# 
# 
# res <- addAllChildren(all_lab, head)
# 
# 
# capture.output(print(res, pruneMethod = 'dist'), 
#                file="res/elga_mit_de.txt")
# 
# capture.output(print(res, pruneMethod = NULL), 
#                file="res/elgaLong.txt")
# 
# 
# codes_und_kategorien <- all_lab %>%
#   left_join(all_lab, by =c('parent' = 'code'), suffix = c('', '.y')) %>%
#   arrange(parent) %>%
#   transmute(kategorie = display.y, code, unique_display)
# 
# 
# write_excel_csv2(codes_und_kategorien, file = 'res/codeUndKategorien.csv')