library(tidyverse)
library(readxl)
library(readr)
library(vroom)
library(rvest)
library(xml2)

assignMicro <- function(loinc){
  bacGenus <- read_csv("data/LPSN/lpsn_gss_2025-03-06.csv", 
                       col_types = cols_only(genus_name = col_character())) |>
    distinct() |>
    rename(genus = genus_name) |>
    mutate(group = "Bacteria")
  
  fungiGenus <- read_html("data/NCBI/fungi.txt") |>
    html_elements(css = "a[title='genus']") |>
    html_text2() |>
    unique()
  
  parasitesGenus <- read_html("https://en.wikipedia.org/wiki/List_of_parasites_of_humans") |>
    html_elements('table.wikitable') |>
    html_table() |>
    bind_rows() |>
    transmute(genus = str_split(`Latin name (sorted)`, '\\s+',
                                simplify = TRUE)[,1]) |>
    mutate(genus = str_remove_all(genus, ':')) |>
    distinct() |>
    mutate(group = "Parasites")
  
  genus <- tibble(genus = fungiGenus) |>
    mutate(group = "Mycology") |>
    distinct() |>
    filter(genus != "Morganella") |>
    bind_rows(bacGenus, parasitesGenus)
  
  loincExclude <- loinc |>
    filter(!is.na(group) | CLASS != 'MICRO')
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    select(-group) |>
    mutate(genus = str_split_i(COMPONENT, '\\.|\\^|/| ', i=1)) |>
    left_join(genus, by='genus') |>
    select(-genus) |>
    mutate(group = if_else(str_detect(COMPONENT, 'Virus|virus|HIV|HTLV'), 
                           'Virus', group)) |>
    mutate(group = if_else(str_detect(COMPONENT, '(B|b)acteria'), 
                           'Bacteria', group)) |>
    mutate(group = if_else(str_detect(COMPONENT, '^Candida'), "Mycology", 
                           group)) |>
    bind_rows(loincExclude)
}

assignPOCT <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  bgaLoincs <- loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    filter(SYSTEM %in% c('BldA', 'BldV', 'BldC', 'BldMV')) |>
    select(COMPONENT, PROPERTY, TIME_ASPCT, SCALE_TYP, METHOD_TYP) |>
    distinct()
  
  # panels <- 
  #   vroom("data/LOINC/2.80/AccessoryFiles/PanelsAndForms/PanelsAndForms.csv",
  #         col_types = cols(.default = col_character()))
  # 
  # bgaPanels <- panels |>
  #   filter(str_detect(ParentName, '(G|g)as '))
  
  bgaBlood <- loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    filter(SYSTEM == 'Bld') |>
    inner_join(bgaLoincs, 
               by = join_by(COMPONENT, PROPERTY, 
                            TIME_ASPCT, SCALE_TYP, METHOD_TYP)) |>
    mutate(group = 'BGA & POCT')
  
  # bgaBlood2 <- loinc |>
  #   filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
  #   filter(!LOINC_NUM %in% bgaBlood$LOINC_NUM) |>
  #   filter(LOINC_NUM %in% bgaPanels$Loinc) |>
  #   filter(SYSTEM == 'Bld') |>
  #   mutate(group = 'BGA & POCT')
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    filter(!LOINC_NUM %in% bgaBlood$LOINC_NUM) |>
    mutate(group = case_when(
      SYSTEM == 'BldA' ~ "BGA & POCT art.",
      SYSTEM == 'BldV' ~ "BGA & POCT ven.",
      SYSTEM == 'BldC' ~ "BGA & POCT cap.",
      SYSTEM == 'BldMV' ~ "BGA & POCT mixed ven.")
    ) |>
      bind_rows(bgaBlood, loincExclude)
      
}

assignHema <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group)) 
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = case_when(
      CLASS == 'HEM/BC' & SYSTEM == 'Bld' ~ 'Blood count',
      CLASS == 'HEM/BC' & (SYSTEM == 'Bone mar' | 
                             SYSTEM == 'Bone marrow') ~ 'Bone marrow',
      CLASS == 'CELLMARK'  ~ 'Immune phenotyping',
      CLASS == 'HEM/BC' & SYSTEM != 'BoneMar' & SYSTEM != 'Bld' ~ 
        'Hematology other')
    ) |>
    bind_rows(loincExclude)
}

assignTransfusion <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group)) 
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = case_when(
      CLASS == "BLDBK" ~ "Immune Hematology",
      CLASS == "HLA" ~ "HLA",
      CLASS == "HPA" ~ "HPA")
    ) |>
    bind_rows(loincExclude)
}

assignAllergy <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group)) 
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = case_when(
      CLASS == 'ALLERGY' & str_detect(COMPONENT, '\\.IgE') ~ 'Allergy IgE',
      CLASS == 'ALLERGY' & str_detect(COMPONENT, '\\.IgG') ~ 'Allergy IgG',
      CLASS == 'ALLERGY' ~ 'Allergy other')
    ) |>
    bind_rows(loincExclude)
}

assignToxoTDM <- function(loinc){
  tdmCurrent <- read_xml("data/HPRA/latestHumanlist.xml") |>
    xml_ns_strip() |>
    xml_find_all('//ActiveSubstance') |>
    xml_text() |>
    str_split_i(' ', 1) |>
    tolower() |>
    unique()
  
  tdmOut <- read_xml("data/HPRA/withdrawnHumanlist.xml") |>
    xml_ns_strip() |>
    xml_find_all('//ActiveSubstance') |>
    xml_text() |>
    str_split_i(' ', 1) |>
    tolower() |>
    unique()
  
  loincExclude <- loinc |>
    filter(!is.na(group) | CLASS != 'DRUG/TOX') 
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(baseComponent = str_split_i(COMPONENT, '\\.|\\^|/| ', i=1)) |>
    mutate(group = case_when(
      baseComponent %in% c(tdmOut, tdmCurrent) ~ 'TDM',
      str_detect(COMPONENT, 'trough|peak') ~ 'TDM',
      .default = 'Tox')
    ) |>
    bind_rows(loincExclude)

}

assignHormones <- function(loinc){
  hormones <- read_html("https://en.wikipedia.org/wiki/List_of_human_hormones") |>
    html_elements('table.wikitable') |>
    html_table() |>
    bind_rows() |>
    mutate(name2 = str_remove_all(Name, '\\(.+\\)')) |>
    mutate(name2 = if_else(str_detect(name2, 'Somatostatin'),
                           'Somatostatin', name2)) |>
    mutate(name2 = if_else(str_detect(name2, 'Insulin'),
                           'Insulin', name2)) |>
    mutate(name2 = if_else(str_detect(name2, 'Angiotensinogen'),
                           'Angiotensinogen', name2)) |>
    mutate(name2 = str_trim(name2)) |>
    filter(!is.na(name2))

  
  elgaRaw <- read_excel(
    "data/ELGA/ValueSet-elga-laborparameter.1.propcsv.xlsx", skip = 3)
  
  hormones2 <- elgaRaw |>
    filter(parent == '06330')
  
  hormones3 <- loinc |>
    filter(LOINC_NUM %in% hormones2$code)
  
  pattern <- str_c(c(hormones$name2, "Angiotensin", hormones3$COMPONENT), 
                   '', collapse = '|')
  
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  res <- loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = if_else(str_detect(COMPONENT, pattern), 
                           'hormones', group)) |>
    bind_rows(loincExclude)
    
}

assignVitamins <- function(loinc){
  elgaRaw <- read_excel(
    "data/ELGA/ValueSet-elga-laborparameter.1.propcsv.xlsx", skip = 3)
  
  vitaminsElga <- elgaRaw |>
    filter(parent == '06340')
  
  vitamins <- loinc |>
    filter(LOINC_NUM %in% vitaminsElga$code)
  
  pattern <- str_c(vitamins$COMPONENT, '', collapse = '|')
  
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = if_else(str_detect(COMPONENT, pattern), 
                           'vitamins', group)) |>
    bind_rows(loincExclude)
  
}

assignChem <- function(loinc){
  elgaRaw <- read_excel(
    "data/ELGA/ValueSet-elga-laborparameter.1.propcsv.xlsx", skip = 3)
  
  proteinsElga <- elgaRaw |>
    filter(parent == '05190')
  
  loinc <- loinc |>
    mutate(baseComponent = str_split_i(COMPONENT, '\\.|\\^|/', 1))
  
  proteins <- loinc |>
    filter(LOINC_NUM %in% proteinsElga$code) %>%
    pull(baseComponent) |>
    unique()
  
  traceElementsElga <- elgaRaw |>
    filter(parent == '05200')
  
  traceElements <- loinc |>
    filter(LOINC_NUM %in% traceElementsElga$code) %>%
    pull(baseComponent) |>
    unique()
  
  chemBase <- loinc |>
    filter(CLASS == 'CHEM') |>
    pull(baseComponent) |>
    unique()
  
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = case_when(
      CLASS == 'CHEM' & SYSTEM %in% c('Body fld', 'Dial fld prt', 
                                      'Periton fld', 'Plr fld', 'Dial fld', 
                                      'Saliva', 'Hair', 'Semen', 'Amnio fld') ~
        'extra mat',
      str_detect(SYSTEM, 'Ser|Plas|Bld') & baseComponent %in% traceElements ~ 
        'trace elememts',
      str_detect(SYSTEM, 'Ser|Plas|Bld') & baseComponent %in% proteins ~ 
        'proteins',
      str_detect(SYSTEM, 'Ser|Plas|Bld') & baseComponent %in% chemBase ~ 
        'chemistry',
      .default = group
    )) |>
    bind_rows(loincExclude)
}

assignMats <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = case_when(
      CLASS != 'MICRO' & SYSTEM == 'Urine' ~ 'Urine',
      CLASS != 'MICRO' & SYSTEM == 'Urine sed' ~ 'Urine sediment',
      CLASS != 'MICRO' & SYSTEM == 'CSF' ~ 'CSF',
      CLASS != 'MICRO' & SYSTEM == 'Stool' ~ 'Stool',
      .default = group
    )) |>
    bind_rows(loincExclude)
}

assignSero <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = if_else(CLASS == 'SERO', 'Sero',  group)) |>
    bind_rows(loincExclude)
}

assignGenetics <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = if_else(str_detect(CLASS, 'MOLPATH'), 'Genetics',  group)) |>
    bind_rows(loincExclude)
}

assignCoagulation <- function(loinc){
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = if_else(str_detect(CLASS, 'COAG'), 'coagulation',  group)) |>
    bind_rows(loincExclude)
}


loincRaw <-  vroom("data/LOINC/2.80/LoincTable/Loinc.csv", 
                col_types = cols(.default = col_character())) %>%
  filter(CLASSTYPE == 1) %>%  # https://simplifier.net/hl7-oo/laboratory-tests-and-panels-uv-ips-duplicate-3
  filter(!CLASS %in% c("CHALSKIN", "H&P.HX.LAB", "H&P.HX", "NR STATS")) %>%
  filter(!CLASS %in% c("ABXBACT")) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(COMPONENT,'Bacteria identified')) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(CLASS, "PATH\\.PROTOCOLS\\.")) %>%
  filter(STATUS == "ACTIVE") %>%
  filter(ORDER_OBS != "Order") |> # nur Codes zur Darstellung
  mutate(group=NA_character_)

loincAll <-  vroom("data/LOINC/2.80/LoincTable/Loinc.csv", 
                               col_types = cols(.default = col_character())) %>%
  filter(CLASSTYPE == 1) %>%  # https://simplifier.net/hl7-oo/laboratory-tests-and-panels-uv-ips-duplicate-3
  filter(!CLASS %in% c("CHALSKIN", "H&P.HX.LAB", "H&P.HX", "NR STATS")) %>%
  filter(!CLASS %in% c("ABXBACT")) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(COMPONENT,'Bacteria identified')) %>% # Bakt nicht für MIO-Laborbefund
  filter(!str_detect(CLASS, "PATH\\.PROTOCOLS\\.")) %>%
  filter(STATUS == "ACTIVE")

loincSorted <- assignMicro(loincRaw) |>
  assignHema() |>
  assignAllergy() |>
  assignTransfusion() |>
  assignPOCT() |>
  assignVitamins() |>
  assignHormones() |>
  assignToxoTDM() |>
  assignChem() |>
  assignMats() |>
  assignSero() |>
  assignGenetics()


sortedCount <- count(loincSorted, group)
unsorted <- loincSorted |> 
  filter(is.na(group))
unsortedCount <- unsorted |> 
  count(CLASS)


