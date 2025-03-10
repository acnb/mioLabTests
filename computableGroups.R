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
  
  bgaBlood <- loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    filter(SYSTEM == 'Bld') |>
    inner_join(bgaLoincs, 
               by = join_by(COMPONENT, PROPERTY, 
                            TIME_ASPCT, SCALE_TYP, METHOD_TYP)) |>
    mutate(group = 'BGA & POCT')
  
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

applyHormones <- function(loinc){
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

applyVitamins <- function(loinc){

  pattern <- str_c(c(hormones$name2, "Angiotensin"), '', collapse = '|')
  
  elgaRaw <- read_excel(
    "data/ELGA/ValueSet-elga-laborparameter.1.propcsv.xlsx", skip = 3)
  
  vitaminsElga <- elgaRaw |>
    filter(parent == '06340')
  
  vitamins <- loinc |>
    filter(LOINC_NUM %in% vitaminsElga$code)
  
  pattern <- str_c(vitamins$COMPONENT, '', collapse = '|')
  
  loincExclude <- loinc |>
    filter(!is.na(group))
  
  res <- loinc |>
    filter(!LOINC_NUM %in% loincExclude$LOINC_NUM) |>
    mutate(group = if_else(str_detect(COMPONENT, pattern), 
                           'vitamins', group)) |>
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



loincSorted <- assignMicro(loincRaw) |>
  assignHema() |>
  assignAllergy() |>
  assignTransfusion() |>
  assignPOCT()


sortedCount <- count(loincSorted, group)
unsorted <- loincSorted |> 
  filter(is.na(parent))
unsortedCount <- loincSorted |> 
  filter(is.na(parent))|> 
  count(, CLASS)

### medis

medis <- read_html("https://www.thieme-connect.de/products/ejournals/html/10.1055/s-0043-116492") |>
  html_table()[[4]] 
  

elgaRaw <- read_excel("data/ELGA/ValueSet-elga-laborparameter.1.propcsv.xlsx", skip = 3)
elga_structure <- read_excel("data/ELGA/ValueSet-elga-laborstruktur.1.propcsv.xlsx")

#parts <- vroom("data/LOINC/2.80/AccessoryFiles/PartFile/Part.csv")
#partLink <- vroom("data/LOINC/2.80/AccessoryFiles/PartFile/LoincPartLink_Primary.csv")

elgaCodes <- loincRaw |>
  inner_join(elgaRaw |> 
               filter(parent %in% c("08260", '08270', '08280', '08290', '08300',
                                    '08310', '08330')) |>
               select(code, parent), 
             by = join_by(LOINC_NUM == code)) |>
  mutate(baseComponent = str_split(COMPONENT, '\\.|\\^|/', simplify = TRUE)[,1]) |>
  mutate(baseComponent = str_remove_all(baseComponent, ' DNA| RNA| Ag| Ab')) |>
  select(baseComponent, parent) |>
  mutate(parent = if_else(parent %in% c('08280', '08290'), '08285', parent)) |>
  distinct() 
  
#   filter(baseComponent != "Entamoeba histolytica" & parent != '10790') |>
#   filter(baseComponent != 'Observation') |>
#   filter(baseComponent != 'Parainfluenza virus 1+2+3+4' & parent != '10790')

elgaCodes |> 
  count(baseComponent) |>
  filter(n > 1) 


loincSorted <- 



  mutate(baseComponent = str_remove_all(baseComponent, ' DNA| RNA| Ag| Ab')) |>
  left_join(elgaCodes, by='bakGenus') |>
  |>
  mutate(baseComponent = str_remove_all(baseComponent, ' DNA| RNA| Ag| Ab')) |>
  left_join(elgaCodes, by='baseComponent') |>
  mutate(parent = case_when(
    !is.na(parent) ~ parent,
   
    SYSTEM == 'BldA' ~ "02060",
    SYSTEM == 'BldV' ~ "02090",
    SYSTEM == 'BldC' ~ "02100",
    SYSTEM == 'BldMV' ~ "02120",
    CLASS == 'HEM/BC' & SYSTEM == 'Bld' ~ '03010',
    CLASS == 'HEM/BC' & (SYSTEM == 'Bone mar' | 
                           SYSTEM == 'Bone marrow') ~ '03020',
    CLASS == 'CELLMARK'  ~ '03030',
    CLASS == 'HEM/BC' & SYSTEM != 'BoneMar' & SYSTEM != 'Bld' ~ '03050',
    CLASS == 'ALLERGY' & str_detect(COMPONENT, '\\.IgE') ~ '12020',
    CLASS == 'ALLERGY' & str_detect(COMPONENT, '\\.IgG') ~ '12090',
    SYSTEM == 'Urine sed' ~ '13740',
    CLASS == 'SERO' ~ '1300',
    SYSTEM == 'Urine' & CLASS == 'CHEM' ~ '13750' ,
    str_detect(COMPONENT, 'virus|Virus') ~ '10780',
    str_detect(COMPONENT, 'larva') ~ '10810',
    str_detect(COMPONENT, '^Candida ') ~ '10800'
  ))


