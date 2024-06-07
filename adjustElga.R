elgaLabAdjusted <- elga_lab %>%
  filter(code != "48348-7") %>% # 10-Hydroxycarbazepine only antiepileptic
  filter(code != "33275-9") %>% #	7-Dehydrocholesterol only error of metabolism
  filter(code != "41645-3") %>% # Calcium.ionized only blood gas
  filter(code != "14720-7") %>% # Ethosuximide only in Psychopharmaka
  filter(code != "14874-2") %>% # PHENobarbital only in Psychopharmaka
  filter(code != "30471-7") %>% # Ethosuximide only in antiepileptic 
  filter(code != "55782-7")  # Oximetry != BGA (not laboratory)


