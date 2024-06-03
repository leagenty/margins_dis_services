
##### 1. Open data

library(tidyverse)
try_data <- read.csv("2. Traits/2.2. TRY_floral_diameter/Raw/31339.csv")
#data from TRY request 19/01/2023

missing_sp <- read.csv(file="2. Traits/2.1. Voj_Lan_floral_diameter/Processed/missing_sp_for_floral_diameter.csv") %>% 
  dplyr::select(-X)
#missing species needed

key <- read.csv(file="2. Traits/2.1. Voj_Lan_floral_diameter/Raw/key_500_ENI_name_species.csv") %>%
  dplyr::select(-1)
# key between code names and species, genus, family, synonyms etc

##### 2.Create the key

try_key <- try_data %>% 
  mutate(species=toupper(SpeciesName)) %>% 
  separate(species,sep=" ",c("Genre","Sp")) %>% 
  mutate(genus=str_sub(Genre, 1, 4))%>% 
  mutate(sp=str_sub(Sp, 1, 7)) %>% 
  mutate(key= paste(genus,sp,sep="")) 

##### 3. Select the column of interests and agregate data at the species level

try_traits <- try_key %>% 
  dplyr::select(SpeciesName,key,DataName,OrigValueStr,OrigUnitStr) %>% 
  filter(DataName=="Inflorescence length (height)"|
           DataName=="Corolla length"|
           DataName=="Flower size (diameter)"|
           DataName=="Flower corolla length"|
           DataName=="Display size (flower size incl. flower head, e.g. Asteraceae)") %>% 
  mutate(OrigValueStr=as.numeric(OrigValueStr)) %>% 
  group_by(SpeciesName,key,DataName,OrigUnitStr) %>% 
  summarize_all(mean) %>% 
  ungroup()

##### 4. Pivot in order to have the trait name as a column

try_traits_names <- try_traits %>% 
  pivot_wider(names_from = DataName,
              values_from = OrigValueStr) %>% 
  dplyr::select(-"Flower size (diameter)"#because we keep the information at the inflorescence scale like in other data 
                ,-"Corolla length") %>% #because only 5 species not in the 1%
  rename("inflo_length_cm" = "Inflorescence length (height)")  %>% 
  rename("display_size_mm" = "Display size (flower size incl. flower head, e.g. Asteraceae)")  %>%
  mutate(inflo_length_mm = inflo_length_cm * 10) %>% 
  dplyr::select(-inflo_length_cm,-OrigUnitStr)

try_inflo_length <- try_traits_names %>% 
  dplyr::select(-display_size_mm) %>% 
  rename(display_size_mm=inflo_length_mm) %>% 
  drop_na()

try_display_size <- try_traits_names %>% 
  dplyr::select(-inflo_length_mm)  %>% 
  drop_na()

try_all <- try_display_size %>% 
  rbind(try_inflo_length)


##### 5. Select the species of the 1% without Poaceae and ferns

try_all_ENI <-  try_all %>% 
  filter(key %in% missing_sp$key)
#write.csv(try_all_ENI,file="2. Traits/2.2. TRY_floral_diameter/Processed/floral_diameter_try.csv")

This only adds one species : Ambrosia artemisifolia

