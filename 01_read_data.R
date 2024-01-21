source('00_plotting.R')

# -------------------- #
# 1. Salmon production data 
# -------------------- #

##  Key Norway production statistics
salmon_tonnes<-1467655 ## Norway total production in Nofima 2020
salmon_fillet_yield<-0.65 ## 65% edible yield from Nofima 2020

##  read Skretting FMFO
fmfo<-read.csv('data/skretting_fmfo.csv') %>% 
    select(-c(fishmeal_yield_percent, fishoil_yield_percent)) %>% 
    group_by(species, scientific_name, year, type) %>% 
    summarise(fishmeal_percent = sum(fishmeal_percent), 
              fish_oil_percent = sum(fish_oil_percent))


## read nutrient content & join fmfo with nutrient
nut<-read.csv('data/Species_Nutrient_Predictions.csv') %>% 
        mutate(scientific_name=species)

fmfo<-left_join(fmfo, nut %>% 
    select(scientific_name, Calcium_mu, Iron_mu, Selenium_mu, Zinc_mu, Omega_3_mu, Vitamin_A_mu), 
    by = 'scientific_name')

## get other as mean of all species
other<-fmfo %>% filter(species != 'Other') %>% 
    ungroup() %>% 
    summarise_at(vars(Calcium_mu:Vitamin_A_mu), mean)

## fill other with means
fmfo$Calcium_mu[fmfo$species == 'Other']<-other$Calcium_mu
fmfo$Iron_mu[fmfo$species == 'Other']<-other$Iron_mu
fmfo$Zinc_mu[fmfo$species == 'Other']<-other$Zinc_mu
fmfo$Selenium_mu[fmfo$species == 'Other']<-other$Selenium_mu
fmfo$Omega_3_mu[fmfo$species == 'Other']<-other$Omega_3_mu
fmfo$Vitamin_A_mu[fmfo$species == 'Other']<-other$Vitamin_A_mu

## create long version
fmfol<-fmfo %>% 
    pivot_longer(Calcium_mu:Vitamin_A_mu, names_to = 'nutrient', values_to = 'conc')

## 3. read Norway production data
## don't have species volumes, so need to estimate the FMFO-weighted yield from Newton Table 7
source('data/newton_2023_table7.R')

newton<-newton %>% 
    left_join(fmfo %>% filter(type=='whole_fish') %>% ungroup() %>% 
                  select(species, fishmeal_percent, fish_oil_percent)) %>% 
    filter(!is.na(fishmeal_percent))  # drop sardine

fish_oil_yield<-weighted.mean(newton$fo_yield, w = newton$fish_oil_percent)/100 # 7.9%
fish_meal_yield<-weighted.mean(newton$fm_yield, w = newton$fishmeal_percent)/100 # 20.2%

nor<-read.csv('data/norway_production.csv') ## Table 12 in Nofima 2020
nor$reduction_prop<-1 # for trimmings
nor$reduction_prop[nor$Product=='Fish oil' & nor$Source == 'Whole_fish']<-fish_oil_yield
nor$reduction_prop[nor$Product=='Fishmeal' & nor$Source == 'Whole_fish']<-fish_meal_yield
nor$wild_fish_use<-nor$Tonnes / nor$reduction_prop

nor_wild_use_from_fo<-nor$wild_fish_use[nor$Source=='Whole_fish' & nor$Product=='Fish oil']
nor_wild_use_from_fm<-nor$Tonnes[nor$Source=='Whole_fish' & nor$Product=='Fishmeal']

## checker - does wild fish for FO make enough FM for salmon production in nor?
nor_wild_use_from_fo * fish_meal_yield # 419,511.5 t of fishmeal
nor_wild_use_from_fo * fish_meal_yield -  nor_wild_use_from_fm # 245,339.5 t of spare fishmeal
## so we need to use the wild fish reduced into fish oil as a basis for nFIFO


# -------------------- #
    # 2. Nutrient data 
# -------------------- #

# load norway food composition table  
edible_nut<-read_excel('data/norway_seafood_composition_table.xlsx') %>% clean_names() %>% 
    select(!starts_with('ref')) %>% 
    select(-edible_part_percent) %>% 
    group_by(scientific_name) %>% 
    summarise_at(vars(omega_3_g:iodine_mg), mean, na.rm=TRUE) %>% 
    select(scientific_name, omega_3_epadha_g, vitamin_a_rae, vitamin_d_mg, vitamin_b12_mg, calcium_mg, iron_mg, iodine_mg, zinc_mg, selenium_mg)

## get edible portion from Willer et al 2022, plus mackerel from Seafish https://seafoodacademy.org/pdfs/seafood-guide.pdf
edible_portion<-data.frame(scientific_name = edible_nut$scientific_name)
edible_portion$ed_portion<-c(0.62, 0.61, 0.62, 0.40, 0.88, 0.5, 0.56)

edible_nut<-left_join(edible_nut, edible_portion)

## salmon nutrient values
salmon<-data.frame(scientific_name = 'Salmo salar',  Tonnes = 1467655) %>% 
    left_join(nut %>% select(scientific_name, Calcium_mu, Iron_mu, Selenium_mu, Zinc_mu, Omega_3_mu, Vitamin_A_mu), by = 'scientific_name')

## create long version
salmonl<-salmon %>% pivot_longer(Calcium_mu:Vitamin_A_mu, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(nut_yield = conc * Tonnes) %>% 
    mutate(nut_yield_hi = conc * Tonnes * 0.88, nut_yield_lo = conc * Tonnes * 0.58, nut_yield = conc * Tonnes * (0.88- (0.88-0.58)/2))

salmon_edible<-data.frame(scientific_name = 'Salmo salar', Tonnes = 1467655) %>% 
    left_join(edible_nut)

salmonl_edible<-salmon_edible %>% 
    pivot_longer(omega_3_epadha_g:selenium_mg, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(nut_yield_hi = conc * Tonnes * 0.88, nut_yield_lo = conc * Tonnes * 0.58, nut_yield = conc * Tonnes * (0.88- (0.88-0.58)/2))

# -------------------- #
    # 3. Diet data 
# -------------------- #
anchovy<-read.csv('data/AnchovyConsumption_NDNS.csv') %>% 
    clean_names() %>% 
    mutate(species = 'Anchovy')

herring<-read.csv('data/HerringConsumption_NDNS.csv') %>% 
    clean_names() %>% 
    mutate(species = 'Herring')

mackerel<-read.csv('data/MackerelConsumption_NDNS.csv') %>% 
    clean_names() %>% 
    mutate(species = 'Mackerel')

whiting<-read.csv('data/WhitingConsumption_NDNS.csv') %>% 
    clean_names() %>% 
    mutate(species = 'Whiting')

salmon<-read.csv('data/SalmonConsumption_NDNS.csv') %>% 
    clean_names() %>% 
    mutate(species = 'Salmon')

diet<-rbind(anchovy, herring, mackerel, whiting, salmon)
