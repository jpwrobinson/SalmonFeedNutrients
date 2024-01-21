source('01_read_data.R')

## fmfo composition all fish
whole<-fmfo %>% filter(type=='whole_fish') %>% 
    ## rescale percents to account for dropping trimmings
    ungroup() %>% mutate(fishoil_tot = sum(fish_oil_percent),
                         fishmeal_tot = sum(fishmeal_percent)) %>% 
    rowwise() %>% mutate(fish_oil_percent = fish_oil_percent / fishoil_tot *100,
                         fishmeal_percent = fishmeal_percent / fishmeal_tot * 100) %>% 
    select(-fishoil_tot, -fishmeal_tot) %>% 
    mutate(total_fishoil = nor_wild_use_from_fo,
           species_total_fishoil = total_fishoil * fish_oil_percent/100,
           total_fishmeal_used = nor_wild_use_from_fm,
           total_fishmeal_from_fo = nor_wild_use_from_fo * fish_meal_yield,
           species_fishmeal_from_fo = species_total_fishoil * fishmeal_percent/100,
           species_fishmeal_used = total_fishmeal_used * fishmeal_percent/100,
           fishmeal_deficit = species_fishmeal_used - species_fishmeal_from_fo,
           species_total_fishmeal = ifelse(fishmeal_deficit > 0, fishmeal_deficit, 0)
           )


## for edible FMFO only
whole_edible<-fmfo %>% filter(type=='whole_fish') %>%
    ## rescale percents to account for dropping trimmings
    ungroup() %>% mutate(fishoil_tot = sum(fish_oil_percent),
                         fishmeal_tot = sum(fishmeal_percent)) %>% 
    rowwise() %>% mutate(fish_oil_percent = fish_oil_percent / fishoil_tot *100,
                         fishmeal_percent = fishmeal_percent / fishmeal_tot * 100) %>% 
    select(-fishoil_tot, -fishmeal_tot) %>% 
    mutate(total_fishoil = nor_wild_use_from_fo,
           species_total_fishoil = total_fishoil * fish_oil_percent/100,
           total_fishmeal_used = nor_wild_use_from_fm,
           total_fishmeal_from_fo = nor_wild_use_from_fo * fish_meal_yield,
           species_fishmeal_from_fo = species_total_fishoil * fishmeal_percent/100,
           species_fishmeal_used = total_fishmeal_used * fishmeal_percent/100,
           fishmeal_deficit = species_fishmeal_used - species_fishmeal_from_fo,
           species_total_fishmeal = ifelse(fishmeal_deficit > 0, fishmeal_deficit, 0)) %>% 
    filter(scientific_name %in% edible_nut$scientific_name) %>% 
    left_join(edible_nut)

## create long version
fmfo_edible<-left_join(fmfo %>% filter(scientific_name %in% edible_nut$scientific_name), 
                       edible_nut)

## create long version
fmfol_edible<-fmfo_edible %>% 
    pivot_longer(omega_3_epadha_g:selenium_mg, names_to = 'nutrient', values_to = 'conc')


## estimate total nutrient yield in whole fish (oil + meal)
wholel_edible<-whole_edible %>% 
    select(!ends_with('mu')) %>% 
    pivot_longer(omega_3_epadha_g:selenium_mg, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(nut_yield_oil = conc * species_total_fishoil * ed_portion, 
           nut_yield_meal = conc * species_total_fishmeal * ed_portion) %>% 
    group_by(nutrient) %>% 
    summarise(nut_yield_oil = sum(nut_yield_oil), nut_yield_meal = sum(nut_yield_meal)) %>% 
    mutate(total_nut_yield = nut_yield_oil + nut_yield_meal) 

## join with salmon nutrient yield
wholel_edible$salmon_nut_yield<-salmonl_edible$nut_yield[match(wholel_edible$nutrient, salmonl_edible$nutrient)]
wholel_edible$salmon_nut_yield_lo<-salmonl_edible$nut_yield_lo[match(wholel_edible$nutrient, salmonl_edible$nutrient)]
wholel_edible$salmon_nut_yield_hi<-salmonl_edible$nut_yield_hi[match(wholel_edible$nutrient, salmonl_edible$nutrient)]

## calculate nfifo
wholel_edible$nfifo<- wholel_edible$salmon_nut_yield / wholel_edible$total_nut_yield
wholel_edible$nfifo_lo<- wholel_edible$salmon_nut_yield_lo / wholel_edible$total_nut_yield
wholel_edible$nfifo_hi<- wholel_edible$salmon_nut_yield_hi / wholel_edible$total_nut_yield