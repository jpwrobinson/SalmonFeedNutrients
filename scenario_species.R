## Scenario simulator
#  FMFO estimates removing 1 species and directing for human consumption 

fracs<-seq(0, 1, by = 0.1)
output<-numeric()
nut_output<-numeric()
fo_output<-numeric()
species<-sp

for(i in 1:length(fracs)){

    whole_edible<-fmfo %>% filter(type=='whole_fish') %>%
        ## rescale percents to account for dropping trimmings
        ungroup() %>% mutate(fishoil_tot = sum(fish_oil_percent),
                             fishmeal_tot = sum(fishmeal_percent)) %>% 
        rowwise() %>% mutate(fish_oil_percent = fish_oil_percent / fishoil_tot *100,
                             fishmeal_percent = fishmeal_percent / fishmeal_tot * 100) %>% 
        select(-fishoil_tot, -fishmeal_tot) %>% 
        filter(species==sp) %>% 
        left_join(edible_nut) %>% 
        ## estimate fish oil and seafood from allocating whole fish
    mutate(total_fishoil = nor_wild_use_from_fo,
           species_total_fishoil = total_fishoil * fish_oil_percent/100,
           species_fishoil_edible = species_total_fishoil * fracs[i],
           species_fishoil_feed = species_total_fishoil * (1-fracs[i]),
           edible_seafood = ed_portion * species_fishoil_edible,
           byproduct_seafood = species_fishoil_edible - edible_seafood) %>% ungroup() %>% 
        ## add fishmeal calculations separately
    mutate(
        total_fishmeal_used = nor_wild_use_from_fm,
        total_fishmeal_from_fo = sum(species_fishoil_feed) * fish_meal_yield,
        species_fishmeal_from_fo = species_fishoil_feed * fishmeal_percent/100,
        species_fishmeal_used = total_fishmeal_used * fishmeal_percent/100,
        fishmeal_deficit = species_fishmeal_used - species_fishmeal_from_fo,
        species_total_fishmeal = ifelse(fishmeal_deficit > 0, fishmeal_deficit, 0)
    ) 
    edible_seafood<-sum(whole_edible$edible_seafood)
    byproduct_seafood<-sum(whole_edible$byproduct_seafood)
    wholefish_fishoil_removed<-sum(whole_edible$species_fishoil_edible)
    fishoil_bau<-sum(nor$Tonnes[nor$Product == 'Fish oil' & nor$Source == 'Whole_fish'])
    fishoil_used<-fishoil_bau - wholefish_fishoil_removed*fish_oil_yield
    
    # save feed required to replace edible whole fish
    missing_feed<-wholefish_fishoil_removed*fish_oil_yield

    # what proportion is this relative to existing fish oil usage
    missing_feed_prop<-missing_feed / fishoil_bau
    print(paste(fracs[i]*100, '% of feed fish allocated for consumption = ', round(missing_feed_prop*100), '% extra non-fish ingredient needed'))
    
    ## estimate total nutrient yield in feed, removing edible wild fish
    nfifo<-whole_edible %>% 
        select(!ends_with('mu')) %>% 
        pivot_longer(omega_3_epadha_g:selenium_mg, names_to = 'nutrient', values_to = 'conc') %>% 
        mutate(fish_oil_nut_yield = conc * species_fishoil_feed * ed_portion,
               fish_meal_nut_yield = conc * species_total_fishmeal * ed_portion,
               edible_nut_yield = conc * edible_seafood) %>% 
        group_by(nutrient) %>% 
        summarise(edible_nut_yield = sum(edible_nut_yield),
                  feed_nut_yield = sum(fish_oil_nut_yield) + sum(fish_meal_nut_yield)) 
    
    ## join with salmon + fish nutrient yield
    nfifo$salmon_nut_yield<-salmonl_edible$nut_yield[match(nfifo$nutrient, salmonl_edible$nutrient)]
    nfifo$seafood_nut_yield<-nfifo$salmon_nut_yield + nfifo$edible_nut_yield
    nfifo$nfifo<-nfifo$feed_nut_yield / nfifo$seafood_nut_yield 

    
    ## Outputs: nFIFO, seafood produced, salmon produced, FO needed
    seafood<-data.frame(seafood = c('Salmon', species), 
                        value  = c(unique(salmonl_edible$Tonnes)* (0.88- (0.88-0.58)/2),
                                                                      edible_seafood),
                        frac = fracs[i])
    
    nuts<-nfifo %>% mutate(frac = fracs[i])
    
    fo<-data.frame(fo_missing = missing_feed, fo_missing_prop = missing_feed_prop, 
                   new_trimmings = byproduct_seafood, frac = fracs[i])
    
    output<-rbind(output, seafood)
    nut_output<-rbind(nut_output, nuts)
    fo_output<-rbind(fo_output, fo)
}

##estimate proportions
output<-output %>% group_by(seafood) %>%  mutate(base = value[which(frac == 0)]) %>% 
    ungroup() %>% mutate(prop = value / base * 100)
                         
nut_output<-nut_output %>% group_by(nutrient) %>%  
    mutate(base = seafood_nut_yield[which(frac == 0)]) %>% 
    ungroup() %>% mutate(prop = seafood_nut_yield / base * 100)

