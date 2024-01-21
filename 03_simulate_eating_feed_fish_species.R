## script to eat each edible species and track change in seafood production and nutrient yield
#  scenario_species.R runs FMFO estimates after removing 1 species and directing for human consumption 

sps<-unique(fmfo_edible$species)

main_out<-numeric()
main_nut<-numeric()
main_fo<-numeric()

for(i in 1:length(sps)){
    sp<-sps[i]
    source('scenario_species.R')

    main_out<-rbind(main_out, output %>% mutate(species = sp))
    main_nut<-rbind(main_nut, nut_output %>% mutate(species = sp))
    main_fo<-rbind(main_fo, fo_output %>% mutate(species = sp))
}

## what is apparent consumption of herring + mackerel in the UK? Add to sim output
# ac data from Robinson et al. 2022 Navigating trade-offs in seafood systems Env Res Lett
ac<-read.csv( file = 'data/UK_GHG_nutrient_catch.csv') %>%
    filter(species %in% c('Herring', 'Mackerel')) %>%
    mutate(apparent_consumption = apparent_consumption * edible_fraction/100) %>%
    distinct(species, apparent_consumption)

main_out<-main_out %>% left_join(ac) %>%
    group_by(species) %>% mutate(value_prop = value / apparent_consumption)
