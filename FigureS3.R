
# Make Extended_Data_Figure_3 diet surveys, depends on diet in:
# source('01_data_load.R')

diet_agg<- diet %>% 
    mutate(n_survey = ifelse(demographic == 'Adult', 566, 528)) %>% 
    group_by(demographic, species, n_survey) %>% 
    summarise(day_se = se_func(g_day), week_se = se_func(g_week),
              g_day = mean(g_day), g_week = mean(g_week),
              n = n_distinct(seriali)) %>% 
    mutate(lo_week = g_week - 2*week_se, hi_week = g_week + 2*week_se,
           prop_population = n / n_survey * 100)

g1<-ggplot(diet_agg, aes(species, g_week, col=demographic))  + 
    geom_point(data = diet, alpha=0.5, size=1, position = position_dodge(w = .5))+
    geom_pointrange(aes(ymin = lo_week, ymax = hi_week, fill = demographic), fatten = 5, col='black', position = position_dodge(w = .5), pch=21) +
    geom_text(data = diet_agg %>% filter(demographic == 'Child'), aes(y = 490, label = paste('n =', n))) +
    geom_text(data = diet_agg %>% filter(demographic == 'Adult'), aes(y = 520, label = paste('n =', n))) +
    labs(x = '', y = 'grams per week') +
    theme(legend.title = element_blank(), legend.position = c(0.13, 0.7)) +
    guides(colour = 'none')

## estimate annual edible consumption by species, based on 2019 population estimate (corrected for 1.5 - 18 years and 19+ years)
    # https://statswales.gov.wales/catalogue/population-and-migration/population/estimates/nationallevelpopulationestimates-by-year-age-ukcountry
adult_pop<-41724010 - 717056 - 708482 - 733067 + 12374961
child_pop<-12697836 - 722881 + 717056 + 708482 + 733067

stats<-diet_agg %>% 
    mutate(population_2019 = ifelse(demographic=='Adult', adult_pop, child_pop)) %>% 
    group_by(demographic) %>% 
    mutate(edible_g_consumed_2019 = g_week*52 * population_2019 * prop_population/100,
           edible_t_consumed_2019  = edible_g_consumed_2019 / 1000000)


g2<-ggplot(stats, aes(species, edible_t_consumed_2019, fill=demographic)) + 
        geom_bar(stat='identity') + 
        labs(x = '', y = 'seafood consumed, t') +
        guides(fill='none') +
        scale_y_continuous(labels=scales::comma, expand=c(0,0)) +
        theme(legend.title = element_blank(), legend.position = c(0.2, 0.85))

pdf(file = 'Extended_Data_Figure_3.pdf', height=3.5, width=9)
print(
    plot_grid(g1, g2, nrow=1, labels=c('a', 'b'))
    )
dev.off()

