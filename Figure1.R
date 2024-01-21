# Make Figure 1, depends on:
# source('02_estimate_nFIFO.R')


# rename nutrients
wholel_edible$nut_lab<-str_replace_all(wholel_edible$nutrient, '_mg', '')
wholel_edible$nut_lab<-str_replace_all(wholel_edible$nut_lab, '_rae', '')
wholel_edible$nut_lab<-str_replace_all(wholel_edible$nut_lab, '_g', '')
wholel_edible$nut_lab<-str_replace_all(wholel_edible$nut_lab, '_', '\\ ')
wholel_edible$nut_lab<-Hmisc::capitalize(wholel_edible$nut_lab)

wholel_edible<-wholel_edible %>% 
                    mutate(nut_lab = recode(nut_lab,
                    'Omega 3 epadha' = 'Omega-3\n(EPA + DHA)',
                    'Vitamin a' = 'Vitamin A', 'Vitamin d' = 'Vitamin D', 
                    'Vitamin b12' = 'Vitamin B12')) %>% 
    mutate(nut_lab2 = recode(nut_lab,
                            'Omega-3\n(EPA + DHA)' = 'Omega-3 (EPA + DHA)'))

## create FMFO composition with edible portions
whole_fig<-whole %>% select(species, scientific_name, species_total_fishoil, species_total_fishmeal) %>% 
            left_join(edible_portion) %>% 
            ungroup() %>% #select(-type) %>% 
            pivot_longer(species_total_fishoil:species_total_fishmeal, names_to = 'type', values_to = 'tonnes') %>% 
            group_by(species, scientific_name) %>% mutate(total_fmfo = sum(tonnes), edible_fmfo = ed_portion * total_fmfo) %>% 
            mutate(type = recode(type, 'species_total_fishmeal' = 'Additional fishmeal', 'species_total_fishoil' = 'Fish oil and fishmeal'))

g_Fig1a<-ggplot(whole_fig, 
            aes(fct_reorder(species, tonnes), tonnes, fill = type)) + 
            geom_bar(stat='identity') + 
            geom_point(aes(fct_reorder(species, tonnes), edible_fmfo), size=2.5, show.legend=F) +
            labs(x ='', y = 'Tonnes') + 
            scale_y_continuous(labels=scales::comma, expand=c(0,0)) +
            coord_flip() +
            theme(legend.position = c(0.8, 0.2), legend.title = element_blank())

g_Fig1b<-ggplot(wholel_edible, aes(fct_reorder(nut_lab, nfifo), nfifo, ymin = nfifo_lo, ymax=nfifo_hi)) + 
    geom_hline(yintercept = 1, linetype=5, colour='grey') +
    geom_pointrange(aes(col=nut_lab2)) + 
    coord_flip(clip='off') +
    scale_colour_manual(values = nut.cols) +
    scale_y_continuous(labels=percent) +
    labs(x = '', y = 'edible nutrient retention') +
    annotate('text', Inf, 0.1,label = 'More wild nutrients', vjust=1, hjust=0, size=2.75, col='grey50') +
    annotate('text', Inf, 1.4,label = 'More farmed nutrients', vjust=1, hjust=0, size=2.75, col='grey50') +
    guides(colour='none')


pdf(file = 'Figure_1.pdf', height=4, width=10)
print(
    plot_grid(g_Fig1a,g_Fig1b, nrow=1, labels=c('a', 'b'), rel_widths=c(1,0.7)))
dev.off()