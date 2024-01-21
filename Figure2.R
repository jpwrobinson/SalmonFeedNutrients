
## Script to make Figure 2 - RDA from FMFO vs salmon, depends on:
# source('02_estimate_nFIFO.R')

# load NRV values
source('rda_reader.R')

# plot theme
th<-theme(
        plot.subtitle = element_text(size=12, colour='black', face=2, hjust=0.5),
        plot.caption = element_text(size=9, colour='#636363', face=1))


rda$nut_lab2<-Hmisc::capitalize(str_replace_all(rda$nutrient, '_', '\\ '))
rda<-rda %>% mutate(nut_lab2 = recode(nut_lab2,
                        'Omega 3 epadha' = 'Omega-3\n(EPA + DHA)',
                        'Vitamin a' = 'Vitamin A', 'Vitamin d' = 'Vitamin D', 
                        'Vitamin b12' = 'Vitamin B12'))

## save nutrient content data
feed_nut<-whole_edible %>% 
    select(!ends_with('mu')) %>% 
    pivot_longer(omega_3_epadha_g:selenium_mg, names_to = 'nutrient', values_to = 'conc') %>% 
    left_join(wholel_edible %>% select(nutrient, nut_lab)) 

salm_nut<-salmonl_edible %>% 
    mutate(species = 'Atlantic salmon') %>% 
    left_join(wholel_edible %>% select(nutrient, nut_lab)) 

dat_avg<-feed_nut %>%
    group_by(nut_lab) %>% 
    summarise(conc = max(conc)) %>% 
    mutate(species = 'FMFO')


# set levels for axis order
feed_nut$nut_lab<-factor(feed_nut$nut_lab)
levels(feed_nut$nut_lab)<-c("'Calcium, mg'",expression('Iodine, '*mu*'g'), "'Iron, mg'", "'Omega-3, g'", expression('Selenium, '*mu*'g'),
                            expression('Vitamin A, '*mu*'g'), expression('Vitamin B12, '*mu*'g'),expression('Vitamin D, '*mu*'g'), "'Zinc, mg'")
salm_nut$nut_lab<-factor(salm_nut$nut_lab)
levels(salm_nut$nut_lab)<-c("'Calcium, mg'",expression('Iodine, '*mu*'g'), "'Iron, mg'", "'Omega-3, g'", expression('Selenium, '*mu*'g'),
                            expression('Vitamin A, '*mu*'g'), expression('Vitamin B12, '*mu*'g'),expression('Vitamin D, '*mu*'g'), "'Zinc, mg'")

rda<-rda %>% filter(!nutrient %in% c('folate'))
rda$nut_lab<-factor(rda$nut_lab, levels = unique(rda$nut_lab)[c(1,5,2,7,3,6,9,8,4)])
levels(rda$nut_lab)<-c("'Calcium, mg'",expression('Iodine, '*mu*'g'), "'Iron, mg'", "'Omega-3, g'", expression('Selenium, '*mu*'g'),
                            expression('Vitamin A, '*mu*'g'), expression('Vitamin B12, '*mu*'g'),expression('Vitamin D, '*mu*'g'), "'Zinc, mg'")

## plot concentrations
g_sp_nut<-ggplot(feed_nut, aes(species, conc)) + 
    geom_bar(stat='identity') + 
    facet_grid(~nut_lab, scales='free_x', labeller=label_parsed) +
    geom_hline(data = salm_nut, aes(yintercept = conc), col='red') +
    # geom_hline(data = rda, aes(yintercept = rni_women), col='blue') +
    coord_flip() +
    labs(x = '', y = 'content per 100 g') +
    scale_x_discrete(limits=rev) +
    scale_y_continuous(expand=expansion(mult = c(0, .05))) + theme_bw()

pdf(file = 'Extended_Data_Figure_1.pdf', height =3, width=12)
print(g_sp_nut)
dev.off()

## join with RDA data
feed_rda<-whole_edible %>% 
    rowwise() %>% 
    mutate(total_wholefish_feed = species_total_fishoil + species_total_fishmeal) %>% 
    select(!ends_with('mu')) %>% 
    pivot_longer(omega_3_epadha_g:selenium_mg, names_to = 'nutrient', values_to = 'conc') %>% 
    left_join(wholel_edible %>% select(nutrient, nut_lab)) %>% 
    left_join(rda %>% select(-nutrient) %>% mutate(nut_lab=nut_lab2), by = 'nut_lab') %>% 
    select(species, scientific_name, total_wholefish_feed, conc, nut_lab, rni_women, rni_men, rni_kids) %>% 
    mutate(percent_rda_140 = conc / rni_women * 140,
           percent_rda_140_men = conc / rni_men * 140,
           percent_rda_40 = conc / rni_kids * 40,
           percent_rda_100 = conc / rni_women * 100,
           portion_rda = rni_women / conc * 100) %>% 
    mutate(percent_rda_140 = ifelse(percent_rda_140 > 100, 100, percent_rda_140),
           percent_rda_140_men = ifelse(percent_rda_140_men > 100, 100, percent_rda_140_men),
           percent_rda_40 = ifelse(percent_rda_40 > 100, 100, percent_rda_40),
           percent_rda_100 = ifelse(percent_rda_100 > 100, 100, percent_rda_100)) 
    
salm_rda<-salmonl_edible %>% 
    left_join(wholel_edible %>% select(nutrient, nut_lab)) %>% 
    left_join(rda %>% select(-nutrient) %>% mutate(nut_lab=nut_lab2), by = 'nut_lab') %>% 
    select(conc, nut_lab, rni_women, rni_men, rni_kids) %>% 
    mutate(percent_rda_140 = conc / rni_women * 140,
           percent_rda_140_men = conc / rni_men * 140,
           percent_rda_40 = conc / rni_kids * 40,
           percent_rda_100 = conc / rni_women * 100,
           portion_rda = rni_women / conc * 100) %>% 
    mutate(percent_rda_140 = ifelse(percent_rda_140 > 100, 100, percent_rda_140),
           percent_rda_140_men = ifelse(percent_rda_140_men > 100, 100, percent_rda_140_men),
           percent_rda_40 = ifelse(percent_rda_40 > 100, 100, percent_rda_40),
           percent_rda_100 = ifelse(percent_rda_100 > 100, 100, percent_rda_100)) %>% 
    mutate(species = 'Atlantic salmon')

dat<-feed_rda %>% select(species, nut_lab, percent_rda_140) %>%
            mutate(percent_rda_140 = percent_rda_140/100) %>%
            pivot_wider(names_from = nut_lab, values_from = percent_rda_140)

dat_avg<-feed_rda %>% select(species, nut_lab, percent_rda_140, total_wholefish_feed) %>%
    mutate(percent_rda_140 = percent_rda_140/100) %>%
    group_by(nut_lab) %>% 
    summarise(percent_rda_140 = weighted.mean(percent_rda_140, w=total_wholefish_feed)) %>% 
    pivot_wider(names_from = nut_lab, values_from = percent_rda_140) %>% 
    mutate(species = 'ECM feed fish')

dat_salm<-salm_rda %>% select(species, nut_lab, percent_rda_140) %>%
    mutate(percent_rda_140 = percent_rda_140/100) %>%
    arrange(desc(percent_rda_140)) %>%
    pivot_wider(names_from = nut_lab, values_from = percent_rda_140)

datP<-rbind(dat_salm, dat_avg %>% select(colnames(dat_salm)))
datP$species<-factor(datP$species, levels=unique(datP$species)[c(2,1)])

## male values
dat_avg_men<-feed_rda %>% select(species, nut_lab, percent_rda_140_men, total_wholefish_feed) %>%
    mutate(percent_rda_140_men = percent_rda_140_men/100) %>%
    group_by(nut_lab) %>% 
    summarise(percent_rda_140_men = weighted.mean(percent_rda_140_men, w=total_wholefish_feed)) %>% 
    pivot_wider(names_from = nut_lab, values_from = percent_rda_140_men) %>% 
    mutate(species = 'ECM feed fish')

dat_salm_men<-salm_rda %>% select(species, nut_lab, percent_rda_140_men) %>%
    mutate(percent_rda_140_men = percent_rda_140_men/100) %>%
    arrange(desc(percent_rda_140_men)) %>%
    pivot_wider(names_from = nut_lab, values_from = percent_rda_140_men)

datP_men<-rbind(dat_salm_men %>% select(colnames(dat_salm)), dat_avg_men %>% select(colnames(dat_salm)))
datP_men$species<-factor(datP_men$species, levels=unique(datP_men$species)[c(2,1)])

## plot RNI 
gRNI<-ggradar(datP, 
            group.colours=c('#7EAE00', '#DE001A'),
            base.size = 0.5,
            group.point.size = 1.5,
            group.line.width = 1,
            background.circle.colour = "white",
            axis.label.size = 3.5,
            fill=TRUE,
            fill.alpha = 0.5,
            legend.text.size = 11,
            legend.position = 'bottom',
            axis.line.colour='darkgrey',
            gridline.mid.colour = "darkgrey",
            gridline.min.colour = "darkgrey",
            gridline.max.colour = "darkgrey",
            grid.label.size = 4.5) + 
    labs(subtitle = 'RNI for women, 100g') +
    coord_equal(clip='off') + th

gRNI_men<-ggradar(datP_men, 
             group.colours=c('#7EAE00', '#DE001A'),
             base.size = 0.5,
             group.point.size = 1.5,
             group.line.width = 1,
             background.circle.colour = "white",
             axis.label.size = 3.5,
             fill=TRUE,
             fill.alpha = 0.5,
             legend.text.size = 11,
             legend.position = 'bottom',
             axis.line.colour='darkgrey',
             gridline.mid.colour = "darkgrey",
             gridline.min.colour = "darkgrey",
             gridline.max.colour = "darkgrey",
             grid.label.size = 4.5) + 
    labs(subtitle = 'RNI for men, 100g') +
    coord_equal(clip='off') + th

pdf(file = 'Extended_Data_Figure_2.pdf', height = 6, width=12)
print(plot_grid(gRNI, gRNI_men, nrow=1, labels=c('a', 'b')))
dev.off()

## now main figure version, select nutrients and varying portion size
dat140<-datP %>% 
    select(species, 'Omega-3\n(EPA + DHA)', 'Vitamin D', 'Vitamin B12', 'Selenium', 'Iodine')

## 100g version (nutrients in 100g)
dat_avg100<-feed_rda %>%
    mutate(percent_rda_100 = percent_rda_100/100) %>%
    group_by(nut_lab) %>% 
    summarise(percent_rda_100 = weighted.mean(percent_rda_100, w=total_wholefish_feed)) %>% 
    pivot_wider(names_from = nut_lab, values_from = percent_rda_100) %>% 
    mutate(species = 'FMFO')

dat_salm100<-salm_rda %>% select(species, nut_lab, percent_rda_100) %>%
    mutate(percent_rda_100 = percent_rda_100/100) %>%
    arrange(desc(percent_rda_100)) %>%
    pivot_wider(names_from = nut_lab, values_from = percent_rda_100) 

dat100<-rbind(dat_salm100, dat_avg100 %>% select(colnames(dat_salm)))
dat100$species<-factor(dat100$species, levels=unique(dat100$species)[c(2,1)])

## portion size version (how much fish to reach RNI?)
datportion<-feed_rda %>% select(species, nut_lab, portion_rda)
datportion_salm<-salm_rda %>% select(species, nut_lab, portion_rda) 

datPortion<-rbind(datportion_salm,datportion %>% select(colnames(datportion_salm)))
datPortion$species<-factor(datPortion$species, levels=rev(unique(datPortion$species)))

gFig2b<-ggradar(dat140, 
             group.colours=c('#7EAE00', '#DE001A'),
             base.size = 0.5,
             group.point.size = 1.5,
             group.line.width = 1,
             background.circle.colour = "white",
             axis.label.size = 3.5,
             fill=TRUE,
             fill.alpha = 0.5,
             legend.text.size = 11,
             legend.position = 'bottom',
             axis.line.colour='darkgrey',
             gridline.mid.colour = "darkgrey",
             gridline.min.colour = "darkgrey",
             gridline.max.colour = "darkgrey",
             grid.label.size = 4.5
             ) +
    coord_equal(clip='off') 


nuts<-c('Omega-3\n(EPA + DHA)', 'Vitamin D', 'Vitamin B12', 'Selenium', 'Iodine')
gFig2a<-ggplot(datPortion %>% filter(nut_lab %in% nuts), aes(portion_rda, species, fill = nut_lab)) + 
    geom_bar(stat='identity') + facet_wrap(~nut_lab,scales='free_x', nrow=1) +
    scale_x_continuous(expand=c(0,0)) +
    geom_vline(xintercept = 140, col='black', linetype=5) +
    labs(x = 'grams to reach daily NRV', y = '') +
    theme(plot.margin = unit(c(2,.1, 2, .1), 'cm')) +
    scale_fill_manual(values = nut.cols2) +
    guides(fill='none') +
    theme(strip.background = element_blank(), axis.ticks.y = element_blank())

pdf(file = 'Figure_2.pdf', height=4, width=14)
print(
    plot_grid(gFig2a, gFig2b, nrow=1, 
                       labels=c('a', 'b'), label_size=14, hjust=0, rel_widths=c(1,.65))
    )
dev.off()

