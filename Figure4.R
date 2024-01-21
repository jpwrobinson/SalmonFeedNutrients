# Script to simulate eating feed fish, make Figure 4

# -------------------- #
## 1. bring in species scenarios for panels
# -------------------- #

# run species scenarios
source('03_simulate_eating_feed_fish_species.R') 

# arrange for plotting
fo_plot<-main_fo %>% 
    left_join(main_out %>% filter(seafood != 'Salmon') %>% 
    select(value, frac, species)) %>% 
    pivot_longer(c(-frac,-species), names_to = 'var', values_to = 'value') %>% 
    mutate(var_lab = recode(var, new_trimmings='New trimmings', 
                            fo_missing = 'Fish oil replacement',
                            value = 'Seafood from edible feed'))

labber<-fo_plot %>% filter(frac==1 & var == 'new_trimmings') 

g_fig4b<-ggplot(fo_plot %>% filter(var != 'fo_missing_prop'), 
           aes(frac, value, col=species)) + 
    geom_line() +
    geom_text(data = labber, aes(label = species), size=2.5,  nudge_x=0.01, hjust=0) +
    facet_wrap(var_lab~., scales='free_y', strip.position='top',nrow=3) +
    theme(legend.title = element_blank(), legend.position = 'none',
          plot.margin=unit(c(0,2,0,.5), 'cm')) +
    labs(
        x = 'wild fish allocated\nfor human consumption', 
        y = 'tonnes') + #guides(col='none') +
    scale_y_continuous(labels=scales::comma)  +
    scale_colour_brewer(palette = 'Dark2') +
    scale_x_continuous(expand=c(0.025, 0.025), labels = scales::percent) +
    coord_cartesian(clip='off')

# -------------------- #
## 2. run feed allocation scenario (no species)
# -------------------- #

source('04_simulate_eating_feed_fish_total.R')

nut_lab<-data.frame(nutrient = unique(nut_output$nutrient),
                    nut = c('Calcium', 'Iodine', 'Iron', 'Omega-3 (DHA + EPA)',
                            'Selenium', 'Vitamin A', 'Vitamin B12', 'Vitamin D', 'Zinc'))

nut_output$nut_lab<-nut_lab$nut[match(nut_output$nutrient, nut_lab$nutrient)]

intersect<-nut_output %>% group_by(nutrient, nut_lab) %>% 
    reframe(frac=frac[which(nfifo<=1)]) %>% 
    group_by(nutrient, nut_lab) %>% 
    summarise(frac=max(frac)) %>% 
    mutate(nfifo = 1)

oversect<-nut_output %>% group_by(nutrient, nut_lab) %>% 
    filter(nfifo>=3) %>% 
    group_by(nutrient, nut_lab) %>% 
    summarise(frac=min(frac)) 

g_Fig4a<-ggplot(nut_output, aes(frac, nfifo, col = nut_lab)) +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, col='grey99', alpha=0.2) +
    geom_vline(xintercept = 0, linetype = 5, col = 'grey') +
    geom_hline(yintercept = 1, linetype = 5, col = 'grey') +
    geom_line() +
    geom_point(data = intersect %>% filter(frac>0), pch=21, col='black', aes(fill = nut_lab), size=3) +
    annotate('text', x=0.01, y=3, label = 'Business as Usual', hjust = 0,size=3, col='grey40') +
    labs(
         x = 'wild fish allocated for human consumption', 
         y = 'edible nutrient retention') +
    scale_y_continuous(expand=c(0.05, 0.025),limits=c(0,3), labels = scales::percent) +
    scale_x_continuous(expand=c(0.025, 0.025), labels = scales::percent) +
    scale_colour_manual(values = nut.cols) +
    scale_fill_manual(values = nut.cols) +
    guides(fill = 'none', color='none') +
    theme(legend.position = c(0.8, 0.7), legend.title = element_blank())

    

pdf(file = 'Figure_4.pdf', height=4, width=10)
print(plot_grid(g_Fig4a, g_fig4b, labels=c('a','b'), ncol=2, rel_widths=c(1, 0.8)))
dev.off()