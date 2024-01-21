
# Estimate and plot FIFO and FFDR for Norway, make Extended Data Fig 4, depends on:
# source('01_data_load.R')

## estimate FIFO, FFDR, eFIFO
## definitions from Aas et al. 2019

# 1. FIFO = wild fish in, salmon out
# FIFO measures the amount of wild fish used in feed for production of one kg of farmed salmon.
fifo = sum(nor$wild_fish_use) / salmon_tonnes
fifo_oil = sum(nor$wild_fish_use[nor$Product=='Fish oil']) / salmon_tonnes
fifo_meal = sum(nor$wild_fish_use[nor$Product=='Fishmeal']) / salmon_tonnes


## 2. FFDR = forage fish in, salmon out
# The forage fish dependency ratio (FFDR) is equivalent to the FIFO, 
# but with only fish meal and fish oil produced from forage fish included.
ffdr = sum(nor$wild_fish_use[nor$Source=='Whole_fish']) / salmon_tonnes
ffdr_oil = nor$wild_fish_use[nor$Source == 'Whole_fish' & nor$Product=='Fish oil'] / salmon_tonnes
ffdr_meal = nor$wild_fish_use[nor$Source == 'Whole_fish' & nor$Product=='Fishmeal'] / salmon_tonnes


## plot
plotter<-data.frame(
    var = c('FIFO', 'FIFO (oil)', 'FIFO (meal)', 'FFDR', 'FFDR (oil)', 'FFDR (meal)'),
    value = c(fifo, fifo_oil, fifo_meal, ffdr, ffdr_oil, ffdr_meal)
)

plotter$var<-factor(plotter$var, levels=c('FIFO', 'FFDR','FIFO (oil)','FFDR (oil)', 'FIFO (meal)', 'FFDR (meal)'))

g8<-ggplot(plotter, (aes(var, value))) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous(expand=c(0,0), limits=c(0,3)) +
    geom_text(aes(label=round(value, 2)), hjust=-.5)  +
    labs(x = '', y = 'value')

pdf(file = 'Extended_Data_Figure_4.pdf', height=3, width=4)
print(g8 + theme_classic())
dev.off()