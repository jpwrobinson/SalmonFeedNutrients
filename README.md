# SalmonFeedNutrients



R scripts and datasets accompanying Willer et al. 2024. 



To reproduce analysis, ```git clone``` this repo, OR download as ZIP, OR click [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jpwrobinson/SalmonFeedNutrients/HEAD?urlpath=rstudio) 

1. Install R packages (or this is done automatically in Binder environment)

```R
install.packages(c('tidyverse', 'cowplot', 'scales', 'janitor', 'readxl', 'ggridges', 'ggradar', 'Hmisc'))
```

2. Source R scripts.

```R
# load and clean data, estimate nFIFO
source('02_estimate_nFIFO.R')

# simulate eating feed fish species
source('03_simulate_eating_feed_fish_species.R')
source('04_simulate_eating_feed_fish_total.R')

# create figures as pdf
source('Figure1.R')
source('Figure2.R')
source('Figure4.R')
source('FigureS3.R')
source('FigureS4.R')
```

