install.packages(c(
	'tidyverse',
	'cowplot',
	'scales',
	'janitor',
	'readxl',
	'ggridges',
	'Hmisc',
	'devtools'
))

devtools::install_github("ricardo-bion/ggradar", 
                          dependencies = TRUE)
