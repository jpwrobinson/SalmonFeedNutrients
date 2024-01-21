# data from Table 7 on FMFO yields per feed species

species <- c('Peruvian anchoveta', 'Blue whiting', 'Herring', 'Mackerel', 'Norway pout',
             'Sandeel','Menhaden', 'Horse mackerel', 'Sprat', 'Sardine')

sciname <- c('Engraulis ringens', 'Micromesistius poutassou', 'Clupea harengus', 'Scomber scombrus',
             'Trisopterus esmarkii', 'Ammodytes marinus', 'Brevoortia patronus', 'Trachurus trachurus',
             'Sprattus sprattus', 'Sardina pilchardus')

fm_yield <- c(23.8, 19.7, 22.1, 19.4, 20.4, 21.5, 21, 23, 18.8, 23)

fo_yield <- c(4.5, 1.9, 11.5, 18.6, 11.5, 4.5, 16, 6, 7.9, 18)

newton<-data.frame(cbind(species, sciname, fm_yield, fo_yield)) %>% 
    mutate(fm_yield = as.numeric(fm_yield),
           fo_yield = as.numeric(fo_yield))
