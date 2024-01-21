library(tidyverse)
library(cowplot)
library(scales)
library(janitor)
library(readxl)
library(ggridges)
library(ggradar)
theme_set(theme_classic())


## nutrient cols
nut.cols<-c('Calcium'='#de2d26', 
            'Iron'='#e6ab02',
            'Zinc'='#3182bd', 
            'Omega-3 (DHA + EPA)' = '#d95f02',
            'Selenium' = '#776EB0',
            'Iodine' = '#35978f',
            'Vitamin A'='#31a354',
            'Vitamin D' = '#fb9a99', 
            'Vitamin B12' = '#b2df8a', 
            'Folate' = '#b15928')


nut.cols2<-c('Calcium'='#de2d26', 
            'Iron'='#e6ab02',
            'Zinc'='#3182bd', 
            'Omega-3\n(DHA + EPA)' = '#d95f02',
            'Selenium' = '#776EB0',
            'Iodine' = '#35978f',
            'Vitamin A'='#31a354',
            'Vitamin D' = '#fb9a99', 
            'Vitamin B12' = '#b2df8a', 
            'Folate' = '#b15928')

# standard error function
se_func<-function(x){
    sd(x)/sqrt(length(x)) ## estimate the sd of the input 'x', divided by the square root of N (= length(x))
}
