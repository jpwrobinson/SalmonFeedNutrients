## UK values

## RNI: Recommended Nutrient Intake 

rda<-data.frame(nutrient = c('calcium', 'iron', 'selenium', 'zinc','iodine', 'vitamin_a', 'omega_3_epadha', 'vitamin_d', 'vitamin_b12', 'folate'))

## women betwee 19-64 years, RNI per day
ca<-700
fe<-14.8
se<-60
zn<-7
i<-140

vita<-600
vitd<-10
vitb12<-1.5
folate<-200

omega<-NA
omega_dhaepa<-0.25

rda$rni_women = c(ca, fe, se, zn, i, vita, omega_dhaepa, vitd, vitb12, folate)

## men betwee 19-64 years, RNI per day
ca<-700
fe<-8.7
se<-75
zn<-7
i<-140

vita<-700
vitd<-10
vitb12<-1.5
folate<-200

omega<-NA
omega_dhaepa<-0.25

rda$rni_men = c(ca, fe, se, zn, i, vita, omega_dhaepa, vitd, vitb12, folate)

## RDA: Recommended Daily Allowance for kids 1 - <5 years
# https://www.nationalacademies.org/our-work/summary-report-of-the-dietary-reference-intakes
## calculating average for 1 - <5 years (4.5 years)
ca<-mean(c(350*3 + 450))/4
fe<-mean(c(6.9*3 + 6.1))/4
se<-mean(c(15*3 + 20))/4
zn<-mean(c(5*3 + 6.5))/4
i<-mean(c(70*3 + 100))/4
omega<-NA
omega_dhaepa<-0.25

vita<-400
vitd<-10
vitb12<-mean(c(0.5*3 + 0.8))/4
folate<-mean(c(70*3 + 100))/4

rda$rni_kids = c(ca, fe, se, zn, i, vita, omega_dhaepa, vitd, vitb12, folate)

