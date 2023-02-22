#############################################################################
# Pierre Germain NTSOLI II                                                  #
# Agronomist- Plant pathologist                                             #
# 28-01-23                                                                  # 
# Les communautés productrices de l'arachide au Cameroun: Connaissances,    #      
# pratiques et perceptions de la contamination des graines par l'aflatoxine #    
#############################################################################



## Chargement des packages----
library(readxl)
library(gtsummary)
library(forestmodel)
library(labelled)
library(broom)
library(broom.helpers)
library(dplyr)
library(questionr)
library(survey)
library(tidyverse)
library(GGally)
library(effects)
library(mlogit)
library(car)
library(esquisse)

## Importation des jeu de données----
genm <- read_excel("C:/Users/MAGAT/Desktop/Ntsoli Articles/mon projet/Enquête final article 1.xlsx", 
                   sheet = "Feuil1")

cse <- read_excel("C:/Users/MAGAT/Desktop/Ntsoli Articles/mon projet/Enquête final article 1.xlsx", 
                   sheet = "CSE")

## Voir les tableau de données----
View(genm)
view(cse)

## Voir la structure des données----
str(genm)
str(cse)

## Aperçu concis du tableau de données avec la fonction glimpse----
glimpse(genm)
glimpse(cse)

lookfor_to_long_format(genm)

## Etiquettes de variables----

## Analyse des caractéristiques socioéconomiques-----

## Découpage des variables quantitatives en classes----
attach(cse)
cse$Age <- cse$`Age de l'enquêté`
str(cse$Age)
cse$superficie <- cse$`Estimation des superficies totales destinées à la culture de l'arachide en ha?`
cse$sco <- cse$`Score de connaissance`

## Recodage de cse[,"Age de l'enquêté"] en cse$Age
## Recodage de cse$Age en cse$Age_rec
cse$Age_rec <- cut(cse$Age,
                   include.lowest = TRUE,
                   right = FALSE,
                   dig.lab = 4,
                   breaks = c(0, 31, 46, 60, 90)
)


## Recodage de cse[,"Estimation des superficies totales destinées à la culture de l'arachide en ha?"] en cse[,"Estimation des superficies totales destinées à la culture de l'arachide en ha?_rec"]
## Recodage de cse$superficie en cse$superficie_rec
cse$superficie_rec <- cut(cse$superficie,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(0, 0.6, 1.1, 3.01, 5)
)

cse$revdollars <- cse$Revenu/610

## Recodage revenu en FCFA----
cse$Revenu_rec <- cut(cse$Revenu,
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 3e+05, 6e+05, 9e+05, 1e+07)
)


## Recodage revenu en dollars----
cse$revdollars_rec <- cut(cse$revdollars,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(0, 501, 1001, 2500, 16000)
)


## Recodage des scores des connaissances----
cse$sco_rec <- cut(cse$sco,
                   include.lowest = TRUE,
                   right = FALSE,
                   dig.lab = 4,
                   breaks = c(0, 10, 16)
)

## Recodage de la variable expérience
cse$exp <- cse$`Depuis combien d'années produisez-vous de l'arachide?`
cse$exp_rec <- cut(cse$exp,
                   include.lowest = TRUE,
                   right = FALSE,
                   dig.lab = 4,
                   breaks = c(0, 11, 21, 31, 80)
)

cse$mc <- cse$`Avez-vous connaissance des mycotoxines chez l'arachide`

view(cse)


## Générer le tableau des caractétistiques socioéconomiques----

cse %>%
  tbl_summary(
    include = c("Age_rec","Sexe","Origine","Statut matrimonial","Niveau d'education",
                "superficie_rec","Pour quelle raison produisez vous l'arachide?/But de la culture",
                "exp_rec","Revenu_rec","revdollars_rec","Type de main d'oeuvre",
                "Type de main d'oeuvre"),
      by = "Region",
    percent = "column",
    missing = "always",
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = list(
      all_categorical() ~ c(0,1),
      all_continuous() ~ c(0,1)
    )
  ) %>%
  add_overall() %>%
  add_p() %>% 
  separate_p_footnotes()

cse %>%
  tbl_summary(
    by = "Region",
    percent = "column",
    missing = "always",
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = list(
      all_categorical() ~ c(0,1),
      all_continuous() ~ c(0,1)
    )
  ) %>%
  add_overall() %>%
  add_p() %>% 
  add_significance_stars(hide_p = FALSE,
                         pattern = "{p.value}{stars}")%>%
  separate_p_footnotes()


chisq.test(cse$Region,cse$Age_rec)
chisq.test(cse$Region,cse$Sexe)
chisq.test(cse$Region,cse$Origine)
fisher.test(cse$Region,cse$`Statut matrimonial`)
chisq.test(cse$Region,cse$`Statut matrimonial`)
chisq.test(cse$Region,cse$`Niveau d'education`)
fisher.test(cse$Region,cse$`Niveau d'education`,simulate.p.value = TRUE)
chisq.test(cse$Region,cse$superficie_rec)
fisher.test(cse$Region,cse$superficie_rec,simulate.p.value = TRUE)
chisq.test(cse$Region,cse$exp_rec)
chisq.test(cse$Region,cse$Revenu_rec)
chisq.test(cse$Region,cse$revdollars_rec)
chisq.test(cse$Region,cse$`Type de main d'oeuvre`)


## Analyse de la perception des producteurs----
perc <- read_excel("C:/Users/MAGAT/Desktop/Ntsoli Articles/mon projet/Enquête final article 1.xlsx", 
                  sheet = "Perceptions")



view(perc)

perc %>%
  tbl_summary(
    by = "Region"
  ) %>%
  add_overall() %>%
  add_p() %>% 
  separate_p_footnotes()

## Analyse pratiques des producteurs----
prac <- read_excel("C:/Users/MAGAT/Desktop/Ntsoli Articles/mon projet/Enquête final article 1.xlsx", 
                   sheet = "PPR")
prac$sech <- prac$`durée du séchage gousses jours`

prac$sech_rec <- cut(prac$sech,
                     include.lowest = TRUE,
                     right = FALSE,
                     dig.lab = 4,
                     breaks = c(0, 8, 15, 30)
)

prac$scoreprac_rec <- cut(prac$scoreprac,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(0, 8, 12)
)

view(prac)

prac %>%
  tbl_summary(
    by = "Region"
  ) %>%
  add_overall() %>%
  add_p() %>%
  separate_p_footnotes()

view(prac)

chisq.test(prac$Region,prac$`Site préférentiel`)
chisq.test(prac$Region,prac$`Période favorable de l'année pour semer l'arachide/Mars`)
chisq.test(prac$Region,prac$`Période favorable de l'année pour semer l'arachide/Avril`)
chisq.test(prac$Region,prac$`Période favorable de l'année pour semer l'arachide/Mai`)
chisq.test(prac$Region,prac$`Période favorable de l'année pour semer l'arachide/Juin`)
chisq.test(prac$Region,prac$`Période favorable de l'année pour semer l'arachide/Juillet`)
chisq.test(prac$Region,prac$`Quelle(s) est/sont la(les) période(s) de récolte de l'arachide dans l'année?/Juillet`)
chisq.test(prac$Region,prac$`Quelle(s) est/sont la(les) période(s) de récolte de l'arachide dans l'année?/Aout`)
chisq.test(prac$Region,prac$`Quelle(s) est/sont la(les) période(s) de récolte de l'arachide dans l'année?/Septembre`)
chisq.test(prac$Region,prac$`Quelle(s) est/sont la(les) période(s) de récolte de l'arachide dans l'année?/Octobre`)
chisq.test(prac$Region,prac$`Quelle(s) est/sont la(les) période(s) de récolte de l'arachide dans l'année?/Novembre`)


chisq.test(prac$Region,prac$`Type de semis`)
chisq.test(prac$Region,prac$`Quel type de système de culture employez-vous pour la production de l'arachide?`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Maïs`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Manioc`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Cotton`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Autre`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Igname`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Mil`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Niébé`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Patate`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Sorgho`)
fisher.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Sorgho`)
chisq.test(prac$Region,prac$`En cas d'association, à quelle culture associez-vous l'arachide?/Autre`)
chisq.test(prac$Region,prac$`Effectuez-vous la rotation?`)
chisq.test(prac$Region,prac$`Culture de rotation avec l'arachide`)
fisher.test(prac$Region,prac$`Culture de rotation avec l'arachide`,simulate.p.value=TRUE)

chisq.test(prac$Region,prac$`Utilisez-vous des fertilisants pour la production de l'arachide?`)
chisq.test(prac$Region,prac$`origine semences`)
fisher.test(prac$Region,prac$`origine semences`)
chisq.test(prac$Region,prac$`Quelles types de variétés utilisez-vous?`)
fisher.test(prac$Region,prac$`Quelles types de variétés utilisez-vous?`)
chisq.test(prac$Region,prac$`Comment reconnaissez-vous un plant d'arachide dont les gousses sont arrivées à maturité?/ Indicateur de maturité/Chutes des feuilles`)
chisq.test(prac$Region,prac$`Comment reconnaissez-vous un plant d'arachide dont les gousses sont arrivées à maturité?/ Indicateur de maturité/Couleur des feuilles passant du vert au jaune pâle`)
chisq.test(prac$Region,prac$`Comment reconnaissez-vous un plant d'arachide dont les gousses sont arrivées à maturité?/ Indicateur de maturité/Autre`)
chisq.test(prac$Region,prac$`Quel est le mode de récolte?`)
fisher.test(prac$Region,prac$`Quel est le mode de récolte?`)
chisq.test(prac$Region,prac$`Comment décortiquez-vous?`)
fisher.test(prac$Region,prac$`Comment décortiquez-vous?`,simulate.p.value = TRUE)
chisq.test(prac$Region,prac$sech_rec)
fisher.test(prac$Region,prac$sech_rec,simulate.p.value = TRUE)
chisq.test(prac$Region,prac$`Triez-vous/séparez-vous les bonnes gousses/graines de celles qui sont attaquées, infectées, perforées ou vides?`)
chisq.test(prac$Region,prac$`Lieu de séchage`)
fisher.test(prac$Region,prac$`Lieu de séchage`)
chisq.test(prac$Region,prac$`Méthode de séchage`)
fisher.test(prac$Region,prac$`Méthode de séchage`)
chisq.test(prac$Region,prac$`Surface de séchage`)
chisq.test(prac$Region,prac$`Comment savez-vous que l’opération de séchage a été bien effectuée ?/Bruit/son de la graine dans la gousse...32`)

chisq.test(prac$Region,prac$`Comment savez-vous que l’opération de séchage a été bien effectuée ?/Graine qui se craque facilement...33`)
chisq.test(prac$Region,prac$`Comment savez-vous que l’opération de séchage a été bien effectuée ?/Autre...34`)
fisher.test(prac$Region,prac$`Comment savez-vous que l’opération de séchage a été bien effectuée ?/Autre...34`)
chisq.test(prac$Region,prac$`Contraintes liées au bon déroulement du séchage/Les précipitations durant la période de séchage`)
chisq.test(prac$Region,prac$`Contraintes liées au bon déroulement du séchage/Les contaminations par les moisissures durant les séchage`)
chisq.test(prac$Region,prac$`Contraintes liées au bon déroulement du séchage/Les attaques des insectes durant le séchage`)
chisq.test(prac$Region,prac$`Contraintes liées au bon déroulement du séchage/La surface de séchage`)
fisher.test(prac$Region,prac$`Contraintes liées au bon déroulement du séchage/La surface de séchage`)
chisq.test(prac$Region,prac$`Indicateur d'altération de la graine/Couleur autre que celle propre à la graine(Noire ou verte)`)
chisq.test(prac$Region,prac$`Indicateur d'altération de la graine/Goût(Amertume)`)
chisq.test(prac$Region,prac$`Indicateur d'altération de la graine/Absence de dégâts liés aux insectes (perforation)`)
chisq.test(prac$Region,prac$`Indicateur d'altération de la graine/Autre`)
chisq.test(prac$Region, prac$`Quelle est la durée du stockage en gousses?`)
fisher.test(prac$Region, prac$`Quelle est la durée du stockage en gousses?`,simulate.p.value = TRUE)
chisq.test(prac$Region, prac$`Quel est le type de sac utilisé pour le stockage des graines?`)
fisher.test(prac$Region, prac$`Quel est le type de sac utilisé pour le stockage des graines?`,simulate.p.value = TRUE)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/J'évite les blessures mécaniques lors de la récolte`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Je stocke dans un endroit bien aéré,couvert et à l'abri des infiltrations`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Je sépare les bonnes graines des mauvaises graines dans chaque lot`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Sechage optimal des gousses/graines (taux d'humidité < OuiNon%)`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Utilisation des pesticides chimiques`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Utilisation des produits naturels`)
fisher.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Utilisation des produits naturels`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Utilisation des sacs en toile doublés de polyéthylène`)
fisher.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Utilisation des sacs en toile doublés de polyéthylène`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Aucune`)
chisq.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Autres`)
fisher.test(prac$Region,prac$`Quelle méthode employez-vous pour garantir une bonne conservation des graines avant le stockage?/Autres`)
chisq.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Dégâts des insectes (infestation des stocks)`)
chisq.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Dégâts des rongeurs`)
chisq.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Moisissures`)
chisq.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Absence d'un produit efficace pour limiter l'action des pestes`)
chisq.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Graines germées`)
fisher.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Graines germées`)
chisq.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Vol`)
fisher.test(prac$Region,prac$`Quelles sont les principales contraintes rencontrées lors du stockage des graines?/Vol`)
chisq.test(prac$Region,prac$`Méthodes employées pour faire face aux contraintes de pendant le stockage?/Application des produits de synthèse sur les graines`)
chisq.test(prac$Region,prac$`Méthodes employées pour faire face aux contraintes de pendant le stockage?/Application des substances naturelles sur les graines`)
chisq.test(prac$Region,prac$`Méthodes employées pour faire face aux contraintes de pendant le stockage?/Exposition des stocks au soleil`)
chisq.test(prac$Region,prac$`Méthodes employées pour faire face aux contraintes de pendant le stockage?/Fumigation de la salle de stockage avant le stockage`)
chisq.test(prac$Region,prac$`Méthodes employées pour faire face aux contraintes de pendant le stockage?/Aucune`)
chisq.test(prac$Region,prac$sech_rec)
chisq.test(prac$Region,prac$scoreprac_rec)
fisher.test(prac$Region,prac$scoreprac_rec)


citation("stats")

## Analyse des connaissances----

con <- read_excel("C:/Users/MAGAT/Desktop/Ntsoli Articles/mon projet/Enquête final article 1.xlsx", 
                   sheet = "Connaissances")
view(con)

con$scoc <- con$`Score de connaissance`

## Recodage de con$scoreC en con$scoreC_rec
con$scoc_rec <- cut(con$scoc,
                    include.lowest = TRUE,
                    right = FALSE,
                    dig.lab = 4,
                    breaks = c(0, 10, 16)
)

con %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    by = "Region"
  ) %>%
  add_overall() %>%
  add_p() %>%
  add_significance_stars(hide_p = FALSE,
                         pattern = "{p.value}{stars}")%>%
  separate_p_footnotes()

chisq.test(con$Region,con$`Avez-vous connaissance des mycotoxines chez l'arachide`)
chisq.test(con$Region,con$`Avez-vous connaissance du lien qui existe entre les contaminations par les moisissures et les contaminations par les mycotoxines chez l'arachide?`)
chisq.test(con$Region,con$`Savez-vous que le choix de la période de semis influence la contamination des graines par les moisissures/mycotoxines?`)
chisq.test(con$Region,con$`Savez-vous que le choix de la variété influence le risque de contamination pré-récolte des graines d'arachide par les moisissures/mycotoxines?`)
chisq.test(con$Region,con$`Connaissez-vous au moins le nom d'une variété résistante à l'infection des graines par les moisissures/mycotoxines (Aspergillus flavus)?`)
chisq.test(con$Region,con$`Savez-vous que le précédent cultural influence le risque de contamination de l'arachide par les moisissures?`)
chisq.test(con$Region,con$`Savez-vous que le désherbage permet de réduire l'infection des gousses/graines par les moisissures/mycotoxines?`)
chisq.test(con$Region,con$`Savez-vous que le traitement des semences aux fongicides/substances naturelles avant le semis est recommandé pour éviter l'infection des gousses/graines par les moisissures/mycotoxines dans la phase post-récolte?`)  
chisq.test(con$Region,con$`Savez-vous que l'utilisation des engrais de ferme/compost/Biochar est recommandé pour réduire les risques de contamination des gousses/graines par les moisissures/mycotoxines (Aspergillus flavus)?`)
chisq.test(con$Region,con$`Savez-vous que contrôler les maladies et les insectes qui attaquent les gousses en champ permet de réduire l'infection des graines par les moisissures/mycotoxines (Aspergillus flavus)?`)
chisq.test(con$Region,con$`Savez-vous que les dommages physiques/blessures subis par les gousses lors de la récolte augmentent le risque de contamination par les moisissures/mycotoxines (Aspergillus flavus)?`)
chisq.test(con$Region,con$`Savez-vous que la rotation avec des plantes autres que les céréales/légumineuses permet réduire le risque de contamination des gousses/graines par les moisissures/mycotoxines?`)
chisq.test(con$Region,con$`Savez-vous que le triage des bonnes graines des mauvaises graines permet réduire le risque de contamination par les moisissures/mycotoxines?`)
chisq.test(con$Region,con$`Connaissez-vous les indicateurs qui vous poussent à dire/affirmer que le séchage a été bien effectué?`)
chisq.test(con$Region,con$`Savez-vous que le séchage permet de minimiser le risque de contamination de l'arachide par les moisissures/mycotoxines?`)
chisq.test(con$Region,con$`Savez-vous que les gousses immatures ne doivent pas être mélangées avec les gousses matures pour éviter les contaminations par les moisissures/mycotoxines?`)
chisq.test(con$Region,con$`Connaissez-vous les précautions à prendre pour assurer un bon stockage de vos graines?`)
chisq.test(con$Region,con$scoc_rec)


## Analyse des contraintes----

cont <- read_excel("C:/Users/MAGAT/Desktop/Ntsoli Articles/mon projet/Enquête final article 1.xlsx", 
                  sheet = "Contraintes")

view(cont)

cont %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    by = "Region"
  ) %>%
  add_overall() %>%
  add_p() %>%
  separate_p_footnotes()

## Regression logistique binaire connaissance----

cse$scorec <- cse$`Score de connaissance`

names(cse) <- make.names(names(cse))
GLM <- glm(conmyco ~ 
             Age_rec +Sexe +Niveau.d.education +Origine+Region, 
           family=binomial(logit), data=cse)

car::vif(GLM)

summary(GLM)
exp(coef(GLM))  # Exponentiated coefficients ("odds ratios")

tbl_regression(GLM,exponentiate = TRUE)%>%
  add_glance_source_note()

ggcoef(GLM,exponentiate = TRUE)
ggcoef_model(GLM,exponentiate = TRUE)

## Regression avec multinomial logit----
## extraire la colonne 1 & 5 en une colonne
cse0 = cse[,c(22,23,2,4,30,6,1,31)]
view(cse0)

str(cse0)
cse0$Age_rec<-as.factor(cse0$Age_rec)
cse0$Sexe<-as.factor(cse0$Sexe)
cse0$Origine<-as.factor(cse0$Origine)
cse0$exp_rec<-as.factor(cse0$exp_rec)
cse0$Niveau.d.education<-as.factor(cse0$Niveau.d.education)
cse0$Region<-as.factor(cse0$Region)
cse0$cmyc<-cse0$mc
cse0$cmyc<-as.factor(cse0$mc)

## Afficher l'entête du tableau de données
names(cse)
names(cse0)


## convertion en data frame spécial pour le mlogit----
c0 = mlogit.data(cse0, shape = "wide", choice = "cmyc")

s0 = mlogit(cmyc ~ 0|Age_rec +Sexe + Origine + Niveau.d.education +exp_rec + Region, data = c0)
summary(s0)

s0$coefficients
exp(s0$coefficients)

tbl_regression(s0, exponentiate = TRUE)%>%
  add_glance_source_note()

ggcoef_model(s0,exponentiate = TRUE)

## Regression logistique binaire score de connaissance----

cse$scorec <- cse$`Score de connaissance`

names(cse) <- make.names(names(cse))
GSCO <- glm(sco_rec ~ 
             Age_rec +Sexe +Niveau.d.education +exp_rec +Region, 
           family=binomial(logit), data=cse)

car::vif(GSCO)

summary(GSCO)
exp(coef(GSCO))  # Exponentiated coefficients ("odds ratios")

tbl_regression(GSCO,exponentiate = TRUE)%>%
  add_glance_source_note()

ggcoef(GSCO,exponentiate = TRUE)
ggcoef_model(GSCO,exponentiate = TRUE)
forest_model(GSCO)


citation("car")
citation("gtsummary")
citation("stats")
citation("esquisse")

## Moyenne des scores----

## Chargement des packages pour ANOVA----
library(agricolae)
library(gvlma)
library(ggplot2)
library(ggrepel)
library(ggpubr)

## Score con----
score <- lm(cse$`Score de connaissance` ~ cse$Region)
gvlma(score)
ggqqplot(residuals(score))
oscore <- aov(cse$`Score de connaissance` ~ cse$Region)
outscore <- LSD.test(oscore,"cse$Region")
outscore
mean(cse$`Score de connaissance`)
sd(cse$`Score de connaissance`)

## Score prac
scorep <- lm(cse$scoreprac ~ cse$Region)
gvlma(score)
ggqqplot(residuals(scorep))
oscorep <- aov(cse$scoreprac ~ cse$Region)
outscorep <- LSD.test(oscorep,"cse$Region")
outscorep
mean(cse$scoreprac)
sd(cse$scoreprac)

## Histogramme avec barre d'erreur-----
hsco <- read_excel("C:/Users/MAGAT/Desktop/Ntsoli Articles/mon projet/Enquête final article 1.xlsx", 
                  sheet = "hsco")
view(hsco)

attach(hsco)

## Start----

hsco %>%
  ggplot(aes(x = `Type de score`,
         y= Score,
         fill = Region,
         ymin = lower-0.3,
         ymax = upper+0.3)) +
  geom_col(width = .5,position = position_dodge(.6))+
  geom_errorbar(width = .2, position = position_dodge(.6))+
  scale_fill_manual(values = c("#FFE4C4","#FED966","#95B8D9","#8B6914"))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,10),
                     breaks = seq(0,9,2))+
  labs(
    x = "Type de score",
    y = "Score"
  )+
  theme_light()
  
  
 




