library(tidyverse)

tidyverse_update()

library(readr)
library(readxl)
library(skimr)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(data.table)
library(stringr)
library(forcats)
library(tidyr)
library(stringi)
library(scales)

#Importation des données
data <-
  read_delim('data_ulule_2019.csv', locale = locale(encoding = stri_enc_get()))

glimpse(data)

# On supprime la première variable qui représente seulement l'index de la ligne

data <- data %>% select(-...1)

# Supprimons les campagnes annulées

data <- data %>% filter(is_cancelled == FALSE)


# On garde les données des deux dernières années
data <- data %>% filter(year(date_start) >= 2019 |
                  (month(date_start) >= 10 &
                     year(date_start) >= 2018))

#Selectionnons les 8 pays ayant le plus de campagnes au total


data  %>% drop_na(country) %>% count(country, sort = TRUE) %>% slice(1:8)

# On récupère la liste des 8 pays ayant le plus de campagnes au total
top8_country <-
  data  %>% drop_na(country) %>% count(country, sort = TRUE) %>% slice(1:8) %>% select(country) %>% as_vector()


data <- data %>% filter(country %in% top8_country)

#Supprimons les données du mois le plus récent car il y a un risque que les données soient incomplétes


most_recent_date <-
  data %>% select(date_start) %>% arrange(desc(date_start)) %>% slice(1) %>% mutate(month = month(date_start), year = year(date_start))

row_to_eliminate <-
  data %>% filter(
    month(date_start) == most_recent_date$month &
      year(date_start) == most_recent_date$year
  )

data <- data %>% anti_join(row_to_eliminate)


#Suppression des variables inutiles pour les visualisations

data <-
  data %>% select(-finished.1,-absolute_url,-is_cancelled,-city)


#Creation de tableaux de taux de conversion



# On récupère les différentes devises afin de les convertir en en euro

rates_currency <-
  data %>% distinct(currency) %>% mutate(Taux = c(0.87, 1, 0.69, 1.17, 0.95))

rates_currency




data <- data %>% mutate(
  amount_raised = case_when(
    currency == 'USD' ~ amount_raised * rates_currency %>% filter(currency ==
                                                                    'USD') %>% select(Taux) %>% as.double(),
    currency == 'CAD' ~ amount_raised * rates_currency %>% filter(currency ==
                                                                    'CAD') %>% select(Taux) %>% as.double(),
    currency == 'GBP' ~ amount_raised * rates_currency %>% filter(currency ==
                                                                    'GBP') %>% select(Taux) %>% as.double(),
    currency == 'CHF' ~ amount_raised * rates_currency %>% filter(currency ==
                                                                    'CHF') %>% select(Taux) %>% as.double(),
    currency == 'EUR' ~ amount_raised * rates_currency %>% filter(currency ==
                                                                    'EUR') %>% select(Taux) %>% as.double()
    
  )
)



#On supprime la colonne currency puisque tout a été converti en EUR

data <- data %>% select(-currency)






#Calculons les nombres de jours des campagnes

data %>% count(is.na(date_start))
# On retrouve 46 campagnes n'ayant pas de date de début, on décide de les supprimer car il n'est pas possible de calculer la durée de la campagne

data <- data %>% filter(!is.na(date_start))

data <-
  data %>% mutate(duration_days = round(as.double(
    difftime(date_end, date_start, units = "days"), units = "days"
  )))


# On récupère les 6 catégories principales

best_categories <-
  data %>%  count(category, sort = TRUE) %>% slice(1:6) %>% select(category) %>% as_vector()

# On garde seulement les 6 catégories principales et le reste des campagnes est placé dans la catégories "other"
data <-
  data %>% mutate(
    category = case_when(
      category %in% best_categories ~ category,!category %in% best_categories ~ 'other'
    )
  )


# -	Evolution du nombre de campagnes crées au fil des mois, sur les deux dernières années

data <-
  data %>% mutate(year_month = parse_date_time(paste0(year(date_start), month(date_start)), 'ym'))

data %>% filter(year(date_start) >= 2019 |
                  (month(date_start) >= 10 &
                     year(date_start) >= 2018)) %>% ggplot() + geom_bar(aes(x = year_month))


## Nombre total nombre total de campagnes crées diagramme empilée

data %>% ggplot(mapping = aes(x = year_month, fill = category)) + geom_bar()


## Nombre total nombre total de campagnes

data %>%  group_by(year_month) %>% summarise(total = n()) %>% ggplot(mapping = aes(x =year_month,y=total)) +geom_line() + scale_x_datetime(breaks = '1 month',labels = label_date(format = "%Y-%m-%d")) + theme_minimal() +labs(title =
                                                                                                                                                                                                                                  'Variabilité du nombre de campagnes par mois', x = 'Date', y = 'Nombre de campagnes') 


#-	Nombre de campagnes par catégorie
data   %>% ggplot(mapping = aes(x = category, fill = category)) + geom_bar()



# -	Nombre de campagnes par pays
data   %>% ggplot(mapping = aes(x = country, fill = country)) + geom_bar()




# -	Distribution de la durée des campagnes


data %>% ggplot() + geom_density(aes(x = duration_days))






# On ajoute la colonne année
data <- data %>% mutate(year = year(date_start))

# On ajoute la colonne mois
data <- data %>% mutate(month = month(date_start))


# On ajoute la colonne jour
data <- data %>% mutate(day = day(date_start))


# -	Evolution de la proportion de campagnes financées au fil des mois, sur les deux dernières années

data %>% filter(year(date_start) >= 2019 |
                  (month(date_start) >= 10 &
                     year(date_start) >= 2018)) %>% group_by(year_month) %>% summarize(prop = sum(goal_raised) /
                                                                                         n() * 100) %>% ggplot() + geom_bar(aes(x = year_month, y = prop), stat = 'identity')




# -	Evolution des montants moyens des campagnes financées au fil des mois / années, sur les deux dernières années

data %>% filter(year(date_start) >= 2019 |
                  (month(date_start) >= 10 &
                     year(date_start) >= 2018)) %>% group_by(year_month) %>% summarize(mean_amount_raised = mean(amount_raised)) %>% ggplot() + geom_bar(aes(x = year_month, y = mean_amount_raised), stat = 'identity')



