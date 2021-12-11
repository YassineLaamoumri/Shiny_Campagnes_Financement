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
library(gganimate)
#Importation des données
data <-
  read_delim('data_ulule_2019.csv', locale = locale(encoding = stri_enc_get()))

glimpse(data)

# On supprime la première variable qui représente seulement l'index de la ligne

data <- data %>% select(-...1)

# Supprimons les campagnes annulées

data <- data %>% filter(is_cancelled == FALSE)


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


# Ajout variable année-mois
data <-
  data %>% mutate(year_month = parse_date_time(paste0(year(date_start), month(date_start)), 'ym'))

# On ajoute la colonne année
data <- data %>% mutate(year = year(date_start))

# On ajoute la colonne mois
data <- data %>% mutate(month = month(date_start))


# On ajoute la colonne jour
data <- data %>% mutate(day = day(date_start))


# On supprimes les lignes dupliquées

data <- data %>% distinct()


# montant moyen des campagnes financées

data %>% group_by(category) %>% summarise(mean_amount = mean(amount_raised)) %>% ggplot(aes(x =
                                                                                              category, y = mean_amount, fill = category)) + geom_bar(stat = 'identity') + theme_minimal() +
  labs(title =
         'Montant moyen des campagnes financées par catégorie', x = 'Catégorie', y = 'Montant moyen')




# Durée médiane des campagnes financées

data %>% group_by(category) %>% summarise(median_amount = median(amount_raised)) %>% ggplot(aes(x =
                                                                                                  category, y = median_amount, fill = category)) + geom_bar(stat = 'identity') + theme_minimal() +
  labs(title =
         'Montant moyen des campagnes financées par catégorie', x = 'Catégorie', y = 'Montant moyen')



# Top 10 campagnes avec les plus hauts financement

test <-
  data %>% group_by(category) %>% arrange(desc(amount_raised)) %>% slice(1:10) %>% select(category, id, amount_raised, year)


write.csv2(as.data.frame(test), 'test.csv')



data_perimetre1() %>%  group_by(year_month) %>% summarise(total = n()) %>% ggplot(mapping = aes(x =
                                                                                                  year_month, y = total)) + geom_line() + scale_x_datetime(breaks = '1 month', labels = label_date(format = "%Y-%m-%d")) + theme_minimal() +
  labs(title =
         'Variabilité du nombre de campagnes par mois', x = 'Date', y = 'Nombre de campagnes')



## En bonus, nous allons effectuer un graphique animé



data_courbe <-
  data %>% group_by(category, year_month) %>% arrange(year_month) %>% summarise(total = sum(amount_raised))

multiple_courbe <- data_courbe %>% ggplot(mapping = aes(x =
                                       year_month, y = total,color = category)) + geom_line() + scale_x_datetime(breaks = '1 month', labels = label_date(format = "%Y-%m-%d")) + theme_minimal() +
  labs(title =
         'Montant des financements de campagnes par mois', x = 'Date', y = 'Nombre de campagnes') + scale_y_continuous()



anim_multiple_courbe <- data_courbe %>% ggplot(mapping = aes(x =
                                                          year_month, y = total,color = category)) + geom_line() + scale_x_datetime(breaks = '1 month', labels = label_date(format = "%Y-%m-%d")) + theme_minimal() +
  labs(title =
         'Montant des financements de campagnes par mois', x = 'Date', y = 'Nombre de campagnes')  + transition_reveal(year_month) +theme_light() +
  view_follow()


anim_multiple_courbe

animate(
  plot = anim_multiple_courbe,
  render = gifski_renderer(),
  height = 600,
  width = 800,
  duration = 15,
  fps = 25
)

anim_save('anim_multiple_courbe.gif')


