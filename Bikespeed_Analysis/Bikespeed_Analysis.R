library(ggplot2)
library(dplyr)
library(gridExtra)
library(readxl)
library(writexl)
library(car)
library(tidyr)
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")  ## Commande qui modifie les paramètres
# régionaux de cette session R, spécifiquement ceux qui contrôlent le formatage
# des dates et heures (LC_TIME), pour utiliser la langue française. Selon les
# configurations système de chaque utilisateur, le paramètre de localisation peut
# être différent d’un utilisateur à l'autre, ce qui peut conduire à des erreurs
# dues à l’incompatibilité entre l’output et la langue dans laquelle ce script a
# été écrit, notamment, en particulier par rapport aux données de calendrier.

#################### 

#### Partie 1 : transformation des données 

####################

# Charger les données 
setwd("C:/Users/olivi/OneDrive/Bureau/Data science/Junior Data Analyst/Projets/Projet de groupe")

borne_data <- read.csv('CB1599_history.csv')
weather_data <- read_excel("uccle.xlsx", skip = 1)
conges <- read_excel("Conges_2024.xlsx")
duree_jour <- read_excel("Durée Ensoleillement.xlsx")

# Conversion de la date et heure 
# Convertir les intervalles de 15 minutes en heures réelles pour borne_data
colnames(borne_data) <- tolower(colnames(borne_data))  
borne_data$date <- as.Date(borne_data$date, format="%Y-%m-%d")
borne_data$heure <- ((borne_data$time.gap) * 15) / 1440
borne_data$heure <- format(as.POSIXct(borne_data$heure * 86400,
                                      origin = "1970-01-01",
                                      tz = "UTC"),
                           "%H:%M")
borne_data <- borne_data %>%
  mutate(heure = ifelse( heure=="00:00" , "24:00",heure)) ## Pour éviter toute
# confusion, nous utiliserons l'heure "24:00" pour représenter minuit, qui est
# effectivement le gap 96 qui équivaut à la dernière récolte de donnée d'une
# même date. Selon les cas, lorsque l'heure "00:00" est utilisée, elle est
# interprétée comme la fin de la journée précédente/début de la journée en
# question (-> minuit). Pour des analyses chronologiques, on considère donc
# qu'il est plus adapté de représenter cette heure comme  étant "24:00".

# Conversion de la date et heure pour weather_data
weather_data <- transform(weather_data,
                          date = as.Date(UTC),  # Extraire uniquement la date
                          heure = as.numeric(format(UTC, "%H")) / 24) # Extraire l'heure en fraction de jour
weather_data$heure <- format(as.POSIXct(weather_data$heure * 86400,
                                        origin = "1970-01-01",
                                        tz = "UTC"),
                             "%H:%M")
## Selon "opendata.meteo.be", les données météo faisant référence à l'heure
## "00:00" comme début de la journée, ont en réalité été obtenues dans une période
## antérieure. Pour des analyses chronologiques et la fusion des fichiers
# on considère donc qu'il est plus adapté de représenter cette
## heure comme "24:00" de la date antérieure.
weather_data <- weather_data %>%
  mutate(
    date = ifelse(heure == "00:00", date - 1, date),
    heure = ifelse(heure == "00:00", "24:00", heure)
  )
weather_data$date <- as.Date(weather_data$date, format="%Y-%m-%d")

# Fusion des données de la borne vélo avec le fichier météo 
merged_data <- merge(borne_data, weather_data, 
                     by=c("date", "heure"), 
                     all.x=TRUE)
# Fusion de merged_data avec les heures de lever et de coucher du soleil 
colnames(duree_jour) <- tolower(colnames(duree_jour))
duree_jour$date <- as.Date(duree_jour$date, format="%Y-%m-%d")
duree_jour_subset <- duree_jour[, c("date","lever","coucher","duree_jour")]

merged_data2 <- merge(merged_data,duree_jour_subset,by = "date")
# Fusion de merged_data avec le fichier congé pour obtenir final_data
colnames(conges) <- tolower(colnames(conges))           
conges$date <- as.Date(conges$date, format="%Y-%m-%d")  
final_data  <- merge(merged_data2, conges, 
                     by="date",
                     all.x=TRUE)

## Ordonner les données de final_data par date et heure
final_data <- final_data[order(final_data$date, final_data$heure),]

write_xlsx(final_data, "final_data.xlsx")

############

#### Partie 2 : Fonction pour création d'un Heatmap et d'un graphique en ligne 

############

analyser_borne <- function(file_name) {
  
  # Extraire le nom de la borne (sans le .csv)
  nom_borne <- tools::file_path_sans_ext(basename(file_name))
  
  borne_data <- read_excel(file_name)
  
  # Nettoyage et transformation
  colnames(borne_data) <- tolower(colnames(borne_data))  
  borne_data$date <- as.Date(borne_data$date, format="%Y-%m-%d")
  
  # Convertir les intervalles de 15 minutes en heures réelles
  borne_data$heure <- ((borne_data$time.gap) * 15) / 1440
  borne_data$heure <- format(as.POSIXct(borne_data$heure * 86400,
                                        origin = "1970-01-01",
                                        tz = "UTC"),
                             "%H:%M")
  
  # Ajouter les variables calendrier en facteurs ordonnés par niveaux
  borne_data$jour_semaine <- weekdays(borne_data$date)
  borne_data$jour_semaine <- factor(borne_data$jour_semaine,
                                    levels=c("lundi","mardi","mercredi","jeudi",
                                             "vendredi","samedi","dimanche"))
  borne_data$mois <- months(borne_data$date)
  borne_data$mois <- factor(borne_data$mois,
                            levels=c("janvier","fevrier","mars","abril","mai",
                                     "juin","juillet","aout","septembre",
                                     "octobre","novembre","decembre"))
  
  # Agréger les données pour la heatmap
  heatmap_data <- borne_data %>%
    mutate(heure = floor((time.gap - 1) / 4) + 1,
           ## Garder uniquement l'heure entière (de 1 à 24)
           average.speed = ifelse(average.speed < 0, NA, average.speed)) %>%
    group_by(jour_semaine, heure) %>%
    summarise(vitesse_moy = sum(count * average.speed, na.rm = TRUE) / sum(count, na.rm = TRUE)) %>%
    ungroup()
  
  # Average.speed est pondérée par count par heure et jour
  
  # Ordre des jours
  heatmap_data$jour_semaine <- factor(heatmap_data$jour_semaine)
  
  # Création de la heatmap (ggplot)
  heatmap_plot <- ggplot(heatmap_data, aes(x = jour_semaine,
                                           y = as.numeric(heure),
                                           fill = vitesse_moy)) +
    geom_tile(color = NA) +  
    geom_text(aes(label = round(vitesse_moy, 2)), size = 3) +
    scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                         midpoint = mean(heatmap_data$vitesse_moy, na.rm = TRUE),
                         limits = c(18.5, 23), oob = scales::squish) +
    scale_y_reverse(breaks = seq(1, 24, 1)) +
    labs(title = paste("Heatmap de la vitesse moyenne -", nom_borne),
         x = "Jour de la semaine", y = "Heure", fill = "Vitesse Moy.") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_discrete(position = "top")  
  
  # Ouvrir une nouvelle fenêtre graphique et afficher la heatmap
  dev.new()
  grid.arrange(heatmap_plot, nrow = 1) # Force l'affichage de la heatmap dans la première moitié
  
  # Réutilisation de `heatmap_data` pour le graphique linéaire
  jours <- unique(heatmap_data$jour_semaine) 
  couleurs <- rainbow(length(jours))  
  
  # Création du graphique linéaire
  dev.new()
  par(mar = c(4,5,4,7), xpd = TRUE)  
  plot(NULL, xlim = c(1, 24), ylim = c(17, 25), xlab = "", ylab = "", type = "n",
       xaxt = "n", yaxt = "n", bty = "n")  
  abline(h = seq(from = 17, to = 25, by = 1), col = "gray", xpd = FALSE)
  
  # Ajout des labels
  mtext("Heure", side = 1, line = 1)
  mtext("Vitesse Moyenne", side = 2, line = 3)
  mtext(paste("Vitesse moyenne par jour en fonction de l'heure -", nom_borne),
        side = 3, line = 1.5, cex = 1.2)
  
  # Ajout des axes
  axis(side = 1, at = seq(1, 24, 1), labels = seq(1, 24, 1), tick = FALSE,
       padj = -2)
  axis(side = 2, at = seq(17, 25, 1), tick = FALSE, las = 2)
  
  # Tracer les courbes pour chaque jour
  for (i in seq_along(jours)) {  
    jour_data <- heatmap_data[heatmap_data$jour_semaine == jours[i], ]
    lines(as.numeric(jour_data$heure), jour_data$vitesse_moy, col = couleurs[i],
          lwd = 2)
    points(as.numeric(jour_data$heure), jour_data$vitesse_moy, col = couleurs[i],
           pch = 16, cex = 0.7)
  }
  
  # Ajout de la légende
  legend("right", legend = jours, col = couleurs, lwd = 1, pch = 16,
         xpd = TRUE, bty = "n", inset = c(-0.31, 0), title = "Jour")
}

# Pour utiliser la fonction avec le fichier CSV de la borne, il suffit de
# remplacer la ligne suivante dans la fonction
#  borne_data <- read_excel(file_name)
# Par 
#  borne_data <- read.csv(file_name)
# Ainsi qu'attribuer le fichier CSV au file_name ci_dessous

# Appel de la fonction avec le fichier CB1599_history.csv 
file_name = "final_data.xlsx"
analyser_borne(file_name)

############ 

#### Partie 3 : Régression linéaire (sans variables météo) 

############

# Charger les données (permet d'avoir un code modulable) 
# Si déjà fait avant la création du fichier excel final_data, on peut aussi 
# simplement utiliser la ligne suivante avant de poursuivre: 
# bike_data = final_data 
bike_data = read_excel("final_data.xlsx")
colnames(bike_data) <- tolower(colnames(bike_data))  ### Vérifier si les colonnes ne sont pas déjà en minuscule suite à la fusion des fichiers
bike_data$date <- as.Date(bike_data$date, format="%Y-%m-%d") ### Idem pour les dates 

#####  Préparation des variables pour les analyses de régression linéaires 
# Insérer variables calendrier sous forme de facteurs pour modèle de régression multiple
bike_data$jour_semaine = factor(weekdays(bike_data$date),
                                levels=c("lundi","mardi","mercredi","jeudi",
                                         "vendredi","samedi","dimanche"))
bike_data$mois = factor(months(bike_data$date),
                        levels=c("janvier","février","mars","avril","mai",
                                 "juin","juillet","août","septembre","octobre",
                                 "novembre","décembre"))
bike_data$vacances = factor(ifelse(is.na(bike_data$conge), 0, 1))
print(bike_data$vacances)

# Convertir les intervalles de 15 minutes en heures réelles
bike_data$heure <- ((bike_data$time.gap) * 15) / 1440
bike_data$heure <- format(as.POSIXct(bike_data$heure * 86400,
                                     origin = "1970-01-01",
                                     tz = "UTC"),
                          "%H:%M")
bike_data <- bike_data %>%
  mutate(heure = ifelse( heure=="00:00" , "24:00",heure))
# Garder uniquement l'heure entière
bike_data$heure_pleine = floor((bike_data$time.gap - 1) / 4) + 1
# Garder uniquement l'heure entière (de 1 à 24)
bike_data$heure_pleine <- as.factor(bike_data$heure_pleine)
# Changer valeurs "-1" par "NA"
bike_data$average.speed = ifelse(bike_data$average.speed < 0, NA,
                                 bike_data$average.speed)

#### Visulisation des vitesses en fonction des heures, jours et mois
# Répartition selon l'heure 
dev.new()
ggplot(na.omit(bike_data[, c("heure_pleine", "average.speed")]),
       aes(x = heure_pleine, y = average.speed)) +
  geom_boxplot() +
  labs(title = "Vitesse moyenne en fonction de l'heure",
       x = "Heure",
       y = "Vitesse moyenne")

# Répartition selon le jour de la semaine
dev.new()
ggplot(na.omit(bike_data[, c("jour_semaine", "average.speed")]),
       aes(x = jour_semaine, y = average.speed)) +
  geom_boxplot() +
  labs(title = "Vitesse moyenne en fonction du jour de la semaine",
       x = "Jour de la semaine",
       y = "Vitesse moyenne")

# Répartition selon le mois (substitut de l'évolution saisonnière)
dev.new()
ggplot(na.omit(bike_data[, c("mois", "average.speed")]),
       aes(x = mois, y = average.speed)) +
  geom_boxplot() +
  labs(title = "Vitesse moyenne en fonction du mois",
       x = "Mois",
       y = "Vitesse moyenne")

#### Régressions linéaires uniquement avec les variables calendrier

## Creation du data subset "reg_data" qui inclut les données pertinentes        
## du data set "bike_data". "reg_data" comprend le nombre de vélos, leur        
## vitesse moyenne pondérée par heure ainsi que le jour de la semaine,          
## le mois correspondant et une variable binaire pour les vacances              

reg_data = bike_data %>%
  select (date, heure_pleine, jour_semaine, mois, count, average.speed, vacances) %>%
  group_by(date, heure_pleine, jour_semaine, mois, vacances) %>%
  summarise(
    average.speed = sum(average.speed * count, na.rm = TRUE) / sum(count,
                                                                   na.rm = TRUE),
  ) %>%
  ungroup ()

# Construction du modèle de régression multiple - version 1
reg_data_clean <- reg_data[!is.na(reg_data$average.speed),]
modele_1 <- lm(average.speed ~ heure_pleine + jour_semaine + mois + vacances,
               data = reg_data_clean)
summary(modele_1)

# R^2 de 0.229 assez faible démontrant un faible pouvoir prédictif (erreur
# standard de 2.22) des variables heure_pleine, jour_semaine mois et vacances sur
# la variable average.speed.

# Construction du modèle de régression multiple - version 2 - Utilisation du data
# set "bike_data" avec des gaps de 15 minutes. 
# L'ensemble des valeurs (variables dépendante et indépendantes) sont pondérées 
# cette fois avec l'argument weights = count
var_modele <- c("heure_pleine","jour_semaine","date","mois","count",
                "average.speed", "vacances")
bike_data_clean = na.omit(bike_data[,var_modele])

modele_2_pond <- lm(average.speed ~ heure_pleine + jour_semaine + mois + vacances,
                    data = bike_data_clean, weights = count)
summary(modele_2_pond)

## Table avec comparaison des statistiques obtenues avec les modèles antérieurs

## Créer une fonction pour extraire les statistiques
extraire_stats <- function(modele) {
  s <- summary(modele)
  data.frame(
    Residual_standard_error = s$sigma,
    degrees_of_freedom = df.residual(modele),
    Multiple_R_squared = s$r.squared,
    Adjusted_R_squared = s$adj.r.squared,
    F_statistic = s$fstatistic[[1]],
    p_value = pf(s$fstatistic[[1]], s$fstatistic[[2]], s$fstatistic[[3]],
                 lower.tail = FALSE)
  )
}

# Extraire les statistiques de chaque modèle
stats_modele_1 <- extraire_stats(modele_1)
stats_modele_2_pond <- extraire_stats(modele_2_pond)

# Combiner les statistiques
statistiques_A <- rbind(stats_modele_1, stats_modele_2_pond)

# Créer un vecteur de noms pour les modèles
noms_modeles <- c("modele_1", "modele_2_pond")

# Attribuer les noms de lignes et transpocer la table
rownames(statistiques_A) <- noms_modeles

# Afficher les statistiques
print(statistiques_A)

## Essais de différents modèles avec transformation de la variable expliquée

# Tentative avec une transformation logarithmique
modele_log <- lm(log(average.speed) ~ heure_pleine + jour_semaine + mois + vacances,
                 data = reg_data_clean)
summary(modele_log)

# Transformation carrée 
# La fonction I() permet d'inhiber l'interprétation spéciale et force
# l'évaluation littérale de l'expression. En effet, x^2 pourrait être interprété
# comme "inclure x et ses interactions jusqu'au second ordre", plutôt que
# littéralement comme le carré de x.

modele_square <- lm(I(average.speed^2) ~ heure_pleine + jour_semaine + mois + vacances,
                    data = reg_data_clean)
summary(modele_square)

# Transformation cubique
modele_cubic <- lm(I(average.speed^3) ~ heure_pleine + jour_semaine + mois + vacances,
                   data = reg_data_clean)
summary(modele_cubic)

# Transformation racine carrée
modele_sqrt <- lm(sqrt(average.speed) ~ heure_pleine + jour_semaine + mois + vacances,
                  data = reg_data_clean)
summary(modele_sqrt)

# Transformation racine cubique
modele_cuberoot <- lm(I(average.speed^(1/3)) ~ heure_pleine + jour_semaine + mois + vacances,
                      data = reg_data_clean)
summary(modele_cuberoot)

## Table avec les statistiques des différents modèles avec transformation de
## la variable expliquée, et comparaison avec modele1

# Extraire les statistiques de chaque modèle
stats_modele_log <- extraire_stats(modele_log)
stats_modele_square <- extraire_stats(modele_square)
stats_modele_cubic <- extraire_stats(modele_cubic)
stats_modele_sqrt <- extraire_stats(modele_sqrt)
stats_modele_cuberoot <- extraire_stats(modele_cuberoot)

# Combiner les statistiques
statistiques_B <- rbind(stats_modele_1, stats_modele_log, stats_modele_square,
                        stats_modele_cubic, stats_modele_sqrt,
                        stats_modele_cuberoot)

# Créer un vecteur de noms pour les modèles
noms_modeles <- c("modele_1", "modele_log", "modele_square", "modele_cubic",
                  "modele_sqrt", "modele_cuberoot")

# Attribuer les noms de lignes et transpocer la table
rownames(statistiques_B) <- noms_modeles

# Afficher les statistiques
print(statistiques_B)

# Les modèles modele_square et modele_cubic ont des erreurs standard résiduelles
# très élevées et peuvent être écartés. Les modèles modele_sqrt, modele_log et
# modele_cuberoot ont les plus petites erreurs standard résiduelles, mais leur
# R^2 ajusté est moins élevé que celui de modele_1. Ces modèles offrent donc
# davantage de précision (c'est-à-dire une meilleure adéquation aux données), en
# expliquant légèrement moins de variance de la vitesse moyenne. Cependant, les
# modèles plus complexes avec un R^2 ajusté peuvent être plus propices au
# sur-ajustement. De plus, la valeur du R^2 n'augmente pas significativement avec
# ces modèles, ce qui nous permet de conclure que nous devrions évaluer les
# hypothèses vis-à-vis du modele_1.

# Vérifier la multicolinéarité entre les variables explicatives

# Calculer les VIFs
vif_resultats <- vif(modele_1)

# Afficher les résultats
print(vif_resultats)

# Le facteur d’inflation de la variance (VIF) qui est bon si inférieur à 10.
# Les valeurs de GVIF sont toutes extrêmement proches de 1. Cela suggère fortement
# qu'il n'y a pas de problème de multicolinéarité significatif entre les variables
# heure_pleine, jour_semaine, mois et vacances dans le modèle. Les erreurs
# standard des coefficients de régression ne sont pas gonflées en raison de la
# multicolinéarité.

###
# Observation de la normalité des résidus à l'aide d'un histogramme des résidus
# Relativement centré autour de 0 et symétrique, ce qui est un bon indicateur de
# normalité «globale». L’absence de pics ou de longues queues très marquées
# suggère qu’il n’y a pas de valeurs extrêmes en disproportion.
dev.new()
hist(modele_1$residuals, breaks = 30,
     main = "Histogramme des résidus - modele_1",
     xlab = "Résidus")

# Q-Q plot des résidus
# Cette forme en S indique souvent des queues de distribution un peu plus épaisses 
# ou un léger aplatissement par rapport à une distribution normale, probablement 
# le cas présent vu la répartition des vitesses moyennes dans une plage assez
# limitée dans la grande majorité des cas. 
# Cela peut aussi refléter des sous-groupes de données non modélisés. Par exemple,
# différents types de véhicules, variation du niveau des cyclistes, âge, contexte
# de déplacement, Sens de circulation (montée ou descente), Point de départ ou
# d'arrivée du trajet (la borne est située à l'entrée du campus de l'ULB).
dev.new()
qqnorm(modele_1$residuals, main = "Normal Q-Q Plot - modele_1")
qqline(modele_1$residuals, col = "red")

## Résidus du modèle en fonction de l'index des observations, pour vérifier
# l'hypothèse d'indépendance. Le graphique ne révèle pas de motifs évidents
# d'autocorrélation (corrélation entre les résidus).Les résidus semblent être
# répartis de manière indépendante.
dev.new()
plot(modele_1$residuals, type = "l",
     main = "Résidus vs. Index - modele_1",
     xlab = "Index des Observations",
     ylab = "Résidus")
abline(h = 0, col = "red")

# Observation de l'homoscédasdicité (la constance de la variance des résidus) et
# de la linéarité. Les résidus semblent relativement bien répartis, sans motif
# d’éventail (pas de cône ouvert/fermé), ce qui suggère peu d’hétéroscédasticité
# (donc une variance des résidus aléatoire)
# Les points semblent être répartis de manière relativement aléatoire autour de
# la ligne rouge (zéro). Il n'y a pas de motif de courbure clair ou de tendance
# identifiable qui indiquerait une non-linéarité. Cela suggère que l'hypothèse
# de linéarité est probablement satisfaite.
dev.new()
plot(modele_1$fitted.values, modele_1$residuals,
     main = "Résidus vs Valeurs ajustées - modele_1",
     xlab = "Valeurs ajustées",
     ylab = "Résidus")
abline(h = 0, col = "red")

# Vérification des outliers avec la distance de Cook 

# Calculer la distance de Cook pour chaque observation
cooks_d <- cooks.distance(modele_1)

# Examiner la distribution avec un histogramme
# La grande majorité des observations semblent avoir une influence très faible
# sur le modèle (distance de Cook proche de 0).
hist(cooks_d, breaks = 50, main = "Histogramme de la distance de Cook", 
     xlab = "Distance de Cook")

# Calculer le quantile 95% 
quantile_95 <- quantile(cooks_d, 0.95)
print(quantile_95)

# Définir le seuil comme le 95ème quantile
seuil <- quantile_95

# Tracer les distances de Cook et ajouter la ligne du seuil
dev.new()
plot(cooks_d, type = "h", main = "Distance de Cook", ylab = "Distance de Cook")
abline(h = seuil, col = "red", lty = 2)
text(x = which(cooks_d > seuil), y = cooks_d[cooks_d > seuil],
     labels = which(cooks_d > seuil), pos = 4, cex = 0.7, col = "blue")

# la majorité des points sont collés au bas (proches de 0) et seules quelques
# observations s’élèvent au-dessus. Les indices en bleu correspondent aux
# observations potentiellement les plus influentes - dont la distance de Cook
# dépasse le niveau moyen

# Extraire les indices des observations influentes
indices_influents <- which(cooks_d > seuil)
length(indices_influents)
print(indices_influents)

# Extraire les observations influentes
influential_obs <- reg_data_clean[indices_influents, ]

# Afficher un résumé
summary(influential_obs)

# Les vitesses influentes ne sont pas nécessairement «extrêmes». 
# Maximum à 44km/h, médiane à 16, et min à 4, soit des valeurs plausibles pour
# des trajets à vélo. Plus de valeurs extrêmes entre minuit et 5 heures du matin,
# les gens allant certainement sensiblement plus vite lorsqu'il n'y a personne
# sur la route. Plus de valeurs le mercredi et surtout le dimanche, laissant
# potentiellement soupçonner une utilisation différente lorsqu'il y a plus de
# temps libre. Plus de situations "anormales" durant les mois plus froids
# (octobre à mars), pouvant être causées par la persistance des vélos électriques
# en hiver (alors disproportionnés) ou par le fait que la proportion de gens se
# rendant à l'université augmente (moins de cyclistes loisir dans cette période).
# De plus, leur point de départ/arrivée se situant à proximité de la borne 
# (pourraient donc les influencer à aller sensiblement plus lentement.

# Afficher quelques lignes pour confirmer l'intuition (pas très concluant, besoin
# de plus d'observations)
head(influential_obs)

# Tout de même une réalisation du modèle sans les observations influentes
reg_data_clean2 <- reg_data_clean[-indices_influents, ]
modele_sans_influents <- lm(average.speed ~ heure_pleine + jour_semaine + mois + vacances,
                            data = reg_data_clean2)
summary(modele_sans_influents)

# Comparer les statistiques du modele_1 par rapport au modele_sans_influents
stats_modele_sans_influents <- extraire_stats(modele_sans_influents)
statistiques_C <- rbind(stats_modele_1, stats_modele_sans_influents)
noms_modeles <- c("modele_1", "modele_sans_influents")
rownames(statistiques_C) <- noms_modeles
print(statistiques_C)

# Vérifier la multicolinéarité entre les variables explicatives
vif_resultats <- vif(modele_sans_influents)
print(vif_resultats)
# Il n'y a pas de problème de multicolinéarité significatif entre les variables,
# dans ce nouveau modèle (sans observations influentes).

# Observation de la normalité des résidus à l'aide d'un histogramme des résidus
# et d'un Q-Q plot des résidus
dev.new()
hist(modele_sans_influents$residuals, breaks = 30,
     main = "Histogramme des résidus - modele_sans_influents",
     xlab = "Résidus")
dev.new()
qqnorm(modele_sans_influents$residuals, main = "Normal Q-Q Plot - modele_sans_influents")
qqline(modele_sans_influents$residuals, col = "red")
# Histogramme Relativement centré autour de 0 et symétrique, ce qui est un bon
# indicateur de normalité «globale». QQ Plot avec queues aux extrémités de la
# courbe, moins proéminentes que pour le modele 1. Il n'y a pas de déviations
# significatives. L'hypothèse de normalité est respectée.

## Résidus du modèle en fonction de l'index des observations, pour vérifier
# l'hypothèse d'indépendance.
dev.new()
plot(modele_sans_influents$residuals, type = "l",
     main = "Résidus vs. Index - modele_sans_influents",
     xlab = "Index des Observations",
     ylab = "Résidus")
abline(h = 0, col = "red")

# Verification des hypothèses de linéarité et homoscédasdicité
dev.new()
plot(modele_sans_influents$fitted.values, modele_sans_influents$residuals,
     main = "Résidus vs Valeurs ajustées - modele_sans_influents",
     xlab = "Valeurs ajustées",
     ylab = "Résidus")
abline(h = 0, col = "red")
# L'hypothèse de linéarité semble remplie, et l'hypothèse d'homoscédasticité
# est également satisfaite, la variance des résidus parait assez aléatoire.

# Les hypothèses sous-jacentes sont vérifiées et supportent un modèle de
# régression linéaire valide, fiable et interprétable jusqu'à un certain point:
# Le R^2 de ce second modèle passe à 0.382 (vs 0.229 initialement) ce qui est
# une augmentation considérable faisant état de l'influence des valeurs extrêmes.
# Le modele_sans_influents a une erreur standard résiduelle plus faible
# (1.553 vs. 2.220). Cela indique que les prédictions du modèle sans les
# points influents sont plus précises, car les erreurs sont plus faibles.
# Cela dit, ce modèle explique que 38% de la variance de la variable dépendante.
# 62% de la variance de la variable dépendante n'est pas expliquée par le modèle.
# Ce modéle est légèrement concluant.

# D'un autre côté, est-ce que les observations exclues devraient elles réellement
# être exclues (valeurs aberrantes, véhicules différents, ... Ou pas ?).
# L'objectif de l'analyse est il de modéliser la tendance générale ou de
# représenter toute la gamme des comportements ? Il semble surtout que cette
# gamme de comportements puisse être influencée par d'autres facteurs : 
# le type de véhicule, l'origine/destination, âge, sens de circulation 
# (descente ou montée) et le climat.  
# Dans cette prochaine partie, nous tenterons d'analyser
# les variables étudiées jusqu'à présent conjointement avec plusieurs variables
# météorologiques.

############### 

#### Partie 4 : Régression avec variables météo

##############

#### Traitement de la variable sun_24_hours

bike_data <- bike_data %>%
  group_by(date) %>%
  fill(sun_minutes_24hours, .direction = "downup") %>%
  ungroup()

# Conversion en chaînes de caractères pour éviter la date automatique de 1899
# Transformer duree_jour, lever et coucher au format "HH:MM"
bike_data$duree_jour <- trimws(as.character(format(bike_data$duree_jour, "%H:%M")))
bike_data$lever      <- trimws(as.character(format(bike_data$lever, "%H:%M")))
bike_data$coucher    <- trimws(as.character(format(bike_data$coucher, "%H:%M")))
bike_data$heure <- trimws(as.character(bike_data$heure))

# Conversion en minutes pour pouvoir comparer les valeurs 
bike_data <- bike_data %>%
  mutate(
    duree_jour_min = as.numeric(substr(duree_jour, 1, 2)) * 60 +
      as.numeric(substr(duree_jour, 4, 5)),
    lever_min      = as.numeric(substr(lever, 1, 2)) * 60 +
      as.numeric(substr(lever, 4, 5)),
    coucher_min    = as.numeric(substr(coucher, 1, 2)) * 60 +
      as.numeric(substr(coucher, 4, 5)),
    heure_min      = as.numeric(substr(heure, 1, 2)) * 60 +
      as.numeric(substr(heure, 4, 5))
  )

# Calcul de l'ensoleillement brut pour une heure ensoleillée complète 
bike_data <- bike_data %>%
  mutate(
    ensoleillement_brut = sun_minutes_24hours / (duree_jour_min / 15)
  )

# Calcul de l'ensoleillement moyen ajusté aux heures de lever et coucher 
# On extrait également l'heure entière pour pouvoir comparer simplement (ex: "07:15" -> 7)
bike_data <- bike_data %>%
  mutate(
    heure_entier = as.numeric(substr(heure, 1, 2)),
    ensoleillement_moyen = case_when(
      # Avant l'heure de lever : pas de soleil
      heure_min < lever_min ~ 0,
      # Pour l'heure du lever : on calcule la proportion d'heure ensoleillée après le lever
      heure_entier == as.numeric(substr(lever, 1, 2)) ~ ((heure_min - lever_min) / 60) * ensoleillement_brut,
      # Pour l'heure du coucher : on calcule la proportion d'heure avant le coucher
      heure_entier == as.numeric(substr(coucher, 1, 2)) ~ ((coucher_min - heure_min) / 60) * ensoleillement_brut,
      # Pour les heures entièrement ensoleillées (entre lever et coucher)
      heure_min >= lever_min & heure_min <= coucher_min ~ ensoleillement_brut,
      # Sinon, pas de soleil
      TRUE ~ 0
    )
  )

#### Traitement de la variable precip_quantity et aggregation des données

# Définir une fonction qui renvoie la première valeur s'il y en a une 
# Utile pour considérer les variables météo en évitant les NA, leur observation 
# étant enregistrée lors du 4ème quart d'heure de chaque heure
first_non_na <- function(x) {
  non_na <- x[!is.na(x)]
  if (length(non_na) > 0) non_na[1] else NA
}

# Aggrégation des données pour la regression en incluant les variables météo
reg_meteo_data <- bike_data %>%
  select(heure_pleine, jour_semaine, date, mois, count, average.speed, 
         temp, pressure, wind_speed, wind_peak_speed, cloudiness, 
         precip_quantity, humidity_relative, ensoleillement_moyen, 
         sun_minutes_24hours) %>%
  group_by(date, heure_pleine, mois, jour_semaine) %>%
  summarise(
    average.speed = sum(average.speed * count, na.rm = TRUE) / sum(count, na.rm = TRUE),
    count = sum(count, na.rm = TRUE),
    ensoleillement_moyen = mean(ensoleillement_moyen, na.rm = TRUE), # Pour l'heure de lever et de coucher en particulier
    precip_quantity   = first_non_na(precip_quantity),
    temp              = first_non_na(temp),
    pressure          = first_non_na(pressure),
    wind_speed        = first_non_na(wind_speed),
    wind_peak_speed   = first_non_na(wind_peak_speed),
    cloudiness        = first_non_na(cloudiness),
    humidity_relative = first_non_na(humidity_relative)
  ) %>%
  ungroup()

# Extraire les mesures de précipitation quotidiennes (elles ne sont disponibles qu’à 6h et 18h)
# Mesure à 6h
precip6 <- reg_meteo_data %>%
  filter(heure_pleine == "6") %>%
  select(date, precip_quantity) %>%
  distinct() %>%
  rename(precip_6 = precip_quantity)

# Mesure à 18h
precip18 <- reg_meteo_data %>%
  filter(heure_pleine == "18") %>%
  select(date, precip_quantity) %>%
  distinct() %>%
  rename(precip_18 = precip_quantity)

# Joindre ces mesures à reg_data_meteo par date
reg_meteo_data <- reg_meteo_data %>%
  left_join(precip6, by = "date") %>%
  left_join(precip18, by = "date")

# Pour les observations de la période 18h - minuit, on souhaite utiliser
# la mesure à 6h du jour suivant.
# Créer une variable date_next = date + 1
reg_meteo_data <- reg_meteo_data %>%
  mutate(date_next = date + 1)

# Extraire la mesure de 6h en la décalant d'un jour
precip6_next <- precip6 %>%
  rename(date_next = date, precip_6_next = precip_6)

# Joindre la valeur de 6h du jour suivant à reg_meteo_data
reg_meteo_data <- reg_meteo_data %>%
  left_join(precip6_next, by = "date_next")

# Imputer la valeur horaire de précipitations avec case_when :
reg_meteo_data <- reg_meteo_data %>%
  mutate(
    # Conversion d'heure_pleine (facteur) en numérique pour les conditions
    heure_num = as.numeric(as.character(heure_pleine))
  ) %>%
  mutate(
    precip_imputed = case_when(
      heure_num <= 6 ~ precip_6,
      heure_num > 6 & heure_num <= 18 ~ precip_18,
      heure_num > 18 ~ precip_6_next,
      TRUE ~ NA_real_  # Pour la cohérence de type(precip_imputed est numérique)
    )
  )

# Enfin, créer la variable "precip_moyenne" en "propageant" la valeur horaire à 
# Toutes les observations de la même heure et en la divisant par 12*4 (=48)
reg_meteo_data <- reg_meteo_data %>%
  mutate(precip_moyenne = precip_imputed / (12 * 4))

# Vérification que l'on obtient bien 24 observations par jour pour chaque 
# variable (à savoir 24)
occurrences <- reg_meteo_data %>%
  group_by(date) %>%
  summarise(
    n_precip = sum(!is.na(precip_moyenne)),
    n_temp = sum(!is.na(temp)),
    n_wind_speed = sum(!is.na(wind_speed)),
    n_wind_peak_speed = sum(!is.na(wind_peak_speed)),
    n_precip_quantity = sum(!is.na(precip_quantity)),
    n_cloudiness = sum(!is.na(cloudiness)),
    n_pressure = sum(!is.na(pressure)),
    n_humidity = sum(!is.na(humidity_relative)),
    n_ensoleillement_moyen = sum(!is.na(ensoleillement_moyen))
  )

# Afficher un aperçu
print(head(occurrences, 10))
print(head(occurrences[, c("n_pressure", "n_humidity", "n_ensoleillement_moyen")], 10))

# Remplacement des NA du reste des variables météo par leur moyenne journalière
reg_meteo_data <- reg_meteo_data %>%
  group_by(date) %>%
  mutate(across(
    .cols = c(temp, pressure, wind_speed, wind_peak_speed, cloudiness, 
              precip_quantity, humidity_relative),
    .fns = ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  ungroup()

# Vérifier s'il reste des valeurs manquantes dans certaines colonnes
reg_meteo_data %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

#### Construction du modèle de régression multiple avec les variables meteo
reg_meteo_data_clean <- reg_meteo_data[!is.na(reg_meteo_data$average.speed), ]
names(reg_meteo_data_clean)

modele_meteo <- lm(average.speed ~ heure_pleine + jour_semaine + mois + wind_speed +
                     cloudiness + wind_peak_speed + precip_moyenne + humidity_relative +
                     ensoleillement_moyen + temp + pressure,
                   data = reg_meteo_data_clean)
summary(modele_meteo)

modele_meteo2 <- lm(average.speed ~ heure_pleine + jour_semaine + mois + wind_speed +
                      precip_moyenne + humidity_relative +
                      ensoleillement_moyen + temp,
                    data = reg_meteo_data_clean)
summary(modele_meteo2)

# Calculer et afficher les VIFs
vif_meteo <- vif(modele_meteo)
vif_meteo2 <- vif(modele_meteo2)
print(vif_meteo)
print(vif_meteo2)

# Le facteur d’inflation de la variance (VIF) qui est bon si inférieur à 10.
# Pas de problème important de multicolinéarité bien qu'on observe des valeurs
# ajustées légèrement plus élevées, certaines variables météo étant liées au 
# même phénomène (soleil et température, vent et pique de vent)

###
# Observation de la normalité des résidus à l'aide d'un histogramme des résidus
# Relativement centré autour de 0 et symétrique, ce qui est un bon indicateur 
# de normalité «globale».
# L’absence de pics ou de longues queues très marquées suggère qu’il n’y a pas 
# de valeurs extrêmes en disproportion.
dev.new()
hist(modele_meteo$residuals, breaks = 30,
     main = "Histogramme des résidus",
     xlab = "Résidus")

## Résidus du modèle en fonction de l'index des observations, pour vérifier
# l'hypothèse d'indépendance. A nouveau, le graphique ne révèle pas de motifs 
# évidents d'autocorrélation. Les résidus semblent être répartis de manière 
# indépendante.
dev.new()
plot(modele_meteo$residuals, type = "l",
     main = "Résidus vs. Index - modele_1",
     xlab = "Index des Observations",
     ylab = "Résidus")
abline(h = 0, col = "red")

# Q-Q plot des résidus
# Toujours cette forme en S illustrant que les "outliers" ne sont pas bien
# expliqués par notre modèle et que d'autres variables jouent potentiellement
# un rôle explicatif.
dev.new()
qqnorm(modele_meteo$residuals)
qqline(modele_meteo$residuals, col = "red")

# Observation de l'homoscédasdicité (la constance de la variance des résidus)
# Les résidus semblent à nouveau relativement bien répartis aléatoirement
dev.new()
plot(modele_meteo$fitted.values, modele_meteo$residuals,
     main = "Résidus vs Valeurs ajustées",
     xlab = "Valeurs ajustées",
     ylab = "Résidus")
abline(h = 0, col = "red")




#########################################
