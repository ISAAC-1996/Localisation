##############Cartographie pharmacie de Dominique pour les acces #############
###############EXTRAPOLATION #########################

setwd("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/Extrapolation_Projet_Ospharm")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(DBI)
library(odbc)
library(datamodelr)
library(DiagrammeR)
library(writexl)
library(sf)
library(leaflet)
library(shiny)
library(geosphere)
library(shinyjs)
library(openxlsx)
library(leaflet)
library(htmlwidgets)

# Connexion à la base de données OSP_DATASTAT
con_osp_datastat <- DBI::dbConnect(odbc::odbc(), 
                                   Database = "OSP_DATASTAT", 
                                   Driver = Sys.getenv("sql_driver"), 
                                   Server = Sys.getenv("sql_server"), 
                                   UID = Sys.getenv("sql_uid"), 
                                   PWD = Sys.getenv("sql_pwd"), 
                                   Port = Sys.getenv("sql_port"))

#geo_data <- st_read("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/Extrapolation_Projet_Ospharm/export.geojson")
#view(geo_data)

#IMPORTATION DES DATA
CA_PHA <- dbGetQuery(con_osp_datastat, "select  n_auto_adhpha_artic,  round(SUM(ca_ht_artic),2) as CA from os_stat_artic a
where n_auto_adhpha_artic in (select n_auto_adhpha from vuProdAdhpha where dextinct_adhpha is null 
and n_auto_adhpha in(select n_auto_adhpha from os_completudepha where periode_completudepha between 202310 and 202409 and moisok_completudepha = 1 group by n_auto_adhpha having COUNT(n_auto_adhpha) = 12))
and periode between 202310 and 202409
group by  n_auto_adhpha_artic
")
CA_PHA  <- as_tibble(CA_PHA)
view(CA_PHA)

INFOS_PHA <- dbGetQuery(con_osp_datastat, "select n_auto_adhpha, coalesce(adresse1_adhpha, adresse1_adhpha) as adresse, cip, nom_region, cp_ville,  case when SUBSTRING(a.cp_ville, 1, 3) = 200 then '2A'
 when SUBSTRING(a.cp_ville, 1, 3) = 201 or SUBSTRING(a.cp_ville, 1, 3) = 202 or SUBSTRING(a.cp_ville, 1, 3) = 206 then '2B'
 when SUBSTRING(a.cp_ville, 1, 3) in (971, 972, 973, 974, 976, 988, 987, 980) then SUBSTRING(a.cp_ville, 1, 3)
 else SUBSTRING(a.cp_ville, 1, 2) end as Departement from vuProdAdhpha a
	left join os_region b on a.region_adhpha = b.n_auto_region
where dextinct_adhpha is null and n_auto_adhpha in(select n_auto_adhpha from os_completudepha where periode_completudepha between 202301 and 202409 and moisok_completudepha = 1 group by n_auto_adhpha having COUNT(n_auto_adhpha) = 21)
")
INFOS_PHA <- as_tibble(INFOS_PHA)
view(INFOS_PHA)


#Dpt de France 
dpt <- read_excel("Input/Les departements de France.xlsx")
dpt <- as_tibble(dpt)
dpt <- dpt%>%
  inner_join(INFOS_PHA, by = c("code" = "Departement"))%>%
  select(code, nom, nom_region)%>%
  distinct(code, .keep_all = TRUE)%>%
  rename(region = nom_region)
view(dpt)

CRM_DATA <- read_excel("Input/BASE.xlsx")
CRM_DATA$idCip <- as.character(CRM_DATA$idCip)
CRM_DATA <- as_tibble(CRM_DATA)
view(CRM_DATA)
CRM_DATA <- CRM_DATA %>%
  mutate(addressPostalcode = str_pad(as.character(addressPostalcode), width = 5, pad = "0")) %>%
  mutate(departement = case_when(
    str_sub(addressPostalcode, 1, 3) %in% c("971", "972", "973", "974", "976", "988", "987", "980") ~ str_sub(addressPostalcode, 1, 3),
    str_sub(addressPostalcode, 1, 3) == "201" ~ "2B",
    str_sub(addressPostalcode, 1, 3) == "206" ~ "2B",
    str_sub(addressPostalcode, 1, 3) == "202" ~ "2B",
    str_sub(addressPostalcode, 1, 3) == "200" ~ "2A",
    str_sub(addressPostalcode, 1, 2) == "99" ~ "ETRANGER",
    TRUE ~ str_sub(addressPostalcode, 1, 2))) %>%
  select(name, idCip, departement, addressPostalcode, addressLine0, addressCityname, Latitude, Longitude)
#view(CRM_DATA)
#Fusion des tables 
TABL <- CA_PHA %>%
  left_join(INFOS_PHA, by = c("n_auto_adhpha_artic" = "n_auto_adhpha")) %>%
  mutate(PANEL = 'OSPHARM_DATASTAT')
view(TABL)

BASE <- CRM_DATA %>%
  left_join(TABL, by = c("idCip" = "cip")) %>%
  left_join(dpt, by = c("departement" = "code"))%>%
  mutate(PANEL = if_else(is.na(PANEL), "NO_DATASTAT", PANEL)) %>%
  distinct(idCip, .keep_all = TRUE)%>%
  select(idCip, n_auto_adhpha_artic, name, Latitude, Longitude)
# Conversion des colonnes Latitude et Longitude en numérique
BASE$Latitude <- as.numeric(BASE$Latitude)
BASE$Longitude <- as.numeric(BASE$Longitude)
view(BASE)

PHA <- read_excel("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carto_Dominique/Pharmacies_carte_Martinique.xlsx",  sheet = 1)
PHA <- as_tibble(PHA)
str(PHA)

#Trouver les doublons de nos études

doublon <- PHA %>% group_by_all() %>% count %>% filter(n == 2)
view(doublon) # voir les doublons dans ma tables. 


# S'assurer que les colonnes clés ne contiennent pas de doublons
BASE_unique <- BASE %>% distinct(n_auto_adhpha_artic, .keep_all = TRUE)
PHA_unique <- PHA %>% distinct(DATASTAT, .keep_all = TRUE)
view(PHA_unique)

# Effectuer la jointure en explicitant la relation many-to-many si nécessaire
PHA_TABLE <- PHA_unique %>%
  left_join(BASE_unique, by = c("n_auto" = "n_auto_adhpha_artic"))%>%
  select(-idCip, -DATASTAT)
View(PHA_TABLE)

################# Les pharmacies qui n'ont pas de correspondance dans BASE
PHA_left_join <- PHA_unique %>%
  left_join(BASE_unique, by = c("n_auto" = "n_auto_adhpha_artic"))
PHA_no_match <- PHA_left_join %>%
  filter(is.na(n_auto)) 
view(PHA_no_match)

################# RAJOUT DES LIGNES INEXISTANTE ###############

new_rows <-PHA <- read_excel("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carto_Dominique/Pharmacies_carte_Martinique.xlsx",  sheet = 3)
new_rows <- as_tibble(new_rows)
str(new_rows)

#Modification des nom des pharma en ajoutant "PHARMACIE" au nom qui ne contienne pas pharma.
new_rows <- new_rows %>%
  mutate(name = paste0("PHARMACIE ", name))
str(new_rows)

#Fusion des deux tables (PHA_TABLE + new_rows)
PHA_TABLE <- rbind(PHA_TABLE, new_rows) 
str(PHA_TABLE)


##################### CARTOGRAPHIE ##########################
# Convertir Latitude et Longitude en numérique si nécessaire
PHA_TABLE <- PHA_TABLE %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude))

# Créer une icône personnalisée
pharmacy_icon <- makeIcon(
  iconUrl = "https://isaac-1996.github.io/Localisation/Pharmacie2.png",
  #iconUrl = "https://isaac-1996.github.io/Localisation/pharmacie.png",
  iconWidth = 25, iconHeight = 25)

# Créer la carte avec leaflet
map <- leaflet(PHA_TABLE) %>%
  addTiles() %>% # Ajouter les tuiles de base
  addMarkers( ~Longitude, ~Latitude, popup = ~name, icon = pharmacy_icon) #popup = Pour ajouter les étiquettes de données. icon = ajouter notre icon creer
# Sauvegarder la carte en HTML
saveWidget(map, "PHA_TABLE_Map1.html", selfcontained = TRUE)
browseURL("PHA_TABLE_Map1.html")


              ###############################################
              ############## Deuxieme affche ################
              ###############################################
#icon
pharmacy_icon <- awesomeIcons(icon = "medkit",library = "fa",markerColor = "green", iconColor = "white")


#CARTE
map <- leaflet(PHA_TABLE) %>%
  addTiles() %>% 
  addAwesomeMarkers(~Longitude, ~Latitude, popup = ~name,icon = pharmacy_icon)

#Sauvegarde et affiche de la carte
saveWidget(map, "C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carto_Dominique/PHA_TABLE_Map2.html", selfcontained = TRUE)
browseURL("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carto_Dominique/PHA_TABLE_Map2.html")

############## TEST 3 AFFICHE 3 ###############

# Ajouter un type d'établissement à la base existante (si non présent)
set.seed(123)  # Pour la reproductibilité
PHA_TABLE <- PHA_TABLE %>%
  mutate(
    type = sample(
      c("École", "Université", "Restaurant", "Supermarché", "Gare", "Aéroport", "Hôpital", "Maternité", "Eglise", "Océan"),
      nrow(PHA_TABLE), replace = TRUE))

# Créer une fonction pour générer les informations des établissements proches pour chaque client
generate_popup <- function(client_row, all_establishments, radius = 10) {
  # Coordonnées du client
  client_coords <- c(client_row$Longitude, client_row$Latitude)
  
  # Calculer les distances entre le client et tous les établissements
  all_establishments <- all_establishments %>%
    rowwise() %>%
    mutate(
      Distance_km = distHaversine(client_coords, c(Longitude, Latitude)) / 1000) %>%
    ungroup()
  
  # Filtrer les établissements proches
  establishments_nearby <- all_establishments %>%
    filter(Distance_km <= radius)
  
  # Construire l'étiquette
  if (nrow(establishments_nearby) > 0) {
    # Liste des établissements proches avec type et distance
    nearby_info <- paste(
      establishments_nearby$type, 
      "(", round(establishments_nearby$Distance_km, 2), "km)", 
      collapse = "; "
    )
    popup <- paste0(
      "Nom : ", client_row$name, "<br>",
      "Établissements proches : ", nearby_info
    )
  } else {
    popup <- paste0("Nom : ", client_row$name, "<br>Pas d'établissements proches trouvés")
  }
  return(popup)
}

# Appliquer la fonction pour tous les clients de la base
clients_with_popups <- PHA_TABLE %>%
  rowwise() %>%
  mutate(
    popup = generate_popup(cur_data(), PHA_TABLE, radius = 10)
  ) %>%
  ungroup()

# Créer une icône pour le client
client_icon <- awesomeIcons(icon = "medkit",library = "fa",markerColor = "green")

# Créer la carte avec tous les clients et leurs étiquettes
map <- leaflet() %>%
  addTiles() %>%
  # Ajouter chaque client avec son popup
  addAwesomeMarkers(
    lng = clients_with_popups$Longitude,
    lat = clients_with_popups$Latitude,
    popup = clients_with_popups$popup,
    icon = client_icon
  )

# Sauvegarder et afficher la carte
saveWidget(map, "PHA_TABLE_Map_All_Clients.html", selfcontained = TRUE)
browseURL("PHA_TABLE_Map_All_Clients.html")
