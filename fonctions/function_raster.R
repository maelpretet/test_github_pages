library(sf)
library(dplyr)
library(ggplot2)

# Charger la carte de la France
fr <- st_read("carte/contour-des-departements.geojson", quiet = TRUE)

# Vérifier le CRS et passer en Lambert-93 si besoin
if (st_crs(fr)$epsg != 2154) {
  fr <- st_transform(fr, 2154)
}

# Découper la carte 
grille10 <- st_make_grid(
  fr,
  cellsize = c(10000, 10000),
  square = TRUE
)

# grille20 <- st_make_grid(
#   fr,
#   cellsize = c(20000, 20000),
#   square = TRUE
# )

create_df_raster <- function(df_opj, grille = grille10, carte){
  df_coord = df_opj %>%
    group_by(jardin_id, latitude, longitude) %>%
    summarise(sum_ab = sum(taxon_count),
              max_year = max(session_year),
              .groups = 'drop') %>%
    filter(!is.na(latitude)) %>%
    select(longitude, latitude, sum_ab, max_year) %>%
    as.data.frame()
  
  points_sf = st_as_sf(df_coord, coords = c("longitude", "latitude"), crs = 4326)
  points_sf = st_transform(points_sf, 2154)
  
  grille_sf <- st_sf(id = 1:length(grille), geometry = grille)
  grille_fr <- st_intersection(grille_sf, st_union(carte))
  points_with_grid <- st_join(points_sf, grille_fr, join = st_intersects)
  
  grille_valeurs <- points_with_grid %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(sum_ab = sum(sum_ab), max_year = max(max_year)) %>%
    mutate(is.butterfly = if_else(sum_ab == 0, "Non observé",
                                  if_else(sum_ab > 5,
                                          "Observé >5 jardin",
                                          "Observé 1-5 jardin") ),
           is.butterfly.2 = if_else(sum_ab == 0,
                                    "Non observé",
                                    "Observé dans un jardin"),
           is.butterfly.year = if_else(sum_ab == 0, "Non observé", 
                                       if_else(max_year >= (as.integer(strftime(Sys.time(), "%Y"))-5),
                                               "Observé sur les 5 dernières années",
                                               "Observé il y a plus de 5 ans"))) %>%
    select(!sum_ab) %>%
    right_join(grille_fr, by = "id") %>%
    mutate(is.butterfly = if_else(is.na(is.butterfly), "Pas de données", is.butterfly),
           is.butterfly.2 = if_else(is.na(is.butterfly.2), "Pas de données", is.butterfly.2),
           is.butterfly.year = if_else(is.na(is.butterfly.year), "Pas de données", is.butterfly.year)) %>%
    st_as_sf()
  
  return(grille_valeurs)
}

fct_raster <- function(df, fill = "is.butterfly.2", df_carte){
  gg <- ggplot() +
    geom_sf(data = df_carte, fill = NA, color = "black", size = 0.2) +
    geom_sf(data = df, aes(fill = !!sym(fill)), color = NA, size = 0.1, alpha = 0.9) +
    theme_minimal() +
    coord_sf()
  if (fill == "is.butterfly.2") {
    gg <- gg + scale_fill_manual(values = c("Observé dans un jardin" = "#23CE6B",
                                            "Non observé" = "#8F8F8F",
                                            "Pas de données" = "#ffffff"))
  }else if (fill == "is.butterfly"){
    gg <- gg + scale_fill_manual(values = c("Observé 1-5 jardin" = "#2CD1F2",
                                            "Observé >5 jardin" = "#84F269",
                                            "Non observé" = "#8F8F8F",
                                            "Pas de données" = "white")) 
  }else if (fill == "is.butterfly.year"){
    gg <- gg + scale_fill_manual(values = c("Observé il y a plus de 5 ans" = "#DB504A",
                                            "Observé sur les 5 dernières années" = "#23CE6B",
                                            "Non observé" = "#A2A2a2"),
                                 na.value = NA, name = "Pas de données") 
  }
  
  return(gg)
}
