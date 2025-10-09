# Variables constantes
year_colors = c("#eb6ecf", "#8a6eeb", "#6e9deb", "#6eebb4", "#ebbc6e",
                "#eb816e", "#d01456", "#783628", "#0b15de", "#4fdce0")
sp_colors = c("#f00000", "#4bdadc", "#31c1f3", "#14779a", "#003ca4", "#0c1eff")
color_flag = "red"
#liste principale des papillons de l'observatoire
liste_principale <- c("Machaons","Flambés", "Demi-deuils",
                      "Paon du jour", "Vulcain", "Belle-dame", "Petites tortues",
                      "Robert-le-diable", "Tabac d'Espagne", "Silène", "Sylvains",
                      "Souci", "Aurores", "Piérides blanches","Gazé","Citrons","Amaryllis",
                      "Myrtil", "Procris", "Mégères", "Tircis", "Lycènes bleus",
                      "Argus verts", "Brun des pélargoniums", "Cuivré", "Hespérides tachetées",
                      "Hespérides orangées", "Moro-sphinx")
# Départements avec numéro et région
reg_dep = read.csv2("carte/departements-france.csv", sep=",")
