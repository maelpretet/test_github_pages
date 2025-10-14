# Librairies
library(dplyr)
library(here)
library(quarto)
library(stringr)

print("flag1")

# Fonctions
if (Sys.getenv("CI") != "true") {
  readRenviron(".env")
}

#liste principale des papillons de l'observatoire
if (Sys.getenv("CI") == "true") {
  liste_principale <- c("Amaryllis", "Argus verts", "Belle-dame", "Paon du jour")
}else{
  liste_principale <- c("Amaryllis", "Argus verts", "Belle-dame", "Paon du jour", "Piérides blanches")
  # liste_principale <- c("Amaryllis", "Argus verts", "Belle-dame", "Citrons",
  #                       "Paon du jour", "Piérides blanches", "Tabac d'Espagne", "Gazé")
  # liste_principale <- c("Amaryllis", "Argus verts", "Aurores", "Belle-dame",
  #                       "Brun des pélargoniums", "Citrons", "Cuivré",
  #                       "Demi-deuils", "Flambés", "Gazé", "Hespérides orangées",
  #                       "Hespérides tachetées", "Lycènes bleus", "Machaons",
  #                       "Mégères", "Moro-sphinx", "Myrtil", "Paon du jour",
  #                       "Petites tortues", "Piérides blanches", "Procris",
  #                       "Robert-le-diable", "Silène", "Souci", "Sylvains",
  #                       "Tabac d'Espagne", "Tircis", "Vulcain")
}

print("flag2")

time = Sys.time()
# Boucle sur les noms d'espèces
for (sp_name in liste_principale) {

  tryCatch({
    filename = paste0("maquette_espece_", sp_name, ".html")

    quarto_render(input = "maquette_espece.qmd",
                  execute_params = list("sp_name" = sp_name),
                  output_file = filename)

    file.rename(filename, file.path("docs", filename))

  }, error = function(e) {
    message(sprintf("Error rendering document_%s.qmd: %s", sp_name, e$message))
  })

}
  
print(Sys.time() - time)

print("flag3")
