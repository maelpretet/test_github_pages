# HEADER --------------------------------------------
#
# Author:     Maël Pretet
# Copyright     Copyright 2024 - Maël Pretet
# Email:      mael.pretet1@mnhn.fr
#
# Date:     2024-03-26
#
# Script Name:    fonctions/create_df_opj.R
#
# Script Description:   Création du data frame opération papillons avec toutes
#   les espèces. Interrogation de la base de données mosaic et enregistrement
#   dans un fichier rds. Si le fichier existe déjà et qu'il date de moins d'une
#   semaine, celui-ci est directement lu.
#
#
# ------------------------------------

library(dplyr)
library(here)
require(httr)
library(readr)

if (Sys.getenv("CI") != "true") {
  readRenviron(".env")
}

source("fonctions/var.R")

# Requête sur la base de données ftp
# - Données récentes
# print("Requête des données récentes")
if (!exists("req")) {
  req <- GET(
    paste0(Sys.getenv('SITE_NAME'), "export_opj.csv"),
    authenticate(Sys.getenv('FTP_USER'), Sys.getenv('FTP_PASSWORD'), type = "basic")
  )
}

# - Données historiques
# print("Requête des données ensemble de la période")
if (!exists("req_old")) {
  req_old <- GET(
    paste0(Sys.getenv('SITE_NAME'), "export_opj_history.csv"),
    authenticate(Sys.getenv('FTP_USER'), Sys.getenv('FTP_PASSWORD'), type = "basic")
  )
}

### Dataframe des données pour toutes les espèces
# -----------------------------------------------
df_data <- readr::read_csv2(content(req, "raw"))
df_old_data <- readr::read_csv2(content(req_old, "raw"))
