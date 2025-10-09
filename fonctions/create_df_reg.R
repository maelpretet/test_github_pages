#----- Carte d'abondance -----#

# Df abondance sur toutes les données (all_data)
# fct df abondance
fct_df_abondance_reg <- function(df){
  return(df_abondance <- df %>% 
           filter(!is.na(jardin_id)) %>%
           group_by(code_region) %>%
           summarise(n = sum(taxon_count),
                     nb_participation = n_distinct(session_id),
                     .groups = 'drop') %>%
           mutate(ab_rel = n/nb_participation,
                  cl_qual = case_when(ab_rel == 0 ~ "Pas de détection",
                                      ab_rel > 0 & ab_rel <= 0.2 ~ "Peu abondant",
                                      ab_rel > 0.2 & ab_rel <= 0.4 ~ "Abondant",
                                      ab_rel > 0.4 & ab_rel <= 0.6 ~ "Très abondant",
                                      ab_rel > 0.6 ~ "Extrêmement abondant")) )
}

df_reg = fct_df_abondance_reg(df = df_sp)
df_reg_old = fct_df_abondance_reg(df = df_sp_old)
df_reg_new = fct_df_abondance_reg(df = df_sp_new)

cat_carte_tendance_moy = c("Pas de détection", "Peu abondant", "Abondant",
                           "Très abondant", "Extrêmement abondant")
couleurs = c("#7f7f7f", "#ffef6c", "#f7b905", "#ff7400", "#ff0000", "#950000")

