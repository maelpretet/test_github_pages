# HEADER --------------------------------------------
#
# Author:     Maël Pretet
# Copyright     Copyright 2024 - Maël Pretet
# Email:      mael.pretet1@mnhn.fr
#
# Date:     2024-03-26
#
# Script Name:    fonctions/function_graphics.R
#
# Script Description:   Fonctions graphiques pour les figures de 
#   la fiche espèce (script "dashboard_espece.qmd")
#
#
# ------------------------------------


#########################################
#------------ Observations -------------#
#########################################

# Histogramme de proportion des abondances d'espèces
#' gg_histo_plotly
#' 
#' @param df_hp A dataframe
#' @param x A character (column's dataframe)
#' @param y A character (column's dataframe)
#' @param fill A character (column's dataframe)
#' @param title A character 
#' @param ytxt A character 
#' @param limits A character vector
#' @param breaks A character vector
#' @param couleur A character vector 
#' 
#' @returns A ggplotly object.
#' 
#' @examples
#' gg_histo_plotly(df_hp = data.frame(nom = c("A", "B", "C"),
#'                                    value = c(0.23, 0.11, 0.219)),
#'                 x = "nom", y = "value", fill = "nom", title = "Proportions",
#'                 limits = c("B", "C", "A"), couleur = c("red", "grey", "grey"))
gg_histo_plotly <- function(df_hp, x = "taxon", y = "rel_ab",
                            fill = "taxon", 
                            title = "Proportion d'abondance de chaque espèce parmi toutes les observations", 
                            ytxt = "% d'abondance",
                            limits, couleur, percent = TRUE){
  
  if (percent) {
    gg <- df_hp %>%
      ggplot() +
      # On effectue un mapping selon les colonnes x et y, et on colore les
      # histogrammes selon la colonne fill
      geom_bar(mapping = aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill),
                             # On ajoute un argument 'text" pour ggplotly
                             # ("x" : "y" [au format % avec une précision au centième 12.9062 -> 12.91])
                             text = paste0(!!sym(x), " : ",
                                           scales::percent(!!sym(y), accuracy = 0.01))),
               stat = "identity") +
      # Mise de l'axe y au format % en précision 1 (21.2% -> 21%)
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }else{
    gg <- df_hp %>%
      ggplot() +
      # On effectue un mapping selon les colonnes x et y, et on colore les
      # histogrammes selon la colonne fill
      geom_bar(mapping = aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill),
                             # On ajoute un argument 'text" pour ggplotly
                             # ("x" : "y" [pas de format])
                             text = paste0(!!sym(x), " : ", !!sym(y))),
               stat = "identity")
  }
  
  gg = gg +
    scale_x_discrete(limits=limits) +       # Fonction pour ordonner l'axe x
    ylab(ytxt) +
    ggtitle(title) +
    theme_cowplot() +
    theme(axis.title.y = element_blank(),   # Suppression du nom de l'axe y
          title = element_text(size = 9),   # Titre en taille 9
          legend.position = "none" ) +      # Suppression de la légende
    # Fonction pour colorer les histogrammes selon des valeurs choisies (dans le même ordre que l'axe x)
    scale_fill_manual(breaks = limits,
                      values = couleur) +
    # Inversion des axes x et y pour que les histogrammes soient horizontaux
    coord_flip()
  
  # On utilise ggplotly avec l'argument "tooltip" pour que le "text" s'affiche
  # quand on passe la souris sur les histogrammes
  return(ggplotly(gg, tooltip = "text") %>% plotly::config(displayModeBar = FALSE))
}

#########################################
#-------- Variations annuelles ---------#
#########################################

# Histogramme d'abondance
#' gg_histo
#'
#' @param df_histo A dataframe
#' @param x A character (column's dataframe)
#' @param y A character (column's dataframe)
#' @param ytxt A character
#' @param dmin A date
#' @param dmax A date
#' @param title A character
#'
#' @return A ggplot object
#'
#' @examples
#' data("economics")
#' gg_histo(df_histo = economics, x = "date", y = "unemploy",
#'          ytxt = "Taux de chômage",
#'          dmin = as.Date("2000-04-01"),
#'          dmax = as.Date("2009-04-01"))
gg_histo <- function(df_histo, x = "session_date", y = "sum_ab",
                     ytxt = "Abondance totale", dmin, dmax, title = ""){
  
  # On fait en sorte qu'on ne voit que les labels soient centrés sur le 1er juillet
  year_mid <- floor_date(as.data.frame(df_histo)[,x], "year") + months(6)
  mid_year_dates <- sort(unique(year_mid))
  
  return(ggplot() +
           # Histogramme avec les axes x et y définis en arguments 
           # !!! Axe x doit être une date
           geom_bar(data = df_histo, aes(x = !!sym(x), y = !!sym(y)),
                    # Couleur et remplissage définis
                    color = "#8A173A", fill="#ab0739", stat="identity") +
           theme_cowplot() +
           # Axe x sous format date "ANNEE-MOIS" avec un intervalle de 1 an
           scale_x_date(breaks = mid_year_dates, date_labels = "%Y",
                        # On définit les limites de l'axe x avec les arguments dmin/dmax
                        limits = c(dmin, dmax)) +
           theme(axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 0, size = 8),
                 axis.title.y =  element_text(size = 11, color = "#ab0739"),
                 axis.text.y = element_text(size = 8),
                 plot.title = element_text(face = "plain", size = 9)) +
           # Lignes pointillées pour séparer les années
           geom_vline(xintercept = mid_year_dates + months(6),
                      color = "#606060", linetype = "dashed") +
           ggtitle(title) +
           ylab(ytxt) )
  
}


# Graphique en courbe
#' gg_line
#'
#' @param df_histo A dataframe
#' @param x A character (column's dataframe)
#' @param y A character (column's dataframe)
#' @param ytxt A character
#' @param dmin A date
#' @param dmax A date
#' @param title A character
#'
#' @return A ggplot object
#'
#' @examples
#' data("economics")
#' gg_line(df_line = economics, x = "date", y = "unemploy",
#'          ytxt = "Taux de chômage",
#'          dmin = as.Date("2000-04-01"),
#'          dmax = as.Date("2009-04-01"))
gg_line <- function(df_line, x = "date", y = "n",
                    xtxt = "Date de participation", ytxt = "Nombre de sessions",
                    color = "#ff795c", dmin, dmax, title = ""){
  
  # On fait en sorte qu'on ne voit que les labels soient centrés sur le 1er juillet
  year_mid <- floor_date(as.data.frame(df_line)[,x], "year") + months(6)
  mid_year_dates <- sort(unique(year_mid))
  
  ggplot(df_line, aes(x = !!sym(x), y = !!sym(y))) +
    geom_line(color = color) +
    theme_cowplot() +
    # Axe x sous format date "ANNEE-MOIS" avec un intervalle de 9 mois entre les labels
    scale_x_date(breaks = mid_year_dates, date_labels = "%Y",
                 limits = c(dmin, dmax)) +
    theme(axis.title.x = element_text(size = 10),
          axis.text.x = element_text(angle = 0, size = 8),
          axis.title.y = element_text(size = 9, color = color),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(face = "plain", size = 9)) +
    ggtitle(title) +
    geom_vline(xintercept = mid_year_dates + months(6),
               color = "#606060", linetype = "dashed") +
    xlab(xtxt) +
    ylab(ytxt)
}

# Carte d'abondance
#' carte_ab
#'
#' @param shape_map An sf object
#' @param fill_map A qualitative vector (same length as number of rows in shape_map)
#' @param fill_title A character
#' @param fill_color A vector (same length as number of categories in fill_map)
#' @param fill_cat A vector (same length as number of categories in fill_map)
#' @param map_title A character
#'
#' @return A ggplot object
#'
#' @examples
#' # Fichier téléchargé depuis : https://www.data.gouv.fr/fr/datasets/carte-des-departements-2-1/
#' # contour-des-departements.geojson
#' france_hex <- read_sf("contour-des-departements.geojson")
#' 
#' vec_fill = rep(c("1", "2", "3"), 31)
#' carte_ab(shape_map = france_hex, fill_map = rep(c("1", "2", "3"), 32),
#'          fill_title = "Légende", fill_color = c("#eb59ff", "#59e6ff", "#ffdc59"),
#'          fill_cat = c("1", "2", "3"), map_title = "Carte")
carte_ab <- function(shape_map, fill_map, fill_title, fill_color,
                     fill_cat, map_title = ""){
  
  carte <- ggplot(shape_map) + 
    geom_sf(aes(fill = fill_map), show.legend = "fill") +
    scale_fill_manual(values = fill_color, breaks = fill_cat, drop = FALSE)+
    labs(fill = fill_title) +
    ggtitle(map_title) +
    theme_light() +
    theme(title = element_text(size = 9))
  
  return(carte)
}

#########################################
#------------- Phénologie --------------#
#########################################

# Graphique en echarts4r
#' aes_echarts
#'
#' @param plot_e An echarts4r object
#' @param xlab A character
#' @param ylab A character
#' @param title A character
#' @param line_color A vector
#' @param one_y A boolean
#'
#' @return An echarts4r pbject
#'
#' @examples
#' 
#' data("economics")
#' aes_echarts(plot_e = economics %>% 
#'               mutate(date_dec = as.integer(strftime(date, "%Y")) %/% 10 * 10) %>%
#'               group_by(date_dec) %>%
#'               e_charts(date) %>%
#'               e_line(unemploy, symbol='none'),
#'             xlab = "Date", ylab = "Unemploy", title = "Unemploy / date",
#'             line_color = c("purple", "blue", "green", "yellow", "orange", "red"),
#'             one_y = FALSE)
#' aes_echarts(plot_e = economics %>%
#'               filter(date >= "2010-01-01") %>%
#'               mutate(date_week = as.integer(strftime(date, "%U")),
#'                      date_year = strftime(date, "%Y")) %>%
#'               group_by(date_year) %>%
#'               e_charts(date_week) %>%
#'               e_line(unemploy, symbol='none'),
#'             xlab = "Date", ylab = "Unemploy", title = "Unemploy / date",
#'             line_color = c("purple", "blue", "green", "yellow", "orange", "red"),
#'             one_y = TRUE)
aes_echarts <- function(plot_e, xlab, ylab, title, line_color, one_y = TRUE,
                        xmax = 53, l1 = 12, l2 = 25, l3 = 38, l4 = 51){
  
  plot_e <- plot_e %>%
    # e_bar(sum_ab) %>%
    e_legend(top = "3%") %>%
    e_tooltip(e_tooltip_pointer_formatter("currency"),
              axisPointer = list(type = "cross")) %>%
    e_datazoom(x_index = 0, type = "slider", bottom = "3%") %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    e_toolbox_feature(feature = "dataView", readOnly = TRUE) %>%
    e_x_axis(name=xlab,
             nameLocation = "middle", nameGap= 27) %>%
    e_grid(bottom = "20%") %>%
    e_color(color = line_color) %>%
    e_y_axis(name=ylab, nameLocation = 'middle',
             nameGap= 50) %>%
    e_title(text = paste(title),
            textStyle = list(fontSize = list(14)))
  
  if (one_y) {
    plot_e <- plot_e %>%
      e_x_axis(max = xmax) %>%
      e_mark_line(emphasis = list(disabled = TRUE), symbol = "none", lineStyle = list(color = "grey"),
                  data = list(xAxis = l1), title = "") %>%
      e_mark_line(data = list(xAxis = l2), title = "") %>%
      e_mark_line(data = list(xAxis = l3), title = "") %>%
      e_mark_line(data = list(xAxis = l4), title = "") %>%
      e_graphic_g(
        elements = list(
          # list(type = "text", left = 'center', top = 20,
          #   style = list(text = 'Texte centré en haut',
          #     font = 'bold 14px sans-serif', fill = 'black')),
          list(type = "text", left = '17%', top = '14%',
               style = list(text = 'Hiver',
                            font = '14px sans-serif',
                            fill = 'black')),
          list(type = "text", left = '35%', top = '14%',
               style = list(text = 'Printemps',
                            font = '14px sans-serif',
                            fill = 'black')),
          list(type = "text", left = '57%', top = '14%',
               style = list(text = 'Été',
                            font = '14px sans-serif',
                            fill = 'black')),
          list(type = "text", left = '75.5%', top = '14%',
               style = list(text = 'Automne',
                            font = '14px sans-serif',
                            fill = 'black', textAlign = 'center'))))
  }
  
  return(plot_e)
}


# Pic d'activité
graph_pic <- function(df_pic, x = "session_year", y = "sum_sp", ecart = "rmse",
                      xlab = "Année", ylab = "Semaine de participation",
                      title = "Semaine du pic d'activité et son écart-type chaque année"){
  
  gg = ggplot(df_pic, aes(x = !!sym(x), y = !!sym(y))) +
    geom_hline(yintercept = 12, color = "#606060", linetype = "dashed") +
    geom_hline(yintercept = 25, color = "#606060", linetype = "dashed") +
    geom_hline(yintercept = 38, color = "#606060", linetype = "dashed") +
    geom_hline(yintercept = 51, color = "#606060", linetype = "dashed") +
    annotate("text", x = 2005.5, y = 5.5, label = "Hiver", color = "#234aa6") +
    annotate("text", x = 2005.5, y = 18.5, label = "Printemps", color = "#5cda30") +
    annotate("text", x = 2005.5, y = 31.5, label = "Été", color = "#da4c30") +
    annotate("text", x = 2005.5, y = 44.5, label = "Automne", color = "#e7972a") +
    geom_errorbar(aes(x=!!sym(x), ymin= !!sym(y)-!!sym(ecart), ymax=!!sym(y)+!!sym(ecart)),
                  width=0.4, colour="#bb680e", alpha=0.9, linewidth=1.3) +
    geom_point(colour = "#ffa600", size = 3) +
    theme_cowplot() +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.title = element_text(face = "plain", size = 14)) +
    ylim(1,52)+
    coord_flip()
  
  return(gg)
}

#########################################
#------------- Grégarité ---------------#
#########################################

# Abondance moyenne par espèce
histo_ab_mean <- function(df_mg, x = "taxon", w = "m_abn", sd = "sd",
                          color_txt = "black", xlab = "Espèce",
                          ylab = "Nombre d'individus", order = "m_abn",
                          title = "Moyenne de l'abondance pour chaque espèce"){
  
  gg = ggplot(df_mg, aes(x = reorder(!!sym(x), !!sym(order)))) +
    geom_bar(aes(weight = !!sym(w)), fill = "#ffae57") +
    geom_errorbar( aes(x=!!sym(x), ymin=!!sym(w)-!!sym(sd), ymax=!!sym(w)+!!sym(sd)),
                   width=0.4, colour="#bb680e", alpha=0.9, size=1.3) +
    coord_flip() +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    theme(title = element_text(size = 9),
          axis.text.y = element_text(color = color_txt))
  
  return(gg)
}

# Classes d'abondance pour l'espèce
#' Title
#'
#' @param df_grega A dataframe
#' @param x A character (column's dataframe)
#' @param y A character (column's dataframe)
#' @param xlab A character
#' @param ylab A character
#' @param title A character
#' @param limits A character
#'
#' @return A ggplot object
#'
#' @examples
#' 
#' histo_grega(df_grega = data.frame(classe = c("1", "2", "3"), freq = c(0.6, 0.3, 0.1)),
#'             x = "classe", y = "freq", xlab = "Classe", ylab = "Frequence", title = "",
#'             limits = c("2", "1", "3"))
histo_grega <- function(df_grega, x = "class_idv", y = "freq_prc",
                        xlab = "Nombre d'individus observés simultanément",
                        ylab = "% d'observations",
                        title = "Distribution de la grégarité de l'espèce",
                        limits = c("1", "2 à 4", "5 et +")){
  return(ggplot(df_grega) +
    geom_bar(aes(x = !!sym(x), y = !!sym(y)),
             stat = "identity",
             fill = "#8cb6ec") +
    scale_x_discrete(limits=limits) +
    theme_cowplot() + 
    labs(x = xlab,
         y = ylab) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggtitle(title) +
    theme(plot.title = element_text(face = "plain", size = 15),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)))
}

# Indice de grégarité par espèce
histo_indice_greg <- function(df_greg_all, x = "taxon", order = "classif",
                              fill = "ab_grega", w = "prop_grega",
                              mean = "classif", sqrt = "sqrt_n",
                              lab_fill = "Grégarité",
                              xlab = "Espèce", ylab = "Proportion des observations",
                              title = paste0("Proportion des observations 1 individu ",
                                             "/ plusieurs individus \npour chaque espèce"),
                              color_txt = "black"){
  
  return(ggplot(df_greg_all, aes(x = reorder(!!sym(x), !!sym(order)), fill = !!sym(fill))) +
    geom_bar(aes(weight = !!sym(w))) +
    geom_errorbar(aes(x = reorder(!!sym(x), !!sym(order)),
                      ymin = !!sym(mean) - !!sym(sqrt),
                      ymax = !!sym(mean) + !!sym(sqrt)),
                  width=.4, color = "#404040") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    coord_flip() +
    labs(fill = lab_fill) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    # theme(title = element_text(size = 9)))
    theme(title = element_text(size = 9),
          axis.text.y = element_text(color = color_txt)))
}

#########################################
#-------------- Jardins ----------------#
#########################################

# Graphiques de ratio des jardins
#' graph_ratio_jardin
#'
#' @param df_jard A dataframe
#' @param x A character (columns's dataframe)
#' @param y A character (columns's dataframe) 
#' @param image A character (columns's dataframe)
#' @param signif A character (columns's dataframe)
#' @param cat_jard A vector
#' @param lim_y A numeric
#' @param xlab A character
#' @param ylab A character
#'
#' @return
#' @export
#'
#' @examples
#' 
#' df_test <- data.frame(cat = c("Moyen", "Petit", "Grand"),
#'                       ratio = c(0.3, 1.7, 0.9),
#'                       image = "https://jeroen.github.io/images/frink.png",
#'                       signif = c("", "*", ""))
#' graph_ratio_jardin(df_jard = df_test, x = "cat", y = "ratio", image = "image",
#'                    signif = "signif", cat_jard = c("Petit", "Moyen", "Grand"),
#'                    lim_y = 2, xlab = "Catégorie", ylab = "Ratio")
graph_ratio_jardin <- function(df_jard, x, y = "ratio", image, signif, cat_jard,
                               lim_y, xlab, ylab = "Ratio des observations"){
  ggplot(df_jard, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point() +
    geom_image(aes(image = !!sym(image)), size = 0.14) +
    geom_text(aes(x = !!sym(x), y = ratio+0.1, label = !!sym(signif)), size = 6) +
    annotate("rect",xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, alpha = 0.1, fill = "#59d7ff") +
    annotate("rect",xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, alpha = 0.1, fill = "red") +
    scale_x_discrete(limits=cat_jard) +
    scale_y_continuous(limits = c(0, lim_y)) +
    xlab(xlab) +
    ylab(ylab) +
    theme_cowplot()
}

# Histogrammes des jardins
histo_jardin <- function(df_jard, x, y = "nobs", fill = "type",
                         limits_x, val_fill, xlab, ylab = "% des observations",
                         title=paste0("% des observations dans une catégorie ",
                                      "pour l'espèce et \nsur toutes les observations")){
  return(df_jard %>%
    ggplot() +
    geom_bar(aes(x = !!sym(x), y = !!sym(y),
                 fill = !!sym(fill)),
             stat = 'identity', position = "dodge") +
    scale_x_discrete(limits=limits_x) +
    scale_fill_manual(values = val_fill)+
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    theme_cowplot() +
    theme(plot.title = element_text(face = "plain", size = 11)))
}

# Position des jardins
carte_point_jardin <- function(france, df_jp, x = "longitude", y = "latitude",
                               color = "Présence", alpha = "Présence", title){
  ggplot(france) + 
    geom_sf(fill = "#f4f4f4") +
    geom_point(data = df_jp, aes(x = !!sym(x), y = !!sym(y),
                                 color=!!sym(color), alpha = !!sym(alpha))) +
    theme_light() +
    ggtitle(title) +
    theme(title = element_text(size = 9)) +
    scale_alpha_manual(values = c(0.75, 1)) +
    guides(alpha = "none")
}

# Barycentre
carte_bary_one_df <- function(france, df_bary, x = "longitude", y = "latitude",
                              color = "taxon", frame ="session_year", col_val,
                              txt = "taxon",
                              title = "Barycentre des jardins et de l'espèce"){
  gg = ggplot(france) +
    geom_sf(fill = "#f0f0f0", color = "#a0a0a0") +
    geom_point(data = df_bary,
               aes(x = !!sym(x), y = !!sym(y), color = !!sym(color),
                   frame = !!sym(frame), text = !!sym(txt))) +
    ggtitle(title) +
    theme(title = element_text(size = 9)) +
    scale_color_manual(values = col_val) +
    labs(color = "Barycentre") +
    theme_minimal()
  
  return(gg %>%
    ggplotly(tooltip = "text")  %>%
    layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) %>%
    animation_opts(transition = 0, frame = 1000))
}

# Barycentre (toutes les espèces)
carte_bary_two_df <- function(france, df_bary, df_bary2, x = "longitude", y = "latitude",
                              color = "nom_esp_min", frame = "session_year", txt = "taxon",
                              customdata ="taxon", col_man,
                              title = "Barycentre des jardins et des espèces"){
  
  gg = ggplot(france) +
    geom_sf(fill = "#f0f0f0", color = "#a0a0a0") +
    geom_point(data = df_bary,
               aes(x = !!sym(x), y = !!sym(y), color = !!sym(color),
                   frame = !!sym(frame), text = !!sym(txt),
                   customdata = paste0("../out/dashboard_espece_",
                                       !!sym(customdata), ".html"))) +
    geom_point(data = df_bary2,
               aes(x = !!sym(x), y=!!sym(y), frame = !!(sym(frame)),
                   text = !!sym(txt)),
               color = "#d50404") +
    scale_color_manual(values = col_man) +
    ggtitle(title) +
    theme(title = element_text(size = 9)) +
    labs(color = "Barycentre") +
    theme_minimal()
  
  gg = ggplotly(gg, tooltip = "text") %>%
    layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) %>%
    animation_opts(transition = 0, frame = 1000)
  
  gg <- htmlwidgets::onRender(gg, "
     function(el, x) {
     el.on('plotly_click', function(d) {
     var url = d.points[0].customdata;
     //url
     window.open(url);
     });
     }
     ")
  return(gg)
}

#########################################
#------- [Test] Cartes par mois --------#
#########################################

# Carte pour un mois d'une année
gg_carte_mois = function(month, df_sp, france){
  df_mois = data.frame(mois = month,
                       jardin_id = 0,
                       latitude = NA,
                       longitude = NA,
                       sum_ab = 0)
  df_migration = df_sp %>%
    mutate(mois = strftime(session_date, "%m")) %>%
    filter(mois == month) %>%
    group_by(mois, jardin_id, latitude, longitude) %>%
    summarise(sum_ab = sum(taxon_count)) %>%
    filter(sum_ab != 0) %>%
    dplyr::union(df_mois)
  
  gg = ggplot(france) +
    geom_sf(fill = "#f0f0f0", color = "#a0a0a0") +
    geom_point(data = df_migration, aes(x = longitude, y=latitude), color = "red") +
    theme_minimal() +
    ggtitle(month)
  
  return(gg)
}

#########################################
#------- [Test] Cartes par mois --------#
#########################################

# Heatmap
heatmap_co_occ <- function(df_heatmap, x = "Var1", y = "Var2", fill = "value",
                           color_x, color_y,
                           title = "Matrice de co-occurence des espèces"){
  return(ggplot(reshape2::melt(df_heatmap),
         aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill))) +
    geom_tile(color = "white") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          title = element_text(size = 9),
          axis.text.x = element_text(size = 8, angle = 90, vjust = 0.4, hjust = 1, colour = color_x),
          axis.text.y = element_text(size = 8, color = color_y)) +
    scale_fill_gradient2(low = "white", high = "#fc1d1a") +
    coord_fixed() +
    ggtitle(title))
}

# Histogramme pour une espèce
histo_co_occ <- function(df_histo, x = "nom", y = "corr", order = "corr",
                         fill = "Corrélation", breaks, values,
                         xlab = "Nom de l'espèce", ylab = "% de co-occurence",
                         title = "% de co-occurence des autres espèces"){
  return(df_histo %>%
    ggplot() +
    geom_bar(aes(x = reorder(!!sym(x), -!!sym(order)), y = !!sym(y),
                 fill = !!sym(fill)), stat = "identity") +
    scale_fill_manual(breaks = breaks,
                      values = values) +
    geom_hline(yintercept = 30, color = "red") +
    scale_x_discrete(limits=rev) +
    coord_flip() +
    theme_minimal_vgrid() +
    theme(axis.text.y = element_text(size = 7), 
          title = element_text(size = 9)) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) )
}

