#'
#' @title Color vector compliant with the graphical charter of the ART
#'
#' @description Create a color vector containing the hexadecimal values of the ART graphical charter
#'
#' @param ... Zero, one or more character strings representing colors of the graphical charter or other colors in hexadecimal format
#'
#' @return A vector containing **color hexadecimal values**. Without argument, the function returns a vector composed of each of the colors of the graphic charter. With arguments, the function returns a vector with all the hexadecimal code of the arguments.
#'
#' @examples color_vector()
#' @examples color_vector("blue", "#00000")
#' @examples color_vector("#00000")
#'
#' @export
color_vector <- function(...){

  # lister les couleurs de l'ART
  colors <- c(
    "ferroviaire" = "#9cc5c4",
    "vertclair" = "#9cc5c4",
    "lightgreen" = "#9cc5c4",

    "autocar" = "#f39200",
    "orange" = "#f39200",

    "autoroute" = "#6c8a99",
    "bleugris" = "#6c8a99",
    "bluegray" = "#6c8a99",

    "billetique" = "#c66c61",
    "donnee" = "#c66c61",
    "rouge" = "#c66c61",
    "red" = "#c66c61",

    "aeroportuaire" = "#815374",
    "violet" = "#815374",
    "purple" = "#815374",

    "bleu" = "#0084ad",
    "2" = "#0084ad",
    "blue" = "#0084ad",

    "vertfonce" = "#006168",
    "1" = "#006168",
    "darkgreen" = "#006168",

    "ratp" = "#35b1c9",
    "bleuclair" = "#35b1c9",
    "lightblue" = "#35b1c9",

    "violetclair" = "#b42573",
    "3" = "#b42573",

    "vert" = "#497629",
    "4" = "#497629",
    "green" = "#497629",

    "rougeclair" = "#ff4539",
    "rougevif" = "#ff4539",
    "6" = "#ff4539",
    "lightred" = "#ff4539",

    "violetfonce" = "#762157",
    "5" = "#762157",
    "darkpurple" = "#762157",

    "gris" = "#8d827a",
    "7" = "#8d827a",
    "gray" = "#8d827a",

    "corporate" = "#24356d",
    "bleufonce" = "#24356d",
    "darkblue" = "#24356d",

    "grisfonce" = "#5b6770",
    "9" = "#5b6770",
    "darkgray" = "#5b6770",

    "gris2" = "#7e7f74",
    "8" = "#7e7f74",
    "gray2" = "#7e7f74")

  # initialiser le vecteur
  output <- c()

  # compléter le vecteur
  for (i in c(...)){
    # avec les couleurs de l'ART renseignées en argument
    if(i %in% names(colors)){
      output <- append(output, colors[i])
    }
  # avec les couleurs hors charte graphique renseignées en argument
    if(!i %in% names(colors)){
      output <- append(output, i)
    }
  }

  # s'il n'y a aucun argument, mettre toutes les couleurs de l'ART dans le vecteur
  if(is.null(output)){
    output <- c(
      "vertclair" = "#9cc5c4",
      "rouge" = "#c66c61",
      "bleu" = "#0084ad",
      "orange" = "#f39200",
      "bleugris" = "#6c8a99",
      "violet" = "#815374",
      "vertfonce" = "#006168",
      "bleuclair" = "#35b1c9",
      "violetclair" = "#b42573",
      "vert" = "#497629",
      "rougeclair" = "#ff4539",
      "violetfonce" = "#762157",
      "gris" = "#8d827a",
      "bleaufonce" = "#24356d",
      "grisfonce" = "#5b6770",
      "gris2" = "#7e7f74")
  }

  output
}
