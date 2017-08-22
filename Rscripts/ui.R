library(shiny)
library(leaflet)


ui <- fluidPage(
  # leafletOutput("mymap")
  
  navbarPage("Dakar by bus",
             tabPanel("Map",
                      tags$style(type = "text/css", "html, body, #map {width:100%;height:100%}"),
                      leafletOutput('map', height = "100%"), #this height variable is what breaks the code.
                      absolutePanel(top = 60, right = 10, draggable=TRUE),
              bootstrapPage(div(class="outer",
                                        tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                        leafletOutput("mymap", width = "100%", height = "100%"),
                                        absolutePanel(top = 60, right = 10, draggable=TRUE
                                        )))
             ),
             
             
             tabPanel("About",
                 withMathJax(),
                 helpText(tags$h1("Prendre le Bus à Dakar"),
                          tags$h3("Définition"),
                          div("Le SIR (standardized incidence ratio) ou standardisation indirecte repose sur la comparaison du nombre total des cas observé dans la population étudiée par rapport au nombre de cas auquel on pourrait s’attendre si cette population était soumise à une force d'incidence donnée (taux de référence).")),
                 helpText("$$ SIR=\\frac{Observé}{Attendus}$$"),
                 helpText("le SIR est une mesure du risque relatif de la population étudiée par rapport à une population de référence."),
                 helpText(tags$h3("Variabilité des SIR et intervalle confiance"),
                          "La variabilité des SIR ne dépend pratiquement que du numérateur O, le dénominateur étant considéré comme non aléatoire,
               Les \\(O_{i}\\) suivent une distribution de poisson d'espérance \\(\\theta_{i}\\)\\(E_{i}\\)
              ou \\(\\theta_{i}\\) correspond au vrai risque relatif de la région \\(i\\)
               dont le SIR est une estimation."),

                 helpText("$$O_{i}\\sim{}P(\\theta_{i}E_{i})$$"),
                 helpText("On met a profit la relation existant entre la loi de Poisson et la loi du Khi2\\(^{1,2}\\) pour calculer l'interval de confiance
                          d'un paramètre d'une loi de Poisson à un niveau alpha donné."),
                 helpText("$$IC\\left[\\frac{\\chi^2_{\\frac{\\alpha}{2};2.O}}{2E};\\frac{\\chi^2_{1-\\frac{\\alpha}{2};2(O+1) }} {2E}\\right]$$"),


                 ##bibliographie
                 helpText("1- Calculating Poisson confidence Intervals in Excel.",br(),
             "Iain Buchan January 2004",br(),
             "Public Health Informatics at the University of Manchester (www.phi.man.ac.uk)"),
                 helpText("2- Intervalle de confiance pour le paramètre d’une loi de Poisson
             Méthode exacte pour échantillons de taille quelconque.")
            )
)
  # p(),
  # actionButton("recalc", "New points")
)

