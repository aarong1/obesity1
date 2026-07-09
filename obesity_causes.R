library(sf)
library(leaflet)
library(ggplot2)

# fast_food_belfast <- st_read("data/export (2).geojson") |> 
#   st_centroid() 
parks <- st_read("data/export (1).geojson") |> 
  st_centroid() 
fast_food <- st_read("data/export.geojson")|> 
  st_centroid() 

# (parks) |> 
#   leaflet() |> 
#   addTiles() |> 
#   addCircles(radius = 50, 
#              label = ~name,
#              popup = ~as.character(name))

# (fast_food) |> 
#   leaflet() |> 
#   addTiles() |> 
#   addCircles(radius = 50, popup = ~as.character(name))

# pts <- parks[sf::st_geometry_type(parks) %in% c("POINT", "MULTIPOINT"), ]
# polys <- parks[sf::st_geometry_type(parks) %in% c("POLYGON", "MULTIPOLYGON"), ]

parks_area <- st_read("data/export (1).geojson") |> 
  st_area()

parks$area <- parks_area

sum(parks$area)/1900000
nrow(parks)/1900000 *100000
nrow(fast_food)/1900000* 100000

pop %>% summarise(ow = sum(bmi %in% c('overweight','obese')), n =n()) %>% 
  mutate(perc_ow=ow/n)

parks$area <- as.numeric(parks_area)
  
metric_cards1 <- function(top,text,change,change_icon = NULL,change_class=NULL,
                          color = 'mediumseagreen',
                          opacity = 'opacity-75'){
div(class = paste("",opacity),

            #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
            div(class = "", style = paste("color:",color), format(top,big.mark = ',',digits=3)),
            div(class = "", text),
            div(class = paste("", change_icon),
                tags$i(class = change_class, change)
            )
    )
  }

metric_cards_parks1 <-  metric_cards1(633,'Parks and Green Spaces','NI', 
                                        color = 'mediumseagreen',
                                        opacity = 'opacity-75')

metric_cards_parks <- metric_card(633,'Parks and Green Spaces','NI', 
                                  color = 'mediumseagreen',
                                  opacity = 'opacity-75')

metric_cards_fast_food <- metric_card(890,'NI','Fast Food Outlets',color = 'mediumseagreen')




# browsable(metric_cards_parks)

# var map = L.map('custom_map1').setView([51.505, -0.09], 13);

# dat <- reduced_pop|>
#       slice_sample(n = 500)
# 
#     dat |>
#       filter(age>25) |>
#       filter(!is.na(bmi)) |>
#       group_by(bmi) |>
#       e_charts(height=290) |>
#       e_density(qrisk_percentile,breaks=5) |>
# 
#       e_mark_line(title = 'Baseline',
#                   data = list(
#                     type = "average",
#                     name = "Average"
#                   )) |>
#       e_theme('walden')

      
      
      
      
  #     e_theme('walden')