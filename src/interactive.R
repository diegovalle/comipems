

## Visualize as a map
##

## data.frame for the map
school_map <- ddply(comi13, .(school), summarise, median_score = median(score),
      lat = lat[1], long = long[1], nstudents = length(score),
                    name = name[1])


qmplot(long, lat, data = na.omit(school_map),
       source = 'stamen', maptype = 'toner',
       color = median_score, size = nstudents)

## create a leaflet interactive version
leaflet <- Leaflet$new()
leaflet$tileLayer(provider = 'Stamen.TonerLite')
leaflet$set(width = 800, height = 600)
leaflet$setView(c(19.45, -99.1), zoom = 11)
## leaflet$circle(c(19.51189, -99.19738))
leaflet$geoJson(toGeoJSON2(as.list(as.data.frame(t(na.omit(school_map)))), lon ="long", lat = "lat"),
             onEachFeature = '#! function(feature, layer){
      layer.bindPopup(feature.properties.popup)
    } !#',
             pointToLayer =  "#! function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: feature.properties[2]/200,
        fillColor: getColor(feature.properties[1]),    
        color: '#000',
        weight: 1,
        fillOpacity: 0.8
      })
    } !#")
##map3$enablePopover(TRUE)
##map3$print("comipems.map")
## map3$save("map.html", cdn = TRUE)




## Polychart interactive charts of enlace ~ comipems data

createLineChart("bu ~ score", enlace.sp,
                "percent who scored 'good' in ENLACE",
                "ENLACE 2010 versus COMIPEMS 2013, by school ('good' in ENLACE - Math)")$save("html/enlace-bueno.html",                                                        cdn = TRUE)

createLineChart("ex ~ score", enlace.sp,
                "percent who scored 'excellent' in ENLACE",
                "ENLACE 2010 versus COMIPEMS 2013, by school ('excellent' in ENLACE - Math)")$save("html/enlace-excelente.html",                                                        cdn = TRUE)

