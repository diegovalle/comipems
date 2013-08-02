

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
## Check the map.html file in the html directory
## for the definition of scaleSize and getColor
## and some css that's necessary
leaflet$geoJson(toGeoJSON2(as.list(as.data.frame(t(na.omit(school_map)))), lon ="long", lat = "lat"),
             onEachFeature = "#! function(feature, layer){
      layer.bindPopup('<table><thead><tr><th><b>'+feature.properties[3]+'<b></th></tr></thead><tr><td>Median Score:</td><td><b>'+feature.properties[1]+'</b></td></tr><tr><td>Number Admitted:</td><td><b>'+feature.properties[2]+'</b></td></tr></table>')
    } !#",
             pointToLayer =  "#! function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: scaleSize(feature.properties[2]),
        fillColor: getColor(feature.properties[1]),    
        color: '#000',
        weight: 1,
        fillOpacity: 0.8
      })
    } !#")


leaflet$legend(position = "bottomright", color = brewer.pal(9, "RdYlBu"),
               labels = sort(unique(cut(school_map$median_score, 9))))
sink(file.path("html", "comipems-map.js"))
leaflet$print("comipems.map")
sink()
##leaflet$save(file.path("html", "temp-map.html"), cdn = TRUE)




## Polychart interactive charts of enlace ~ comipems data

createLineChart("bu ~ score", enlace.sp,
                "percent who scored 'good' in ENLACE",
                "ENLACE 2010 versus COMIPEMS 2013, by school ('good' in ENLACE - Math)")$save("html/enlace-bueno.html",                                                        cdn = TRUE)

createLineChart("ex ~ score", enlace.sp,
                "percent who scored 'excellent' in ENLACE",
                "ENLACE 2010 versus COMIPEMS 2013, by school ('excellent' in ENLACE - Math)")$save("html/enlace-excelente.html",                                                        cdn = TRUE)


## r.bu <- rPlot(value ~ variable , group = "schoolid",
##               data = mins, type = "line",
##               tooltip="function(item){return item.name +'\n'}",
##               title = "Enlace",
##               color = "schoolid")
## r.bu
