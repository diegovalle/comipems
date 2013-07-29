
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
map3 <- Leaflet$new()
map3$tileLayer(provider = 'Stamen.TonerLite')
map3$set(width = 800, height = 600)
map3$setView(c(19.45, -99.1), zoom = 11)
## map3$circle(c(19.51189, -99.19738))
map3$geoJson(toGeoJSON2(as.list(as.data.frame(t(na.omit(school_map)))), lon ="long", lat = "lat"),
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

## Compare the two best skools
p <- ggplot(subset(comi13, schoolid %in% c(509, 603)),
       aes(score, group = schoolid, fill = as.factor(schoolid))) +
         geom_histogram(alpha = .8, binwidth = 1, position = "identity") +
         scale_fill_discrete("School", labels = c("CECyT NÚM. 9", "ESCUELA NAL. PREPARATORIA\nPLANTEL NÚMERO 6")) +
  labs(title = "Top 2 schools based on COMIPEMS scores")
addSave(p, "top2.png")


## Now the top 10 best schools
comi.median <- ddply(comi13, .(name), transform,
      median = median(score))
top10 <- ddply(comi13, .(schoolid), summarise,
      median = median(score))
top10 <- top10[order(-top10$median)[1:20],]$schoolid
p <- ggplot(subset(comi.median, schoolid %in% top10),
       aes(reorder(name, score, fun = median), score)) +
  geom_jitter(alpha = .2) +
  geom_boxplot(fill = "transparent", color = "red") +
  coord_flip()+
  labs(title = "Top 20 schools based on COMIPEMS median score") +
  xlab("school")
addSave(p, "top20.png", width = 13.00)

p <- ggplot(comi11, aes(score, fill = "2011")) +
  geom_histogram(binwidth = 1, alpha = .5) +
  geom_histogram(data = comi13, aes(score, fill = "2013"),
                 binwidth = 1, alpha = .5) +
  scale_fill_hue("Year") +
  labs(title = "Histograms of COMIPEMS scores")
addSave(p, "histogram.png", width = 13.00)

p <- ggplot(na.omit(comi13), aes(score)) +
  geom_histogram(binwidth = 1, alpha = .5, fill = "blue") +
  facet_wrap(~ modalidad, ncol =  1) +
  labs(title = "Histograms of COMIPEMS scores by type of school")
addSave(p, "histogram-type.png", width = 13.00)

p <- ggplot(comi13[!is.na(comi13$statecode),], aes(score)) +
  geom_histogram(binwidth = 1, alpha = .5) +
  facet_wrap(~ state, ncol =  1)+
  labs(title = "Histograms of COMIPEMS scores by state where the school of admittance is located")
addSave(p, "histogram-state.png", width = 13.00)


comi13$type <- NA
comi13$type[comi13$schoolid %in% 600:699] <- "UNAM"
comi13$type[comi13$schoolid %in% 500:599] <- "IPN"
cch <- subset(comi13, type %in% c("UNAM", "IPN"))
cch$cch <- ifelse(cch$schoolid %in% c(602,604,607,609,615), "cch", "prepa")
cch$cch <- str_c(cch$cch, cch$type)
other <- subset(comi13, !type %in% c("UNAM", "IPN"))
other$cch <- "Other"
cch <- rbind(cch, other)
cch$cch <- mapvalues(cch$cch, c("cchUNAM", "prepaIPN", "prepaUNAM", "Other"),
          c("CCH (UNAM)", "CECyT (IPN)", "ENP (UNAM)", "Other"))
cch$cch <- reorder(cch$cch, -cch$score, median)

p <- ggplot(cch,
       aes(score, group = cch, fill = cch)) +
  geom_histogram(alpha = 1, binwidth = 1) +
  scale_fill_manual(values = c("#e6ab2b", "#1b3665", "#842521", "gray")) +
  facet_wrap(~cch, ncol = 1) +
  labs(title = "Histograms of COMIPEMS scores")
addSave(p, "histogram-unam-poli.png", width = 13.00)

p <- ggplot(cch,
       aes(reorder(cch, score, median),
           score, group = cch)) +
  geom_jitter(alpha = .1) +
  coord_flip() +
  geom_boxplot(color = "red", fill = "transparent") +
  xlab("school") +
  labs(title = "Boxplot of COMIPEMS scores by university system")
addSave(p, "boxplot-unam-poli.png", width = 13.00)


ddply(cch, .(cch), summarise, median(score))


ggplot(cch,
       aes(score, group = type, fill = type)) +
  geom_histogram(alpha = .8, binwidth = 1)





enlace$INSUFICIENTE <- as.numeric(as.character(enlace$INSUFICIENTE))
enlace$ELEMENTAL <- as.numeric(as.character(enlace$ELEMENTAL))
enlace$BUENO <- as.numeric(as.character(enlace$BUENO))
enlace$EXCELENTE <- as.numeric(as.character(enlace$EXCELENTE))
enlace$X..DE.ALUMNOS.EVALUADOS.MENOR.AL.80. <- ifelse(enlace$X..DE.ALUMNOS.EVALUADOS.MENOR.AL.80. == "N",
                                                      FALSE, TRUE)
enlace.sp <- ddply(enlace, .(CLAVE.DE.LA.ESCUELA), summarise,
      ins = mean(INSUFICIENTE),
      ele = mean(ELEMENTAL),
      bu = mean(BUENO),
      ex = mean(EXCELENTE),
                   name = SchoolName[1])



comi.quantile <- ddply(na.omit(comi13.cct), .(CCT), summarise,
      score = quantile(score, .7))


enlace.sp <- merge(enlace.sp, comi.quantile, by.x = "CLAVE.DE.LA.ESCUELA",
      by.y = "CCT")

p <- ggplot(enlace.sp, aes(score, ex, label = name)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "ENLACE 2010 versus COMIPEMS 2013, by school ('excellent' in ENLACE- Verbal)" ) +
  ylab("percent who scored 'excellent' in ENLACE") +
  xlab("COMIPEMS score")
addSave(p, "ex-comi.png", width = 13.00,
        text = "Data Source: Gaceta de Resultados COMIPEMS 2013 and ENLACE 2010")

p <- ggplot(enlace.sp, aes(score, bu)) +
  geom_point(shape = 2) +
  geom_smooth(method = "loess") +
  labs(title = "ENLACE 2010 versus COMIPEMS 2013, by school ('good' in ENLACE - Verbal)" ) +
  ylab("percent who scored 'good' in ENLACE") +
  xlab("COMIPEMS score")
addSave(p, "bu-comi.png", width = 13.00,
        text = "Data Source: Gaceta de Resultados COMIPEMS 2013 and ENLACE 2010")


p <- ggplot(enlace.df.12,
       aes(EXCELENTE/100, reorder(NOMBRE.DE.LA.ESCUELA, EXCELENTE, max),
           color = IPN, shape = Shift))+
  geom_point(aes(size = ALUMNOS.PROGRAMADOS.PARA.SER.EVALUADOS)) +
  labs(title = "Top high schools with more than 50 students in the Federal District") +
  xlab("students scoring 'excelent' in the math portion of ENLACE 2013") +
  ylab("school") +
  scale_x_continuous(labels = percent) +
  scale_size("Number of\nstudents") +
  scale_shape_manual(breaks = c("Morning", "Afternoon"), values = c(17,16)) +
  scale_color_hue(breaks = c("TRUE", "FALSE"))
addSave(p, "top-enlace-df.png", width = 13.00)


r.bu <- rPlot(bu ~ score, data = enlace.sp, type = "point",
            tooltip="function(item){return item.name +'\n'}",
            title = "Enlace")
r.bu$guides(x = list(title = "COMIPEMS score"),
            y = list(title = "percent who scored 'good' in ENLACE"),
            title = list(title = "da"))
r.bu$addParams(title = "ENLACE 2010 versus COMIPEMS 2013, by school ('good' in ENLACE - Verbal)")
r.bu$save("html/enlace-bueno.html", cdn = TRUE)

r.ex <- rPlot(ex ~ score, data = enlace.sp, type = "point",
            tooltip="function(item){return item.name +'\n'}")
r.bu$guides(x = list(title = "COMIPEMS score"),
            y = list(title = "percent who scored 'excellent' in ENLACE"),
            title = list(title = "da"))
r.bu$addParams(title = "ENLACE 2010 versus COMIPEMS 2013, by school ('excellent' in ENLACE - Verbal)")
r.ex$save("html/enlace-excelente.html", cdn = TRUE)

dv <- DR_data(enlace.sp[,c("ins", "ele", "bu", "ex")], trafo = TRUE)
reg <- DirichReg(dv ~ score, data = enlace.sp)
pred <- as.data.frame(predict(reg))
names(pred) <- c("pred.ins", "pred.ele", "pred.bu", "pred.ex")
pred <- as.data.frame(cbind(pred, enlace.sp))
#plot(dv)
ggplot(pred, aes(pred.ins, ins)) +
  geom_point()

ggplot(pred, aes(pred.ele, ele)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(pred, aes(pred.bu, bu)) +
  geom_point()

ggplot(pred, aes(pred.ex, ex)) +
  geom_point()







