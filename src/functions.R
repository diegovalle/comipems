median_cl_boot <- function(x, conf.int = 0.95, B = 1000, na.rm = TRUE, reps = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  n <- length(x)
  xm <- median(x)
  if (n < 2)
    return(data.frame(y = xm, ymin = NA, ymax = NA))
  resamples <- lapply(1:B, function(i) sample(x, replace=T))
  r.median <- sapply(resamples, median)
  quant <- quantile(unlist(r.median),
                    c((1 - conf.int)/2, (1 + conf.int)/2))
  names(quant) <- NULL
  Median <- median(x)
  data.frame(y = Median,
             ymin = quant[1],
             ymax = quant[2])
}

toGeoJSON2 <- function(list_, lat = 'latitude', lon = 'longitude'){
  x = lapply(list_, function(l){
    if (is.null(l[[lat]]) || is.null(l[[lon]])){
      return(NULL)
    }
    list(
      type = 'Feature',
      geometry = list(
        type = 'Point',
        coordinates = as.numeric(c(as.character(l[["long"]]),
          as.character(l[["lat"]])))
      ),
      properties = l[!(names(l) %in% c(lat, lon))]
    )
  })
  setNames(Filter(function(x) !is.null(x), x), NULL)
}

addSource <- function(plot, text = "Data Source: Gaceta de Resultados COMIPEMS 2013") {
  plot <- arrangeGrob(plot, 
              sub = textGrob(text,
                x = 0, hjust = -0.1, vjust=0.1,
                gp = gpar(fontface = "italic", fontsize = 9,
                  col = "gray50")))
  return(plot)
}
saveChart <- function(p, filename, width = 9.60, height = 6.00) {
  ggsave(filename = file.path("graphs", filename), plot = p, dpi = 100,
         width = width, height = height)
}
addSave <- function(p, filename, width = 9.60, height = 6.00,
                    text = "Data Source: Gaceta de Resultados COMIPEMS 2013") {
  saveChart(addSource(p, text = text), filename, width, height)
}

createLineChart <- function(formula, data,
                            ytext,
                            title.text) {
  formula = as.formula(formula)
  level.enlc <- as.character(terms(formula)[[2]])
  r.bu <- rPlot(formula, data = data, type = "point",
                tooltip="function(item){return item.name +'\n'}",
                title = "Enlace")
  r.bu$guides(x = list(title = "95th percentile COMIPEMS score",
                min = min(data$score)-1,
                max = max(data$score)+1),
              y = list(title = ytext,
                min = min(data[[level.enlc]])-2,
                max = max(data[[level.enlc]])+2))
  r.bu$addParams(title = title.text)
  
  enlace.loess <- data.frame(score = data$score,
                             bu = predict(loess(formula, data = data)))
  names(enlace.loess) <- c("score", level.enlc)
  r.bu$layer(data = enlace.loess, type = 'line', 
             color = list(const = 'black'), copy_layer = TRUE, tooltip = NULL,
             size = list(const = 4))
  r.bu$set(width = 600)
  ## r.bu$set(padding = list(top = 0, left = 0, bottom = 0, right = 150)) 
  return(r.bu)
}
