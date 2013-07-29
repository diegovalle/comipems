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
