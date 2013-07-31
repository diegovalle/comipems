## Read comipems from 2011

comi11 <- read.csv("data/comipems2011.csv.bz2", na.strings = "-0")

## Latitude and longitude data from mapas.comipems.org
## see the file in scrape-map to see how it was obtained
latlong <- read.csv("data/latlong.csv", na.strings=c("-", ""),
                    colClasses = c("character", "numeric", "numeric"))
schools <- read.csv("data/school-list.csv")
## Data merging the schoolid used by comipems and the cct used by enlace
codes <- read.csv("data/codes.csv", na.strings = "")

codes <- merge(codes, schools[, c("schoolid", "Domicilio",
                                  "Institución", "Nombre.del.Plantel")],
               all.x = TRUE, all.y = FALSE)
names(codes) <- c("schoolid", "CCT", "Type", "Address",
                  "Institution", "SchoolName")

enlace <- local({
  mx <- read.csv("data/enlace-mx10.csv", stringsAsFactors=FALSE)
  df <- read.csv("data/enlace-df10.csv", stringsAsFactors=FALSE)
  enlace <- rbind(mx, df)
  enlace
})
########################################################
#Clean COMIPEMS results
########################################################

#Get rid of the data that wasn't cleaned during scraping
comi11 <- comi11[comi11$school != "",]
comi11$lowgpa <- ifelse(comi11$lowgpa == "#", TRUE, FALSE)

comi11$score <- as.numeric(as.character(comi11$score))
#http://www.eluniversal.com.mx/notas/439282.html
comi11[which(comi11$score == max(comi11$score, na.rm = TRUE)),]$score == 127 #Highest score

comi11$concentration <- as.numeric(str_sub(format(comi11$school, width = 6), 4,6))

comi11$schoolid <- ifelse(!is.na(as.numeric(str_sub(comi11$school, 1,3))),
                       as.numeric(str_sub(comi11$school, 1,3)),
                       as.character(comi11$school))
nrow(comi11[which(!is.na(comi11$schoolid) & is.na(comi11$type)),]) == 0
#Check that students attengind match public records
#http://www.denoticia.info/?p=402
nrow(comi11[grep("^6.+", comi11$schoolid),]) == 35638 #UNAM
nrow(comi11[grep("^5.+", comi11$schoolid),]) == 21216 #Poli

########################################################
#Section
########################################################
enlace <- merge(enlace, codes, by.x ="CLAVE.DE.LA.ESCUELA", by.y = "CCT")


#write.csv(enlace, "enlace.csv")
#latlong$school <- as.numeric(latlong$school)
#latlong <- latlong[,c("school", "long", "lat")]
latlong$school <- str_replace_all(format(latlong$school, digits = 6,
                                         justify = "right"), " ", "0")
comi11 <- merge(comi11, latlong,
      by = "school", all.x = TRUE)

comi11$scaled <- scale(as.numeric(comi11$score))
comi11$schoolid2 <- as.numeric(comi11$schoolid)
comi11 <- merge(comi11, unique(na.omit(codes[, c("schoolid", "Type", "CCT")])),
              by.x = "schoolid2", by.y = "schoolid",
              all.x = TRUE, all.y = FALSE)




latlong <- read.csv("data/latlong.csv", na.strings=c("-", ""))
comi13 <- read.csv("data/comipems2013.csv.bz2",
                   na.strings = c("-", "en",
                     "de", "presentó"),
                   stringsAsFactors = FALSE)
comi13$studentid <- as.numeric(as.character(comi13$studentid))
comi13 <- comi13[!is.na(comi13$studentid),]
comi13 <- comi13[,1:4]
comi13$score <- as.numeric(as.character(comi13$score))
comi13$lowgpa <- ifelse(comi13$lowgpa == "#", TRUE, FALSE)


## The first three digits uniquely identify the school
## Make sure to add leading zeroes
extractDigits <- function(str) {
  str_replace_all(format(str, width = 6, justify = "right"),
                                      " ", "0")
}
comi13$schoolid <- ifelse(!is.na(as.numeric(comi13$school)),
                       as.numeric(str_sub(extractDigits(comi13$school),1,3)),
                       NA)

comi13 <- merge(comi13, latlong, by = "school", all.x = TRUE)
comi13 <- ddply(comi13, .(school), transform, minscore2013 = min(score))

comi13$modalidad <- mapvalues(comi13$modalidad, c(1,2,3),
                    c("Carrera Técnica Profesional",
                      "Bachillerato Profesional",
                      "Bachillerato Tecnológico"))

comi13$state <- mapvalues(comi13$statecode, c(9, 15),
                    c("Distrito Federal",
                      "Estado de México"))

comi13.cct <- join(comi13,
                      unique(na.omit(codes[, c("schoolid", "Type", "CCT")])))

## Test that the data matches the COMIPEMS website
## http://comipems.org.mx/dtsestadisticos.php
test_that("Promedio del número de aciertos obtenido en el examen por la totalidad de los sustentantes:",
          expect_that(round(mean(comi13$score, na.rm = TRUE)), equals(71)))
test_that("Concursantes registrados", expect_that(nrow(comi13), equals(310163)))
test_that("No presentaron examen:", expect_that(sum(is.na(comi13$score)),
                                               equals(11936)))
test_that("Tienen derecho a escoger otra opción con lugares disponibles",
          expect_that(sum(comi13$school == "CDO"),
                                               equals(33695)))
test_that("Obtuvieron lugar en una de las opciones que escogieron",
          expect_that(length(na.omit(as.numeric(comi13$school))),
                                               equals(246401)))
test_that("max",
          expect_that(sum(comi13$score == 127, na.rm = TRUE), equals(5)))

enlace.df.12 <- read.csv("data/Enlace_2012.csv")
enlace.df.12$IPN <- str_detect(enlace.df.12[[1]], "CECYT|WALTER")

