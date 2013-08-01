## Compare the two best skools
p <- ggplot(subset(comi13, schoolid %in% c(509, 603)),
       aes(score, group = schoolid, fill = as.factor(schoolid))) +
         geom_histogram(alpha = .8, binwidth = 1, position = "identity") +
         scale_fill_manual("School", labels = c("ESCUELA NAL. PREPARATORIA\nPLANTEL NÚMERO 6", "CECyT NÚM. 9"),
                             values = c("#842521", "#e6ab2b"),
                           breaks = c(603, 509)) +
  labs(title = "Top 2 schools based on COMIPEMS scores")
                    
addSave(p, "top2.png")


## Now the top 10 best schools
comi.median <- ddply(comi13, .(name), transform,
      median = median(score))
comi.median$type <- "Other"
comi.median$type[comi.median$schoolid %in% 600:699] <- "UNAM"
comi.median$type[comi.median$schoolid %in% 500:599] <- "IPN"
top10 <- ddply(comi13, .(schoolid), summarise,
      median = median(score))
top10 <- top10[order(-top10$median)[1:20],]$schoolid
p <- ggplot(subset(comi.median, schoolid %in% top10),
       aes(reorder(name, score, fun = median), score, color = type)) +
  geom_jitter(alpha = .1) +
  geom_boxplot(fill = "transparent", color = "red") +
  coord_flip()+
  labs(title = "Top 20 schools based on COMIPEMS median score") +
  xlab("school")+
  scale_color_manual("system", breaks = c("UNAM", "IPN", "Other"),
                     labels = c("UNAM", "IPN", "Other"),
                    values = c("#842521", "#1b3665", "#e6ab2b"))+
    guides(color = guide_legend("system",
             override.aes = list(alpha = 1)))
addSave(p, "top20.png", width = 13.00)

## How does the score distribution look like
p <- ggplot(comi11, aes(score, fill = "2011")) +
  geom_histogram(binwidth = 1, alpha = .5) +
  geom_histogram(data = comi13, aes(score, fill = "2013"),
                 binwidth = 1, alpha = .5) +
  scale_fill_hue("Year") +
  labs(title = "Histograms of COMIPEMS scores") +
  annotate(geom = "text", label = "Before 2013\na minimum score\nof 31 was required\nto enter high school", x = 8, y = 2050, hjust = 0,
           size = 3)
addSave(p, "histogram.png",
        text = "Data Source: Gaceta de Resultados COMIPEMS 2011 and 2013")

## Histograms by modalidad
p <- ggplot(comi13[!is.na(comi13$modalidad),], aes(score)) +
  geom_histogram(binwidth = 1, alpha = .5) +
  facet_wrap(~ modalidad, ncol =  1) +
  labs(title = "Histograms of COMIPEMS scores, by type of school")
addSave(p, "histogram-type.png")

p <- ggplot(comi13[!is.na(comi13$statecode),], aes(score)) +
  geom_histogram(binwidth = 1, alpha = .5) +
  facet_wrap(~ state, ncol =  1)+
  labs(title = "Histograms of COMIPEMS scores, by state where the school of admittance is located")
addSave(p, "histogram-state.png")

## Histograms by university system
##Find out if the high school is run by the UNAM or Poli
comi13$type <- NA
comi13$type[comi13$schoolid %in% 600:699] <- "UNAM"
comi13$type[comi13$schoolid %in% 500:599] <- "IPN"
cch <- subset(comi13, type %in% c("UNAM", "IPN"))
## These are the CCHs which are part of the UNAM
cch$cch <- ifelse(cch$schoolid %in% c(602,604,607,609,615), "cch", "prepa")
cch$cch <- str_c(cch$cch, cch$type)
other <- subset(comi13, !type %in% c("UNAM", "IPN"))
other$cch <- "Other"
cch <- rbind(cch, other)
cch$cch <- mapvalues(cch$cch, c("cchUNAM", "prepaIPN", "prepaUNAM", "Other"),
          c("CCH (UNAM)", "CECyT (IPN)", "ENP (UNAM)", "Other"))
cch$cch <- reorder(cch$cch, -cch$score, median)
medians <- ddply(cch, .(cch), summarise, median = median(score, na.rm = TRUE))

p <- ggplot(cch,
       aes(score, group = cch, fill = cch)) +
  geom_histogram(alpha = 1, binwidth = 1) +
  geom_vline(data = medians, aes(xintercept = median), color = "red",
             alpha = .5) +
  scale_fill_manual("university\nsystem",
                    values = c("#e6ab2b", "#1b3665", "#842521", "gray")) +
  facet_wrap(~cch, ncol = 1) +
  labs(title = "Histograms and medians of COMIPEMS scores, by university system")
addSave(p, "histogram-unam-poli.png")

p <- ggplot(cch,
       aes(reorder(cch, score, fun = median, na.rm = TRUE),
           score, group = cch)) +
  geom_jitter(alpha = .05) +
  coord_flip() +
  geom_boxplot(color = "red", fill = "transparent") +
  xlab("school") +
  labs(title = "Boxplot of COMIPEMS scores by university system")
addSave(p, "boxplot-unam-poli.png")


ddply(cch, .(cch), summarise, median(score))


ggplot(cch,
       aes(score, group = type, fill = type)) +
  geom_histogram(alpha = .8, binwidth = 1)

## How has the minimum score evolved since 2010
comi13$type2 <- comi13$type
comi13$type2[str_detect(comi13$name, "PREPARATORIA OFICIAL")] <- "Prepa Oficial"
comi13$type[is.na(comi13$type2)] <- "Other"
mins <- ddply(comi13, .(schoolid), summarise,
              min10 = minscore2010[1],
              min11 = minscore2011[1],
              min12 = minscore2012[1],
              min13 = minscore2013[1],
              state = state[1],
              type = type2[1],
              name = name[1])
mins <- melt(mins, id = c("schoolid", "state", "type", "name"))
mins$variable <- str_replace(mins$variable, "min", "20")

p <- ggplot(mins, aes(variable, value,
                 group = schoolid, color = type)) +
  geom_line(alpha = .8) +
  labs(title = "Trends in minimum score, by school") +
  xlab("year") +
  ylab("minimum score")+
  guides(color = guide_legend("system",
           override.aes = list(alpha = 1))) +
  scale_color_manual(breaks = c("UNAM", "IPN", "Other", "Prepa Oficial"),
                     labels = c("UNAM", "IPN", "Other", "Prepa Oficial"),
                     values = c("#842521", "gray","#1b3665", "#e6ab2b" ))
addSave(p, "trends.png",
        text = "Data Source: http://mapas.comipems.org.mx/")  

## Plot ENLACE ~ COMIPEMS
enlace$INSUFICIENTE.1 <- as.numeric(as.character(enlace$INSUFICIENTE.1))
enlace$ELEMENTAL.1 <- as.numeric(as.character(enlace$ELEMENTAL.1))
enlace$BUENO.1 <- as.numeric(as.character(enlace$BUENO.1))
enlace$EXCELENTE.1 <- as.numeric(as.character(enlace$EXCELENTE.1))
enlace$X..DE.ALUMNOS.EVALUADOS.MENOR.AL.80. <- ifelse(enlace$X..DE.ALUMNOS.EVALUADOS.MENOR.AL.80. == "N",
                                                      FALSE, TRUE)
enlace.sp <- ddply(enlace, .(CLAVE.DE.LA.ESCUELA), summarise,
      ins = mean(INSUFICIENTE.1),
      ele = mean(ELEMENTAL.1),
      bu = mean(BUENO.1),
      ex = mean(EXCELENTE.1),
                   name = SchoolName[1])


## The 95th percentile seems like a good choice since so few
## students score 'good' or 'excellent' in ENLACE
comi.quantile <- ddply(na.omit(comi13.cct), .(CCT), summarise,
      score = quantile(score, .95))


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
  labs(title = "Top public and private high schools with more than 50 students based on ENLACE 2012 (Federal District only)") +
  xlab("students scoring 'excelent' in the math portion of ENLACE 2012") +
  ylab("school") +
  scale_x_continuous(labels = percent) +
  scale_size("Number of\nstudents") +
  scale_shape_manual(breaks = c("Morning", "Afternoon"), values = c(17,16)) +
  scale_color_manual(breaks = c("TRUE", "FALSE"),
                     values = c("#00a195", "#822422"))
addSave(p, "top-enlace-df.png", width = 13.00,
        text = "Data Source: Resultado oficial ENLACE 2012 www.enlace.sep.gob.mx")

pisa <- data.frame(score = c(453, 419),
                   se = c(3.8, 1.8),
                   country = c("Hispanics\nin the US", "Mexico"))
p <- ggplot(pisa, aes(country, score, color = country)) +
  geom_errorbar(aes(ymin = score - se, ymax = score + se), width = .05) +
  geom_point(size = 3) +
  coord_flip() +
  ylab("PISA math score 2009") +
  scale_color_manual(values = c("blue", "darkgreen"), guide=FALSE) +
  labs(title = "Hispanics score 0.34 standard deviations above Mexicans in the PISA math test")
addSave(p, "pisa.png", width = 8, height = 5,
        text = "Source: OECD, Program for International Student Assessment (PISA), 2009")

## Some dirichlet regressions
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




