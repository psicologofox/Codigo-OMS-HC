#cargar ipak
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#cargar paquetes
packages <- c("parameters","apa","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN", "semTools")
ipak(packages)

#exportar base de datos
ROMSH <- read_sav("D:/Noel Valdivia/Dropbox/Nueva carpeta/copia para factor/ROMSH.sav")
View(ROMSH)
attach(ROMSH)

#Obtenemos el KMO y la prueba de bartlett
REdaS::KMOS(ROMSH)
bartlett.test(ROMSH)

#alpha de la escala total 
acromsh <- psych::alpha(ROMSH)
acromsh

#procedemos al analisis de factores
resoms <- parameters::n_factors(ROMSH, rotate = "oblimin", fm = "wls", n =NULL)
plot(resoms)
as.data.frame(resoms)

#analisis factorial exploratorio
romshfa<- psych::fa(ROMSH, nfactors = 3, fm = "wls", rotate = "oblimin", cor = "poly")
print(romshfa, digits = 2, cut = .30, sort = TRUE)

#analisis factorial confirmatorio

#probamos el modelo de un factor
oneoms<- 'Estr151 =~ oms1 + oms2 + oms3 + oms4 + oms5 + oms6 + oms7 + oms8 + oms9 + oms10 + oms11 + oms12 + oms13 + oms14 + oms15'
oneomscfa <- lavaan::cfa(oneoms, orthogonal=TRUE, data=ROMSH, estimator="WLSMV", ordered = names(ROMSH))
summary(oneomscfa, fit.measures=TRUE, standardized = TRUE)
fitmeasures(oneomscfa, standardiezed = TRUE)

#probamos el modelo de 3 factores
tresoms <- 'act =~ oms1 + oms9 + oms10 + oms11 + oms13 + oms15
ds =~ oms2 + oms6 + oms7 + oms12 + oms14
Bus =~ oms3+ oms4 +oms5 + oms8'
tresomscfa <- lavaan::cfa(tresoms, orthogonal=FALSE, data=ROMSH, estimator="WLSMV", ordered = names(ROMSH))
summary(tresomscfa, fit.measures=TRUE, standardized = TRUE)
fitmeasures(tresomscfa, standardiezed = TRUE)

#re especificaciones#
modindices(tresomscfa, sort = TRUE, maximum.number = 10)
tresoms_mod_01 <- 'act153 =~ oms1 + oms9 + oms10 + oms11 + oms13 + oms15
ds153 =~ oms2 + oms6 + oms7 + oms12 + oms14
Bus153 =~ oms3+ oms4 +oms5 + oms8
oms6 ~~ oms7
oms5 ~~ oms8
oms3 ~~ oms5
oms9 ~~ oms13
oms13 ~~ oms15'
tresomscfa_01 <- lavaan::cfa(tresoms_mod_01, orthogonal=FALSE, data=ROMSH, estimator="WLSMV", ordered = names(ROMSH))
summary(tresomscfa_01, fit.measures=TRUE, standardized = TRUE)
modindices(tresomscfa_01, sort = TRUE, maximum.number = 10)
fitmeasures(tresomscfa_01)
#plot
tidySEM::graph_sem(cfar153_01)
lavaanPlot::lavaanPlot(model = tresomscfa_01, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, star = c("regress"))