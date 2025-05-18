library(foreign)

# ejemplo 2 cesarias diferencia de medias 
Dt<- read.arff("caesarian.csv.arff")
alpha=0.05
summary(Dt)
Dt$Age= as.numeric(as.character( Dt$Age))
tt=t.test(Age ~ Caesarian, data=Dt)
names(tt)
tcalc=tt$statistic
pval=tt$p.value
tcalc
pval
tt

##ejemplo  2  coloesterol Diferencia  de  medias  pareadas
t.test(Dt2$chol52,DT2$chol62, paired= T)
t.test(Dt2$dchol,mu=0)

#ejemplo 3 puntajes  mentales (no parametricas)
# Leer un archivo de texto usando read.table
nMS <- read.table("NMS.txt", header = TRUE, sep = " ")
#test de wilcoxn
#ver

#ejemplo 4 diestas perdida de peso
dieta <- read.table("DietWeigthLoss.txt", header = TRUE, sep = "\t")
dieta
# Crear el boxplot
boxplot(dieta$WeightLoss ~ dieta$Diet, data = dieta, 
        xlab = "Categoría de Dieta", ylab = "Pérdida de Peso",
        main = "Boxplot de Pérdida de Peso por Categoría de Dieta")
anova=aov(WeightLoss ~ Diet , data=dieta)
summary(anova)
#anova no parametrico 
kruskal.test(WeightLoss ~ Diet , data=dieta)
#test honesto  de  tukey
par(las=1)
TKHSD=TukeyHSD(anova)

plot(TKHSD)
#la dieta c  es diferente  a la dieta a, la dieta c es diferente a,
#la dieta c es  diferente a ladieta  b


#ejemplo  5 conluciones
concl <- read.table("Concusions.txt", header = TRUE, sep = " ")
chi<-chisq.test(concl)
chi
fisher.test(concl)
