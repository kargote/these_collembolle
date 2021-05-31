summary(data)
str(data)
data$temps<-as.factor(data$temps)
data$d_in<-as.factor(data$d_in)
data$bloc<-as.factor(data$bloc)

library(lme4)
library(car)

m0<-lmer(d_fin~d_in+temps+(1|bloc), data=data)
Anova(m0)

shapiro.test(residuals(m0)) # normalité des résidus du modèle 
bartlett.test(d_fin~d_in+temps, data=data)

leveneTest(data$d_fin, data$d_in, data$temps) # homoscédasticité des variances = égalité des variances

qqPlot(lm(d_fin~d_in, data=data), envelope =.99)

library(rstatix)
data %>%
  group_by(d_in) %>%
  identify_outliers(d_fin)

library(multcomp)
mcomp<-glht(m0,linfct=mcp(d_in="Tukey"))
mp<-summary(mcomp,test=adjusted(type="holm"))
cld(mp, level=0.05)


m1<-lmer(t_repro~d_in+(1|bloc)+(1|temps), data=data)
Anova(m1)

shapiro.test(residuals(m1)) # normalité les résidus du modèle 
bartlett.test(d_fin~d_in+temps, data=data)

qqPlot(lm(t_repro~d_in+temps+bloc, data=data), envelope =.99)
data %>%
  group_by(d_in, temps, bloc) %>%
  identify_outliers(t_repro)

datas3<-data[-3,]

m2<-lmer(t_repro~d_in+(1|bloc)+(1|temps), data=datas3)
Anova(m2)

shapiro.test(residuals(m2))

datas368<-datas3[-67,]

m3<-lmer(t_repro~d_in+(1|bloc)+(1|temps), data=datas368)
Anova(m3)
shapiro.test(residuals(m3))

qqPlot(lm(t_repro~d_in+temps+bloc, data=datas368), envelope =.99)


m4<-lmer(d_fin~temps+(1|bloc), data=data)
Anova(m4)

shapiro.test(residuals(m4))
bartlett.test(data$d_fin, data$temps)

summary(data)

m5<-glm(d_fin~d_in+temps+bloc, family=poisson, data=data)

summary(m5)
Anova(m5)
dispersiontest(m5, trafo=1) # fonctionne que pour GLM poisson
hist(m5$res)

E1 <- resid(m5, type = "pearson")
N <- nrow(data)
p <- length(coef(m5))
sum(E1^2) / (N - p) # 45

plot(m5)

m6<-glm(d_fin~d_in+temps+bloc, family=quasipoisson, data=data)
summary(m6)
Anova(m6)
hist(m6$res)

E1 <- resid(m6, type = "pearson")
N <- nrow(data)
p <- length(coef(m6))
sum(E1^2) / (N - p) #45

m7<-glm.nb(d_fin~d_in+temps+bloc, data=data)
summary(m7)
Anova(m7)
hist(m7$res)

E1 <- resid(m7, type = "pearson")
N <- nrow(data)
p <- length(coef(m7))
sum(E1^2) / (N - p) #0.9

m8<-glm(t_repro~d_in+bloc+temps, family=poisson, data=data)
summary(m8)
Anova(m8)
dispersiontest(m8, trafo=1) 
hist(m8$res)

m9<-glm(t_repro~d_in+bloc+temps, family=quasipoisson, data=data)
summary(m9)
Anova(m9)
hist(m9$res)

m10<-glm.nb(t_repro~d_in+bloc+temps, data=data)
summary(m10)
Anova(m10)
hist(m10$res)

boxplot(data$d_fin~as.factor(data$d_in)) #Boxplot Cecile
hist(data$d_fin) #Distribution de donnes -> faire pour les 2 mélangé et pour chaque date