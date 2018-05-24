require(gamlss)
require(MASS)
require(MPV)

bici1 <- read.csv(file= "day.csv", header = T, sep = ";")
View(bici1)
str(bici1)
dim(bici1)

bici <- subset(bici1, yr== "1", select = c(season,weekday,workingday,mnth,
                                          weathersit,temp,atemp,hum, 
                                          windspeed, cnt))
View(bici)

class(bici$cnt)

class(bici$temp)
bici$temp <- as.numeric(as.character(sub("," , ".", bici$temp)))
class(bici$temp)

class(bici$atemp)
bici$atemp <- as.numeric(as.character(sub("," , ".", bici$atemp)))
class(bici$atemp)

class(bici$hum)
bici$hum <- as.numeric(as.character(sub("," , ".", bici$hum)))
class(bici$hum)

class(bici$windspeed)
bici$windspeed <- as.numeric(as.character(sub("," , ".", bici$windspeed)))
class(bici$windspeed)

class(bici$season)
bici$season <- as.factor(bici$season)
class(bici$season)

class(bici$mnth)
bici$mnth <- as.factor(bici$mnth)
class(bici$mnth)


class(bici$weekday)
bici$weekday <- as.factor(bici$weekday)
class(bici$weekday)

class(bici$workingday)
bici$workingday <- as.factor(bici$workingday)
class(bici$workingday)

class(bici$weathersit)
bici$weathersit <- as.factor(bici$weathersit)
class(bici$weathersit)

par(mfrow=c(2,2))
plot(x=bici$temp, y= bici$cnt, main='Temperatura vs conteo',
     xlab= 'Temperatura', ylab='Conteo')
plot(bici$atemp,y = bici$cnt, main='Temperatura de la sensación vs conteo',
     xlab= 'Temperatura de la sensación', ylab='Conteo')
plot(bici$hum,y = bici$cnt, main='Humedad vs conteo',
     xlab= 'Humedad', ylab='Conteo')
plot(bici$windspeed,y = bici$cnt, main='velocidad del viento vs conteo',
     xlab= 'Velocidad del viento', ylab='Conteo')
plot(bici$season,bici$cnt)
plot(bici$mnth, bici$cnt)
plot(bici$weekday,bici$cnt)
plot(bici$workingday,bici$cnt)
plot(bici$weathersit,bici$cnt)


###
max(bici$temp)
min(bici$temp)
mean(bici$temp)
sd(bici$temp)

max(bici$atemp)
min(bici$atemp)
mean(bici$atemp)
sd(bici$atemp)

max(bici$hum)
min(bici$hum)
mean(bici$hum)
sd(bici$hum)

max(bici$windspeed)
min(bici$windspeed)
mean(bici$windspeed)
sd(bici$windspeed)


primavera <- subset(bici12, season== "1")
verano <- subset(bici12, season== "2")
otoño <- subset(bici12, season== "3")
invierno <- subset(bici12, season== "4")
 sum(primavera$cnt)


library(dplyr)
bicitemw <- bici %>%
  group_by(weathersit) %>%
  summarise(
    temp.min = min(temp),
    temp.max = max(temp),
    temp.med = median(temp),
    temp.stdev = sd(temp),
    temp.mean = mean(temp), 
    cnt = sum(cnt))
bicitemw

bicitem <- bici %>%
  group_by(season) %>%
  summarise(
    temp.min = min(temp),
    temp.max = max(temp),
    temp.med = median(temp),
    temp.stdev = sd(temp),
    temp.mean = mean(temp), 
    cnt = sum(cnt))
bicitem

biciesta <- bici %>%
  group_by(season) %>%
  summarise(
    cnt.min = min(cnt),
    cnt.max = max(cnt),
    cnt.med = median(cnt),
    cnt.stdev = sd(cnt),
    cnt.mean = mean(cnt), 
    cnt = sum(cnt))
biciesta




require(usdm)
require(mctest)
x<-bici[,-c(10,1,2,3,4,5)]
y<-bici[,10]

mctest(x, y)
mctest(x, y, type="i")
omcdiag(x,y)
imcdiag(x,y)
mc.plot(x,y)

#No se utiliza atemp porque tiene problemas de multicolinealidad
x<-bici[,-c(10,1,2,3,4,5,7)]
y<-bici[,10]

mctest(x, y)
mctest(x, y, type="i")
omcdiag(x,y)
imcdiag(x,y)
mc.plot(x,y)

cor(x,y)

# Esto se hizo para mirar si la variable weekday
# mostraba cambio con respecto a los meses
bici12 <- subset(bici1, yr=="1") 
View(bici12)

class(bici12$yr)
bici12$yr <- as.factor(bici12$yr)
class(bici12$yr)

class(bici12$mnth)
bici12$mnth <- as.factor(bici12$mnth)
class(bici12$mnth)

class(bici12$weekday)
bici12$weekday <- as.factor(bici12$weekday)
class(bici12$weekday)


par(mfrow=c(2,2))

enero <- subset(bici12, mnth== "1", select = c(mnth, weekday,cnt))
View(enero)
plot(enero$weekday, enero$cnt)

febrero <- subset(bici12, mnth== "2", select = c(mnth, weekday,cnt))
View(febrero)
plot(febrero$weekday, febrero$cnt)

marzo <- subset(bici12, mnth== "3", select = c(mnth, weekday,cnt))
View(marzo)
plot(marzo$weekday, marzo$cnt)

abril <- subset(bici12, mnth== "4", select = c(mnth, weekday,cnt))
View(abril)
plot(abril$weekday, abril$cnt)

mayo <- subset(bici12, mnth== "5", select = c(mnth, weekday,cnt))
View(mayo)
plot(mayo$weekday, mayo$cnt)

junio <- subset(bici12, mnth== "6", select = c(mnth, weekday,cnt))
View(junio)
plot(junio$weekday, junio$cnt)

julio <- subset(bici12, mnth== "7", select = c(mnth, weekday,cnt))
View(julio)
plot(julio$weekday, julio$cnt)

agos <- subset(bici12, mnth== "8", select = c(mnth, weekday,cnt))
View(agos)
plot(agos$weekday, agos$cnt)

sept <- subset(bici12, mnth== "9", select = c(mnth, weekday,cnt))
View(sept)
plot(sept$weekday, sept$cnt)

oct <- subset(bici12, mnth== "10", select = c(mnth, weekday,cnt))
View(oct)
plot(oct$weekday, oct$cnt)
  
nov <- subset(bici12, mnth== "11", select = c(mnth, weekday,cnt))
View(nov)
plot(nov$weekday, nov$cnt)

dic <- subset(bici12, mnth== "12", select = c(mnth, weekday,cnt))
View(dic)
plot(dic$weekday, dic$cnt)

primavera <- subset(bici12, season== "1", select = c(season, weekday,cnt))
plot(primavera$weekday, primavera$cnt)

verano <- subset(bici12, season== "2", select = c(season, weekday,cnt))
plot(verano$weekday, verano$cnt)

otoño <- subset(bici12, season== "3", select = c(season, weekday,cnt))
plot(otoño$weekday, otoño$cnt)

invierno <- subset(bici12, season== "4", select = c(season, weekday,cnt))
plot(invierno$weekday, invierno$cnt)

###############################################

# distribución de la variable respuesta

library(shiny)
four.hist <- function(k, f, p,datos) {
  dt <- datos
  mod <- fitDist(dt[, p], type=f, k=k)
  par(mfrow=c(2, 2), bg='gray98')
  for(i in 1:4){
    denst <- density(dt[, p])
    res <- histDist(dt[, p], family=names(mod$fits)[i],
                    main='', 
                    ylab='Density',
                    xlab=p, las=1,
                    line.wd=3,
                    line.ty=1,
                    line.col='dodgerblue2',
                    ylim=c(0, (2 * max(denst$y))))
    gaic <- round(-2 * logLik(res) + k * length(res$parameters), 2)
    title(main=paste(i, ')', names(mod$fits)[i], 
                     'distribution with GAIC=', gaic),
          col.main='blue4')
    param <- c('mu', 'sigma', 'nu', 'tau') 
    np <- length(res$parameters)
    fun1 <- function(x) eval(parse(text=x))
    hat.param <- sapply(as.list(paste('res$', param[1:np], sep='')), fun1)
    hat.param <- round(hat.param, digits=2)
    txt <- paste('hat(', param[1:np], ')==', hat.param, sep='')
    txt <- paste(txt, collapse=', ')
    legend('topright', bty='n',
           legend=eval(parse(text=paste('expression(', txt, ')'))))
  }
}


View(bici)
aa <- hist(bici$cnt)
four.hist(k = 2, f = "realplus", p= 10, datos = bici ) #GB2 Y GG
four.hist(k = 2, f = "realAll", p= 10, datos = bici ) #SN2 Y SHASHo

#GB2 
#SHASHo u=8101.16 sigma=1937.63

colnames(bici)
require(gamlss)

mod1gb <- gamlss(cnt~season+workingday+weathersit+temp
               +hum+windspeed, data=bici, family = GB2,
               control=gamlss.control(n.cyc=1000))
summary(mod1gb)
#
mod1gg <- gamlss(cnt~season+workingday+weathersit+temp
               +hum+windspeed, data=bici, family = GG,
               control=gamlss.control(n.cyc=1000))
summary(mod1gg)
#

mod1sn <- gamlss(cnt~season+workingday+weathersit+temp
               +hum+windspeed, data=bici, family = SN2,
               control=gamlss.control(n.cyc=1000))
summary(mod1sn)
#

mod1 <- gamlss(cnt~season+workingday+weathersit+temp
               +hum+windspeed, data=bici, family = SHASHo,
               control=gamlss.control(n.cyc=1000))
summary(mod1)
#

mod2gb <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
               +hum+windspeed, data=bici, family = GB2,
               control=gamlss.control(n.cyc=1000))
summary(mod2gb)
#

mod2gg <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
               +hum+windspeed, data=bici, family = GG,
               control=gamlss.control(n.cyc=1000))
summary(mod2gg)
#

mod2sn <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
               +hum+windspeed, data=bici, family = SN2,
               control=gamlss.control(n.cyc=1000))
summary(mod2sn)
#

mod2 <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
                 +hum+windspeed, data=bici, family = SHASHo,
                 control=gamlss.control(n.cyc=1000))
summary(mod2)
#

mod3gb <- gamlss(cnt~season+workingday+weathersit+temp
               +hum+windspeed+(hum*windspeed), data=bici, family = GB2,
               control=gamlss.control(n.cyc=1000))
summary(mod3gb)
#

mod3gg <- gamlss(cnt~season+workingday+weathersit+temp
               +hum+windspeed+(hum*windspeed), data=bici, family = GG,
               control=gamlss.control(n.cyc=1000))
summary(mod3gg)
#

mod3sn <- gamlss(cnt~season+workingday+weathersit+temp+
               +hum+windspeed+(hum*windspeed), data=bici, family = SN2,
               control=gamlss.control(n.cyc=1000))
summary(mod3sn)
#
mod3 <- gamlss(cnt~season+workingday+weathersit+temp+
                   +hum+windspeed+(hum*windspeed), data=bici, family = SHASHo,
                 control=gamlss.control(n.cyc=1000))
summary(mod3)
#

mod4gb <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
               +hum+windspeed+(hum*windspeed), data=bici, family = GB2,
               control=gamlss.control(n.cyc=1000))
summary(mod4gb)
#

mod4gg <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
               +hum+(hum*windspeed)+windspeed, data=bici, family = GG,
               control=gamlss.control(n.cyc=1000))
summary(mod4gg)
#

mod4sn <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
                 +hum+(hum*windspeed)+windspeed, data=bici, family = SN2,
                 control=gamlss.control(n.cyc=1000))
summary(mod4sn)
#

mod4 <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
                 +hum+(hum*windspeed)+windspeed, data=bici, family = SHASHo,
                 control=gamlss.control(n.cyc=1000))
summary(mod4)
#

mod5gb <- gamlss(cnt~season+workingday+weathersit+temp
                +hum+windspeed+(temp*season), data=bici, family = GB2,
                control=gamlss.control(n.cyc=1000))
summary(mod5gb)
#

mod5gg <- gamlss(cnt~season+workingday+weathersit+temp
                  +hum+windspeed+(temp*season), data=bici, family = GG,
                 control=gamlss.control(n.cyc=1000))
summary(mod5gg)
#

mod5sn <- gamlss(cnt~season+workingday+weathersit+temp
                 +hum+windspeed+(temp*season), data=bici, family = SN2,
                 control=gamlss.control(n.cyc=1000))
summary(mod5sn)
#

mod5 <- gamlss(cnt~season+workingday+weathersit+temp
               +hum+windspeed+(temp*season), data=bici, family = SHASHo,
               control=gamlss.control(n.cyc=1000))
summary(mod5)
#

mod6gb <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
                +hum+windspeed+(temp*season), data=bici, family = GB2,
                control=gamlss.control(n.cyc=1000))
summary(mod6gb)
#

mod6gg <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
                 +hum+windspeed+(temp*season), data=bici, family = GG,
                 control=gamlss.control(n.cyc=1000))
summary(mod6gg)
#

mod6sn <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
                 +hum+windspeed+(temp*season), data=bici, family = SN2,
                 control=gamlss.control(n.cyc=1000))
summary(mod6sn)
#

mod6 <- gamlss(cnt~season+workingday+weathersit+temp+I(temp^2)
                 +hum+windspeed+(temp*season), data=bici, family = SHASHo,
                 control=gamlss.control(n.cyc=1000))
summary(mod6)
#

mod7 <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season  + 
                 windspeed +hum + workingday + (temp*season) + (temp*weathersit),
              data = bici, family = SHASHo,
              control=gamlss.control(n.cyc=1000))
summary(mod7)
#

mod9<- gamlss(cnt~season + workingday+ weathersit+ temp + I(temp^2) + 
                I(temp^3)+ (season*temp)+ 
                (hum* windspeed), data= bici,family = SHASHo,
              control = gamlss.control(n.cyc = 400))
summary(mod9)

mod10gg <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season  + 
                  windspeed +hum + workingday  + (temp*weathersit),
                data = bici, family = GG,
                control=gamlss.control(n.cyc=10000))
summary(mod10gg)
#

mod10sn <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season + 
                    windspeed +hum + workingday  + (temp*weathersit),
                  data = bici, family = SN2,
                  control=gamlss.control(n.cyc=10000))
summary(mod10sn)
#

mod10 <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season + 
                 windspeed +hum + workingday  + (temp*weathersit),
               data = bici, family = SHASHo,
               control=gamlss.control(n.cyc=10000))
summary(mod10)
#
mod11gb <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season + 
                  windspeed +hum  + (temp*weathersit),
                data = bici, family = GB2,
                control=gamlss.control(n.cyc=10000))
summary(mod11gb)
#
mod11gg <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season + 
                    windspeed +hum  + (temp*weathersit),
                  data = bici, family = GG,
                  control=gamlss.control(n.cyc=10000))
summary(mod11gg)
#
mod11sn <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season + 
                    windspeed +hum  + (temp*weathersit),
                  data = bici, family = SN2,
                  control=gamlss.control(n.cyc=10000))
summary(mod11sn)
#
mod11 <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3) + weathersit + season + 
                    windspeed +hum  + (temp*weathersit),
                  data = bici, family = SHASHo,
                  control=gamlss.control(n.cyc=10000))
summary(mod11)
#
mod12gb <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                  windspeed +hum,
                data = bici, family = GB2,
                control=gamlss.control(n.cyc=10000))
summary(mod12gb)
#
mod12gg <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                    windspeed +hum,
                  data = bici, family = GG,
                  control=gamlss.control(n.cyc=10000))
summary(mod12gg)
#
mod12sn <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                    windspeed +hum,
                  data = bici, family = GB2,
                  control=gamlss.control(n.cyc=10000))
summary(mod12sn)
#
mod12 <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                    windspeed +hum,
                  data = bici, family = SHASHo,
                  control=gamlss.control(n.cyc=10000))
summary(mod12)
#
mod13gb <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                  windspeed +hum+(temp*season),
                data = bici, family = GB2,
                control=gamlss.control(n.cyc=10000))
summary(mod13gb)
#
mod13gg <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                    windspeed +hum+(temp*season),
                  data = bici, family = GG,
                  control=gamlss.control(n.cyc=10000))
summary(mod13gg)
#
mod13sn <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                    windspeed +hum+(temp*season),
                  data = bici, family = SN2,
                  control=gamlss.control(n.cyc=10000))
summary(mod13sn)
#
mod13 <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                    windspeed +hum+(temp*season),
                  data = bici, family = SHASHo,
                  control=gamlss.control(n.cyc=10000))
summary(mod13)


GAIC(mod1gb,mod1gg,mod1sn,mod1,mod2gg,mod2sn,mod2,mod3gg,mod3sn,mod3,
     mod4gg,mod4sn,mod4,mod5gb,mod5gg,mod5sn,mod5,mod6gg,mod6sn,mod6,mod7,mod9,
     mod10,mod10gg,mod10sn,mod11gb,mod11gg,mod11sn,mod11,mod12gb,
     mod12gg,mod12sn,mod12,mod13gb,mod13gg,mod13sn,mod13)

aic <-GAIC(mod12,mod11,mod13gg,mod11sn,mod11gb)
aic

par(mfrow=c(2,2))
wp(mod12)
wp(mod13gg)
wp(mod11sn)
wp(mod11gb)

par(mfrow=c(1,2))
wp(mod12)
wp(mod11)

plot(mod12)

require(MASS)

horiz <- gamlss(cnt ~ temp +I(temp^2) +I(temp^3)  + season + 
                   windspeed +hum,
                 data = bici, family = SHASHo,
                 control=gamlss.control(n.cyc=10000))

modback <- stepAIC(object=horiz, trace=F, direction="backward", k=2)
summary(modback)

empty.model <- lm(cnt ~ 1, data=bici)
modforw <- stepAIC(empty.model, trace=T, direction="forward",
                   scope=list(upper=horiz,lower=empty.model))
summary(modforw)


#
summary(mod12)
aic

y <- bici$cnt
yajus1 <- fitted(mod12,newdata=bici)
cor(yajus1,y)

yajus2 <- fitted(mod11,newdata=bici)
cor(yajus2,y)

yajus3 <- fitted(mod11sn,newdata=bici)
cor(yajus3,y)

yajus4 <- fitted(mod11gb,newdata=bici)
cor(yajus4,y)

yajus5 <- fitted(mod13gg,newdata=bici)
cor(yajus5,y)

Rsq(mod12)
Rsq(mod11)
Rsq(mod11sn)
Rsq(mod11gb)
Rsq(mod13gg)


