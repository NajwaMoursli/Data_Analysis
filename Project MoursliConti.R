#Projet D'Analyse de Données: SpotifyAudio Features
#Najwa Moursli, Paolo Conti

######################################     DATASET      #################################################
data=read.csv("SpotifyAudioFeaturesApril2019.csv", header=T)
n = dim(data)[1] #130663 songs
p = dim(data)[2] #17 features
names(data)
head(data)

#Delete duplicates
data = data[!duplicated(data$track_id), ]

n = dim(data)[1] #130326 songs
p = dim(data)[2] #17 features
head(data)

data_mod = data

############################     EXPLORATIVE ANALYSIS OF THE VARIABLES      #############################

############################                  UNIVARIATE                    #############################

#acousticness
par(mar = rep(1, 4))
par(mfrow = c(2,1))
hist(data_mod[,4])
boxplot(data_mod[,4])


#danceability
par(mar = rep(2, 4))
par(mfrow = c(2,1))
hist(data_mod[,5])
boxplot(data_mod[,5])
data_mod = subset(data_mod, data_mod$danceability!=0) #remove noises


#duration
#I think it's more clear: transform ms in seconds
data_mod[,6] = data_mod[,6]/1000
colnames(data_mod)[6] = "duration"

par(mar = rep(2, 4))
par(mfrow = c(2,1))
hist(data_mod[,6])
boxplot(data_mod[,6]) #really confusing
range(data_mod[,6]) #from 3 seconds to 93 minutes!
#I consider significant for popularity the range between 2.30 minutes and 8 minutes
data_mod=subset(data_mod, data_mod$duration>2.5*60 & data_mod$duration<8*60)
range(data_mod[,6])
hist(data_mod[,6])
boxplot(data_mod[,6])


#energy
par(mar = rep(2, 4))
par(mfrow = c(2,1))
hist(data_mod[,7])
boxplot(data_mod[,7]) #really confusing
#We noticed the presence of songs which energy close to 1 or to 0 (from 0.998 to 1, and from 0 to 0.001)
#which are just recording and reproduction of water sounds.
#Since the goal of our analysis is to analyze the popularity, we decided to remove this data from the dataset
data_mod = subset(data_mod, data_mod$energy<0.998 & data_mod$energy>0.001)
#Further cleaning:
data_mod=data_mod[-grep("White Noise",data_mod$artist_name),] 
data_mod=data_mod[-grep("Rain Sounds",data_mod$artist_name),]
data_mod=data_mod[-grep("ASMR",data_mod$track_name),] 
data_mod=data_mod[-grep("ASMR",data_mod$artist_name),] 


#instrumentalness:
par(mar = rep(2, 4))
par(mfrow = c(2,1))
hist(data_mod[,8])
boxplot(data_mod[,8]) 

vocal = subset(data_mod, instrumentalness<0.01)
dim(vocal)[1]/dim(data_mod)[1]
hist(vocal[,8])
boxplot(vocal[,8]) 
#around 67% of the songs have an instrumentalness smaller than 1%:
#which is predictable cause instrumentalness goes to zero whenever there are lyrics

#according to Spotify description:    
# "Values above 0.5 are intended to represent instrumental tracks, 
# but confidence is higher as the value approaches 1.0"
instrumental = subset(data_mod, instrumentalness >0.5)
dim(instrumental)[1]/dim(data_mod)[1] #20% of songs considered instrumental
hist(instrumental[,8])
boxplot(instrumental[,8]) 


#key -> QUALITATIVE
barplot(table(factor(data_mod[,9])))
boxplot(data_mod[,9])


#liveness -> I transofrm liveness in qualitative/categorical
hist(data_mod$liveness)
boxplot(data_mod$liveness)
live = subset(data_mod, data_mod$liveness>0.8)
dim(live)[1]/dim(data_mod)[1] #only 1.5% of the songs are live


#loudness
hist(data_mod$loudness)
boxplot(data_mod$loudness)
range(data_mod$loudness)  #around -60 and 0 dB 


#mode -> categorical/qualitative
barplot(table(factor(data_mod$mode)))
boxplot(as.numeric(data_mod$mode))


#speechiness
hist(data_mod$speechiness)
boxplot(data_mod$speechiness)
speech = subset(data_mod, speechiness > 0.66)
dim(speech)[1]/dim(data_mod)[1]*100 #0.33 %


#tempo
hist(data_mod$tempo)
boxplot(data_mod$tempo)


#time_signature
hist(data_mod$time_signature)
boxplot(data_mod$time_signature)


#valence
hist(data_mod$valence)
boxplot(data_mod$valence)


#popularity
#I make it homogeneous as other variables -> range between 0 and 1
data_mod$popularity = data_mod$popularity / 100
hist(data_mod$popularity)
boxplot(data_mod$popularity)





############################       MULTI EXPLOARTIVE ANALYSIS       ####################################
data = data_mod


##################### LIBRARIES #####################
library(knitr)
knitr::opts_chunk$set(tidy=FALSE, 
                      fig.width=8,
                      fig.height=5,
                      fig.align='left',
                      warning=FALSE,
                      message=FALSE,
                      echo=TRUE)
options(width = 120)
library(ggplot2)
library(colorspace)
library(gridExtra)
library(RColorBrewer)
buylrd = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
           "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") 
myColRamp = colorRampPalette(c(buylrd))
library(corrplot)


par(mfrow=c(1,1))
par(mar = c(5.1, 4.1, 4.1, 2.1))
library(corrplot)
A=cor(data[, 4:17]) #matrix of corrolation
corrplot(A, method = "ellipse")
#strong positive corrolation between ENERGY and LOUDNESS
#positive corrolation between DANCEABILITY and LOUDNESS, DANCEABILITY and VALENCE, ...
#strong negative corrolation between ENERGY and ACOUSTICNESS, ACOUSTICNESS and LOUDNESS
#negative corrolation between INSTRUMENTALNESS and LOUDNESS


############# Energy VS loudness ##################
#par(mfrow=c(1,1))
#par(mar = c(5.1, 4.1, 4.1, 2.1))
smoothScatter(x=data$loudness,y = data$energy,
              colramp=myColRamp,
              main="ENERGY VS LOUDNESS",
              xlab="Loudness",
              ylab="Energy")
#Energy is a measure of intensity. Typically, energetic tracks feel fast, loud and noisy.
#For example, death metal has high energy, while a Bach prelude has a low value.
# Loudness is a measure of the overall loudness of a track.
# As we can see from the scatterplot, these two variables are pretty correlated and this is actually intuitive.


############# Energy VS Acousticness ##################
smoothScatter(x=data$acousticness,y = data$energy,
              colramp=myColRamp,
              main="ENERGY VS ACOUSTICNESS",
              xlab="Acousticness",
              ylab="Energy")



#Now let's look at two variables which are not corrolated,
#but they are signifcant to understand the meaning of these variables: INSTRUMENTALNESS VS SPEECHINESS

############# Instrumentalness VS speechiness ##################
smoothScatter(x=data$speechiness,y = data$instrumentalness,
              colramp=myColRamp,
              main="INSTRUMENTALNESS VS SPEECHINESS",
              xlab="Speechiness",
              ylab="Instrumentalness")
#From the scatterplot we can see there is a group of points with high instrumentalness,so 
# they are made only of instruments, so there are no lyrics and that's why the speechiness is low. 
#Then there's an other group of points which is less intuitive: 
#instrumentalness is low, so there are words in the tracks, but speechiness is low as well, 
# so the words are actually sung and not spoken, like in the typical pop songs. 
# Moving on the right we have more and more spoken words, as in the case of rap songs and, 
# finally, with high values of speechiness we have audio books, poems and interviews



#Our variable of interest is POPULARITY: 
#from the corrolation plot it doesn't seem like depending on single variables.
#Indeed from the following scatterplots we see that the cloud of points is spread along all the values of popularity. 

#Examples:
#Popularity VS danceability
smoothScatter(x=data$danceability,y = data$popularity,
              colramp=myColRamp,
              main="POPULARITY VS DANCEABILITY",
              xlab="Danceability",
              ylab="Popularity")

#Popularity VS energy
smoothScatter(x=data$energy,y = data$popularity,
              colramp=myColRamp,
              main="POPULARITY VS ENERGY",
              xlab="Energy",
              ylab="Popularity")







#####################################     APC / AFC      ################################################
n = dim(data)[1] #102156 chansons
#102156/130326 = 0.78 -> nous avons supprimés 22% des chansons


####################     FAMD     #########################
#FAMD: Analyse Factorielle des données mixtes (mélange ACP et ACM) avec
#Les variables quantitatives et qualitatives sont normalisées 
#au cours de l'analyse afin d'équilibrer l'influence de chaque ensemble de variables.

library(FactoMineR)
#data$mode = as.factor(data$mode)
#data$key = as.factor(data$key)
str(data)
    #data1=data[,4:17]
    #famd<-FAMD(data1, ncp = 5, sup.var = NULL, ind.sup = NULL, graph = TRUE) 
#Il s'avère que cette fonction ne fonctionne pas sur un trop grand jeu de données

#Strategies: 
#- Soit on rend tout qualitatif en regroupant les variables quantitatives sous forme de classe 
#      -> NON car nous avons beaucoup plus de variables quantitatives que qualitatives
# - Soit on effectue une AFC des variables qualitatives (ou ACM si plus de 2 vairables qualitatives)
# et on utilise les premières composantes principales comme vairables quantitatives à la place des variables qualitatives.
#      -> OUI, c'est cl'approche que l'on va tester

qualitative = data[,c(9,12)]
str(qualitative)#le types de variables est maintenant quantitatives
names(qualitative)
windows(height = 5, width = 7)
barplot(table(qualitative[,2],qualitative[,1]), xlab = "Key", ylab = "Nb Chansons", col = c('red','blue'), ylim = c(0,17000))
legend("topright", legend = c("Minor", "Major"),  fill = c('red','blue'))
dev.off()

qualitative.CA = data.frame( Minor = table(qualitative)[,1],Major = table(qualitative)[,2])#tableau de ces variables 
res.acp = PCA(qualitative.CA,scale.unit=TRUE, ncp=5, graph=T)
summary(res.acp)
print(res.acp)
#L'analyse a été conduite sur 1708 individus, décrits par 2 variables


#Call:
#  PCA(X = qualitative.CA, scale.unit = TRUE, ncp = 5, graph = T) 


#Eigenvalues
#Dim.1   Dim.2
#Variance               1.001   0.999
#% of var.             50.029  49.971
#Cumulative % of var.  50.029 100.000

#Individuals (the 10 first)
# "Dist    Dim.1    ctr   cos2    Dim.2    ctr   cos2  
# 0.0113 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0118 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0131 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0138 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0148 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0149 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0153 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0158 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.016  |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   0.0161 |  0.034 |  0.000  0.000  0.000 | -0.034  0.000  1.000 |
#   
#   Variables
# Dim.1    ctr   cos2    Dim.2    ctr   cos2  
# Minor  |  0.707 50.000  0.500 |  0.707 50.000  0.500 |
# Major  | -0.707 50.000  0.500 |  0.707 50.000  0.500 |"

 
library("factoextra")
eig.val <- get_eigenvalue(res.acp)
eig.val
#eigenvalue variance.percent cumulative.variance.percent
#Dim.1  1.0005858         50.02929                    50.02929
#Dim.2  0.9994142         49.97071                   100.00000

fviz_eig(res.acp, addlabels = TRUE, ylim = c(0, 50))


#ACP fonctionne avec ce package également

install.packages('ade4')
library(ade4)
res = dudi.coa(qualitative.CA)
#on selectionne un axe
summary(res)

"Class: coa dudi
Call: dudi.coa(df = qualitative.CA)

Total inertia: 0.07363

Eigenvalues:
  Ax1 
0.07363 

Projected inertia (%):
  Ax1 
100 

Cumulative projected inertia (%):
  Ax1 
100 "
res$tab
#variables quantitative

#Question: Peut on utiliser res$tab comme variable Quantitatives à la place Mode et Key ?
#Nous créeons donc une nouvelle varaible Quantitative basée sur les résultats précédents:
keymode = data$key #initialisation
for(i in 1:n){
  keymode[i] = res$tab[(data$key[i])+1,(data$mode[i])+1]
}
#keymode[1] == 0.1576083
#data$key[1] == 1, data$mode[1] == 1 --> res$tab[2,2] = 0.1576083



#Nouvelle base de données avec seulement des variables quantitatives:
new.data = data.frame(data[,-c(9,12)], keymode)
#write.table(new.data, file = 'data_quantitative.txt')

data = read.table('data_quantitative.txt')
n = dim(data)[1] # 102156
p = dim(data)[2] #16 et non 17 comme auparavant puisque l'on a fusionné deux variables de modalités en une variable quantitative 


###############       ACP      ################
#Maintenat que nous avons seulement des varaibles quantitatives nous pouvons procédé à une ACP
data.pca = data[,-c(1,2,3)] #suppression variables qualitative (that we keep to identify a song)
p = dim(data.pca)[2] #13

library(FactoMineR)
pca = PCA(data.pca) #SCALE = TRUE par defaut
plot(pca,choix="var",)    # graphe des variables
#plot(pca, choix = "var",axes=c(2, 3)) #deuxième et troisième composante
#plot(pca, choix = "var", axes = c(1,3)) #premiere et troisième composante
summary(pca)
#On remarque que la variable "keymode" n'est pas très significative

for(i in 1:5){
  x11(width = 60, height = 20)
  barplot(pca$svd$V[,i], names.arg = names(data.pca), cex.names = .65, main = paste(i,"° composante principale"))
}
graphics.off()
#Premier axe: Fort loudness, energy, danceability, Faible acousticness et instrumentalness
#...

#Choix du nombre de composante principale:
par(mfrow=c(1,1))
barplot(pca$eig[,1],main="Valeurs Propres",names.arg=1:nrow(pca$eig))
#On considère les 5 premières composantes pour notre étude: 60% de la variance sont expliqués
summary(pca)
scores = pca$ind$coord[,1:5]

#LOADINGS: pca$svd$V
#coordonnées: pca$ind$coord

#Premier axe: instrumental de la musique à gauche {instrumentalness, acousticness, duration} - chansons à texte à droite {energy, loudness,danceability,valence, speechiness}
#Second axe: la musique rock en haut {duration, energy, loudness,acousticness} - musique pop en bas ? {danceability, valence, acousticness} 
#Troisième axe :   {acousticness, energy, loudness, tempo},         {danceability,duration,instrumentalness,liveness,speechiness, time_signature}


#1-2
#Haut-Gauche: On suppose que cela correspond aux musiques CLASSIQUES: Forte instrumentalness, duration, acousticness et basse danceability, valence and speechiness
#Bas-Droite: On suppose que cela correspond au RAP: Forte speechiness, danceability, valence and Basse acousticness, instrumentalness
#Bas-Gauche: On suppose que cela correspond au JAZZ et musiques relaxantes: Forte acousticness, instrumental, low energy, loudness
#Haut-Droite: On suppose que cela correspond à la POP : Forte energy, loudness, basse instrumentalness and danceability

#1-3
# : high speechiness and keymode (?), low popularity

#2-3






####################################       CLUSTERS        #############################################
#setwd("D:/Università/Erasmus/Parigi/Corsi/Analyse des donnes/Project")
data = read.table('data_quantitative.txt')
data.quantitative=data[,-c(1,2,3,13)] #I remove time signature, because it's not meaningful at this point
names_variables = names(data.quantitative)
#I scale the variables in order to have a better understanding
data.quantitative = scale(data.quantitative)
medie = colMeans(data.quantitative)

# From PCA we have a suggestion to follow a division based on the following musical genres.
# But we can't just rely on our personal knowledge or intuition about musical genre, so we want to analyze
# more precisely the main characteristics of the musical genres.
# We performed a qualitative analysis in the following way: we looked for some of the most relevant artist 
# for each musical genre (who are present in our data set) and we analized the characteristic of this group 
# and we compared it with the average of all the data set and with the characteristic of the other musical genres.

# This analysis is also usefull, cause we hope that we can use it to recognize some musical genres
#in the clusters that we are going to create


##############     Classic Music       ##############
classic = ifelse(data$artist_name %in% c("Johann Sebastian Bach","Wolfgang Amadeus Mozart","Ludwig van Beethoven", "Claude Debussy", "FrÃ©dÃ©ric Chopin") ,1,0)
#I generate a binary variable to select some of the most famous artists of classic music
table(classic)

#We're comparing the classic subset with the all the dataset with respect to each original variables,
#by the means of an univariate explorative analysis (boxplot)
x11()
par(mfrow=c(3,4))
for(i in 1:4){
  for(j in 1:3){
    boxplot(data.quantitative[,(i-1)*3+j]~ classic, col = c('green','red'), ylab = names_variables[(i-1)*3+j],cex = 0.25)
  }
}

classic.medie = colMeans(subset(data.quantitative, classic==1))
classic.medie
medie

#Results: 
#Very high -> acousticness, INSTRUMENTALNESS, 
#high-> duration
#very low-> danceability, ENERGY, popularity, loudness
#low-> tempo, valence, speechiness, keymode
#same-> liveness

#Our intuition and assumption is valid: classic music has a very high acousticness, instrumentalness and
#longer duration with respect to the ohter genres. While it has a lower danceability, energy and loudness.
graphics.off()


##########      RAP MUSIC      ###########
rap = ifelse(data$artist_name %in% c("Eminem","Kanye West","Jay-Z", "Drake", "Lil Wayne", "50 Cent") ,1,0)
table(rap)
#only 94 songs

x11()
par(mfrow=c(3,4))
for(i in 1:4){
  for(j in 1:3){
    boxplot(data.quantitative[,(i-1)*3+j]~ rap, col = c('green','red'), ylab = names_variables[(i-1)*3+j],cex = 0.25)
  }
}

rap.medie = colMeans(subset(data.quantitative, rap==1))
rap.medie
medie

#Very high POPULARITY, SPEECHINESS, danceability
#high: LIVENESS, duration 
#very low INSTRUMENTALNESS, Acousticness
#low  
#same: liveness, keymode, valence, tempo, LOUDNESS, ENERGY
graphics.off()

#As we expected: we've a very high speechiness, danceability and popularity and a very low acousticness and instrumentalness


############       POP MUSIC      ##############
pop = ifelse(data$artist_name %in% c("Lady Gaga", "Rihanna", "Justin Bieber", "Michael Jackson", "Madonna", "Katy Perry", "Ariana Grande", "Coldplay", "Bruno Mars", "Maroon 5", "Beyoncé") ,1,0)
table(pop)
#only 115 songs of Lady Gaga, Rihanna, Justin Bieber, Micheal Jackson, Madonna, Beyoncé and so on. 
#It's kinda weird cause any of this artist has made hundreds of songs,... but this is our dataset.

x11()
par(mfrow=c(3,4))
for(i in 1:4){
  for(j in 1:3){
    boxplot(data.quantitative[,(i-1)*3+j]~ pop, col = c('green','red'), ylab = names_variables[(i-1)*3+j],cex = 0.25)
  }
}

pop.medie = colMeans(subset(data.quantitative, pop==1))
pop.medie
medie

#Very high-> POPULARITY
#high-> liveness, danceability, duration
#very low-> INSTRUMENTALNESS 
#low-> valence, Acousticness
#same: liveness, keymode, tempo, LOUDNESS, ENERGY, speechiness
graphics.off()


##############     HOUSE MUSIC      #################
house = ifelse(data$artist_name %in% c("Avicii", "Martin Garrix", "Nicky Romero", "David Guetta", "deadmau5", "Afrojack", "Kaskade", "Daft Punk", "Armin van Buuren") ,1,0)
table(house)
#330

x11()
par(mfrow=c(3,4))
for(i in 1:4){
  for(j in 1:3){
    boxplot(data.quantitative[,(i-1)*3+j]~ house, col = c('green','red'), ylab = names_variables[(i-1)*3+j],cex = 0.25)
  }
}

house.medie = colMeans(subset(data.quantitative, house==1))
house.medie
medie

#Very high POPULARITY, ENERGY
#high: liveness, danceability, loudness
#very low: Acousticness
#low: valence, valence
#same: liveness, tempo, speechiness, INSTRUMENTALNESS, duration
graphics.off()


###############     ROCK MUSIC      ##############
rock = ifelse(data$artist_name %in% c("Pink Floyd", "Guns N' Roses", "Nirvana", "The Rolling Stones", "Queen", "The Beatles", "Green Day", "Metallica") ,1,0)
table(rock)
#75

x11()
par(mfrow=c(3,4))
for(i in 1:4){
  for(j in 1:3){
    boxplot(data.quantitative[,(i-1)*3+j]~ rock, col = c('green','red'), ylab = names_variables[(i-1)*3+j], main =names(names_variables)[(i-1)*3+j], cex = 0.25)
  }
}

rock.medie = colMeans(subset(data.quantitative, rock==1))
rock.medie
medie

#Very high LIVENESS, ENERGY, popularity, DURATION
#low: danceability
#same: tempo, speechiness, INSTRUMENTALNESS, duration, ...
graphics.off()


################      JAZZ MUSIC      ################
jazz = ifelse(data$artist_name %in% c("Louis Armstrong", "John Coltrane", "Thelonious Coltrane", "Duke Ellington", "Ella Fitzgerald") ,1,0)
table(jazz)
#92

x11()
par(mfrow=c(3,4))
for(i in 1:4){
  for(j in 1:3){
    boxplot(data.quantitative[,(i-1)*3+j]~ jazz, col = c('green','red'), ylab = names(names_variables)[(i-1)*3+j],cex = 0.25)
  }
}

jazz.medie = colMeans(subset(data.quantitative, jazz==1))
jazz.medie
medie

#Very high: ACOUSTICNESS
#very low: energy, danceability
#low:  loudness, valence, popularity (boxplot more compact in the low part), tempo, instrumentalness (?)
#same: liveness, speechiness, duration
graphics.off()


###############      Reggaeton MUSIC       ###############
reggaeton = ifelse(data$artist_name %in% c("Daddy Yankee", "Nikcy Jam", "J Balvin", "Don Omar", "Bad Bunny", "Maluma", "Yandel", "Wisin") ,1,0)
table(reggaeton)
#90

x11()
par(mfrow=c(3,4))
for(i in 1:4){
  for(j in 1:3){
    boxplot(data.quantitative[,(i-1)*3+j]~ reggaeton, col = c('green','red'), ylab = names(names_variables)[(i-1)*3+j],cex = 0.25)
  }
}

reggaeton.medie = colMeans(subset(data.quantitative, reggaeton==1))
reggaeton.medie
medie

#Very high POPULARITY
#high:  VALENCE, liveness, danceability, speechiness, energy
#very low: Acousticness
#low: valence, instrumentalness
#same: liveness, tempo, duration, loudness
graphics.off()


############### Comparison among all the genres ############## 
medie.tot = rbind(classic.medie, rap.medie, pop.medie, house.medie, jazz.medie, reggaeton.medie, rock.medie)

#Comparisong among all the genres wrt to each single variable
x11(height = 18, width = 32)
par(mfrow=c(3,4))
for(i in 1:12){  barplot(medie.tot[,i], main = names_variables[i], names.arg = c('classic','rap','pop','house','jazz','reg', 'rock'), cex.names = 0.8, col = 1:7)}
graphics.off()

#Comparison among all the genres wrt to the principal components
library(FactoMineR)
pca = PCA(data.quantitative, graph = F)
#I rewrite the averages in the coordinates of the first 5 principal components
medie.tot.pca = medie.tot %*% pca$svd$V[,1:5]

x11(height = 18, width = 35)
par(mfrow=c(2,3))
for(i in 1:5){  barplot(medie.tot.pca[,i], main = paste(i,"° principal component"),names.arg = c('classic','rap','pop','house','jazz','reg','rock'), cex.names = 0.8, col = 1:7)}
plot.new()
legend("center", legend = c('classic','rap','pop','house','jazz','reg','rock'),  fill = 1:7,cex = 1.75)
dev.off()



#Let's perform a clusterization by K-MEANS
############################  K-MEANS on DATA NON SCALED #################################
library(mvtnorm)
library(rgl)
library(car)
library(MASS)

#Original data:
data.quantitative=data[,-c(1,2,3,13)] #I remove time signature, because it's not meaningfull at this point


#Since K-means provides a local optimum and it's really sensible with the initialization,
#Our strategy can be to provide the position of the initial centers of the clusters 
#instead of taking a random choice

result.k = kmeans(data.quantitative, centers = rbind(classic.medie, rap.medie, pop.medie, rap.medie, jazz.medie))
#The centers are not far enough!!!

#I think that the problem might be that even there are clear difference among the musical characteristics
#of the different musical genres, we have too many variables who have close values and this results in 
#having very close centers.

#For the moment we work with random initialization and let's try to understand the optimal number of clusters


############### Random centers K-Means -> Choosing the best number of clusters ############################
#ATTENTION!!!: the following for cycle takes a lot of time! (2-3 minutes)
inertie.intra <- rep(0,times=7)
for (k in 1:7){
  kmeans.result <- kmeans(scale(data.quantitative) ,centers=k,nstart=50)
  inertie.intra[k] <- kmeans.result$tot.withinss/kmeans.result$totss
  print(k) #just to realize how long it takes
}
# graphique
x11()
par(mfrow=c(1,1))
plot(1:7,inertie.intra,type="b",xlab="Nb. de groupes",ylab="% inertie intra")
dev.off()
#We don't see a clear "elbow", but we have a linear behaviour:
#We think it's the best to consider at least 4 or 5 clusters, 
#corresponding to 4 or 5 musical genres 


#let's work with 4 clusters
########################### K MEANS - RESULT ANALYSIS ##############################################
result.k = kmeans(data.quantitative, centers = 4, nstart = 100)

#Let's compare the characteristics of the centers of each clusters
#(value of the centers for each original variables)
x11(width = 24, height = 18)
par(mfrow=c(3,4))
for(i in 1:12){  barplot(result.k$centers[,i], main = names_variables[i], ylab = names_variables[i], col = 1:5)}
graphics.off()

#Looking at the most relevant variables, we would like to see a separation between clusters:
#we expect: 
#classic and Jazz songs to have high acousticness, instrumentalness, while the other genres low.
#Rap songs to have a high speechiness and so on...

#Focus on some main variables: Instrumentalness, Energy, Acousticness, Speechiness
x11()
par(mfrow=c(2,2))
plot(data.quantitative[,5], col=result.k$cluster, cex=0.5, main="Instrumentalness", ylab=names_variables[5])
plot(data.quantitative[,4], col=result.k$cluster, cex=0.5, main="Energy", ylab= names_variables[4])
plot(data.quantitative[,1], col=result.k$cluster, cex=0.5, main="Acousticness", ylab = names_variables[1])
plot(data.quantitative[,8], col=result.k$cluster, cex=0.5, main="Speechiness", ylab = names_variables[8])
#horrible!

#Let's give a look at all the variables
x11()
par(mfrow=c(3,4))
for(i in 1:12){
  plot(data.quantitative[,i], col=result.k$cluster, cex=0.5, main=names_variables[i], ylab = names_variables[i])
}

#We have used non standardized variables -> duration and tempo are on a bigger scale!!!
#so the clustering is only determined by these variables (duration and tempo)!!!
#Indeed:
x11(width = 130, height = 85)
par(mfrow=c(2,1))
barplot(colMeans(data.quantitative), cex.names = .5)
boxplot(data.quantitative)
graphics.off()

#We also have a mess in 3D
plot3d(data.quantitative[,5], data.quantitative[,4], data.quantitative[,8], col=result.k$cluster, cex=0.5, xlab = "Instrumentalness", ylab = "Energy", zlab = "Speechiness")
legend3d("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),  fill = 1:4, cex = 1)
#also horrible!


#Conclusion:
#The clusterization wrt to the original variables is not really meaningfull!
#We desired to see a separation at least with respect to the most relevant variables such as:
# Instrumentalness, energy, speechiness, acousticness.

#A possible solution is to perform K-MEANS on the SCALED VARIABLES


################################## K-MEANS ON SCALED DATA ##############################################
data.quantitative = scale(data.quantitative)

set.seed(1)
result.k = kmeans(data.quantitative, centers = 4, nstart = 100)

#Looking at the most relevant variables, we would like to see a separation between clusters.
#we expect: 
#classic and Jazz songs to have high acousticness, instrumentalness, while the other genres low.
#Rap songs to have a high speechiness and so on...
x11()
par(mfrow=c(2,2))
plot(data.quantitative[,5], col=result.k$cluster, cex=0.5, main="Instrumentalness", ylab=names_variables[5])
plot(data.quantitative[,4], col=result.k$cluster, cex=0.5, main="Energy", ylab= names_variables[4])
plot(data.quantitative[,1], col=result.k$cluster, cex=0.5, main="Acousticness", ylab = names_variables[1])
plot(data.quantitative[,8], col=result.k$cluster, cex=0.5, main="Speechiness", ylab = names_variables[8])
#So much better than before!

#Let's give a look at all the variables
x11()
par(mfrow=c(3,4))
for(i in 1:12){
  plot(data.quantitative[,i], col=result.k$cluster, cex=0.5, main=names_variables[i], ylab = names_variables[i])
}

plot3d(data.quantitative[,5], data.quantitative[,4], data.quantitative[,8], col=result.k$cluster, cex=0.5, xlab = "Instrumentalness", ylab = "Energy", zlab = "Speechiness")
legend3d("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),  fill = 1:4, cex = 1)

#In order to have a more clear division of the groups we can try to visualize the clusters
#with respect to the principal components rather than on the original variables.
#I perform a PCA and we take into account the first 5 principal components (we'll do a 3d plot
#so we'll plot only the first 3 principal components)

pca = PCA(data.quantitative, graph = F)
scores = pca$ind$coord[,1:5] #I consider the first 5 principal components (I reduce from dimension 13 to dimension 5)

#3D cluster wrt principal components:
plot3d(scores[,1], scores[,2], scores[,3], col=result.k$cluster, xlab = "1st PC", ylab = "2nd PC", zlab = "3rd PC")
legend3d("topright", legend = c("Cluster 1", "Cluster2", "Cluster3", "Cluster4"),  fill = 1:4, cex = 1)
#so much better!

#Visualization of the division in cluster for each principal component:
x11(width = 120, height = 60)
par(mfrow=c(2,3))
for(i in 1:5){
  plot(scores[,i], col=result.k$cluster, cex=0.5, main=paste(i,"Principal Component"))
}
plot.new()
legend("center", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),  fill = 1:4,cex = 1.5)
graphics.off()


################################## ANALYSIS OF THE CLUSTERS #############################################
#Let's compare the characteristic of the centers of each clusters
#in order to highilight their characteristic 

#Characteristics of the clusters wrt original variables
#(value of the centers of the clusters for each of the original variable)
x11(width = 24, height = 18)
par(mfrow=c(3,4))
for(i in 1:12){  barplot(result.k$centers[,i], main = names_variables[i], ylab = names_variables[i], col = 1:5)}


#Characteristics of the clusters wrt principal components
#(value of the centers of the clusters for each of the principal components)
#Notice: I transformed the centers in the coordinates of the principal components
x11(width = 120, height = 60)
par(mfrow=c(2,3))
result.centers.pca = result.k$centers %*% pca$svd$V[,1:5]
for(i in 1:5){
  barplot(result.centers.pca[,i], col=1:4, main=paste(i,"Principal Component"))
}
plot.new()
legend("center", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),  fill = 1:4,cex = 1.5)

#Characaterization of cluseters:
#(There can be some mismatch between the order of our list and the number of the cluster)

#1) We consider the cluster with very low first principal component, low second component, null third component.
# Looking at the correspondent values of variables:
# High: acousticness, instrumentalness. Low: danceability, energy, loudness.
# We expect to represent:  classic MUSIC
classic_index = which(result.k$size == min(result.k$size[-which.min(result.k$size)])) 
#tecnical way in order to identify always this cluster as classic music
cluster_classic = data[result.k$cluster==classic_index,]
cluster_classic_medie = result.k$centers[classic_index,]
#let's give a look at the artists in this cluster in order to understand if our assumption is valid
summary(cluster_classic$artist_name)
#Johann Sebastian Bach: 1887 songs      
#Wolfgang Amadeus Mozart: 992 songs         
#Ludwig van Beethoven: 459 songs
#Claude Debussy: 320 songs


#2) Cluster with very high second, positive first component and null third component
#High duration, energy, liveness. Low: danceability, acousticness -> HOUSE/ROCK
house_index = which(result.k$size == max(result.k$size[-which.max(result.k$size)]))  #tecnical code in order to identify always this cluster as house-rock music
result.k$size[house_index] #28064
cluster_house = data[result.k$cluster==house_index,]
cluster_house_medie = result.k$centers[house_index,]
summary(cluster_house$artist_name)
#Armin van Burren (DJ): 148 songs
#Above & Beyond (electronic music): 98 songs
#Image sounds (electronic dance): 91 songs
#R.E.M. (rock music): 72 songs  ROCK
#The Rolling stones: 27 songs ROCK


#3) Cluster with high 1st pc and low 2nd pc, very high 3rd pc
#High: Speechiness, danceability. Low: instrumentalness
rap_index = which.min(result.k$size)  #tecnical code in order to identify always this cluster as rap music
result.k$size[rap_index] #15291
cluster_rap = data[result.k$cluster==rap_index,]
cluster_rap_medie = result.k$centers[rap_index,]
summary(cluster_rap$artist_name)
#Waka Flocka Flame: 110 songs (rap)
#DJ Fuqua: 81 songs (rap)
#Chief Keef: 49 songs (rap)
#Lud Foe: 49 songs (rap)


#4) Cluster with positive first component, negative second component and very low third component
#High: valence, danceability, popularity  --> POP/reggaeton
pop_index = which.max(result.k$size)
result.k$size[pop_index ] #42178
cluster_pop = data[result.k$cluster==pop_index ,]
cluster_pop_medie = result.k$centers[pop_index ,]
summary(cluster_pop$artist_name)
#Los Cadetes de Linares 192 songs -> spanish/latin/reggaeton 
#Duo Libano : 136 songs ->spanish/latin/reggaeton 
#Sia: 31 songs
#Ariana Grande: 23 songs
#This dataset is full of latin/spanish music!!!
graphics.off()

#Projection on the plane of the first two principal components:
x11()
plot(scores[,1],scores[,2], col = result.k$cluster, xlab = "1st PC", ylab = "2nd PC", cex = 0.001, asp = 1, xlim = c(-5,5), ylim = c(-5,5))
plot(pca, choix = "var", axes = c(1,2))
points(scores[,1],scores[,2], col = result.k$cluster, xlab = "1st PC", ylab = "2nd PC", cex = 0.001)


#Finally let's verify if our assumptions on the cluster are valid
#by checking if the characteristics of the cluster correspond to the ones of the musical genres

#Just commands to order the cluster in the graph with the same order we used in the list before:
cluster_names =rep(0,4) 
cluster_names[which.max(result.k$size)] = "Pop/Reggaeton"
cluster_names[which.min(result.k$size)] = "Rap"
cluster_names[which(result.k$size == max(result.k$size[-which.max(result.k$size)]))] = "House/Rock"
cluster_names[which(result.k$size == min(result.k$size[-which.min(result.k$size)]))] = "Classic"


#1st cluster VS classic music:
classic.medie
cluster_classic_medie
x11(width = 130, height = 90)
par(mfrow=c(2,1))
barplot(classic.medie, col = 1:length(classic.medie),cex.names = 0.6, main = "Most relevant classic artists")
barplot(cluster_classic_medie, col = 1:length(classic.medie), cex.names = 0.6, main = "Empirical classic cluster")
graphics.off()
#All the variables of the data in the cluster correspond to the variables of the most relevant 
#artists in classic music

#2nd cluster VS house/rock music:
house_rock.medie = colMeans(subset(data.quantitative, house==1 | rock==1))
house_rock.medie
cluster_house_medie
x11(width = 130, height = 90)
par(mfrow=c(2,1))
barplot(house_rock.medie, col = 1:length(rock.medie), cex.names = 0.6, main = "Most relevant House/Rock artists")
barplot(cluster_house_medie, col = 1:length(cluster_house_medie), cex.names = 0.6, main =  "Empirical House/Rock cluster")
graphics.off()
#Again, we have good corrispondence between our "theoretical clusters" (namely the collection of
#house and rock artists) with the cluster we've found. 
#Except of popularity (see later)

#3rd cluster VS rap music:
rap.medie
cluster_rap_medie
x11(width = 130, height = 90)
par(mfrow=c(2,1))
barplot(rap.medie, col = 1:length(rap.medie), cex.names = 0.6, main = "Most relevant Rap artists")
barplot(cluster_rap_medie, col = 1:length(cluster_rap_medie), cex.names = 0.6, main = "Empirical Rap cluster")
graphics.off()
#Again, it's very good

#4th cluster VS POP/reggaeton music:
pop_reggaeton = (pop==1 | reggaeton ==1)
pop_reggaeton.medie = colMeans(subset(data.quantitative, pop_reggaeton))
cluster_pop_medie
x11(width = 130, height = 90)
par(mfrow=c(2, 1))
barplot(reggaeton.medie, col = 1:length(pop.medie), cex.names = 0.6, main = "Most relevant Pop/Reggaeton artists")
barplot(cluster_pop_medie, col = 1:length(cluster_pop_medie), cex.names = 0.6, main = "Empirical Pop/Reggaeton cluster")
graphics.off()
#It's good but not amazing. But we have to take into account that the our group of chosen songs
#from pop and reggaeton music is made of just 205 songs, while the corresponding cluster is made of
# 42.178 songs!! It's normal having this kind of differences

#Popularity:
#In all the groups we can see that the "theoretical popularity" is higher than the
#popularity we found in the cluster: but this is due to the fact, that the artists used 
#as examples to find out the characteristic of a specific music genres are the most relevant,
#therefore they are also very famous!


#Pie-chart of the size of the clusters:
x11()
par(mfrow = c(1,1))
pie(result.k$size, col = 1:4, label = cluster_names, main = "Cluster sizes")
graphics.off()


# Since we have divided the data into the 4 music genres, let's introduce in the dataset a variable to record this:
cluster = result.k$cluster
cluster = ifelse(cluster==which(result.k$size == min(result.k$size[-which.min(result.k$size)])), 'classic', cluster)
cluster = ifelse(cluster==which(result.k$size == max(result.k$size[-which.max(result.k$size)])), 'House_Rock',cluster)
cluster = ifelse(cluster==which.min(result.k$size), 'Rap', cluster)
cluster = ifelse(cluster==which.max(result.k$size), 'Pop_Reggaeton',cluster)
data = data.frame(data, cluster)
#write.table(data, file = 'data_cluster.txt')







#################################       MODEL         ##############################################
#setwd("D:/Università/Erasmus/Parigi/Corsi/Analyse des donnes/Project")
#data = read.table("data_cluster.txt", header = T)
head(data)

# Idea:
# We're afraid that the only musical characteristic are not enought to explain popularity.
# Especially we suppose there is a popoularity related to the ARTISIT itslef which clearly influences
# the popualrity of the individual song.
# The popularity index in this dataset is a measure of the number of times a songs has been listened.
# But if we think about the case where two artists (one very famous and one not famous) produce
# the exact same songs: we expect an higher index of popularity for the song whose artist is famous
# than the popularity index of the songs whose arist is not famous,
# EVEN THOUGH ALL the other caracteristics (ALL THE OTHER VARIABLES) are exactly the same.
# We would like to construct a model also in order to perform PREDICTION, namely to predict
# the popularity of a new song. Therefore we must take into account the popularity of the artist.

# So let's construct an extra variable which is supposed to represent the POPULARITY OF AN ARTIST:
# We build it for every artist by taking the average of the popularity index among all the songs from the artist

artist_averages=aggregate(data$popularity, by=list(artist=data$artist_name), FUN=mean)

#Carefull!! The following code takes around 15/20 minutes
#(We added the file: 'data_final.txt' in which we added the new variable, so you can go on with that
#dataset without wasting time in running this part of the code)

artist_popularity = rep(0, dim(data)[1])
data = data.frame(data, artist_popularity)
for(i in 1:length(artist_averages$artist)){
  data[which(data$artist_name == artist_averages$artist[i]),18] = artist_averages$x[i] #18: column corresponding to "artist_popularity"!
}

#Once again, we save this dataset with this new variable measuring the artist popularity:

#write.table(data, file = 'data_final.txt')


################################# MODEL ###########################################################
rm(list=ls())
data = read.table('data_final.txt', header = T)
library(MASS)
library(car)

# At this moment we are ready to construct our model. We would like to construct a model who takes into
# account the genre of the song we have esimated previously.
# In order to do so, we construct binary variables which allow us to split the contribution of each of
# the variable in our dataset into the 4 different musical genres
dummy_houserock = ifelse(data$cluster == "House_Rock",1,0)
dummy_rap = ifelse(data$cluster == "Rap",1,0)
dummy_popreggaeton = ifelse(data$cluster == "Pop_Reggaeton",1,0)


# Since we have considered 4 musical genres -> we build 3 "dummy" variables
# in order to take into account the membership to a group
# 1 0 0  : house_rock
# 0 1 0  : rap
# 0 0 1  : pop_reggaeton
# 0 0 0  : classic
# These variables are gonna substitute the labels defined by "cluster"

#I insert these variables in the data frame
data_quantitative = data.frame(data, houserock = dummy_houserock, rap = dummy_rap, popreggaeton = dummy_popreggaeton )
data_quantitative = data_quantitative[,-c(1,2,3,17)] #I remove qualitative variables (and variable "cluster")
n = dim(data_quantitative)[1]
p = dim(data_quantitative)[2]
head(data_quantitative)

# Now we fit a linear model, taking into account all the quantitative variables
# plus the dummy variables and their interaction with original variables in order to find a model
# which takes into account the membership to musical genres:

#Before doing this we extract 300 songs from the dataset:
#we're going to use them to test the prediction of the model we are going to build

#Test songs:
set.seed(3)
test = sample(1:dim(data_quantitative)[1],300) #I extract 300 songs
data_test = data_quantitative[test,]

#Songs used to build the model:
data_model = data_quantitative[-test,]

attach(data_model)

model0 = lm(popularity ~ houserock + rap + popreggaeton +
              acousticness + acousticness:houserock  + acousticness:rap + acousticness:popreggaeton +
              danceability + danceability:houserock  + danceability:rap + danceability:popreggaeton +
              duration + duration:houserock  + duration:rap + duration:popreggaeton +
              energy + energy:houserock + energy:rap + energy:popreggaeton + 
              instrumentalness + instrumentalness:houserock + instrumentalness:rap + instrumentalness:popreggaeton + 
              liveness + liveness:houserock + liveness:rap + liveness:popreggaeton +
              loudness + loudness:houserock + loudness:rap + loudness:popreggaeton +
              speechiness + speechiness:houserock + speechiness:rap + speechiness:popreggaeton + 
              tempo + tempo:houserock + tempo:rap + tempo:popreggaeton + 
              time_signature + time_signature:houserock + time_signature:rap + time_signature:popreggaeton + 
              valence + valence:houserock + valence:rap + valence:popreggaeton + 
              keymode + keymode:houserock + keymode:rap + keymode:popreggaeton + 
              artist_popularity #+ artist_popularity:houserock + artist_popularity:rap + artist_popularity:popreggaeton
)

summary(model0)

#Plot of the coefficients
x11()
par(mfrow=c(1,1))
barplot(model0$coefficients, col = 'lightblue')
#From the full model (without checking the significance of any variable), we can clearly see that
#the popularity of a song is mainly described from the popularity of the artist. Indeed
sort(model0$coefficients,decreasing = TRUE)[1]
#artist_popularity:  0.9836193 

#let's look at the remaining coefficients (we remove artist popularity from the graph)
barplot(model0$coefficients[-17])
#They are all very small (max in absolute value is around 0.1). Indeed:
sort(model0$coefficients[-17],decreasing = TRUE)[1]

graphics.off()


######################## Testing the significance of the coefficients ##############################
#In this section we perform a test on the significance of the coefficients of the model.
#In the model we built, we can test the significance of a variable on each of the musical genres,
#(for instance, we can test significance of acousticness on RAP songs, instrumentalness on CLASSIC, etc.)
#But at this moment, we just look at the significance of a variable on ALL the musical genres 
#(for instance, if the variable "danceability" has an influence on ALL genres: CLASSIC, HOUSE/ROCK, RAP, POP/REGGAETON)

#The following code is a bit complicated and tricky, but actually
#I am just collecting the p-values of the test of significance of a variable simultaneously
#on all the musical genres (performed by the command "linearyHypothesis") 

n_beta = length(model0$coefficients) #number of coefficients
beta_zero = diag(n_beta) 
p_values = rep(0,(n_beta-1)/4 + 1) #vector which will store the p-values of the simultaneous significance test
nomi = c("Intercept")

p_values[1] = linearHypothesis(model0, beta_zero[c(1,2,3,4),], rep(0, 4))$Pr[2] #p-values on intercept 
for(i in 1:((n_beta-1)/4 -1)){
  p_values[i+1] = linearHypothesis(model0, beta_zero[c(i+4,17+3*i-2,18+3*i-2,19+3*i-2),], rep(0, 4))$Pr[2] 
  nomi = c(nomi,  names(model0$coefficients)[i+4])
  print(names(model0$coefficients)[c(i+4,17+3*i-2,18+3*i-2,19+3*i-2)])
}
p_values[(n_beta-1)/4 + 1] = linearHypothesis(model0, beta_zero[c(17),], 0,)$Pr[2]
nomi = c(nomi, "artist_popularity")


#P-values for test of significance of each variable on all the groups:
x11(width = 150, height = 90)
plot(p_values)
lines(p_values, lty = 3)
abline(h=0.01, lty = 3, col = 'red')
#We refuse all the variables whose test p-values is higher than 0.01 
#(basically we are perform a test of simultaneous significance at level 0.1%)
#p-value 0.01 is actually not that big to refuse the significance, but since we've so many variables
#We just wanna keep the most significant ones
nomi[which(p_values>0.01)]
#"duration"         "instrumentalness" "time_signature"   "keymode"       

#So variables "duration", "instrumentalness", "time_signature", "keymode"
#are not particularly significant in our model, so we can discard them, and build a reduced model.

####################################### MODEL REDUCTION ##########################################
model1 = lm(popularity ~ houserock + rap + popreggaeton +
              acousticness + acousticness:houserock  + acousticness:rap + acousticness:popreggaeton +
              danceability + danceability:houserock  + danceability:rap + danceability:popreggaeton +
              #              duration + duration:houserock  + duration:rap + duration:popreggaeton +
              energy + energy:houserock + energy:rap + energy:popreggaeton + 
              #              instrumentalness + instrumentalness:houserock + instrumentalness:rap + instrumentalness:popreggaeton + 
              liveness + liveness:houserock + liveness:rap + liveness:popreggaeton +
              loudness + loudness:houserock + loudness:rap + loudness:popreggaeton +
              speechiness + speechiness:houserock + speechiness:rap + speechiness:popreggaeton + 
              tempo + tempo:houserock + tempo:rap + tempo:popreggaeton + 
              #              time_signature + time_signature:houserock + time_signature:rap + time_signature:popreggaeton + 
              valence + valence:houserock + valence:rap + valence:popreggaeton + 
              #              keymode + keymode:houserock + keymode:rap + keymode:popreggaeton + 
              artist_popularity #+ artist_popularity:houserock + artist_popularity:rap + artist_popularity:popreggaeton
)

summary(model1)

#plot of the coefficients
x11()
par(mfrow=c(1,1))
barplot(model1$coefficients, col = 'lightblue')
#From the full model (without checking the significance of any variable), we can clearly see that
#the popularity of a song is mainly described from the popularity of the artist

#let's look at the remaining coefficients (we remove artist popularity from the graph)
barplot(model1$coefficients[-13])
graphics.off()

######################### As before:
n_beta = length(model1$coefficients)
beta_zero = diag(n_beta)
p_values = rep(0,(n_beta-1)/4 + 1)
nomi = c("Intercept")

p_values[1] = linearHypothesis(model1, beta_zero[c(1,2,3,4),], rep(0, 4))$Pr[2] 
for(i in 1:((n_beta-1)/4 -1)){
  p_values[i+1] = linearHypothesis(model1, beta_zero[c(i+4,13+3*i-2,14+3*i-2,15+3*i-2),], rep(0, 4))$Pr[2] 
  nomi = c(nomi,  names(model1$coefficients)[i+4])
  print(names(model1$coefficients)[c(i+4,13+3*i-2,14+3*i-2,15+3*i-2)])
}
p_values[(n_beta-1)/4 + 1] = linearHypothesis(model1, beta_zero[c(13),], 0,)$Pr[2]
nomi = c(nomi, "artist_popularity")

#P-values for test of significance of each variable:
x11(width = 150, height = 90)
plot(p_values)
lines(p_values, lty = 3)
abline(h=0.01, lty = 3, col = 'red')
#Not significant variables are:
nomi[which(p_values>0.01)]
#"tempo"

#We can proceed with a further reduction removing also the variable "tempo"


######################################### FINAL MODEL #################################################
model1 = lm(popularity ~ houserock + rap + popreggaeton +
              acousticness + acousticness:houserock  + acousticness:rap + acousticness:popreggaeton +
              danceability + danceability:houserock  + danceability:rap + danceability:popreggaeton +
              #              duration + duration:houserock  + duration:rap + duration:popreggaeton +
              energy + energy:houserock + energy:rap + energy:popreggaeton + 
              #              instrumentalness + instrumentalness:houserock + instrumentalness:rap + instrumentalness:popreggaeton + 
              liveness + liveness:houserock + liveness:rap + liveness:popreggaeton +
              loudness + loudness:houserock + loudness:rap + loudness:popreggaeton +
              speechiness + speechiness:houserock + speechiness:rap + speechiness:popreggaeton + 
              #              tempo + tempo:houserock + tempo:rap + tempo:popreggaeton + 
              #              time_signature + time_signature:houserock + time_signature:rap + time_signature:popreggaeton + 
              valence + valence:houserock + valence:rap + valence:popreggaeton + 
              #              keymode + keymode:houserock + keymode:rap + keymode:popreggaeton + 
              artist_popularity #+ artist_popularity:houserock + artist_popularity:rap + artist_popularity:popreggaeton
)

summary(model1)

#Plot of the coefficients:
x11()
par(mfrow=c(1,1))
barplot(model1$coefficients, col = 'lightblue')
#From the full model (without checking the significance of any variable), we can clearly see that
#the popularity of a song is mainly described from the popularity of the artist

#let's look at the remaining coefficients (we remove artist popularity from the graph)
barplot(model1$coefficients[-12], col = 'lightblue')
graphics.off()



#This time, for each variable of the model, we draw a barplot representing the coefficients in the model
#For each variable we have for coefficients: 
#the 4 contributions to popularity (one for each of the musical genres)!

n_beta = length(model1$coefficients)
beta_zero = diag(n_beta)
p_values = rep(0,(n_beta-1)/4 + 1)
nomi = c("Intercept")

x11()
par(mfrow = c(3,3))
p_values[1] = linearHypothesis(model1, beta_zero[c(1,2,3,4),], rep(0, 4))$Pr[2] 
barplot(model1$coefficients[c(1,2,3,4)], col = 1:4, names.arg = c("Classic","House/Rock","Rap","Pop/Reggaeton"), main = names(model1$coefficients)[1], sub = paste("p_value = ",p_values[1]))
for(i in 1:((n_beta-1)/4 -1)){
  p_values[i+1] = linearHypothesis(model1, beta_zero[c(i+4,12+3*i-2,13+3*i-2,14+3*i-2),], rep(0, 4))$Pr[2] 
  nomi = c(nomi,  names(model1$coefficients)[i+4])
  print(names(model1$coefficients)[c(i+4,12+3*i-2,13+3*i-2,14+3*i-2)])
  barplot(model1$coefficients[c(i+4,12+3*i-2,13+3*i-2,14+3*i-2)], col = 1:4, names.arg = c("Classic","House/Rock","Rap","Pop/Reggaeton"), main = names(model1$coefficients)[i+4], sub = paste("p_value = ",p_values[i+1]))
}
p_values[(n_beta-1)/4 + 1] = linearHypothesis(model1, beta_zero[c(12),], 0,)$Pr[2]
nomi = c(nomi, "artist_popularity")
plot.new()
legend('center', legend = c("Classic","House/Rock","Rap","Pop/Reggaeton"), fill = 1:4)

#At this point, we can give an interpretation of the coefficients of the model
#always keeping in our mind that their contribution to popularity is negligible with respect to the contribution
#of the variable "artist popularity"

#INTERCEPT: very important! The intercept represents simply the appartenance to a musical group:
#High values for POP/REGGAETON, then in decreasing order RAP and HOUSE/ROCK.
#While we have a negative intercept for CLASSIC music.
#The only fact that a song is a POP/REGGAETON is making it already more popular than a CLASSIC song!
#And this makes absolutely sense!


#Analysis of the other variables:
#VALENCE: in all the musical genres except RAP -> valence has a negative contribution to popularity:
#          negative songs are more likely to be popular. (in RAP music, happy songs are more popular)
#SPEECHINESS, LOUDNESS, LIVENESS: high positive contribution to popularity in POP, RAP, HOUSE/ROCK (with decreasing values).
#             negative contribution in Classic songs
#ENERGY: positive contribution in ROCK/HOUSE, negative in others (especially RAP and POP)
#DANCEABILITY: positive contribution in CLASSIC and RAP music, while NEGATIVE in HOUSE and POP
#ACOUSTICNESS: POSITIVE contribution in CLASSIC music, while NEGATIVE in all the other musical genres

graphics.off()

#P-values for test of significance of each variable:
x11(width = 150, height = 90)
plot(p_values)
lines(p_values, lty = 3)
abline(h=0.01, lty = 3, col = 'red')
#Not significant variables are:
nomi[which(p_values>0.01)]

#All variables are signficant



###########################################################
#Last step of reduction:
#The variables left are all signficant simultaneously on the all musical genres.
#Now we want to check if we can neglect their contribution to some specific musical genre
model2 = step(model1)
summary(model2)

length(model1$coefficients) #33
length(model2$coefficients) #30
#We actually lose just 3 coefficients: houserock:acousticness, houserock:speechiness,rap:valence
#Which means: the contribution to popularity from acousticness and speechiness is the same in CLASSIC and HOUSEROCK songs,
#the contribution to popularity from valence is the same in CLASSIC and RAP

#We don't consider this reduction very significant, therefore, I keep the model1! 


################################# FINAL MODEL ########################################

# popularity ~ acousticness + danceability + energy + liveness + speechiness + valence + artist_popularity


############################# TESTING MODEL HYPOTHESYS #####################################
#We perform the test of the model hypothesys on the reduced model, namely:
# popularity ~ acousticness + danceability + energy + liveness + speechiness + valence + artist_popularity
#because it wouldn't make sense to test hypohtesys at every step of the reduction

x11()
par(mfrow=c(2,2))
plot(model1)
#The hypothesys of our model are not strong:
#in particular we don't have gaussianity and homoschedasticity (not homogeneous distribution of residuals around zero)
graphics.off()


#We also want to test if there is collinearity between the variables
#(in order to do so, we don't consider the subdivision of the variables into groups, otherwise it would be obvious to spot collinearity)
model = lm(popularity ~acousticness + danceability + energy + liveness + speechiness + valence + artist_popularity)

par(mfrow=c(1,1))
vif(model)
barplot(vif(model), col = rainbow(7), ylim = c(0,2.5))
#No signficant corrolation with the final variables of the model -> good

detach(data_model)



########################################## TESTING OUR MODEL ###########################################
#data_test
data_test = data_test[order(data_test$popularity),] 
#I sort the songs in ascending popularity (just for a clean graphical visualization of popularity)

#Prediction with our final model:
predicted_pop = predict(model1, data_test[,])
#Conf <- predict(model1, data_test[,], interval='confidence', level= 0.99)

#I compare the REAL VALUES of POPULARITY with the PREDICTED VALUES of POPULARITY
true_predicted = data.frame(true = data_test$popularity, predicted = predicted_pop)
head(true_predicted)

x11()
plot(1:dim(true_predicted)[1],true_predicted[,1], type = 'l')
points(1:dim(true_predicted)[1],true_predicted[,1], pch = 19, cex =.3)
lines(1:dim(true_predicted)[1],true_predicted[,2], col = 'red')
points(1:dim(true_predicted)[1],true_predicted[,2], col = 'red', pch = 19, cex = 0.3)
legend('topleft', legend = c("Real popularity", "Predicted popularity"), fill = c('black','red'))
#From the graph, we seem to notice that:
# - For small values of the actual popularity, the predcited values of our model are generally higher than the real ones
# - For high values of the actual popularity, the predicted values of our model are generally smaller than the real ones

#Let's highilight this, by dividing the test-dataset in 3 groups: low popularity, medium popularity, high popularity
#(each of the groups is made of 100 songs)

x11(width = 200, height = 150)
par(mfrow=c(1,3))
for(i in 1:3){
  plot((1:100)+(i-1)*100,true_predicted[(1:100)+(i-1)*100,1], type = 'l', ylim = c(0,max(true_predicted)))
  points((1:100)+(i-1)*100,true_predicted[(1:100)+(i-1)*100,1], pch = 19, cex =.3)
  lines((1:100)+(i-1)*100,true_predicted[(1:100)+(i-1)*100,2], col = i+1)
  points((1:100)+(i-1)*100,true_predicted[(1:100)+(i-1)*100,2], col = i+1, pch = 19, cex = 0.3)
}
#Nothing special: we just can see a bit better what we highlighted before
graphics.off()  

#Now let's draw a graph of the difference between the PREDICTED and the REAL popularity of the songs:
#(let's keep the subdivision in the tre groups)
x11()
dist = true_predicted[,2]-true_predicted[,1]
plot(dist)
points(1:100, dist[1:100], col = 'red', pch = 19)
points(101:200,dist[101:200], col = 'green', pch = 19)
points(201:300,dist[201:300], col = 'blue', pch = 19)
legend('topleft',legend = c("Low popularity", "Medium popularity", "High popularity"), fill = c('red','green', 'blue'))
abline(h=0, lwd = 2)
abline(h=0.08, lwd = 1.5, lty = 2)
abline(h=-0.08, lwd = 1.5, lty = 2)
#Once again, we can see that our prediction is general greater than reality in case of songs with real low popularity
#and our prediction is smaller than reality in case of songs with high popularity.

#Why?
#The reason why is because our model basically relies only on the popularity of the artist,
#which is computed by taking the AVERAGE of all the songs of an artist.
#Therefore all the predicted tends to an average popularity, so our prediction is not really accurate
#in case of very low popularity or very high popularity.

#How many songs has a difference in popularity less than 8% ? (songs between the dashed lines)
length(subset(dist, abs(dist)<0.08)) #242
#242/300 -> > 80%

#mean error and variance:
mean(abs(dist)) #0.0481
var(abs(dist))  #0.0032

graphics.off()



#Extra:
##################################################################################################
#So we found out that popularity is mainly related to the popularity of the artist and this makes sense
#but our goal was to understand what are the reasons to determine popularity not related to the popularity
#of the artist. So we can "fix this variable", by considering for instance a SINGLE ARTIST and trying to
#understand what makes their songs popular! -> example Johann Sebastian Bach

####################################### BACH ####################################################
bach = subset(data_quantitative, data$artist_name == "Johann Sebastian Bach")
bach = data.frame(scale(bach[,-(14:17)]))
head(bach)
mod_bach = lm(popularity ~ acousticness + danceability + energy + liveness + speechiness + valence, data = bach )
summary(mod_bach)
mod_bach1 = step(mod_bach)
summary(mod_bach1)
x11()
par(mfrow=c(2,1))
barplot(mod_bach1$coefficients, col = 1:length(mod_bach1$coefficients), cex.names = 0.5)
abline(h=0)
barplot(model1$coefficients[c(1,6,7,8,10)], col = 1:length(mod_bach1$coefficients), cex.names = 0.5)
abline(h=0)
graphics.off()
#our model gave similar results as the specific model built onlt on Bach's song,
#so it's a satisfactory result

