#####Install readxl package to read excel files#####
install.packages("readxl")
library(readxl)
#####Find the path to the excel file#######
file.choose()

#########Data analysis of entropy data Nasopharyngeal samples##################
#########Read excel file##########
read_excel("/Users/victorrojas/Desktop/Publicación Covid-2021 /Resultados /2 oronasofharinx/2 shanon entropy data cut oronasofarinx.xlsx")
##########Name the excel file###############
oronasofaringeal2.1<-read_excel("/Users/victorrojas/Desktop/Publicación Covid-2021 /Resultados /2 oronasofharinx/2 shanon entropy data cut oronasofarinx.xlsx")
#########View excel file##########
View(oronasofaringeal2.1)
######Find the path to the excel file#############
file.choose()
#######Subset function to extract data with entropy greater than or equal to 0.5#########
subset(x=oronasofaringeal2.1,subset = Entropy>=0.5)
######Assign subset variable name##########
nasoentropy2<-subset(x=oronasofaringeal2.1,subset = Entropy>=0.5)
########Call vector##########
nasoentropy2
#########Plot nasoentropy2##########
plot(nasoentropy2,type="h")
text(nasoentropy2,
     labels = row.names(nasoentropy2),
     cex = 0.6, pos = 3, col = "black")

########Data analysis of entropy data Sputum samples########
#######Find the path to the excel file###########
file.choose()
########Read excel file##########
read_excel("/Users/victorrojas/Desktop/Publicación Covid-2021 /Resultados /2 lung/2 shanon entropy data cut lung .xlsx")
##########Name the excel file###############
lung2<-read_excel("/Users/victorrojas/Desktop/Publicación Covid-2021 /Resultados /2 lung/2 shanon entropy data cut lung .xlsx")
#########View excel file##########
View(lung2)
#######Subset function to extract data with entropy greater than or equal to 0.5#########
subset(x=lung2,subset = Entropy>=0.5)
######Assign subset variable name##########
lungentropy2<-subset(x=lung2,subset = Entropy>=0.5)
########Call vector##########
lungentropy2
#########Plot lungentropy2##########
plot(lungentropy2,type="h")
text(lungentropy2,
     labels = row.names(lungentropy2),
     cex = 0.6, pos = 3, col = "black")

#######Data analysis of entropy data Tracheal aspirates samples##########
file.choose()
read_excel("/Users/victorrojas/Desktop/Publicación Covid-2022/Resultados /Resultados 2 /Brasil/lung /shanon entropy data brazil lung cut alignment.xlsx")
lungbrazil<-read_excel("/Users/victorrojas/Desktop/Publicación Covid-2022/Resultados /Resultados 2 /Brasil/lung /shanon entropy data brazil lung cut alignment.xlsx")
View(lungbrazil)
#######Subset function to extract data with entropy greater than or equal to 0.5#########
subset(x=lungbrazil,subset = Entropy>=0.5)
lungbrazilentropy<-subset(x=lungbrazil,subset = Entropy>=0.5)
lungbrazilentropy
#########Plot lungbrazilentropy##########
plot(lungbrazilentropy,type="h")
text(lungbrazilentropy,
     labels = row.names(lungbrazilentropy),
     cex = 0.6, pos = 3, col = "black")

#########Box plot of entropies in the different types of samples############
Nasofaringeal
Sputum
Traqueal_aspirate
########Assign names to datasets ############
Nasofaringeal<-nasoentropy2
Sputum<-lungentropy2
Traqueal_aspirate<-lungbrazilentropy
##########Box plot##########
boxplot(Nasofaringeal$Entropy,Sputum$Entropy,Traqueal_aspirate$Entropy,
        ylab= "Diversity",names = c("Nasopharyngeal
n=9","Sputum
n=12","Tracheal aspirate 
n=6"),col = rainbow(ncol(trees)))
########Wilcoxon test#########
wilcox.test(Nasofaringeal$Entropy,Sputum$Entropy)
wilcox.test(Nasofaringeal$Entropy,Traqueal_aspirate$Entropy)

########Extract entropy data at positions within the S gene in nasopharyngeal samples#########
subset(x=oronasofaringeal2.1,subset = Position %in% 21563:25384)
sproteinmut<-subset(x=oronasofaringeal2.1,subset = Position %in% 21563:25384)
sproteinmut
##########Plot S gene positions with entropies >=0.15 in nasopharyngeal samples############
plot(sproteinmut,type="h")
text(sproteinmut,
     labels = row.names(sproteinmut),
     cex = 0.6, pos = 3, col = "black")
########Extract entropy data at positions within the S gene in Sputum samples#########
subset(x=lung2,subset = Position %in% 21563:25384)
sproteinmutlung<-subset(x=lung2,subset = Position %in% 21563:25384)
sproteinmutlung
##########Plot S gene positions with entropies >=0.30 in Sputum samples############
plot(sproteinmutlung,type="h")
text(sproteinmutlung,
     labels = row.names(sproteinmutlung),
     cex = 0.6, pos = 3, col = "black")
########Extract entropy data at positions within the S gene in Tracheal aspirates#########
subset(x=lungbrazil,subset = Position %in% 21563:25384)
sproteinmutlungbrazil<-subset(x=lungbrazil,subset = Position %in% 21563:25384)
sproteinmutlungbrazil
###########Plot S gene positions with entropies >=0.15 in Tracheal aspirates############
plot(sproteinmutlungbrazil,type="h")
text(sproteinmutlungbrazil,
     labels = row.names(sproteinmutlungbrazil),
     cex = 0.6, pos = 3, col = "black")

#########Box plot N gene########
###########Extract data for entropies >=0.5 in Nasopharyngeal samples and filter by N gene###########
subset(x=Nasofaringeal,subset = Position %in% 28274:29533)
Ngen<-subset(x=Nasofaringeal,subset = Position %in% 28274:29533)
Ngen
###########Extract data for entropies >=0.5 in Sputum samples and filter by N gene ######
subset(x=Sputum,subset = Position %in% 28274:29533)
Ngensputum<-subset(x=Sputum,subset = Position %in% 28274:29533)
Ngensputum
###########Extract data for entropies >=0.5 in Tracheal aspirates samples and filter by N gene######
subset(x=Traqueal_aspirate,subset = Position %in% 28274:29533)
Ngentracheal<-subset(x=Traqueal_aspirate,subset = Position %in% 28274:29533)
Ngentracheal
#########Box plot entropy data of the N gene in different samples##########
boxplot(Ngen$Entropy,Ngensputum$Entropy,Ngentracheal$Entropy,
        ylab= "Diversity",names = c("Nasopharyngeal
n=4","Sputum 
n=4","Tracheal aspirate 
n=4"),col = rainbow(ncol(trees)))
########Wilcoxon test#########
wilcox.test(Ngen$Entropy,Ngensputum$Entropy)
wilcox.test(Ngen$Entropy,Ngentracheal$Entropy)

########Box plot S gene#########
#######Subset function to extract data for entropy >= 0.15 of the S gene Nasopharyngeal samples#########
subset(x=oronasofaringeal2.1,subset = Entropy>=0.15)
######Assign subset variable name##########
nasoentropy2<-subset(x=oronasofaringeal2.1,subset = Entropy>=0.15)
########Call vector##########
nasoentropy2
######Extract S gene positions#########
subset(x=nasoentropy2,subset = Position %in% 21563:25384)
sproteinnaso<-subset(x=nasoentropy2,subset = Position %in% 21563:25384)
sproteinnaso
######Subset function to extract entropy data >= 0.30 from S-gene sputum samples#########
subset(x=lung2,subset = Entropy>=0.30)
#######Assign subset variable name#########
sputum2<-subset(x=lung2,subset = Entropy>=0.30)
########Call vector##########
sputum2
######Extract S gene positions#########
subset(x=sputum2,subset = Position %in% 21563:25384)
sproteinsputum<-subset(x=sputum2,subset = Position %in% 21563:25384)
sproteinsputum
########Subset function to extract data for entropy >= 0.15 of the S gene Tracheal aspirates#########
subset(x=lungbrazil,subset = Entropy>=0.15)
######Assign subset variable name##########
lungbrazilentropys<-subset(x=lungbrazil,subset = Entropy>=0.15)
########Call vector##########
lungbrazilentropys
######Extract S gene positions#########
subset(x=lungbrazilentropys,subset = Position %in% 21563:25384)
sproteinlungbrazil<-subset(x=lungbrazilentropys,subset = Position %in% 21563:25384)
sproteinlungbrazil
##########Box plot entropy data of the S gene in different samples##########
boxplot(sproteinnaso$Entropy,sproteinsputum$Entropy,sproteinlungbrazil$Entropy,
        ylab= "Diversity",names = c("Nasopharyngeal
n=2","Sputum 
n=1","Tracheal aspirate 
n=3"),col = rainbow(ncol(trees)))
########Wilcoxon test#########
wilcox.test(sproteinnaso$Entropy,sproteinsputum$Entropy)
wilcox.test(sproteinnaso$Entropy,sproteinlungbrazil$Entropy)