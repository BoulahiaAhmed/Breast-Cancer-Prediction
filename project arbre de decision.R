# Preparation du data

#Labels: 
   #1=Healthy controls 
   #2=Patients  

data<-dataR2

#Regardant la structure du notre data

str(data)

# on cree une vouvelle variable qui change le type de variable "classification" du int a un facteur

data$Classification1 <- factor(data$Classification)

#Partionnement du data en deux groupe training (70% du data) et test (30% du data)
#on utilise un seed aleatoire
set.seed(1234)

#on appele pd notre partionnement du data
pd<-sample(2,nrow(data),replace =TRUE, prob=c(0.7,0.3))

# on affecte la premiere colonne du pd a une vouvelle var train
train<- data[pd==1,]

# et la deuxiemme colonne du pd au variable validation
validation <- data[pd==2,]








# =======================  ARBRE DE DECISION AVEC LE PACKAGE PARTY


library(party)

#on cree une nouvelle variable on utilisant la fonction ctree
#sur la variable du type facteur

# ctree control pour controler notre arbre: 
# mincriterion est la p-valeur du variable 
# minsplit : la branche va etre diviser en deux sauf si on a au moins le nombre d'observation indiquer 




tree<- ctree(Classification1~Age+BMI+Glucose+Insulin+HOMA+Resistin , controls = ctree_control(mincriterion=0.9,minsplit=10))
plot(tree,main="ARBRE DE DECISION avec un niveau de confiance 90%")
# 1 on remarque que le Glucose est la variable la plus importante pour la classification 
# 2 si le taux de glucose est superieure a 91 : (80% le patient est malade, 20% healthy)
# 3 si le glucose < 91 et le Resistin > 12.936 : (60% malade , 40% healthy)
# 4 si le glucose est <91 et le resistin <12.936 : (18% malade 82% healthy)



# ==================== PREDICTION sur l'echantillion validation 

predict(tree,validation,type="prob")

# l'orsqu'on execute cette commande on obtient 2 probabilités pour
# chaque observation du notre echantillion validation



#================================== Calcule de l'erreur du prediction for 'validtion' data


tab<- table(predict(tree),validation$Classification1)
print(tab)
# les nombre hors diagonal represente l'erreur du classification pour le calculer on fait
1-sum(diag(tab))/sum(tab)









#==================== ARBRE DE DECISION AVEC LA BIBLIOTHEQUE rpart


library(rpart)
library(rpart.plot)

tree1<- rpart(Classification1~Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1, data=train)
rpart.plot(tree1,extra=1,main="ARBRE DE DECISION AVEC LA BIBLIOTHEQUE rpart" ,sub="extra=1")

# on ajoute extre=1 pour avoir le nombre d'observation sur chaque noeud

rpart.plot(tree1,extra=4,main="ARBRE DE DECISION AVEC LA BIBLIOTHEQUE rpart" ,sub="extra=4")
# sin on met extra=4 on va avoir la probailité d'appartition aux groupe 1 ou 2 sur chaque noeud 





dataa<-dataR2

set.seed(111)
bo <- Boruta(Classification1~., data=dataa, doTrace=2, maxRuns=100)







