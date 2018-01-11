#Name of Quantlet :Principal Component Analysis (PCA)

#Description: PCA function from scratch
#PCA can be thought of as fitting an n-dimensional ellipsoid to the data,
#where each axis of the ellipsoid represents a principal component. If some axis of the ellipsoid is small,
#then the variance along that axis is also small, and by omitting that axis and its corresponding principal 
#component from our representation of the dataset, we lose only a commensurately small amount of information.

#Author :Julien Kraemer

#Example :Run the PCA on a set of player attributes ,visualize the results in 2-dimension and compare with the function dudi.pca

rm(list=ls())

library(psych)
library(ade4)

#2/We extract quantitative information

#We load the table Player_all
if(file.exists("Player_all.rds")){ #Check if the Finished files are in the directory
  Player_all = readRDS("Player_all.rds")
} else {source("data_cleaning.R")}



#2/We extract the quantitative informations

#We generate here a new database where we average over the several games of each player
quant=Player_all[,c(4,6,7,11:38)] #We only keep a subset of nonquantitative variables
quant=na.omit(quant) #We have to delete the rows containing some missing values #not the best way to do
L=levels(as.factor(quant$player_fifa_api_id))
length(L) #number of different players : 10582
Player_Attributes_quant_mean=aggregate(quant,by=list(quant$player_fifa_api_id), mean)[-1]


#3/PCA
#Be careful of the type of the table (Have you only kept the quantitative part ?)
#We fix a certain thresold for the desired variance, by default 0.8
PCA=function(table,norm=T,order=2,desiredvariance=0.8) #default value of order is 2 (more convenient for plotting) , it's the maximal number of PC you want
{
  check=apply(table,2,is.numeric) #We check if we have only quantitative values
  if (mean(check)==1)
  {
    table=scale(table,center=TRUE,scale=norm) #We center the dataset (but we don't scale)
    n=dim(table)[1] #number of indivuals (rows)
    p=dim(table)[2] #number of variables (columns)
    D=1/n*diag(rep(1,n)) #matrix with the weights of indivuals
    Q=diag(rep(1,p))
    S=t(table)%*%D%*%table #sample covariance matrix
    u=diag(S)
    u=1/sqrt(u)
    D1s=diag(u)
    R=D1s%*%S%*%D1s #sample correlation matrix
    eigenvalues=eigen(S)$values
    eigenvectors=eigen(S)$vectors
    inertia=sum(diag(S)) #inertia is the sum of the eigenvalues
    cum=cumsum(eigen(S)$values)/inertia # cumulative energy /inertia
    i=1
    while (cum[i]<desiredvariance) 
    {
      i=i+1
    }
    i=min(order,i)
    #We project the table on the i first vectors
    #We compute the main factors and the coordinates of the individuals on the i first axes
    #Main factors are main axes since Q=Ip
    #Gk for 1<=k<=i fulfill : Fk=table*uk where (uk)_k are the main axes
    FF=matrix(rep(0,n*i),ncol=i) #zero matrix for storing the Fk's
    G=matrix(rep(0,p*i),ncol=i) #zero matrix for storing the Gk's
    nor=matrix(rep(0,p*i),ncol=i)
    cor=matrix(rep(0,i*p),ncol=p)
    for (k in 0:i) 
    {
      FF[,k]=table%*%eigenvectors[,k]
      G[,k]=sqrt(eigenvalues[k])*eigenvectors[,k]
    }
    for (m in 1:i)
    {
      for (j in 1:p)
      {
        cor[m,j]=G[j,m]/sqrt(1/n*t(table[,j])%*%table[,j])
      }
    }
    return(list(FF=FF, G=G,cor=cor,cum=cum,R=R)) #We return FF,G,cor,the inertia proportion and the correlation matrix
    #Coordinates of points in the area of variables
  }
  else 
  {
    stop("The argument table must be a matrix/dataframe of exclusive quantitative values")
  }
}

#5/ First results

#We can check that we obtain the same results with the function of R

pca=PCA(Player_Attributes_quant_mean[-1], order = 5) #All of the attributes without the id

cum=pca$cum #relative cumsum of the inertia 
FF=pca$FF #contributions of the players to the axes (It should be interesting to visualize the positions of the players in the plane F1-F2)
G=pca$G #contributions of the variables to the axes (rule of thumb : We only keep the contributions greather than 1/p*100=2.7)
R=pca$R #correlation matrix

test =  principal(r =Player_Attributes_quant_mean[,-1],nfactors = 5, rotate = "none" )


#6/Comparison with the normed CPA using the function of R


pca1=dudi.pca(Player_Attributes_quant_mean[,-1],center=TRUE,scale=TRUE) #normed PCA

###### Please wait before running further
inertia=inertia.dudi(pca1, col.inertia=TRUE)
round(pca1$eig,2) #eigenvalues
round(cumsum(pca1$eig)/sum(pca1$eig),2) # relative cumsum of the inertia (cumsum of inertia /inertia)
#Around 66% of the inertia is caught by the plane (F1-F2)
GR=round(pca1$co,2) #G
FR=round(pca1$li,2)#F

# graphical representation of the variables and of the players in the plane (F1-F2)

#correlation circle
s.corcircle(G,label=NULL,sub = "Correlation circle", csub = 2.5) #Without the legend
s.corcircle(GR,label=NULL,sub = "Correlation circle", csub = 2.5)#We compare with the correlation circle of R

s.corcircle(G,clab=0.5,sub = "Correlation circle", csub = 2.5)
#The well represented variables are those which are near the edge of the correlation circle and the variables near (0,0) aren't well represented

#Representation of the players in the plane (F1-F2)
plot(FF[,1],FF[,2],xlab="first axis",ylab="second axis",main="Representation of the players in the plane (F1-F2)")

#We can distinguish two group of players : which are the characteristics of those two groups ?

#Contribution of the variables to the axes :

lam1=(eigen(R)$values)[1]
lam2=(eigen(R)$values)[2]

n=dim(Player_Attributes_quant_mean)[1]

#Contribution of the players to the first axis
ctr1players=1/(n*lam1)*100*FF[,1]^2

#Contribution of the players to the second axis
ctr2players=1/(n*lam2)*100*FF[,2]^2

#Rule of thumb :
#We only consider the contributions greather than 1/n*100

Importantplayers1=which(ctr1players>(1/n*100))
Importantplayers2=which(ctr2players>(1/n*100))

length(Importantplayers1)
length(Importantplayers2)
#There are 2020 important players for the first axis and 4138 for the second axis

#Those results are not very meaningful because we have deleted some players of the dataset in order to run the PCA (one requirement was to have no missing values)


#Contribution of the variables to the several axes
ctr1variables=G[,1]^2/lam1*100
ctr2variables=G[,2]^2/lam2*100

#Rule of thumb :
#We only consider the contributions greather than 1/p*100
p=dim(Player_Attributes_quant_mean)[2]
Importantvariables1=which(ctr1variables>(1/p*100))
Importantvariables2=which(ctr2variables>(1/p*100))


ctr1variables[Importantvariables1]*sign(G[Importantvariables1,1])
names(Player_Attributes_quant_mean)[Importantvariables1]
#If we share the plane(F1-F2) into 4 quadrants, we can say that the players in the first and second quadrant are characterized by their crossing, finishing, short passing...gk_dividing and
#the players in the third and fourth quadrants by their handling, positionning and reflexes
ctr2variables[Importantvariables2]*sign(G[Importantvariables2,2])
names(Player_Attributes_quant_mean)[Importantvariables2]

#Players with (very) negative coordinates on F2 are (pretty) charaterized by their agression ,interceptions , marking , standing tacke and sliding tackle
#The more the players have a positive coordinate on F2,the more they are characterized by their finishing

#Characteristics of the two groups :

#The first group of players is characterized by its agression ,interceptions , marking , standing tacke and sliding tackle and the effect of the other attributes is not so important 

#The second group of players is charaterized by its finishing , volleys and its agility.


#Interpretation in higher dimensions :
pca5=PCA(Player_Attributes_quant_mean[,-1],order=5)
FF=pca5$FF #contributions of the players to the axes (It should be interesting to visualize the positions of the players in the plane F1-F2)
G=pca5$G #contributions of the variables to the axes (rule of thumb : We only keep the contributions greather than 1/p*100=2.7)
R=pca5$R #correlation matrix

p=dim(Player_Attributes_quant_mean[,-1])[2]
order=dim(G)[2]
ctrvariables=matrix(NA,nrow=p,ncol=order)
for (i in 1:order)
{
  ctrvariables[,i]=G[,i]^2/(eigen(R)$values)[i]*100
}

for (i in 1:order)
{
  Importantvariables=which(ctrvariables[,i]>(1/p*100))
  Importantctr=ctrvariables[Importantvariables,i]*sign(G[Importantvariables,i])
  N1="The variables which contribute negatively to the"
  N2="-th axis are :"
  N=paste(N1,i,N2);
  print(N)
  print(names(Player_Attributes_quant_mean)[Importantvariables*(Importantctr<0)])
  print("and their contributions are :")
  print(Importantctr[Importantctr<0])
  F1="The variables which contribute positively to the"
  F2="-th axis are :"
  F=paste(F1,i,F2);
  print(F)
  print(names(Player_Attributes_quant_mean)[Importantvariables*(Importantctr>0)])
  print("and their contributions are :")
  print(Importantctr[Importantctr>0])
}

pairs(FF)




