library(mvtnorm)
library(nlme)

corr <- c(0.9,0.75,0.5,0.25)
mu<-c(0,0) ; varx=1 ; vary=1 ; n<-50 ; B<-1000


x = array(data = NA, dim=c(100,1000,4)) ; y = array(data = NA, dim=c(100,1000,4))
alpha0<-1 ; alpha1<-5 ; beta0<-5 ; beta1<-2 ; zi<-as.numeric(c(rep(0,n),rep(1,n)))


eps = list()


# Same variance for X and Y
se11 = list(); ce11 = list(); se22 = list(); ce22 = list(); se33 = list(); ce33 = list()
se44 = list(); ce44 = list(); se55 = list(); ce55 = list(); se66 = list(); ce66 = list()
se77 = list(); ce77 = list(); se88 = list(); ce88 = list(); se99 = list(); ce99 = list()
se1010 = list(); ce1010 = list()

# X = 1 and Y varies
se12 = list(); ce12 = list(); se13 = list(); ce13 = list(); se14 = list(); ce14 = list()
se15 = list(); ce15 = list(); se16 = list(); ce16 = list(); se17 = list(); ce17 = list()
se18 = list(); ce18 = list(); se19 = list(); ce19 = list(); se110 = list(); ce110 = list()

# X = 2 and Y varies
se21 = list(); ce21 = list(); se23 = list(); ce23 = list(); se24 = list(); ce24 = list()
se25 = list(); ce25 = list(); se26 = list(); ce26 = list(); se27 = list(); ce27 = list()
se28 = list(); ce28 = list(); se29 = list(); ce29 = list(); se210 = list(); ce210 = list()

# X = 3 and Y varies
se31 = list(); ce31 = list(); se32 = list(); ce32 = list(); se34 = list(); ce34 = list()
se35 = list(); ce35 = list(); se36 = list(); ce36 = list(); se37 = list(); ce37 = list()
se38 = list(); ce38 = list(); se39 = list(); ce39 = list(); se310 = list(); ce310 = list()


# X = 4 and Y varies
se41 = list(); ce41 = list(); se42 = list(); ce42 = list(); se43 = list(); ce43 = list()
se45 = list(); ce45 = list(); se46 = list(); ce46 = list(); se47 = list(); ce47 = list()
se48 = list(); ce48 = list(); se49 = list(); ce49 = list(); se410 = list(); ce410 = list()


# X = 5 and Y varies
se51 = list(); ce51 = list(); se52 = list(); ce52 = list(); se53 = list(); ce53 = list()
se54 = list(); ce54 = list(); se56 = list(); ce56 = list(); se57 = list(); ce57 = list()
se58 = list(); ce58 = list(); se59 = list(); ce59 = list(); se510 = list(); ce510 = list()



# X = 6 and Y varies
se61 = list(); ce61 = list(); se62 = list(); ce62 = list(); se63 = list(); ce63 = list()
se64 = list(); ce64 = list(); se65 = list(); ce65 = list(); se67 = list(); ce67 = list()
se68 = list(); ce68 = list(); se69 = list(); ce69 = list(); se610 = list(); ce610 = list()



# X = 7 and Y varies
se71 = list(); ce71 = list(); se72 = list(); ce72 = list(); se73 = list(); ce73 = list()
se74 = list(); ce74 = list(); se75 = list(); ce75 = list(); se76 = list(); ce76 = list()
se78 = list(); ce78 = list(); se79 = list(); ce79 = list(); se710 = list(); ce710 = list()


# X = 8 and Y varies
se81 = list(); ce81 = list(); se82 = list(); ce82 = list(); se83 = list(); ce83 = list()
se84 = list(); ce84 = list(); se85 = list(); ce85 = list(); se86 = list(); ce86 = list()
se87 = list(); ce87 = list(); se89 = list(); ce89 = list(); se810 = list(); ce810 = list()


# X = 9 and Y varies
se91 = list(); ce91 = list(); se92 = list(); ce92 = list(); se93 = list(); ce93 = list()
se94 = list(); ce94 = list(); se95 = list(); ce95 = list(); se96 = list(); ce96 = list()
se97 = list(); ce97 = list(); se98 = list(); ce98 = list(); se910 = list(); ce910 = list()


# X = 10 and Y varies
se101 = list(); ce101 = list(); se102 = list(); ce102 = list(); se103 = list(); ce103 = list()
se104 = list(); ce104 = list(); se105 = list(); ce105 = list(); se106 = list(); ce106 = list()
se107 = list(); ce107 = list(); se108 = list(); ce108 = list(); se109 = list(); ce109 = list()




vc <- function(vx, vy, cor){
  
  Sigma <- list()
  Sigma[[1]] <- matrix(c(vx, corr[1]*sqrt(vx*vy), corr[1]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  Sigma[[2]] <- matrix(c(vx, corr[2]*sqrt(vx*vy),  corr[2]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  Sigma[[3]] <- matrix(c(vx, corr[3]*sqrt(vx*vy), corr[3]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  Sigma[[4]] <- matrix(c(vx, corr[4]*sqrt(vx*vy), corr[4]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  return(Sigma)
}


# X =1 and Y varies
Sigma11 <- vc(1,1,corr);  Sigma12 <- vc(1,2,corr); Sigma13 <- vc(1,3,corr); Sigma14 <- vc(1,4,corr)
Sigma15 <- vc(1,5,corr);  Sigma16 <- vc(1,6,corr); Sigma17 <- vc(1,7,corr); Sigma18 <- vc(1,8,corr)
Sigma19 <- vc(1,9,corr);  Sigma110 <- vc(1,10,corr)

# X =2 and Y varies
Sigma21 <- vc(2,1,corr);  Sigma22 <- vc(2,2,corr); Sigma23 <- vc(2,3,corr); Sigma24 <- vc(2,4,corr)
Sigma25 <- vc(2,5,corr);  Sigma26 <- vc(2,6,corr); Sigma27 <- vc(2,7,corr); Sigma28 <- vc(2,8,corr)
Sigma29 <- vc(2,9,corr);  Sigma210 <- vc(2,10,corr)

# X =3 and Y varies
Sigma31 <- vc(3,1,corr);  Sigma32 <- vc(3,2,corr); Sigma33 <- vc(3,3,corr); Sigma34 <- vc(3,4,corr)
Sigma35 <- vc(3,5,corr);  Sigma36 <- vc(3,6,corr); Sigma37 <- vc(3,7,corr); Sigma38 <- vc(3,8,corr)
Sigma39 <- vc(3,9,corr);  Sigma310 <- vc(3,10,corr)

# X =4 and Y varies
Sigma41 <- vc(4,1,corr);  Sigma42 <- vc(4,2,corr); Sigma43 <- vc(4,3,corr); Sigma44 <- vc(4,4,corr)
Sigma45 <- vc(4,5,corr);  Sigma46 <- vc(4,6,corr); Sigma47 <- vc(4,7,corr); Sigma48 <- vc(4,8,corr)
Sigma49 <- vc(4,9,corr);  Sigma410 <- vc(4,10,corr)

# X =5 and Y varies
Sigma51 <- vc(5,1,corr);  Sigma52 <- vc(5,2,corr); Sigma53 <- vc(5,3,corr); Sigma54 <- vc(5,4,corr)
Sigma55 <- vc(5,5,corr);  Sigma56 <- vc(5,6,corr); Sigma57 <- vc(5,7,corr); Sigma58 <- vc(5,8,corr)
Sigma59 <- vc(5,9,corr);  Sigma510 <- vc(5,10,corr)

# X =6 and Y varies
Sigma61 <- vc(6,1,corr);  Sigma62 <- vc(6,2,corr); Sigma63 <- vc(6,3,corr); Sigma64 <- vc(6,4,corr)
Sigma65 <- vc(6,5,corr);  Sigma66 <- vc(6,6,corr); Sigma67 <- vc(6,7,corr); Sigma68 <- vc(6,8,corr)
Sigma69 <- vc(6,9,corr);  Sigma610 <- vc(6,10,corr)

# X =7 and Y varies
Sigma71 <- vc(7,1,corr);  Sigma72 <- vc(7,2,corr); Sigma73 <- vc(7,3,corr); Sigma74 <- vc(7,4,corr)
Sigma75 <- vc(7,5,corr);  Sigma76 <- vc(7,6,corr); Sigma77 <- vc(7,7,corr); Sigma78 <- vc(7,8,corr)
Sigma79 <- vc(7,9,corr);  Sigma710 <- vc(7,10,corr)


# X =8 and Y varies
Sigma81 <- vc(8,1,corr);  Sigma82 <- vc(8,2,corr); Sigma83 <- vc(8,3,corr); Sigma84 <- vc(8,4,corr)
Sigma85 <- vc(8,5,corr);  Sigma86 <- vc(8,6,corr); Sigma87 <- vc(8,7,corr); Sigma88 <- vc(8,8,corr)
Sigma89 <- vc(8,9,corr);  Sigma810 <- vc(8,10,corr)

# X =9 and Y varies
Sigma91 <- vc(9,1,corr);  Sigma92 <- vc(9,2,corr); Sigma93 <- vc(9,3,corr); Sigma94 <- vc(9,4,corr)
Sigma95 <- vc(9,5,corr);  Sigma96 <- vc(9,6,corr); Sigma97 <- vc(9,7,corr); Sigma98 <- vc(9,8,corr)
Sigma99 <- vc(9,9,corr);  Sigma910 <- vc(9,10,corr)

# X =10 and Y varies
Sigma101 <- vc(10,1,corr);  Sigma102 <- vc(10,2,corr); Sigma103 <- vc(10,3,corr); Sigma104 <- vc(10,4,corr)
Sigma105 <- vc(10,5,corr);  Sigma106 <- vc(10,6,corr); Sigma107 <- vc(10,7,corr); Sigma108 <- vc(10,8,corr)
Sigma109 <- vc(10,9,corr);  Sigma1010 <- vc(10,10,corr)




# X= 1 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma11[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se11[[k]] <- data.frame(x[,,k][,])
    ce11[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce11) = c("0.9", "0.75", "0.5", "0.25")
names(se11) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma12[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se12[[k]] <- data.frame(x[,,k][,])
    ce12[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce12) = c("0.9", "0.75", "0.5", "0.25")
names(se12) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma13[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se13[[k]] <- data.frame(x[,,k][,])
    ce13[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce13) = c("0.9", "0.75", "0.5", "0.25")
names(se13) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma14[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se14[[k]] <- data.frame(x[,,k][,])
    ce14[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce14) = c("0.9", "0.75", "0.5", "0.25")
names(se14) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma15[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se15[[k]] <- data.frame(x[,,k][,])
    ce15[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce15) = c("0.9", "0.75", "0.5", "0.25")
names(se15) = c("0.9", "0.75", "0.5", "0.25")



# X= 1 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma16[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se16[[k]] <- data.frame(x[,,k][,])
    ce16[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce16) = c("0.9", "0.75", "0.5", "0.25")
names(se16) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma17[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se17[[k]] <- data.frame(x[,,k][,])
    ce17[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce17) = c("0.9", "0.75", "0.5", "0.25")
names(se17) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma18[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se18[[k]] <- data.frame(x[,,k][,])
    ce18[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce18) = c("0.9", "0.75", "0.5", "0.25")
names(se18) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma19[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se19[[k]] <- data.frame(x[,,k][,])
    ce19[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce19) = c("0.9", "0.75", "0.5", "0.25")
names(se19) = c("0.9", "0.75", "0.5", "0.25")


# X= 1 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma110[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se110[[k]] <- data.frame(x[,,k][,])
    ce110[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce110) = c("0.9", "0.75", "0.5", "0.25")
names(se110) = c("0.9", "0.75", "0.5", "0.25")

save(list = c("se11","ce11","se12","ce12","se13","ce13","se14","ce14","se15","ce15","se16",
              "ce16","se17","ce17","se18","ce18","se19","ce19","se110","ce110") , 
                  file="XVaianceis1.RData")



# X= 2 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma21[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se21[[k]] <- data.frame(x[,,k][,])
    ce21[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce21) = c("0.9", "0.75", "0.5", "0.25")
names(se21) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma22[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se22[[k]] <- data.frame(x[,,k][,])
    ce22[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce22) = c("0.9", "0.75", "0.5", "0.25")
names(se22) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma23[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se23[[k]] <- data.frame(x[,,k][,])
    ce23[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce23) = c("0.9", "0.75", "0.5", "0.25")
names(se23) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma24[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se24[[k]] <- data.frame(x[,,k][,])
    ce24[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce24) = c("0.9", "0.75", "0.5", "0.25")
names(se24) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma25[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se25[[k]] <- data.frame(x[,,k][,])
    ce25[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce25) = c("0.9", "0.75", "0.5", "0.25")
names(se25) = c("0.9", "0.75", "0.5", "0.25")



# X= 2 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma26[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se26[[k]] <- data.frame(x[,,k][,])
    ce26[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce26) = c("0.9", "0.75", "0.5", "0.25")
names(se26) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma27[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se27[[k]] <- data.frame(x[,,k][,])
    ce27[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce27) = c("0.9", "0.75", "0.5", "0.25")
names(se27) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma28[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se28[[k]] <- data.frame(x[,,k][,])
    ce28[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce28) = c("0.9", "0.75", "0.5", "0.25")
names(se28) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma29[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se29[[k]] <- data.frame(x[,,k][,])
    ce29[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce29) = c("0.9", "0.75", "0.5", "0.25")
names(se29) = c("0.9", "0.75", "0.5", "0.25")


# X= 2 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma210[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se210[[k]] <- data.frame(x[,,k][,])
    ce210[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce210) = c("0.9", "0.75", "0.5", "0.25")
names(se210) = c("0.9", "0.75", "0.5", "0.25")

save(list = c("se21","ce21","se22","ce22","se23","ce23","se24","ce24","se25","ce25","se26",
              "ce26","se27","ce27","se28","ce28","se29","ce29","se210","ce210") , 
     file="XVaianceis2.RData")



# X= 3 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma31[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se31[[k]] <- data.frame(x[,,k][,])
    ce31[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce31) = c("0.9", "0.75", "0.5", "0.25")
names(se31) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma32[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se32[[k]] <- data.frame(x[,,k][,])
    ce32[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce32) = c("0.9", "0.75", "0.5", "0.25")
names(se32) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma33[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se33[[k]] <- data.frame(x[,,k][,])
    ce33[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce33) = c("0.9", "0.75", "0.5", "0.25")
names(se33) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma34[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se34[[k]] <- data.frame(x[,,k][,])
    ce34[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce34) = c("0.9", "0.75", "0.5", "0.25")
names(se34) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma35[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se35[[k]] <- data.frame(x[,,k][,])
    ce35[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce35) = c("0.9", "0.75", "0.5", "0.25")
names(se35) = c("0.9", "0.75", "0.5", "0.25")



# X= 3 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma36[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se36[[k]] <- data.frame(x[,,k][,])
    ce36[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce36) = c("0.9", "0.75", "0.5", "0.25")
names(se36) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma37[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se37[[k]] <- data.frame(x[,,k][,])
    ce37[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce37) = c("0.9", "0.75", "0.5", "0.25")
names(se37) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma38[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se38[[k]] <- data.frame(x[,,k][,])
    ce38[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce38) = c("0.9", "0.75", "0.5", "0.25")
names(se38) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma39[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se39[[k]] <- data.frame(x[,,k][,])
    ce39[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce39) = c("0.9", "0.75", "0.5", "0.25")
names(se39) = c("0.9", "0.75", "0.5", "0.25")


# X= 3 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma310[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se310[[k]] <- data.frame(x[,,k][,])
    ce310[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce310) = c("0.9", "0.75", "0.5", "0.25")
names(se310) = c("0.9", "0.75", "0.5", "0.25")

save(list = c("se31","ce31","se32","ce32","se33","ce33","se34","ce34","se35","ce35","se36",
              "ce36","se37","ce37","se38","ce38","se39","ce39","se310","ce310") , 
     file="XVaianceis3.RData")




# X= 4 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma41[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se41[[k]] <- data.frame(x[,,k][,])
    ce41[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce41) = c("0.9", "0.75", "0.5", "0.25")
names(se41) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma42[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se42[[k]] <- data.frame(x[,,k][,])
    ce42[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce42) = c("0.9", "0.75", "0.5", "0.25")
names(se42) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma43[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se43[[k]] <- data.frame(x[,,k][,])
    ce43[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce43) = c("0.9", "0.75", "0.5", "0.25")
names(se43) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma44[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se44[[k]] <- data.frame(x[,,k][,])
    ce44[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce44) = c("0.9", "0.75", "0.5", "0.25")
names(se44) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma45[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se45[[k]] <- data.frame(x[,,k][,])
    ce45[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce45) = c("0.9", "0.75", "0.5", "0.25")
names(se45) = c("0.9", "0.75", "0.5", "0.25")



# X= 4 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma46[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se46[[k]] <- data.frame(x[,,k][,])
    ce46[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce46) = c("0.9", "0.75", "0.5", "0.25")
names(se46) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma47[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se47[[k]] <- data.frame(x[,,k][,])
    ce47[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce47) = c("0.9", "0.75", "0.5", "0.25")
names(se47) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma48[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se48[[k]] <- data.frame(x[,,k][,])
    ce48[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce48) = c("0.9", "0.75", "0.5", "0.25")
names(se48) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma49[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se49[[k]] <- data.frame(x[,,k][,])
    ce49[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce49) = c("0.9", "0.75", "0.5", "0.25")
names(se49) = c("0.9", "0.75", "0.5", "0.25")


# X= 4 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma410[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se410[[k]] <- data.frame(x[,,k][,])
    ce410[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce410) = c("0.9", "0.75", "0.5", "0.25")
names(se410) = c("0.9", "0.75", "0.5", "0.25")


save(list = c("se41","ce41","se42","ce42","se43","ce43","se44","ce44","se45","ce45","se46",
              "ce46","se47","ce47","se48","ce48","se49","ce49","se410","ce410") , 
     file="XVaianceis4.RData")



# X= 5 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma51[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se51[[k]] <- data.frame(x[,,k][,])
    ce51[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce51) = c("0.9", "0.75", "0.5", "0.25")
names(se51) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma52[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se52[[k]] <- data.frame(x[,,k][,])
    ce52[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce52) = c("0.9", "0.75", "0.5", "0.25")
names(se52) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma53[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se53[[k]] <- data.frame(x[,,k][,])
    ce53[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce53) = c("0.9", "0.75", "0.5", "0.25")
names(se53) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma54[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se54[[k]] <- data.frame(x[,,k][,])
    ce54[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce54) = c("0.9", "0.75", "0.5", "0.25")
names(se54) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma55[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se55[[k]] <- data.frame(x[,,k][,])
    ce55[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce55) = c("0.9", "0.75", "0.5", "0.25")
names(se55) = c("0.9", "0.75", "0.5", "0.25")



# X= 5 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma56[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se56[[k]] <- data.frame(x[,,k][,])
    ce56[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce56) = c("0.9", "0.75", "0.5", "0.25")
names(se56) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma57[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se57[[k]] <- data.frame(x[,,k][,])
    ce57[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce57) = c("0.9", "0.75", "0.5", "0.25")
names(se57) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma58[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se58[[k]] <- data.frame(x[,,k][,])
    ce58[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce58) = c("0.9", "0.75", "0.5", "0.25")
names(se58) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma59[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se59[[k]] <- data.frame(x[,,k][,])
    ce59[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce59) = c("0.9", "0.75", "0.5", "0.25")
names(se59) = c("0.9", "0.75", "0.5", "0.25")


# X= 5 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma510[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se510[[k]] <- data.frame(x[,,k][,])
    ce510[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce510) = c("0.9", "0.75", "0.5", "0.25")
names(se510) = c("0.9", "0.75", "0.5", "0.25")


save(list = c("se51","ce51","se52","ce52","se53","ce53","se54","ce54","se55","ce55","se56",
              "ce56","se57","ce57","se58","ce58","se59","ce59","se510","ce510") , 
     file="XVaianceis5.RData")


# X= 6 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma61[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se61[[k]] <- data.frame(x[,,k][,])
    ce61[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce61) = c("0.9", "0.75", "0.5", "0.25")
names(se61) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma62[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se62[[k]] <- data.frame(x[,,k][,])
    ce62[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce62) = c("0.9", "0.75", "0.5", "0.25")
names(se62) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma63[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se63[[k]] <- data.frame(x[,,k][,])
    ce63[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce63) = c("0.9", "0.75", "0.5", "0.25")
names(se63) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma64[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se64[[k]] <- data.frame(x[,,k][,])
    ce64[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce64) = c("0.9", "0.75", "0.5", "0.25")
names(se64) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma65[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se65[[k]] <- data.frame(x[,,k][,])
    ce65[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce65) = c("0.9", "0.75", "0.5", "0.25")
names(se65) = c("0.9", "0.75", "0.5", "0.25")



# X= 6 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma66[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se66[[k]] <- data.frame(x[,,k][,])
    ce66[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce66) = c("0.9", "0.75", "0.5", "0.25")
names(se66) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma67[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se67[[k]] <- data.frame(x[,,k][,])
    ce67[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce67) = c("0.9", "0.75", "0.5", "0.25")
names(se67) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma68[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se68[[k]] <- data.frame(x[,,k][,])
    ce68[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce68) = c("0.9", "0.75", "0.5", "0.25")
names(se68) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma69[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se69[[k]] <- data.frame(x[,,k][,])
    ce69[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce69) = c("0.9", "0.75", "0.5", "0.25")
names(se69) = c("0.9", "0.75", "0.5", "0.25")


# X= 6 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma610[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se610[[k]] <- data.frame(x[,,k][,])
    ce610[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce610) = c("0.9", "0.75", "0.5", "0.25")
names(se610) = c("0.9", "0.75", "0.5", "0.25")

save(list = c("se61","ce61","se62","ce62","se63","ce63","se64","ce64","se65","ce65","se66",
              "ce66","se67","ce67","se68","ce68","se69","ce69","se610","ce610") , 
     file="XVaianceis6.RData")




# X= 7 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma71[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se71[[k]] <- data.frame(x[,,k][,])
    ce71[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce71) = c("0.9", "0.75", "0.5", "0.25")
names(se71) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma72[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se72[[k]] <- data.frame(x[,,k][,])
    ce72[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce72) = c("0.9", "0.75", "0.5", "0.25")
names(se72) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma73[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se73[[k]] <- data.frame(x[,,k][,])
    ce73[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce73) = c("0.9", "0.75", "0.5", "0.25")
names(se73) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma74[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se74[[k]] <- data.frame(x[,,k][,])
    ce74[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce74) = c("0.9", "0.75", "0.5", "0.25")
names(se74) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma75[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se75[[k]] <- data.frame(x[,,k][,])
    ce75[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce75) = c("0.9", "0.75", "0.5", "0.25")
names(se75) = c("0.9", "0.75", "0.5", "0.25")



# X= 7 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma76[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se76[[k]] <- data.frame(x[,,k][,])
    ce76[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce76) = c("0.9", "0.75", "0.5", "0.25")
names(se76) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma77[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se77[[k]] <- data.frame(x[,,k][,])
    ce77[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce77) = c("0.9", "0.75", "0.5", "0.25")
names(se77) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma78[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se78[[k]] <- data.frame(x[,,k][,])
    ce78[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce78) = c("0.9", "0.75", "0.5", "0.25")
names(se78) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma79[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se79[[k]] <- data.frame(x[,,k][,])
    ce79[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce79) = c("0.9", "0.75", "0.5", "0.25")
names(se79) = c("0.9", "0.75", "0.5", "0.25")


# X= 7 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma710[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se710[[k]] <- data.frame(x[,,k][,])
    ce710[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce710) = c("0.9", "0.75", "0.5", "0.25")
names(se710) = c("0.9", "0.75", "0.5", "0.25")


save(list = c("se71","ce71","se72","ce72","se73","ce73","se74","ce74","se75","ce75","se76",
              "ce76","se77","ce77","se78","ce78","se79","ce79","se710","ce710") , 
     file="XVaianceis7.RData")



# X= 8 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma81[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se81[[k]] <- data.frame(x[,,k][,])
    ce81[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce81) = c("0.9", "0.75", "0.5", "0.25")
names(se81) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma82[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se82[[k]] <- data.frame(x[,,k][,])
    ce82[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce82) = c("0.9", "0.75", "0.5", "0.25")
names(se82) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma83[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se83[[k]] <- data.frame(x[,,k][,])
    ce83[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce83) = c("0.9", "0.75", "0.5", "0.25")
names(se83) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma84[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se84[[k]] <- data.frame(x[,,k][,])
    ce84[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce84) = c("0.9", "0.75", "0.5", "0.25")
names(se84) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma85[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se85[[k]] <- data.frame(x[,,k][,])
    ce85[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce85) = c("0.9", "0.75", "0.5", "0.25")
names(se85) = c("0.9", "0.75", "0.5", "0.25")



# X= 8 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma86[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se86[[k]] <- data.frame(x[,,k][,])
    ce86[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce86) = c("0.9", "0.75", "0.5", "0.25")
names(se86) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma87[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se87[[k]] <- data.frame(x[,,k][,])
    ce87[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce87) = c("0.9", "0.75", "0.5", "0.25")
names(se87) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma88[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se88[[k]] <- data.frame(x[,,k][,])
    ce88[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce88) = c("0.9", "0.75", "0.5", "0.25")
names(se88) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma89[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se89[[k]] <- data.frame(x[,,k][,])
    ce89[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce89) = c("0.9", "0.75", "0.5", "0.25")
names(se89) = c("0.9", "0.75", "0.5", "0.25")


# X= 8 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma810[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se810[[k]] <- data.frame(x[,,k][,])
    ce810[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce810) = c("0.9", "0.75", "0.5", "0.25")
names(se810) = c("0.9", "0.75", "0.5", "0.25")

save(list = c("se81","ce81","se82","ce82","se83","ce83","se84","ce84","se85","ce85","se86",
              "ce86","se87","ce87","se88","ce88","se89","ce89","se810","ce810") , 
     file="XVaianceis8.RData")


# X= 9 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma91[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se91[[k]] <- data.frame(x[,,k][,])
    ce91[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce91) = c("0.9", "0.75", "0.5", "0.25")
names(se91) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma92[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se92[[k]] <- data.frame(x[,,k][,])
    ce92[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce92) = c("0.9", "0.75", "0.5", "0.25")
names(se92) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma93[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se93[[k]] <- data.frame(x[,,k][,])
    ce93[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce93) = c("0.9", "0.75", "0.5", "0.25")
names(se93) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma94[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se94[[k]] <- data.frame(x[,,k][,])
    ce94[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce94) = c("0.9", "0.75", "0.5", "0.25")
names(se94) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma95[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se95[[k]] <- data.frame(x[,,k][,])
    ce95[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce95) = c("0.9", "0.75", "0.5", "0.25")
names(se95) = c("0.9", "0.75", "0.5", "0.25")



# X= 9 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma96[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se96[[k]] <- data.frame(x[,,k][,])
    ce96[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce96) = c("0.9", "0.75", "0.5", "0.25")
names(se96) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma97[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se97[[k]] <- data.frame(x[,,k][,])
    ce97[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce97) = c("0.9", "0.75", "0.5", "0.25")
names(se97) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma98[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se98[[k]] <- data.frame(x[,,k][,])
    ce98[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce98) = c("0.9", "0.75", "0.5", "0.25")
names(se98) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma99[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se99[[k]] <- data.frame(x[,,k][,])
    ce99[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce99) = c("0.9", "0.75", "0.5", "0.25")
names(se99) = c("0.9", "0.75", "0.5", "0.25")


# X= 9 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma910[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se910[[k]] <- data.frame(x[,,k][,])
    ce910[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce910) = c("0.9", "0.75", "0.5", "0.25")
names(se910) = c("0.9", "0.75", "0.5", "0.25")


save(list = c("se91","ce91","se92","ce92","se93","ce93","se94","ce94","se95","ce95","se96",
              "ce96","se97","ce97","se98","ce98","se99","ce99","se910","ce910") , 
     file="XVaianceis9.RData")



# X= 10 and Y=1
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma101[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se101[[k]] <- data.frame(x[,,k][,])
    ce101[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce101) = c("0.9", "0.75", "0.5", "0.25")
names(se101) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=2
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma102[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se102[[k]] <- data.frame(x[,,k][,])
    ce102[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce102) = c("0.9", "0.75", "0.5", "0.25")
names(se102) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=3
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma103[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se103[[k]] <- data.frame(x[,,k][,])
    ce103[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce103) = c("0.9", "0.75", "0.5", "0.25")
names(se103) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=4
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma104[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se104[[k]] <- data.frame(x[,,k][,])
    ce104[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce104) = c("0.9", "0.75", "0.5", "0.25")
names(se104) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=5
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma105[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se105[[k]] <- data.frame(x[,,k][,])
    ce105[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce105) = c("0.9", "0.75", "0.5", "0.25")
names(se105) = c("0.9", "0.75", "0.5", "0.25")



# X= 10 and Y=6
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma106[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se106[[k]] <- data.frame(x[,,k][,])
    ce106[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce106) = c("0.9", "0.75", "0.5", "0.25")
names(se106) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=7
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma107[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se107[[k]] <- data.frame(x[,,k][,])
    ce107[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce107) = c("0.9", "0.75", "0.5", "0.25")
names(se107) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=8
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma108[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se108[[k]] <- data.frame(x[,,k][,])
    ce108[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce108) = c("0.9", "0.75", "0.5", "0.25")
names(se108) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=9
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma109[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se109[[k]] <- data.frame(x[,,k][,])
    ce109[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce109) = c("0.9", "0.75", "0.5", "0.25")
names(se109) = c("0.9", "0.75", "0.5", "0.25")


# X= 10 and Y=10
set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigma1010[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    se1010[[k]] <- data.frame(x[,,k][,])
    ce1010[[k]] <- data.frame(y[,,k][,])
  }
}
names(ce1010) = c("0.9", "0.75", "0.5", "0.25")
names(se1010) = c("0.9", "0.75", "0.5", "0.25")

save(list = c("se101","ce101","se102","ce102","se103","ce103","se104","ce104","se105","ce105","se106",
              "ce106","se107","ce107","se108","ce108","se109","ce109","se1010","ce1010") , 
     file="XVaianceis10.RData")


