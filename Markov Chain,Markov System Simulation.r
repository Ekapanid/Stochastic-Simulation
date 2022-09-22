
#CREATING THE TRANSITION MATRIX

a<-c(1/4,0,3/4) 
b<-c(2/16,4/16,10/16) 
z<-c(1/4,1/4,1/2)  
P<-rbind(a,b,z) 


 i<-1 
 k<-1 
 j<-1  
 P<-P%*%P 
 P 
 #STUDYING ASYMPTOTIC BEHAVIOR OF TRANSITION MATRICES
 
 for (i in 1:2){ 
  for(k in 2:3){ 
     t<-0 
     for(j in 1:3){ 
       u<-min(P[i,j],P[k,j]) 
       t<-t+u 
     } 
     print(t) 
   } 
 } 
 
 P<-P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P 
 P 
 
 
# SIMULATION OF DISCRETE MARKOV CHAINS
 
 N<-5 
t<-1 
 X<-1 
 y<-0 
 
 for (t in 1:5){ 
  u<-runif(1) 
  if(u<=P[X,1]){ 
 y<-1 
  } 
  if(P[X,1]<u & u<=P[X,1]+P[X,2]){ 
   y<-2 
 } 
  if(P[X,1]+P[X,2]<u & u<=P[X,1]+P[X,2]+P[X,3]){ 
    y<-3 
 } 
  print(y)  
 X<-y 
} 

#SIMULATION OF CONTINUOUS MARKOV CHAINS

q1<-c(-5,3,2)
q2<-c(2,-2,0)
q3<-c(1,0,-1)

Q<-rbind(q1,q2,q3)

t<-0 
 X<-2 
 N<-0.5 
  
 while (t<N){ 
   e<-rexp(1,-Q[X,X]) 
   t<-t+e 
   if(t<N){ 
   u<-runif(1) 
     if(u<=P[X,1]){ 
       y<-1 
     } 
     if(P[X,1]<u & u<=P[X,1]+P[X,2]){ 
       y<-2 
     } 
     if(P[X,1]+P[X,2]<u & u<=P[X,1]+P[X,2]+P[X,3]){ 
       y<-3 
     } 
     X<-y 
     print(X) 
   } 
 } 



 

