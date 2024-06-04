#1

estimare=function(R,r,esantion){
  
 pctgen=0;
 
 x1min= -R -r;
 x1max= R +r;
 x2min=x1min;
 x2max=x1max;
 x3min=-r;
 x3max=r;
 
 for (i in 1:esantion) 
 {
   x = runif(1, x1min, x1max);
   y = runif(1, x2min, x2max);
   z = runif(1, x3min, x3max);
   if ((z^2 + (sqrt(x^2 + y^2) - R)^2) < r^2)
      pctgen = pctgen + 1;
 }
 volcub=((R+r)^2)*r;
 volum=(pctgen/esantion)*volcub;
 return(volum);
 
}

erori=function(R,r,esantion)
{
  volreal= 2*pi^2*R*r^2;
  volestimat=estimare(R,r,esantion);
  eroare=abs(volestimat-volreal)/volreal*100;
  print(eroare);
}
estimare(10,3,1000);
estimare(10,3,2000);
estimare(10,3,5000);

erori(10,3,1000);
erori(10,3,2000);
erori(10,3,5000);

#2
arie_triunghi = function(a, b, c, d, esantion) {
pctgen = 0
for (i in 1:esantion) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    
    if (y >= 0 && y <= 2*x && y <= 6 - 3*x)
      pctgen = pctgen + 1
  }
  arie = (pctgen / esantion) * ((b - a)  * (d - c))
  return(arie)
}

arie_triunghi(0, 10, 0, 20, 20000)

#3
#a

integrala1 = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    sum=sum+(2*x-1)/(x*x-x-6)
  }
  return(2*sum/N);
}
integrala1_average= function(k, N) {
  estimates = vector();
  for(i in 1:k)
    estimates[i] = integrala1(N);
  cat(mean(estimates)," ,valoare reala ", log(3)-log(2));
}
integrala1_average(30,20000);

integrala2 = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, 3, 11);
    sum=sum+(x+4)/(x-3)^(1/3);
  }
  return(8*sum/N);
}
integrala2_average= function(k, N) {
  estimates = vector();
  for(i in 1:k)
    estimates[i] = integrala2(N);
  cat(mean(estimates)," ,valoare reala", 61.2);
}
integrala2_average(30,20000);

integrala3 = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, 0, 1000);
    sum=sum+x*exp(-x^2);
  }
  return(1000*sum/N);
}
integrala3_average= function(k, N) {
  estimates = vector();
  for(i in 1:k)
    estimates[i] = integrala3(N);
  cat(mean(estimates)," ,valoare reala", 0.5);
}
integrala3_average(30,20000);

#4

#a.
iSocialize15k=function(n,p,q)
{utilizatori=10000;
ani=0;
while(utilizatori<15000&&ani<=100)
  {utilizatori=dbinom(1,n,p)+dbinom(1,utilizatori,1-q);
  ani=ani+1;}
if(ani==101)
  print("mai mult de un secol")
else 
  return(ani);
}
iSocialize15k(1000,0.25,0.01);

#b 
utilizatoriprezenti=function(deadline){
  utilizatori=10000;
  ani=0;
  while(utilizatori<15000&&ani<=deadline)
  {utilizatori=dbinom(1,1000,0.25)+dbinom(1,utilizatori,1-0.01);
  ani=ani+1;}
  return (utilizatori)
}
probabilitateisocial = function(N) {
  for(i in 1:N) {
    nr=utilizatoriprezenti(40);
    nr=dbinom(1,10000/12*10,0.25/12*10)+dbinom(1,10000/12*10,1-0.01/12*10);
    if(nr> 15000) 
    s = s + 1;
  }
  return(s/N);
}
probabilitateisocial(5000)

