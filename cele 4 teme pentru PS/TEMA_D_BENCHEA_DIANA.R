#1

confidence_interval_modificat = function(alfa,filename)
{ x=read.table(filename, header = TRUE)
n=length(x$probabilitati)
sample_mean=mean(x$probabilitati)
critical_z = qnorm(1 - alfa/2)
sigma=sqrt(92.16)
a = sample_mean - critical_z*sigma/sqrt(n)
b = sample_mean + critical_z*sigma/sqrt(n)
interval = c(a, b)
return(interval)
}

confidence_interval_modificat(0.05,"probabilitati.csv")
confidence_interval_modificat(0.01,"probabilitati.csv")

#2

conf_interval_modificat=function(alfa,filename)
  {
  x=read.table(filename, header = TRUE)
  n=length(x$statistica)
  sample_mean=mean(x$statistica)
  s=sd(x$statistica)
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  interval = c(a, b)
  return(interval)
  
}

conf_interval_modificat(0.05,"statistica.csv")
conf_interval_modificat(0.01,"statistica.csv")

#3


test_proportion=function(alfa,n,succese,p,type)
{
  p_prim = succese/n
  if(type=="r")
    critical_z = qnorm(1 - alfa)
  if(type=="l")
    critical_z = qnorm(alfa)
  if(type=="s")
    critical_z = qnorm(1 - alfa/2)
  z_score = (p_prim - p)/sqrt(p*(1 - p)/n)
  return(c(z_score,critical_z))
}

test_proportion(0.01,100,14,0.15,"s")
test_proportion(0.05,100,14,0.15,"s")