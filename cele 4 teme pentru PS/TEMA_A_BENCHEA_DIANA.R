

#ex1

#a
probabilitati= function(lambda, p, n, m, k){
prob_poisson=dpois(k:m, lambda)
prob_geom<-dgeom(k:m, p)
prob_b<-dbinom(k:m, n, p)

#print("Poisson:",$prob_poisson)
#print("Geometric:",$prob_geom)
#print("BINOMIAL:",$prob_b)
cat("POISSON: ", prob_poisson, "\n", "GEOMETRIC: ", prob_geom, "\n", "B: ", prob_b, "\n")
}

probabilitati(3.5,0.4,10,7,2)

#b

rep_grafica= function(lambda, p, n, m, k){
  y = dpois(k:m, lambda)
  barplot(y, space = 0, main="POISSON", sub = " ", xlab ="axa x", ylab = "axa y")
  y = dgeom(k:m, p)
  barplot(y, space = 0, main="GEOMETRIC", sub = " ", xlab ="axa x", ylab = "axa y")
  y = dbinom(k:m, n, p) 
  barplot(y, space = 0, main="BINOMIAL", sub = " ", xlab ="axa x", ylab = "axa y")
}

rep_grafica(3.5,0.4,10,7,2)

#c

k_minim=function(lambda){
  i=0;
  k=0;
  maxim=1-10^(-6)
  while(i<=maxim)
  { i=i+dpois(k,lambda)
    if(i >= maxim)
        return(k)
    k=k+1
  }
}

k_minim(3.5)

#ex2

#a

frecvente_note = function(file_name) {
  note = read.table(file_name, header = TRUE, sep = ",")
  frecv_abs_p = as.vector(table(note$P))
  frecv_abs_s = as.vector(table(note$S))
  frecv_rel_p = frecv_abs_p / length(note$P)
  frecv_rel_s = frecv_abs_s / length(note$S)
  medie_p = mean(note$P)
  medie_s = mean(note$S)
  cat("Probabilitate:\n frecventele absolute:", frecv_abs_p, "\n frecventele relative: ", frecv_rel_p, "\n media notelor: ", medie_p, "\n")
  cat("Statistica:\n frecventele absolute:", frecv_abs_s, "\n frecventele relative: ", frecv_rel_s, "\n media notelor: ", medie_s, "\n")
}
frecvente_note("note_PS.txt")

#b

val_aberante= function(file_name, esantion_name) {
  date = read.table(file_name, header = TRUE, sep = ",")
  esantion = date[[esantion_name]]
  medie = mean(esantion)
  s = sd(esantion)
  aberante = esantion_nou = vector()
  j = k = 0
  for (i in 1:length(esantion)) {
    if (esantion[i] < medie - 2 * s || esantion[i] > medie + 2 * s) {
      j = j + 1
      aberante[j] = esantion[i]
    }
    else {
      k = k + 1
      esantion_nou[k] = esantion[i]
    }
  }
  hist(esantion_nou, breaks = seq(1, 10, by = 1), main = "distributia frecventelor", xlab = "axa x", ylab = "axa y", col = "purple")
  return(esantion_nou)
}

val_aberante("note_PS.csv", "P")