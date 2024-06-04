#1
#a
  generareperm = function(n) 
  {
    U = runif(n, 0, 1)
    permutare = 1:n
    for (i in 2:n)
      {
        key = U[i]
        key_index = permutare[i]
        j = i - 1
        while (j >= 1 && U[j] > key) 
          {
          U[j + 1] = U[j]
          permutare[j + 1] = permutare[j]
          j = j - 1
          }
        
        U[j + 1] = key
        permutare[j + 1] = key_index
       }
  
  print(permutare)
}

generareperm(6)

#b

generaresir = function(n, k)
{
  sir_biti = list()
  for (i in 1:n) 
  {
    sir = sample(c(0,1), k, replace = TRUE)
    sir_biti[[i]] = sir
  }
  return(sir_biti)
}
generaresir(8,5)

compararelexicografica = function(Wi, Wj) 
  {
  Lij = min(length(Wi), length(Wj))
  for (x in 1:Lij) 
    {
    if (Wi[[x]]<Wj[[x]])
      return(TRUE) 
    if (Wi[[x]] > Wj[[x]])
      return(FALSE)
    }
  while (TRUE) {
    if (length(Wi) < length(Wj))
      Wi = c(Wi, sample(c(0, 1), 1))
    else if (length(Wj) < length(Wi))
      Wj = c(Wj, sample(c(0, 1), 1))
    else {
      Wi = c(Wi, sample(c(0, 1), 1))
      Wj = c(Wj, sample(c(0, 1), 1))
    }
    if (Wi[[length(Wi)]] < Wj[[length(Wj)]])
      return(TRUE)
    else if (Wi[[length(Wi)]] > Wj[[length(Wj)]])
      return(FALSE)
  }
}
W = generaresir(10, 6)
W
Wi=W[[3]]
Wj=W[[8]]
rezultat = compararelexicografica(Wi, Wj)
cat("It is ",compararelexicografica(Wi, Wj), " that the first word is bigger than the second.\n")

#c

QuickSort = function(sir) {
  if (length(sir) <= 1)
    return(sir)
  x = sample(1:length(sir), 1);
  pornire = sir[[x]];
  st = list() ;
  dr = list()
  for (i in 1:length(sir))
    if (i != x) 
      {
        if (compararelexicografica(sir[[i]], pornire) == TRUE)
          st = c(st, list(sir[[i]]))
        else
          dr = c(dr, list(sir[[i]]))
       }
  
  sorted_st = QuickSort(st)
  sorted_dr = QuickSort(dr)
  
  return(c(sorted_st, list(pornire), sorted_dr))
}
words = generare_biti(4, 8)
words
QuickSort(words)

#d


#2 #b 
##Putem creste sansele de a gasi o taiatura cat mai mare prin a rula de mai multe ori
##algoritmul din rezolvarea corecta a subpunctului a(pe care eu nu stiu sa o fac din 
##pacate), alegand apoi maximul din taiaturile gasite.
