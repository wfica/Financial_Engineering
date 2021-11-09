pay_off = function(S, K, czy_call = T) if(czy_call) pmax(S - K, 0) else pmax(K - S, 0)

delta = function(v, dS) (v[1] - v[3]) / (2 * dS)
gamma = function(v, dS) (v[1] - 2 * v[2] + v[3])  / dS^2

wycena = function(v, dt, dS, S, r, zmiennosc_roczna){
  d = delta(v, dS)
  g = gamma(v, dS)
  if (length(zmiennosc_roczna) != 1){
    if (g < 0) zmiennosc_roczna = zmiennosc_roczna[1]
    else zmiennosc_roczna = zmiennosc_roczna[2]
  }
  a = zmiennosc_roczna^2 * S^2 / 2
  b = r * S
  c = -r
  return(v[2] + (a * g + b * d + c * v[2]) * dt)
}

#wycena(c(5,0,0), 0.0001, 5, 2149, 0.015, c(0.15,0.25))
#wycena(c(0,2399,2398), 0.01, 1, 2399, 0.015, 0.2)



finite_diference_EU_call = function(dS, dt, t = 0.83, K, r, zmiennosc_roczna, bariera){
  V = function(v, S) wycena(v = v, dt = dt, dS = dS, S = S, r = r, zmiennosc_roczna = zmiennosc_roczna)
  
  S_v = seq(0, bariera, dS)
  t_v = seq(0, t, dt)
  n_S = length(S_v)
  n_t = length(t_v)
  siatka = matrix(0, n_S, n_t)
  siatka[, n_t] = pay_off(S = S_v, K = K, czy_call = T)
  siatka[n_S, n_t] = 0
  for (j in (n_t - 1):1) {
    for (i in (n_S - 1):2) {
      siatka[i, j] = V(siatka[(i + 1):(i - 1), j + 1], S_v[i])
      if (sum(siatka[i:(i + 1), j + 1]) == 0) break
    }
  }
  row.names(siatka) = S_v
  return(siatka)
}
# dt <= 1/(zmiennosc_roczna^2*bariera^2)
# dS <= 2a/abs(b)

a = finite_diference_EU_call(dS = 5, dt = 0.0001, K = 2150, r = 0.015, zmiennosc_roczna = 0.2, bariera = 2400, t = 0.83)
a[which(row.names(a) == 2175), 1]


finite_diference_EU_put = function(dS, dt, t = 0.83, K, r, zmiennosc_roczna, bariera){
  V = function(v, S) wycena(v = v, dt = dt, dS = dS, S = S, r = r, zmiennosc_roczna = zmiennosc_roczna)
  
  S_v = seq(bariera, K * 3, dS) #chyba cena wykonania * 3 miaa byæ
  t_v = seq(0, t, dt)
  n_S = length(S_v)
  n_t = length(t_v)
  siatka = matrix(0, n_S, n_t)
  siatka[, n_t] = pay_off(S = S_v, K = K, czy_call = F)
  siatka[1, n_t] = 0
  for (j in (n_t - 1):1) {
    for (i in (2:(n_S - 1))) {
      siatka[i, j] = V(siatka[(i + 1):(i - 1), j + 1], S_v[i])
      if (sum(siatka[i:(i+1), j + 1]) == 0) break
    }
  }
  row.names(siatka) = S_v
  return(siatka)
}

a = finite_diference_EU_put(dS = 50, dt = 0.00001, K = 2150, r = 0.015, zmiennosc_roczna = 0.2, bariera = 1900, t = 0.83)
a[which(row.names(a) == 2000), 1]
