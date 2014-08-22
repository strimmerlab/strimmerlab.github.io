
# Vergleich Varianzschätzer

n = 10     # Zahl der Beobachtungen
b = 100000 # Zahl der Wiederholungen in der Simulation


# Simulation
v.unbiased = double(b)
v.ml = double(b)   # maximum likelihood
v.mm = double(b)  # minimum MSE
for (i in 1:b)
{
  # wahres Modell hat Varianz=1
  y = rnorm(n, sd=1) 

  # drei verschiedene Schätzer der Varianz
  v.unbiased[i] = var(y)                     # Nenner 1/(n-1)
  v.ml[i]       = v.unbiased[i]*(n-1)/n      # Nenner 1/n
  v.mm[i]       = v.unbiased[i]*(n-1)/(n+1)  # Nenner 1/(n+1)
}

# Bias der drei Schätzer:
mean(v.unbiased) - 1   #  0.0006  (Theor. Wert: 0)
mean(v.ml) - 1         # -0.0994  (Theor. Wert: -0.1)
mean(v.mm) - 1         # -0.1813  (Theor. Wert: -0.1818)

# Varianz der drei Schätzer
var(v.unbiased)  # 0.2297    (Theor. Wert: 0.2222)
var(v.ml)        # 0.1860    (Theor. Wert: 0.18)
var(v.mm)        # 0.1537    (Theor. Wert: 01488)

# MSE der drei Schätzer
var(v.unbiased) + (mean(v.unbiased) - 1)^2  # 0.2297 (Theor. Wert: 0.2222)
var(v.ml) + (mean(v.ml) - 1)^2              # 0.1959 (Theor. Wert: 0.19)
var(v.mm) + (mean(v.mm) - 1)^2              # 0.1866 (Theor. Wert: 0.1818)






