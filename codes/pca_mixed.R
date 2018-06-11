# Primera prueba: 150 observaciones con 5 variables
# 1 ordinal con 3 niveles
# 4 cuantitativas

set.seed(123)

# Se crea la variable cualitativa
X <- data.frame(tipo = factor(x = sample(x  = 1:3, 
                                         size = 150, 
                                         replace = T,
                                         prob = 1:3),
                              levels = 1:3, 
                              labels = c("Tipo 1", "Tipo 2", "Tipo 3")))
# Las variables cuantitativas son las siguientes
# Para el tipo 1:
# N(-5,1)
# Beta(3,1)
# Gamma(3,1)
# Cauchy(0,1)
# Para el tipo 2:
# N(0,2)
# Beta(1,2)
# Gamma(1,7)
# Cauchy(10,5)
# Para el tipo 3:
# N(5,0.7)
# Beta(5,5)
# Gamma(2,2)
# Cauchy(-3,0.5)

# Vectores auxiliares
mu_n <- c(-5,0,5)
sigma_n <- c(1,2,0.7)
beta1 <- c(3,1,5)
beta2 <- c(1,2,5)
gamma1 <- c(3,1,2)
gamma2 <- c(1,7,2)
cauchy1 <- c(0,10,-3)
cauchy2 <- c(1,5,0.5)

# Creación de valores
x1 <- rnorm(n = 150, 
            mean = mu_n[as.numeric(X$tipo)], 
            sd = sqrt(sigma_n[as.numeric(X$tipo)]))
x2 <- rbeta(n = 150, 
            shape1 = beta1[as.numeric(X$tipo)],
            shape2 = beta2[as.numeric(X$tipo)])
x3 <- rgamma(n = 150,
             shape = gamma1[as.numeric(X$tipo)],
             scale = gamma2[as.numeric(X$tipo)])
x4 <- rcauchy(n = 150,
              location = cauchy1[as.numeric(X$tipo)],
              scale = cauchy2[as.numeric(X$tipo)])

# Juntarlo todo en un dataframe
X$V1 <- x1
X$V2 <- x2
X$V3 <- x3
X$V4 <- x4
rm("x1","x2","x3","x4")

# Lo normal es hacer continuos los valores cualitativos al 'discretizar'
Y1 <- X
Y1$tipo <- as.numeric(Y1$tipo) / length(unique(Y1$tipo))

# Para lo que sigue va a ser útil tener los siguientes vectores
# Los tipos de las observaciones
# Su conversión a numérico
# Las proporciones del tipo
tipos <- X$tipo
num_tipos <- as.numeric(tipos)
i1 <- sum(num_tipos == 1)
i2 <- sum(num_tipos == 2)
i3 <- sum(num_tipos == 3)
primal_cutoff <- (1:3)/3
prop_tipo <- sapply(sort(unique(num_tipos)), function(x) mean(num_tipos == x))
prop_cuant <- cumsum(prop_tipo)

# Idea 1: utilizar proporciones como cortes cuantílicos
Y2 <- X
Y2$tipo <- prop_cuant[num_tipos]

# Idea 2: utilizar valores aleatorios entre cortes
p1 <- numeric(150)
p2 <- numeric(150)
for(i in 1:150){
    p1[i] <- runif(n = 1, 
                   min = ifelse(num_tipos[i] == 1, 
                                0, 
                                primal_cutoff[num_tipos[i]-1]),
                   max = primal_cutoff[num_tipos[i]])
    p2[i] <- runif(n = 1, 
                   min = ifelse(num_tipos[i] == 1, 
                                0, 
                                prop_cuant[num_tipos[i]-1]),
                   max = prop_cuant[num_tipos[i]])
}
Y3 <- X
Y4 <- X
Y3$tipo <- p1
Y4$tipo <- p2
rm("p1","p2")

# Idea 3: hacer un ranking por medio de distancias
# Construir matriz con distancias bajo normas 1 y 2 con y sin pesos
Z <- as.matrix(X[,2:5])
singular_values <- svd(x = Z)$d
pesos <- singular_values / sum(singular_values)
mu1 <- colMeans(x = Z[num_tipos == 1,])
mu2 <- colMeans(x = Z[num_tipos == 2,])
mu3 <- colMeans(x = Z[num_tipos == 3,])
norma1 <- function(x, w = rep(1, length(x))){
    sum(abs(x) * w)
}
norma2 <- function(x, w = rep(1, length(x))){
    sqrt(sum(x^2 * w))
}

# Construir una matriz de distancias
D1 <- data.frame(distancia1 = numeric(150),
                 distancia2 = numeric(150),
                 distancia1w = numeric(150),
                 distancia2w = numeric(150),
                 tipo = num_tipos,
                 ind = 1:150)
W <- matrix(nrow = 150, ncol = 4)
for(i in 1:150){
    if(num_tipos[i] == 1){
        W[i,] <- Z[i,] - mu1
    } else {
        if(num_tipos[i] == 2){
            W[i,] <- Z[i,] - mu2
        } else {
            W[i,] <- Z[i,] - mu3
        }
    }
}
for(i in 1:150){
    D1[i,-c(5,6)] <- c(norma1(W[i,]),
                  norma2(W[i,]),
                  norma1(W[i,], w = pesos),
                  norma2(W[i,], w = pesos))
}
rm("W","Z")
rm("mu1","mu2","mu3","pesos","singular_values")

# Determinista con primal_cutoffs
p1 <- seq(from = 0, to = primal_cutoff[1], length.out = i1)
p2 <- seq(from = primal_cutoff[1], to = primal_cutoff[2], length.out = i2)
p3 <- seq(from = primal_cutoff[2], to = primal_cutoff[3], length.out = i3)
# Sin peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(1, 6)]
N2 <- D1[D1$tipo == 2, c(1, 6)]
N3 <- D1[D1$tipo == 3, c(1, 6)]
N1$p <- p1[order(N1$distancia1, decreasing = T)]
N2$p <- p2[order(N2$distancia1, decreasing = T)]
N3$p <- p3[order(N3$distancia1, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y5 <- X
Y5$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(2, 6)]
N2 <- D1[D1$tipo == 2, c(2, 6)]
N3 <- D1[D1$tipo == 3, c(2, 6)]
N1$p <- p1[order(N1$distancia2, decreasing = T)]
N2$p <- p2[order(N2$distancia2, decreasing = T)]
N3$p <- p3[order(N3$distancia2, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y6 <- X
Y6$tipo <- x
# Con peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(3, 6)]
N2 <- D1[D1$tipo == 2, c(3, 6)]
N3 <- D1[D1$tipo == 3, c(3, 6)]
N1$p <- p1[order(N1$distancia1w, decreasing = T)]
N2$p <- p2[order(N2$distancia1w, decreasing = T)]
N3$p <- p3[order(N3$distancia1w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y7 <- X
Y7$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(4, 6)]
N2 <- D1[D1$tipo == 2, c(4, 6)]
N3 <- D1[D1$tipo == 3, c(4, 6)]
N1$p <- p1[order(N1$distancia2w, decreasing = T)]
N2$p <- p2[order(N2$distancia2w, decreasing = T)]
N3$p <- p3[order(N3$distancia2w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y8 <- X
Y8$tipo <- x

# Determinista con proporciones cuantílicas
p1 <- seq(from = 0, to = prop_cuant[1], length.out = i1)
p2 <- seq(from = prop_cuant[1], to = prop_cuant[2], length.out = i2)
p3 <- seq(from = prop_cuant[2], to = prop_cuant[3], length.out = i3)
# Sin peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(1, 6)]
N2 <- D1[D1$tipo == 2, c(1, 6)]
N3 <- D1[D1$tipo == 3, c(1, 6)]
N1$p <- p1[order(N1$distancia1, decreasing = T)]
N2$p <- p2[order(N2$distancia1, decreasing = T)]
N3$p <- p3[order(N3$distancia1, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y9 <- X
Y9$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(2, 6)]
N2 <- D1[D1$tipo == 2, c(2, 6)]
N3 <- D1[D1$tipo == 3, c(2, 6)]
N1$p <- p1[order(N1$distancia2, decreasing = T)]
N2$p <- p2[order(N2$distancia2, decreasing = T)]
N3$p <- p3[order(N3$distancia2, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y10 <- X
Y10$tipo <- x
# Con peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(3, 6)]
N2 <- D1[D1$tipo == 2, c(3, 6)]
N3 <- D1[D1$tipo == 3, c(3, 6)]
N1$p <- p1[order(N1$distancia1w, decreasing = T)]
N2$p <- p2[order(N2$distancia1w, decreasing = T)]
N3$p <- p3[order(N3$distancia1w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y11 <- X
Y11$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(4, 6)]
N2 <- D1[D1$tipo == 2, c(4, 6)]
N3 <- D1[D1$tipo == 3, c(4, 6)]
N1$p <- p1[order(N1$distancia2w, decreasing = T)]
N2$p <- p2[order(N2$distancia2w, decreasing = T)]
N3$p <- p3[order(N3$distancia2w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y12 <- X
Y12$tipo <- x

# Aleatorio con primal_cutoffs
p1 <- sort(runif(n = i1, min = 0, max = primal_cutoff[1]))
p2 <- sort(runif(n = i2, min = primal_cutoff[1], max = primal_cutoff[2]))
p3 <- sort(runif(n = i3, min = primal_cutoff[2], max = primal_cutoff[3]))
# Sin peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(1, 6)]
N2 <- D1[D1$tipo == 2, c(1, 6)]
N3 <- D1[D1$tipo == 3, c(1, 6)]
N1$p <- p1[order(N1$distancia1, decreasing = T)]
N2$p <- p2[order(N2$distancia1, decreasing = T)]
N3$p <- p3[order(N3$distancia1, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y13 <- X
Y13$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(2, 6)]
N2 <- D1[D1$tipo == 2, c(2, 6)]
N3 <- D1[D1$tipo == 3, c(2, 6)]
N1$p <- p1[order(N1$distancia2, decreasing = T)]
N2$p <- p2[order(N2$distancia2, decreasing = T)]
N3$p <- p3[order(N3$distancia2, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y14 <- X
Y14$tipo <- x
# Con peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(3, 6)]
N2 <- D1[D1$tipo == 2, c(3, 6)]
N3 <- D1[D1$tipo == 3, c(3, 6)]
N1$p <- p1[order(N1$distancia1w, decreasing = T)]
N2$p <- p2[order(N2$distancia1w, decreasing = T)]
N3$p <- p3[order(N3$distancia1w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y15 <- X
Y15$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(4, 6)]
N2 <- D1[D1$tipo == 2, c(4, 6)]
N3 <- D1[D1$tipo == 3, c(4, 6)]
N1$p <- p1[order(N1$distancia2w, decreasing = T)]
N2$p <- p2[order(N2$distancia2w, decreasing = T)]
N3$p <- p3[order(N3$distancia2w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y16 <- X
Y16$tipo <- x

# Aleatorio con proporciones cuantílicas
p1 <- sort(runif(n = i1, min = 0, max = prop_cuant[1]))
p2 <- sort(runif(n = i2, min = prop_cuant[1], max = prop_cuant[2]))
p3 <- sort(runif(n = i3, min = prop_cuant[2], max = prop_cuant[3]))
# Sin peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(1, 6)]
N2 <- D1[D1$tipo == 2, c(1, 6)]
N3 <- D1[D1$tipo == 3, c(1, 6)]
N1$p <- p1[order(N1$distancia1, decreasing = T)]
N2$p <- p2[order(N2$distancia1, decreasing = T)]
N3$p <- p3[order(N3$distancia1, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y17 <- X
Y17$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(2, 6)]
N2 <- D1[D1$tipo == 2, c(2, 6)]
N3 <- D1[D1$tipo == 3, c(2, 6)]
N1$p <- p1[order(N1$distancia2, decreasing = T)]
N2$p <- p2[order(N2$distancia2, decreasing = T)]
N3$p <- p3[order(N3$distancia2, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y18 <- X
Y18$tipo <- x
# Con peso
# Bajo norma 1
N1 <- D1[D1$tipo == 1, c(3, 6)]
N2 <- D1[D1$tipo == 2, c(3, 6)]
N3 <- D1[D1$tipo == 3, c(3, 6)]
N1$p <- p1[order(N1$distancia1w, decreasing = T)]
N2$p <- p2[order(N2$distancia1w, decreasing = T)]
N3$p <- p3[order(N3$distancia1w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y19 <- X
Y19$tipo <- x
# Bajo norma 2
N1 <- D1[D1$tipo == 1, c(4, 6)]
N2 <- D1[D1$tipo == 2, c(4, 6)]
N3 <- D1[D1$tipo == 3, c(4, 6)]
N1$p <- p1[order(N1$distancia2w, decreasing = T)]
N2$p <- p2[order(N2$distancia2w, decreasing = T)]
N3$p <- p3[order(N3$distancia2w, decreasing = T)]
x <- numeric(150)
for(i in N1$ind){
    x[i] <- N1[N1$ind == i, 3]
}
for(i in N2$ind){
    x[i] <- N2[N2$ind == i, 3]
}
for(i in N3$ind){
    x[i] <- N3[N3$ind == i, 3]
}
Y20 <- X
Y20$tipo <- x

rm("D1","N1","N2","N3","i","i1","i2","i3","p1","p2","p3","x")

# En resumen se crearon 20 matrices "equivalentes" a X por distintos métodos.
# Aquí un pequeño resumen
# Y1: Reescalamiento usual de los datos categóricos
# Y2: Reescalamiento con puntos de corte que dependen de la proporción de cada uno de
# los datos categóricos
# Y3: Los valores que se asignan a las clases son aleatorios en intervalos definidos
# por los cortes usuales
# Y4: Los valores que se asignan a las clases son aleatorios en intervalos definidos
# por los cortes generados por las proporciones
# Y5: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes usuales bajo la norma 1
# usual
# Y6: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes usuales bajo la norma 2
# usual
# Y7: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes usuales bajo la norma 1
# con pesos dados por ciertos valores singulares
# Y8: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes usuales bajo la norma 2
# con pesos dados por ciertos valores singulares
# Y9: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes que se obtienen por medio 
# de las proporciones bajo la norma 1 usual
# Y10: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes que se obtienen por medio 
# de las proporciones bajo la norma 2 usual
# Y11: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes que se obtienen por medio 
# de las proporciones bajo la norma 1 con pesos
# Y12: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma determinista haciendo una partición dados los cortes que se obtienen por medio 
# de las proporciones bajo la norma 2 con pesos
# Y13: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes usuales 
# bajo la norma 1 usual
# Y14: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes usuales 
# bajo la norma 2 usual
# Y15: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes usuales 
# bajo la norma 1 con pesos
# Y16: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes usuales 
# bajo la norma 2 con pesos
# Y17: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes por proporciones 
# bajo la norma 1 usual
# Y18: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes por proporciones 
# bajo la norma 2 usual
# Y19: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes por proporciones 
# bajo la norma 1 con pesos
# Y20: Los valores asignados a las observaciones dependen de una métrica, se obtienen de
# forma aleatoria con intervalos inducidos por una partición dados los cortes por proporciones 
# bajo la norma 2 con pesos
