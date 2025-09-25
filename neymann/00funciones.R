#ss.rel <- function(x, r, a=0.05)
#{
#    m <- mean(x)
#    s <- sd(x)
#    N <- length(x)
#    z <- qnorm(1-a/2)
#    n <- z^2 * s^2 / ((r^2 * m^2)+(z^2 * s^2 / N))
#    ceiling(n)
#}

#ss.abs <- function(x, e, a=0.05)
#{
#    s <- sd(x)
#    N <- length(x)
#    z <- qnorm(1-a/2)
#    n <- z^2 * s^2 / (e^2 + z^2 * s^2 / N)
#    ceiling(n)
#}

#ss.rel.ney <- function(x, str, r, a=0.05)
#{
#    #Sample size for mean with relative error,
#    #stratificacion and nh of minimun variance (neyman)
#    m <- mean(x)
#    N <- length(x)
#    z <- qnorm(1-a/2)
#    Nh <- tapply(x, str, length)
#    Nh <- Nh[!is.na(Nh)] 
#    Wh <- Nh / N
#    Sh <- tapply(x, str, sd)
#    Sh <- Sh[!is.na(Sh)]
#    S2h <- tapply(x, str, var)
#    S2h <- S2h[!is.na(S2h)]
#    S_WhSh <- as.numeric(Wh %*% Sh)
#
#    n <- (Wh %*% Sh)^2 / ((m*r/z)^2 + (Wh%*%S2h / N))
#    n <- as.numeric(n)
#    nh <- Wh*Sh / S_WhSh * n
#    ceiling(nh)
#}

neyman <- function(x, str, exh, r, a=0.05) {
    # Calcula la afijacion de Neyman
	# x variable proxy, str estratos, exh estratos exhaustivos, r error relativo
	# a nivel de confinza
	# ej: neyman(x=empleo, str=list(cnae,intempleo), exh=exh, r=0.055)
	# exh debe de tener la misma estructura que los estratos y 1 si el estrato
	# es exhaustivo y 0 si no
    m <- mean(x)
    N <- length(x)
    z <- qnorm(1-a/2)
    Nh <- tapply(x, str, length)
    Wh <- Nh / N
    Sh <- tapply(x, str, sd)
    S2h <- tapply(x, str, var)

	noexh <- ifelse(exh == 0, 1, 0)
	Np <- sum(Nh*noexh)

	n <- sum(Wh*Sh*noexh)^2 / ((m*r/z)^2 * (N/Np)^2 + sum(Wh*S2h*noexh)/Np)
	nh <- Wh*Sh*noexh / sum(Wh*Sh*noexh) * n + Nh*exh
	ceiling(nh)
}

neyman2 <- function(x, str, exh, r, a=0.05) {
    # Calcula la afijacion de Neyman con dos criterios, primero el que garantiza
    # un error relativo r[1] para el estimador del global y segundo el que garantiza
    # un error relativo r[2] para cada uno de los niveles de la primera variable de
    # estratificación. Ahora r es un vector de 2 elementos.
    # Por ejem. Si estratificamos por 2 sectores de actividad y 4 niveles de empleo
    # y r es igual a c(0.03, 0.05), esta función dara los tamanos por estratos que
    # garantizan un error relativo del 3% para el estimador del global y un 5% para cada uno
    # de los 2 sectores de actividad
	# OJO: Ahora r necesita dos valores!!!!!!!
    global <- neyman(x, str, exh, r[1], a)
    parcial <- tapply(1:length(x), str[[1]], function(z) neyman(x[z], str[[2]][z], exh[1,], r[2], a), simplify=T)
    parcial <- do.call(rbind, parcial)
    resultado <- global
    resultado[] <- NA
    for (i in 1:nrow(global)) {
        for (j in 1:ncol(global)) {
            resultado[i,j] <- max(global[i,j], parcial[i,j])
        }
    }
    resultado
}
	


    
