################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################

### Introducción

  demo(graphics)
  library(lattice)


################################################################################

### plot()

  x <- runif(50, 0, 4)
  y <- runif(50, 0, 4)

  plot(x, y, main = "Título principal",
	sub = "subtítulo",
	xlab = "x label",
	ylab = "y label", xlim = c(-1, 5),
	ylim = c(1, 5))

	abline(h = 2, lty = 1)
	abline(v = 0, lty = 2)

	text(1, 4, "Algo de texto")

	mtext("mtext", side = 1)
	mtext("mtext en side 2", side = 2,
	line = -3, cex = 2)


################################################################################

### Variaciones de plot()

  z <- cbind(x,y)

  plot(z)
  plot(y ~ x)

  plot(log(y + 1) ~ x) # transformacion de y

  plot(x, y, type = "p")
  plot(x, y, type = "l")
  plot(x, y, type = "b")

  plot(c(1,5), c(1,5))

 legend(1, 4, c("uno", "dos", "tres"), lty = 1:3,
   col = c("red", "blue", "green"),
   pch = 15:17, cex = 2)
   
#############################################


  x <- runif(50, 0, 4)
  y <- runif(50, 0, 4)
  sexo <- c(rep("v", 20), rep("m", 30))

  plot(x, y, type = "n")
  text(x, y, labels = sexo)

  
################################################################################

### Plot: pch, col

  plot(x, y, type = "n")
  points(x, y, pch = 3, col = "red")

  plot(x, y,pch=3,col="red")


# Tipos de puntos:

  plot(c(1, 10), c(1, 3), type = "n", axes = FALSE, xlab = "", ylab="")
  points(1:10, rep(3, 10), pch = 1:10, cex = 2, col = "blue")
  points(1:10, rep(2, 10), pch = 11:20, cex = 2,col = "red")
  points(1:10, rep(1, 10), pch = 21:30, cex = 2,col = "blue", bg = "yellow")


################################################################################

### Tipos de líneas:

  plot(x=c(0, 10),y=c(0, 10), type = "n", xlab ="",
   ylab ="")
  plot(c(0, 10), c(0, 10), type = "n",
   ann=FALSE)

 for(i in 1:10){
   abline(0, i/5, lty = i, lwd = 2)
 }



################################################################################

### Múltiples gráficos por ventana

  par(mfrow = c(2, 2))
  plot(rnorm(10))
  plot(runif(5), rnorm(5))
  plot(runif(10))
  plot(rnorm(10), rnorm(10))
  
  
  
################################################################################

### Identificación interactiva de datos

  x <- 1:10
  y <- sample(1:10)
  nombres <- paste("punto", x, ".",y, sep ="")
  plot(x, y)
  identify(x, y, labels = nombres)

#locator devuelve la posición de los puntos.

  plot(x, y)
# locator()
  text(locator(1), "el marcado", adj = 0)


################################################################################

### Datos multivariantes



# Una "pairwise scatterplot matrix":
  X <- matrix(rnorm(1000), ncol = 5)
  colnames(X) <- c("a", "id", "edad", "loc","weight")
  pairs(X)

# Conditioning plots(revelan, entre otros, interacciones):
  Y <- as.data.frame(X)
  Y$sexo <- as.factor(c(rep("Macho", 80),rep("Hembra", 120)))
  coplot(weight ~ edad | sexo, data = Y)
  coplot(weight ~ edad | loc, data = Y)
  coplot(weight ~ edad | loc + sexo, data = Y)

  library(lattice)
  demo(lattice)


################################################################################

### Boxplots

  Y <- as.data.frame(X)
  Y$sexo <- as.factor(c(rep("Macho", 80),
  rep("Hembra", 120)))

  attach(Y)
  boxplot(weight)
  plot(sexo, weight)
  detach()
  boxplot(weight ~ sexo, data = Y,
  col = c("red", "blue"))



  x<-rnorm(50)
  boxplot(x)
  rug(x, side=2)


################################################################################

### Jittering in scatter plots


  dc1 <- sample(1:5, 500, replace = TRUE)

  dc2 <- dc1 + sample(-2:2, 500,replace = TRUE,prob = c(1, 2, 3, 2, 1)/9)

  plot(dc1, dc2)

  plot(jitter(dc1), jitter(dc2))


################################################################################

### Histogramas

  hist(rnorm(200),col="lightblue",xlab="Presión arterial",
   main="Histograma de Prueba")

  hist(rnorm(200),col="lightblue",xlab="Presión arterial",
   main="Histograma de Prueba",breaks=100)


################################################################################

### Adición de rectas de regresión

  x <- rnorm(50)
  y <- rnorm(50)
  plot(x, y)
  lines(lowess(x, y), lty = 2)
  abline(lm(y ~ x), lty = 3)


############################################

  library(car)

  X <- matrix(rnorm(1000), ncol = 5)
  colnames(X) <- c("a", "id", "edad", "loc","weight")


  coplot(a ~ edad | loc, panel = panel.car, data = X) ???? NO FUNCIONA

  scatterplot.matrix(X, diagonal = "density")


################################################################################

### Guardando Gráficos



pdf(file = "f1.pdf", width = 8, height = 10)
plot(rnorm(10))
dev.off()


plot(runif(50))
dev.copy2eps()


################################################################################

### Márgenes con expresiones

  f<-function(x) x*(x+1)/2
  x<-1:20
  y<-f(x)
  plot(x,y,xlab="",ylab="")
  mtext("Plotting la expression",side=3,line=2.5)
  mtext(expression(y==sum(i,1,x,i)),side=3,line=0)
  mtext("La primera variable",side=1,line=3)
  mtext("La segunda variable",side=2,line=3)


################################################################################

### Series temporales

  x<-seq(0,2*pi,by=0.1)

  y<-sin(x)

  y1<-cos(x)

  plot(x,y,col="green",type="l", lwd=3)

  lines(x,y1,col="red",lwd=3)

  mtext("Seno y Coseno Plot",side=3,line=1)


################################################################################

### Añadir labels dentro del plot

  x<-rnorm(2000)

  hist(x,xlab="Peso",
  main="Cuanto pesan",col="blue")

  rect(1,0,3.81,220,border="red",lwd=4)

  text(1.8,240,"En riesgo",col="red",cex=1.25)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################