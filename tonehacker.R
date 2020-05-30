# Ingeniería inversa de curvas RGB con R
# www.overfitting.net
# https://www.overfitting.net/2017/09/ingenieria-inversa-de-curvas-rgb-con-r.html

library(tiff)


# CÁLCULO DE CURVAS RGB

# Leemos imágenes antes/después
antes=readTIFF("mazinger1.tif", native=F, convert=F)
despues=readTIFF("mazinger2.tif", native=F, convert=F)

curva=array(0,c(256,3))

# Versión con bucles recorriendo la imagen
curvanum=array(0,c(256,3)) # Número de píxeles que han contribuido al cálculo
for (x in 1:ncol(antes)) {
  for (y in 1:nrow(antes)) {
    for (canal in 1:3) {
      inputx=round(antes[y,x,canal]*255)+1
      curva[inputx,canal]=curva[inputx,canal]+despues[y,x,canal]
      curvanum[inputx,canal]=curvanum[inputx,canal]+1
      }
    }
  }
curva=round(curva/curvanum*255)  # Promediamos y normalizamos a rango entero 0..255

# Versión vectorizada
antesint=round(antes*255)+1  # Rango entero 1..256
for (canal in 1:3) {
  for (inputx in 1:256) {
    indices=which(antesint[,,canal]==inputx)
    curva[inputx,canal]=mean(despues[,,canal][indices])  # Sintaxis matricial
    # Donde falte dato en la curva (which(is.nan(curva[]))) habrá un NaN que plot ignorará
  }
}
curva=round(curva*255)  # Normalizamos a rango entero 0..255

# Dibujamos curvas RGB
xaxis=seq(0, 255, len=256)
plot(xaxis, curva[,1], type='l', col='red',
      main="Curvas RGB", xlab="IN", ylab="OUT", ylim=c(0,255))
lines(xaxis, curva[,2], col='green')
lines(xaxis, curva[,3], col='blue')
lines(xaxis, xaxis, col='black', lty='dotted')
abline(h=255*c(0,1/4,1/2,3/4,1), v=255*c(0,1/4,1/2,3/4,1), col='black', lty='dotted')


# APLICAR CURVA A IMAGEN EXTERNA

# Aplicaremos la curva como una LUT
procesado=readTIFF("giulietta.tif", native=F, convert=F)
procesado=round(procesado*255)+1  # Rango entero 1..256

# Versión con bucles recorriendo la imagen
for (x in 1:ncol(procesado)) {
  for (y in 1:nrow(procesado)) {
    for (canal in 1:3) procesado[y,x,canal]=curva[procesado[y,x,canal],canal]
  }
}

# Versión vectorizada (genial R!)
for (canal in 1:3) procesado[,,canal]=curva[,canal][procesado[,,canal]]

writeTIFF(procesado/255, "procesado.tif", bits.per.sample=16, compression="LZW")


# HACKEAR VIRADO EN BN

despues=readTIFF("virado.tif", native=F, convert=F)  # Leemos imagen con tonos a hackear

# Generamos imagen en BN (el antes) a partir de la anterior
antes=despues
antes[,,1]=(despues[,,1]+despues[,,2]+despues[,,3])/3  # Estimamos curvas promedio alrededor de diagonal OUT=IN
antes[,,2]=antes[,,1]
antes[,,3]=antes[,,1]

# Calculamos las curvas RGB que llevan de antes a despues -> curva[]
# ...

# Aplicamos esas curvas a nuestra imagen a virar
procesado=readTIFF("floresbn.tif", native=F, convert=F)
procesado=round(procesado*255)+1  # Rango entero 1..256
for (canal in 1:3) procesado[,,canal]=curva[,canal][procesado[,,canal]]
writeTIFF(procesado/255, "floresviradas.tif", bits.per.sample=16, compression="LZW")


