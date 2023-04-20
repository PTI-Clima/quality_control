# Author: Miguel Tomas Burguera <http://www.eead.csic.es/home/staffinfo?Id=459>; Erosion, and Soil and Water Evaluation , EEAD, CSIC <http://www.eead.csic.es>. Fergus Reig Gracia <http://fergusreig.es/>; Environmental Hydrology, Climate and Human Activity Interactions, Geoenvironmental Processes, IPE, CSIC <http://www.ipe.csic.es/hidrologia-ambiental/>
# Version: 1.0

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/> <http://www.gnu.org/licenses/gpl.txt/>.
#####################################################################

## Funciones que se llaman desde Funciones_QC (no desde controles generales)

##############################################################
#### FUNCIONES PARA DETECCION Y TRATAMIENTO DE DUPLICADOS ####
##############################################################

#' funcion para detectar duplicados
#'
#' @param datos matriz de datos
#' @param metadatos matriz de metadatos de los datos
#' @param var tipo de datos (tmin, pr...)
#' @param data_source fuente de los datos (AEMET o SIAR)
#'
#' @return duplicados es una lista que contiene 4 elementos. Los duplicados a 28, 29, 30 y 31 dias; metadatos_meses_duplicados_detectados vale '1' en duplicados, '0' en originales, 'NA' en no disponibles
#' @export
#'
#' @examples
deteccion_duplicados <-
  function(datos, metadatos, var, data_source = C_AEMET) {
    if ((var == C_HR | var == C_W) & data_source != C_SIAR) {
      for (ndias in 28:31) {
        dupl <- buscar_duplicados(datos, metadatos, nz = 30, n = ndias * 4, var)
        datos_duplicados <- dupl$dupl
        metadatos <- dupl$meta
        dupl <- agrupar_duplicados(datos_duplicados, n = ndias * 4)
        if (ndias == 31) {
          duplicados_31 <- dupl
        }
        if (ndias == 30) {
          duplicados_30 <- dupl
        }
        if (ndias == 29) {
          duplicados_29 <- dupl
        }
        if (ndias == 28) {
          duplicados_28 <- dupl
        }
      }
    }
    if (var == C_MAX |
        var == C_MIN |
        var == C_INS | var == C_RA | var == C_PP | data_source == C_SIAR) {
      for (ndias in 28:31) {
        dupl <- buscar_duplicados(datos, metadatos, n = ndias, var = var)
        datos_duplicados <- dupl$dupl
        metadatos <- dupl$meta
        dupl <- agrupar_duplicados(datos_duplicados, n = ndias)
        if (var == C_PP &&
            (length(dupl) > 1 || !is.null(dupl[[1]]))) {
          #CORREGIDO_MTOMAS
          for (i in 1:length(dupl)) {
            y <- dupl[[i]]
            for (j in 1:dim(y)[1]) {
              y[j, ] <-
                datos[c(which(
                  datos$INDICATIVO == y[j, 'INDICATIVO'] &
                    datos$YEAR == y[j, 'YEAR'] & datos$MES == y[j, 'MES']
                )), ]
            }
            dupl[[i]] <- y
          }
        }
        
        if (ndias == 31) {
          duplicados_31 <- dupl
        }
        if (ndias == 30) {
          duplicados_30 <- dupl
        }
        if (ndias == 29) {
          duplicados_29 <- dupl
        }
        if (ndias == 28) {
          duplicados_28 <- dupl
        }
      }
    }
    c <-
      which(
        duplicados_28 %in% duplicados_31 |
          duplicados_28 %in% duplicados_30 |
          duplicados_28 %in% duplicados_29
      )
    duplicados_28 <- duplicados_28[-c]
    
    c <-
      which(duplicados_29 %in% duplicados_30 |
              duplicados_29 %in% duplicados_31)
    duplicados_29 <- duplicados_29[-c]
    
    c <- which(duplicados_30 %in% duplicados_31)
    duplicados_30 <- duplicados_30[-c]
    
    c <- which(unlist(lapply(duplicados_31, is.null)))
    if (length(c) > 0) {
      duplicados_31 <- duplicados_31[-c]
    }
    duplicados <-
      list(duplicados_28, duplicados_29, duplicados_30, duplicados_31)
    
    return(list(duplicados = duplicados, metadatos = metadatos))
  }

#' busca los meses duplicados entre dia 1 y dia 'n', meses duplicados pero que no tienen el mismo número de dias.
#'
#' @param data matriz de datos
#' @param meta matriz de metadatos de los datos
#' @param nz dias con dato necesarios para hacer estas comprobaciones.
#' @param n numero de dias que queremos comprobar (de 28 a 31, para comprobar todos los meses posibles)
#' @param var tipo de dato (tmin, pr...)
#'
#' @return lista de matriz de datos y de metadatos
#' @export
#'
#' @examples
buscar_duplicados <- function(data, meta, nz = 10, n, var) {
  data_2 <- data
  
  datos <- rowSums(!is.na(data[, 4:ncol(data)]))
  if (var == C_PP) {
    data_2[, 4:34] <- t(apply(
      data[, 4:34],
      1,
      FUN = function(z) {
        datos <- sum(!is.na(z))
        if (datos > 25 & datos < 31) {
          z[is.na(z)] <- 0
        }
        return(z)
      }
    ))
  }
  ceros <- rowSums(data[, 4:(n + 3)] > 0, na.rm = T)
  acumulado <- rowSums(data[, 4:(n + 3)], na.rm = T)
  w <- duplicated(data_2[, 4:(n + 3)])
  ww <- duplicated(data_2[, 4:(n + 3)], fromLast = TRUE)
  
  if (var == C_PP) {
    a <- which(datos > nz & ceros > 2 & acumulado > 50 & (w == T |
                                                            ww == T))
  } else if (var == C_W) {
    a <- which(datos > nz & ceros > 20 & (w == T | ww == T))
  } else {
    a <- which(datos > nz & (w == T | ww == T))
  }
  
  dupl <- data_2[a, ]
  
  ## se marca el mes completo como duplicado
  meta[a, 4:ncol(data)] <- 1
  return(list(dupl = dupl, meta = meta))
}

#' crea una lista de 4 posiciones (28, 29, 30 y 31 días) que tiene valores: '2' (eliminar), '3' (conservar), '4' (conservar por cercanía espacial) eliminar contiene las filas de los elementos que se tienen que eliminar
#'
#' @param datos matriz de datos
#' @param duplicados
#' @param metadatos matriz de metadatos de los datos
#' @param dist matriz de distancias
#' @param var tipo de dato (tmin, pr...)
#' @param data_source matriz de metadatos de los datos
#'
#' @return lista de matrices de datos a eliminar y metadatos
#' @export
#'
tratamiento_duplicados <-
  function(datos,
           duplicados,
           metadatos,
           dist,
           var,
           data_source = C_AEMET) {
    if (var == C_MIN |
        var == C_MAX | var == C_INS | var == C_PP | data_source == C_SIAR) {
      ndias <- c(28:31)
    } else{
      if (var == C_HR | var == C_W) {
        ndias <- c((28:31) * 4)
      }
    }
    metadatos_final <- list(metadatos, metadatos, metadatos, metadatos)
    names(metadatos_final) <- c('28', '29', '30', '31')
    for (i in 1:4) {
      metadatos_final[[i]] <-
        tratamiento_unicos(x = datos, s = duplicados[[i]], b = metadatos, d = dist, n = ndias[i])
    }
    
    ## detectamos los meses que se tienen que eliminar.
    w <-
      which(
        metadatos_final[[1]] == 2 |
          metadatos_final[[2]] == 2 | metadatos_final[[3]] == 2 |
          metadatos_final[[4]] == 2,
        arr.ind = T
      )
    w <- w[-c(which(w[, 2] < 4)), ]
    w <- unique(w[, 1])
    
    return(list(eliminar = w, metadatos = metadatos_final))
  }

#' funcion para decdidir que hacemos con duplicados. ¿Borramos o conservamos por distancia?
#'
#' @param x matriz de datos
#' @param s
#' @param b matriz de metadatos de los datos
#' @param d matriz de distancias
#' @param n 1, 2, 3 o 4 que equivalen a '28', '29', '30' o '31' días
#' @param cons TRUE para eliminar duplicados consecutivos
#'
#' @return matriz de metadatos de los datos
#' @export
#'
tratamiento_unicos <- function(x, s, b, d, n, cons = F) {

  th_dist = 1000
  # En cada posicion de la lista 's' guardamos los meses duplicados
  if (length(s) > 0) {
    for (i in 1:length(s)) {
      y <- s[[i]]
      y <- y[c(order(y$YEAR, y$MES)), ]
      
      if (length(unique(y$INDICATIVO)) == 1) {
        conservar <- vector(mode = 'logical', length = dim(y)[1])
        
        # Comprobamos si las duplicidades se producen entre meses consecutivos
        if (cons == T) {
          for (j in 1:(dim(y)[1] - 1)) {
            for (k in (j + 1):dim(y)[1]) {
              if ((y[j, 'YEAR'] == y[k, 'YEAR'] & ((y[j, 'MES'] + 1) == y[k, 'MES'])) |
                  (((y[j, 'YEAR'] + 1) == y[k, 'YEAR']) &
                   (y[j, 'MES'] == 12) & (y[k, 'MES'] == 1))) {
                conservar[j] <- T
              }
            }
          }
          # Si hay dos o mas T consecutivos hay que convertirlo en False
          w <- which(conservar == T)
          if (length(w) > 1) {
            for (j in 1:(length(w) - 1)) {
              if (w[j] + 1 == w[j + 1]) {
                conservar[w[j]] <- conservar[w[j + 1]] <- F
              }
            }
          }
        }
        # Marcamos para eliminar '2' o conservar '3'
        for (j in 1:dim(y)[1]) {
          if (!conservar[j]) {
            b[c(which(b$YEAR == y[j, 'YEAR'] & b$MES == y[j, 'MES'] &
                        b$INDICATIVO == y[j, 'INDICATIVO'])), 4:ncol(b)] <-
              2
          } else {
            b[c(which(b$YEAR == y[j, 'YEAR'] & b$MES == y[j, 'MES'] &
                        b$INDICATIVO == y[j, 'INDICATIVO'])), 4:ncol(b)] <-
              3
          }
        }
        
      } else {
        # Si pertenecen a distintas estaciones
        conservar <- vector(mode = 'logical', length = dim(y)[1])
        
        for (k in 1:(dim(s[[i]])[1] - 1)) {
          for (l in (k + 1):dim(s[[i]])[1]) {
            if ((y[k, 'INDICATIVO'] == y[l, 'INDICATIVO']) & (cons == T)) {
              if ((y[k, 'YEAR'] == y[l, 'YEAR'] & ((y[k, 'MES'] + 1) == y[l, 'MES'])) |
                  (((y[k, 'YEAR'] + 1) == y[l, 'YEAR']) &
                   (y[k, 'MES'] == 12) & (y[l, 'MES'] == 1))) {
                conservar[k] <- T
                b[c(which(b$YEAR == y[k, 'YEAR'] &
                            b$MES == y[k, 'MES'] &
                            b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
                  3
              }
            }
            if (y[k, 'INDICATIVO'] != y[l, 'INDICATIVO']) {
              if ((y[k, 'YEAR'] == y[l, 'YEAR']) &
                  (y[k, 'MES'] == y[l, 'MES']) &
                  (d[c(which(d$INDICATIVO == as.character(y[k, 'INDICATIVO']))),
                     as.character(y[l, 'INDICATIVO'])] <= th_dist)) {
                conservar[k] <- T
                conservar[l] <- T
                b[c(which(b$YEAR == y[k, 'YEAR'] &
                            b$MES == y[k, 'MES'] &
                            b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
                  4
                b[c(which(b$YEAR == y[l, 'YEAR'] &
                            b$MES == y[l, 'MES'] &
                            b$INDICATIVO == y[l, 'INDICATIVO'])), 4:ncol(b)] <-
                  4
              }
              if ((y[k, 'YEAR'] != y[l, 'YEAR']) &
                  (y[k, 'MES'] != y[l, 'MES']) &
                  (d[c(which(d$INDICATIVO == as.character(y[k, 'INDICATIVO']))),
                     as.character(y[l, 'INDICATIVO'])] >= 10000)) {
                conservar[k] <- T
                conservar[l] <- T
                b[c(which(b$YEAR == y[k, 'YEAR'] &
                            b$MES == y[k, 'MES'] &
                            b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
                  4
                b[c(which(b$YEAR == y[l, 'YEAR'] &
                            b$MES == y[l, 'MES'] &
                            b$INDICATIVO == y[l, 'INDICATIVO'])), 4:ncol(b)] <-
                  4
              }
            }
          }
        }
        
        
        for (k in 1:(dim(s[[i]])[1])) {
          if (!conservar[k]) {
            b[c(which(b$YEAR == y[k, 'YEAR'] & b$MES == y[k, 'MES'] &
                        b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
              2
          }
        }
      }
    }
    b[, 1] <- as.character(b[, 1])
  }
  return(b)
}

#' funcion para agrupar los duplicados. ¿Quien está duplicado con quien?
#'
#' @param data matriz de datos
#' @param n posición final de periodo (depende de número de días del mes y de si tenemos varios datos diarios o solo uno)
#' @param m posición de inicio del periodo
#'
#' @return lista de matrices de duplicados
#' @export
#'
#' @examples
agrupar_duplicados <- function(data, n = 31, m = 1) {
  
  s <- list(NULL)
  if (dim(data)[1] > 0) {
    rownames(data) <- 1:dim(data)[1]
    
    unicos <- which(!duplicated(data[, (m + 3):(n + 3)]))
    unicos <- data[unicos, ]
    
    for (i in 1:dim(unicos)[1]) {
      s[[i]] <-
        join(
          data,
          unicos[i, 1:(n + 3)],
          by = colnames(data)[(m + 3):(n + 3)],
          type = "inner",
          match = DS_ALL
        )[, 1:ncol(data)]
    }
  }
  return(s)
}



#' detecta decenas duplicadas dentro de los meses de datos
#'
#' @param data matriz de datos
#' @param metadatos matriz de metadatos de los datos
#' @param var tipo de datos (tmin, pr...)
#' @param data_source fuente de los datos (AEMET, SIAR)
#'
#' @return lista de matriz de metadatos y una lista de duplicados por decenas de días del mes
#' @export
#'
#' @examples
deteccion_decenas_duplicadas <-
  function(data, metadatos, var, data_source = C_AEMET) {
    for (i in 1:3) {
      dupl <-
        decadal_duplicated(data = data,
                           meta = metadatos,                           
                           var = var, dec = i,
                           data_source = data_source)

      datos_duplicados <- dupl$dupl
      metadatos <- dupl$meta
      
      if ((var == C_HR | var == C_W) & data_source != C_SIAR) {
        ini <- c(1, 41, 81)
        fin <- c(40, 80, 124)
      } else {
        ini <- c(1, 11, 21)
        fin <- c(10, 20, 31)
      }
      if (i == 1) {
        dupl_1 <- agrupar_duplicados(datos_duplicados, n = fin[i], m = ini[i])
      }
      if (i == 2) {
        dupl_2 <- agrupar_duplicados(datos_duplicados, n = fin[i], m = ini[i])
      }
      if (i == 3) {
        dupl_3 <- agrupar_duplicados(datos_duplicados, n = fin[i], m = ini[i])
      }
    }
    
    ## controlamos que sea el mismo mes, el mismo año o la misma estacion
    dupl_1 <- filter_dec_dupl(dupl_1)
    dupl_2 <- filter_dec_dupl(dupl_2)
    dupl_3 <- filter_dec_dupl(dupl_3)
    
    dec_dupl <- list(dupl_1, dupl_2, dupl_3)
    return(list(metadatos = metadatos, dec_dupl = dec_dupl))
  }

#' detectar decenas climaticas duplicadas
#'
#' @param data matriz de datos
#' @param meta matriz de metadatos de los datos
#' @param var tipo de datos (tmin, pr...)
#' @param dec si busca en la 1, 2 o 3 decena del mes
#' @param data_source fuente de los datos (AEMET, SIAR)
#'
#' @return lista de matrices de duplicados y metadatos
#' @export
#'
#' @examples
decadal_duplicated <-
  function(data,
           meta,
           var,
           dec = 1,
           data_source = C_AEMET) {
    if ((var == C_HR | var == C_W) & data_source != C_SIAR) {
      a <- c(4, 44, 84)
      b <- c(43, 83, 127)
      
    } else{
      a <- c(4, 14, 24)
      b <- c(13, 23, 34)
    }
    
    ini <- a[dec]
    fin <- b[dec]
    
    ## Buscamos los meses duplicados entre dia 1 y dia 10
    w <- duplicated(data[, ini:fin])
    ww <- duplicated(data[, ini:fin], fromLast = TRUE)
    
    notnas <- rowSums(!is.na(data[, ini:fin]))
    
    duplicados <- which(notnas > 4 & (w == T | ww == T))
    
    if (var == C_W) {
      zeros <- rowSums(data[, ini:fin] != 0, na.rm = T)
      duplicados <- which(notnas > 4 & zeros > 10 & (w == T | ww == T))
    }
    if (var == C_PP) {
      zeros <- rowSums(data[, ini:fin] != 0, na.rm = T)
      acumulado <- rowSums(data[, ini:fin], na.rm = T)
      # suppressWarnings: Warning cuando todos los valores son NAs, es un caso concido, se elimina el warning para no molestar por pantalla
      maxim <- suppressWarnings(apply(data[, ini:fin], 1, max, na.rm = T))
      duplicados <-
        which(notnas > 3 &
                zeros > 2 & acumulado > 50 & maxim > 50 & (w == T | ww == T))
    }
    
    if (var == C_INS) {
      zeros <- rowSums(data[, ini:fin] != 0, na.rm = T)
      dif <- apply(
        data[, ini:fin],
        1,
        FUN = function(z) {
          if (sum(!is.na(z)) > 2) {
            return(max(z, na.rm = T) - min(z, na.rm = T))
          } else {
            return(0)
          }
          
        }
      )
      duplicados <- which(notnas > 4 &
                            zeros > 2 & dif > 5 & (w == T | ww == T))
    }
    
    dupl <- data[duplicados, ]
    meta[duplicados, ini:fin] <- 1
    
    return(list(dupl = dupl, meta = meta))
  }

#' filtrar decenas duplicadas. Si la deteccion es entre estaciones diferentes, tiene que ser mismo mes y mismo año
#'
#' @param dat lista de matrices de duplicados
#'
#' @return lista de matrices de duplicados que cumplen la condición
#' @export
#'
#' @examples
filter_dec_dupl <- function(dat) {
  w <- NULL
  for (i in 1:length(dat)) {
    if (length(unique(dat[[i]]$INDICATIVO)) == 1) {
      w <- c(w, i)
    } else {
      if (length(unique(dat[[i]]$YEAR)) == 1 &
          length(unique(dat[[i]]$MES)) == 1) {
        w <- c(w, i)
      }
    }
  }
  dat <- dat[w]
  return(dat)
}

#' tratar decadas duplicadas. Si tenemos una decena duplicada solo eliminamos esa decena, si tenemos
#' dos decenas duplicadas, borramos ambas
#'
#' @param s lista de datos duplicados
#' @param b matriz de metadatos de los datos
#' @param dec encontradas 1 decena duplicada o 2 decenas duplicadas en el mes
#' @param m posición de inicio
#' @param n posición de fin (según númoer de datos diarios y numero de días del mes)
#'
#' @return matriz de metadatos de los datos
#' @export
#'
#' @examples
tratamiento_decadas <- function(s, b, dec, m = 1, n = 10) {
  if (dec == 1) {
    if (length(s) > 0) {
      for (i in 1:length(s)) {
        y <- s[[i]]
        for (j in 1:dim(y)[1]) {
          b[b$INDICATIVO == y[j, 'INDICATIVO'] &
              b$YEAR == y[j, 'YEAR'] & b$MES == y[j, 'MES'], (m + 3):(n + 3)] <-
            5
        }
      }
    }
  }
  if (dec == 2) {
    if (length(s) > 0) {
      for (i in 1:length(s)) {
        y <- s[[i]]
        for (j in 1:dim(y)[1]) {
          b[b$INDICATIVO == y[j, 'INDICATIVO'] &
              b$YEAR == y[j, 'YEAR'] & b$MES == y[j, 'MES'], 4:ncol(b)] <- 2
        }
      }
    }
  }
  return(b)
}

#' devuelve '2' el mes tiene dos decenas duplicadas, hay que eliminar el mes entero. '5' el mes tiene una única decena duplicada. Hay que eliminar únicamente esa decena.
#'
#' @param dec_dupl lista de datos duplicados
#' @param metadatos matriz de metadatos de los datos
#' @param var tipo de datos (tmin, pr...)
#' @param data_source fuente de los datos (AEMET, SIAR)
#'
#' @return matriz de metadatos
#' @export
#'
#' @examples
tratar_decenas_duplicadas <-
  function(dec_dupl, metadatos, var, data_source = C_AEMET) {
    ## eliminamos los meses que tienen las 3 decenas duplicadas
    w <- which(unlist(lapply(dec_dupl, length)) != 0)
    if (length(w) == 3) {
      for (j in 1:3) {
        del <- NULL
        for (i in 1:length(dec_dupl[[j]])) {
          z <- dec_dupl[[j]][[i]][, 4:ncol(metadatos)]
          w <- which(duplicated(z) == T)
          if (length(w) > 0) {
            del <- c(del, i)
          }
        }
        if (length(del) > 0) {
          dec_dupl[[j]] <- dec_dupl[[j]][-c(del)]
        }
      }
    }
    
    ## Meses con dos decenas duplicadas
    w_1 <-
      which((dec_dupl[[1]] %in% dec_dupl[[2]]) |
              (dec_dupl[[1]] %in% dec_dupl[[3]]))
    w_2 <- which(dec_dupl[[2]] %in% dec_dupl[[3]])
    
    if (length(w_1) > 0) {
      metadatos <-
        tratamiento_decadas(dec_dupl[[1]][w_1], metadatos, dec = 2)
    }
    if (length(w_2) > 0) {
      metadatos <-
        tratamiento_decadas(dec_dupl[[2]][w_2], metadatos, dec = 2)
    }
    
    ## Meses con una unica decena duplicada
    if (length(dec_dupl[[1]]) > 0) {
      metadatos <-
        tratamiento_decadas(dec_dupl[[1]][-c(w_1)], metadatos, dec = 1)
    }
    
    if ((var == C_HR | var == C_W) & data_source != C_SIAR) {
      ini <- c(1, 41, 81)
      fin <- c(40, 80, 124)
    } else {
      ini <- c(1, 11, 21)
      fin <- c(10, 20, 31)
    }
    
    
    w_2 <-
      which((dec_dupl[[2]] %in% dec_dupl[[1]]) |
              (dec_dupl[[2]] %in% dec_dupl[[3]]))
    if (length(dec_dupl[[2]]) > 0) {
      metadatos <-
        tratamiento_decadas(dec_dupl[[2]][-c(w_2)],
                            metadatos,
                            dec = 1,
                            m = ini[2],
                            n = fin[2])
    }
    
    w_3 <-
      which((dec_dupl[[3]] %in% dec_dupl[[1]]) |
              (dec_dupl[[3]] %in% dec_dupl[[2]]))
    if (length(dec_dupl[[3]]) > 0) {
      metadatos <-
        tratamiento_decadas(dec_dupl[[3]][-c(w_3)],
                            metadatos,
                            dec = 1,
                            m = ini[3],
                            n = fin[3])
    }
    return(metadatos)
  }

#' tratamiento de meses con 25 días identicos
#'
#' @param x matriz de datos
#' @param s lista de matrices de duplicados
#' @param b matriz de metadatos
#' @param d matriz de distancias
#'
#' @return matriz de metadatos
#' @export
#'
#' @examples
tratamiento_unicos_25 <- function(x, s, b, d) {
  th_dist = 1000
  n = 31

  # En cada posicion de la lista 's' guardamos los meses duplicados
  for (i in seqf(1, length(s))) {
    y <- s[[i]]
    y <- y[c(order(y$YEAR, y$MES)), ]
    
    if (length(unique(y$INDICATIVO)) == 1) {
      conservar <- vector(mode = 'logical', length = dim(y)[1])
      
      # pasamos a eliminar los meses que no hay que conservar
      for (j in 1:dim(y)[1]) {
        b[c(which(b$YEAR == y[j, 'YEAR'] & b$MES == y[j, 'MES'] &
                    b$INDICATIVO == y[j, 'INDICATIVO'])), 4:ncol(b)] <-
          2
      }
      
    } else {
      # Si pertenecen a distintas estaciones
      conservar <- vector(mode = 'logical', length = dim(y)[1])
      
      for (k in seqf(1, (dim(s[[i]])[1] - 1))) {
        for (l in (k + 1):dim(s[[i]])[1]) {
          if (y[k, 'INDICATIVO'] == y[l, 'INDICATIVO']) {
            b[c(which(b$YEAR == y[k, 'YEAR'] & b$MES == y[k, 'MES'] &
                        b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
              2
          }
          if (y[k, 'INDICATIVO'] != y[l, 'INDICATIVO']) {
            if ((y[k, 'YEAR'] == y[l, 'YEAR']) &
                (y[k, 'MES'] == y[l, 'MES']) &
                (d[c(which(d$INDICATIVO == as.character(y[k, 'INDICATIVO']))),
                   as.character(y[l, 'INDICATIVO'])] <= th_dist)) {
              conservar[k] <- T
              conservar[l] <- T
              b[c(which(b$YEAR == y[k, 'YEAR'] & b$MES == y[k, 'MES'] &
                          b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
                4
              b[c(which(b$YEAR == y[l, 'YEAR'] & b$MES == y[l, 'MES'] &
                          b$INDICATIVO == y[l, 'INDICATIVO'])), 4:ncol(b)] <-
                4
              
              different <-
                x[c(which(b$YEAR == y[k, 'YEAR'] &
                            b$MES == y[k, 'MES'] &
                            b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] -
                x[c(which(b$YEAR == y[l, 'YEAR'] &
                            b$MES == y[l, 'MES'] & b$INDICATIVO == y[l, 'INDICATIVO'])), 4:ncol(b)]
              borrar <- which(different != 0) + 3
              b[c(which(b$YEAR == y[k, 'YEAR'] & b$MES == y[k, 'MES'] &
                          b$INDICATIVO == y[k, 'INDICATIVO'])), borrar] <-
                5
              b[c(which(b$YEAR == y[l, 'YEAR'] & b$MES == y[l, 'MES'] &
                          b$INDICATIVO == y[l, 'INDICATIVO'])), borrar] <-
                5
            }
            if ((y[k, 'YEAR'] != y[l, 'YEAR']) &
                (y[k, 'MES'] != y[l, 'MES']) &
                (d[c(which(d$INDICATIVO == as.character(y[k, 'INDICATIVO']))),
                   as.character(y[l, 'INDICATIVO'])] >= 10000)) {
              conservar[k] <- T
              conservar[l] <- T
              b[c(which(b$YEAR == y[k, 'YEAR'] & b$MES == y[k, 'MES'] &
                          b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
                4
              b[c(which(b$YEAR == y[l, 'YEAR'] & b$MES == y[l, 'MES'] &
                          b$INDICATIVO == y[l, 'INDICATIVO'])), 4:ncol(b)] <-
                4
            }
          }
        }
      }
      
      for (k in seqf(1, (dim(s[[i]])[1]))) {
        if (!conservar[k]) {
          b[c(which(b$YEAR == y[k, 'YEAR'] & b$MES == y[k, 'MES'] &
                      b$INDICATIVO == y[k, 'INDICATIVO'])), 4:ncol(b)] <-
            2
        }
      }
    }
  }
  b[, 1] <- as.character(b[, 1])
  return(b)
}

#' para detectar duplicidades entre decenas climaticas de un mismo mes; metadatos de salida vale '1' para los casos que estan duplicados, '0' para datos originales y 'NA'
#'
#' @param data matriz de datos
#' @param meta matriz de metadatos de los datos
#' @param var tipo de datos (tmin, pr...)
#' @param data_source data_source fuente de los datos (AEMET, SIAR)
#'
#' @return lista de matrices de datos duplicados y metadatos
#' @export
#'
#' @examples
intradecadal_duplicated <-
  function(data, meta, var, data_source = C_AEMET) {
    if (var == C_MIN |
        var == C_MAX |
        var == C_INS | var == C_RA | var == C_PP | data_source == C_SIAR) {
      ini <- c(4, 14, 24)
      fin <- c(13, 23, 34)
    } else{
      if (var == C_HR | var == C_W) {
        ini <- c(4, 44, 84)
        fin <- c(43, 83, 127)
      }
    }
    notnas_1 <- rowSums(!is.na(data[, ini[1]:fin[1]]))
    notnas_2 <- rowSums(!is.na(data[, ini[2]:fin[2]]))
    notnas_3 <- rowSums(!is.na(data[, ini[3]:fin[3]]))
    
    w_1 <- which(notnas_1 > 4)
    w_2 <- which(notnas_2 > 4)
    w_3 <- which(notnas_3 > 4)
    
    if (var == C_INS) {
      nonzeros_1 <- rowSums(data[, ini[1]:fin[1]] != 0)
      nonzeros_2 <- rowSums(data[, ini[2]:fin[2]] != 0)
      nonzeros_3 <- rowSums(data[, ini[3]:fin[3]] != 0)
      
      w_1 <- which(notnas_1 > 4 & nonzeros_1 > 2)
      w_2 <- which(notnas_2 > 4 & nonzeros_2 > 2)
      w_3 <- which(notnas_3 > 4 & nonzeros_3 > 2)
    }
    if (var == C_PP) {
      nonzeros_1 <- rowSums(data[, ini[1]:fin[1]] != 0)
      nonzeros_2 <- rowSums(data[, ini[2]:fin[2]] != 0)
      nonzeros_3 <- rowSums(data[, ini[3]:fin[3]] != 0)
      
      acumulado_1 <- rowSums(data[, ini[1]:fin[1]])
      acumulado_2 <- rowSums(data[, ini[2]:fin[2]])
      acumulado_3 <- rowSums(data[, ini[3]:fin[3]])
      
      w_1 <- which(notnas_1 > 4 & nonzeros_1 > 2 & acumulado_1 > 50)
      w_2 <- which(notnas_2 > 4 & nonzeros_2 > 2 & acumulado_1 > 50)
      w_3 <- which(notnas_3 > 4 & nonzeros_3 > 2 & acumulado_1 > 50)
    }
    
    dupl <- NULL
    ## Comprobamos 1a decena con la 2a decena
    w <- which(w_1 %in% w_2)
    w <- w_1[w]
    
    for (i in 1:length(w)) {
      a <- data[w[i], ini[1]:fin[1]]
      b <- data[w[i], ini[2]:fin[2]]
      c <- data[w[i], ini[3]:fin[3]]
      # suppressWarnings: Caso conocido, en un mes de 31 días el 3º periodo es más largo
      d <- suppressWarnings(rbind(as.numeric(a), as.numeric(b), as.numeric(c)))
      if (duplicated(d[1:2, ])[2] == T) {
        duplicado <- c(w[i], 1, 2)
        dupl <- rbind(dupl, duplicado)
      }
      if (duplicated(d[1:3, ])[2] == T) {
        duplicado <- c(w[i], 1, 3)
        dupl <- rbind(dupl, duplicado)
      }
      if (duplicated(d[2:3, ])[2] == T) {
        duplicado <- c(w[i], 2, 3)
        dupl <- rbind(dupl, duplicado)
      }
    }
    
    if (length(dupl) > 0) {
      for (i in 1:dim(dupl)[1]) {
        if (dupl[i, 2] == 1) {
          meta[dupl[i, 1], ini[1]:fin[1]] <- 2
        }
        if (dupl[i, 2] == 2) {
          meta[dupl[i, 1], ini[2]:fin[2]] <- 2
        }
        if (dupl[i, 3] == 2) {
          meta[dupl[i, 1], ini[2]:fin[2]] <- 2
        }
        if (dupl[i, 3] == 3) {
          meta[dupl[i, 1], ini[3]:fin[3]] <- 2
        }
      }
    }
    return(list(dupl = dupl, meta = meta))
  }


################################################################
#### FUNCIONES PARA DETECCION Y TRATAMIENTO DE CONSECUTIVOS ####
################################################################

#' obtenemos la resolucion de los datos
#'
#' @param datos matriz de datos
#' @param metadatos matriz de metadatos correspondientes a los datos
#'
#' @return matriz de metadatos
#' @export
#'
#' @examples
obtener_resolucion <- function(datos, metadatos) {
  for (i in 4:ncol(datos)) {
    ## para cada columna primero determinamos la resolucion de los datos
    resolucion <-
      aggregate(
        datos[, i],
        by = list(datos$YEAR, datos$MES),
        FUN = function(z) {
          length(which((round(z / 10, 0) - z / 10) != 0 &
                         abs(round(z / 10, 0) - z / 10) != 0.5))
        }
      )
    resolucion <-
      resolucion[c(order(resolucion$Group.1, resolucion$Group.2)), ]
    w <- which(resolucion$x > 5)
    if (length(w) > 0) {
      for (j in 1:length(w)) {
        ww <-
          which(datos$YEAR == resolucion$Group.1[w[j]] &
                  datos$MES == resolucion$Group.2[w[j]])
        www <- which(!is.na(datos[ww, i]))
        metadatos[ww[www[1:length(www)]], i] <- 1
      }
    }
  }
  return(metadatos)
}

#' deteccion de datos consecutivos iguales
#'
#' @param datos matriz de datos
#' @param metadatos matriz de metadatos de los datos
#' @param C cuantos días consecutivos buscamos, 7 o 14
#'
#' @return
#' @export
#'
#' @examples
consecutivos <- function(datos, metadatos, C = 7) {
  for (i in 4:ncol(datos)) {
    w <- rle(datos[, i])
    ww <- which(w$length > C)
    if (length(ww) > 0) {
      for (j in 1:length(ww)) {
        a <- sum(w$length[1:(ww[j] - 1)])
        metadatos[(a + 1):(a + w$length[ww[j]]), i]  <- 1
      }
    }
  }
  return(metadatos)
}

#' organizar metadatos de consecutivos
#'
#' @param metadatos_a matriz de metadatos
#' @param metadatos_b matriz de metadatos
#' @param x resolucion de los datos
#' @param var tipo de datos (tmin, pr...)
#'
#' @return matriz de metadatos
#' @export
#'
#' @examples
combinar_metadatos_consecutivos <-
  function(metadatos_a,
           metadatos_b,
           x = NULL,
           var = C_MAX) {
    if (var == C_MAX | var == C_MIN) {
      ## metadatos_consecutivos_14 <-  se tienen que eliminar todos los dias
      ## metadatos_consecuitvos_7  <-  se tienen que eliminar los dias que resolucion_datos vale '1'
      
      for (i in 4:ncol(metadatos_b)) {
        w <- which(metadatos_b[, i] == 1 | (metadatos_a[, i] == 1 &
                                              x[, i] == 1))
        if (length(w) > 0) {
          metadatos_b[w, i] <- 13
        }
      }
    }
    if (var == C_HR) {
      for (i in 4:ncol(metadatos_b)) {
        w <- which(metadatos_b[, i] == 1 | (metadatos_a[, i] == 1 &
                                              x[, i] < 95))
        if (length(w) > 0) {
          metadatos_b[w, i] <- 13
        }
      }
    }
    if (var == C_INS) {
      for (i in 4:ncol(metadatos_b)) {
        w <- which(metadatos_b[, i] == 1 | metadatos_a[, i] == 13)
        if (length(w) > 0) {
          metadatos_b[w, i] <- 13
        }
      }
    }
    return(metadatos_b)
  }

###########################################

#' comprobar que hay valores por debajo del 70% del máximo para la fecha y posición del dato
#'
#' @param datos matriz de datos
#' @param metadatos matriz de metadatos de los datos
#'
#' @return matriz de metadatos
#' @export
#'
#' @examples
comprobar_consecutivos_insolacion <- function(datos, metadatos) {
  load.data('coords_ins.RData')
  
  w <- which(metadatos == 1, arr.ind = T)
  w <- w[-c(which(w[, 2] < 4)), ]
  if (length(w) > 0) {
    stations <- colnames(metadatos)[unique(w[, 2])]
    
    DF <- NULL
    for (i in 1:length(stations)) {
      LAT <-
        as.numeric(as.character(coords[c(which(coords$INDICATIVO == stations[i])), 'LATITUD']))
      if (LAT > 10000) {
        LAT <- round(LAT / 10000)
      }
      if (LAT > 1000 & LAT < 10000) {
        LAT <- round(LAT / 100)
      }
      ww <- w[c(which(w[, 2] == unique(w[, 2])[i])), ]
      fecha <- datos[ww[, 1], 1:3]
      dato <- datos[ww[, 1], ww[1, 2]] / 10
      mes <- fecha$MES
      
      max <- maximo_teorico(mes, LAT)
      nN <- dato / max
      if (length(which(nN < 0.7)) > 0) {
        www <- which(nN < 0.7 & nN != 0)
        if (length(www) > 0) {
          for (j in 1:length(www)) {
            metadatos[ww[www[j], 1], ww[www[j], 2]] <- 13
          }
        }
      }
    }
  }
  
  return(metadatos)
}

#' Máximo teórico de horas de sol
#'
#' @param month mes
#' @param lat latitud
#' @param var tipo de datos (insolació o radiación)
#' @param dias día del mes
#'
#' @return valor
#' @export
#'
#' @examples
maximo_teorico <- function(month,
                           lat,
                           var = C_INS,
                           dias = NULL) {
  msum <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 302, 334) + 15
  J <- msum[month]
  
  if (!is.null(dias)) {
    J <- J - 15 + dias
  }
  
  # delta: solar declination, rad (1 rad = 57.2957795 deg) (eq. 1.25)
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  # dr: relative distance Earth-Sun, [] (eq. 1.24)
  dr <- 1 + 0.033 * cos(0.0172 * J)
  
  # omegas: sunset hour angle, rad (eq. 1.23)
  latr <- lat / 57.2957795
  
  ## We have only one value of J
  ## We have 'nstations' values of latr
  
  omegas <- acos(-tan(latr) * tan(delta))
  N <- 7.64 * omegas
  Ra <-
    37.6 * dr * (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) *
                   sin(omegas))
  if (var == C_INS) {
    return(N)
  }
  if (var == C_RA) {
    return(Ra)
  }
  
}

#' agrupa la matriz de duplicados
#'
#' @param x matriz de datos
#' @param a matriz de duplicados
#'
#' @return lista de matrices
#' @export
#'
#' @examples
agrupar_duplicados_offset <- function(x, a) {
  suppressPackageStartupMessages(require(plyr))
  
  if (dim(a)[1] == 0) {
    return(NULL)
  } #CORREGIDO_MTOMAS
  
  rownames(a) <- 1:dim(a)[1]
  
  unicos <- a[c(which(!duplicated(a[4:34]))), ]
  
  s <- list(NULL)
  for (i in 1:dim(unicos)[1]) {
    y <- join(a,
                        unicos[i, 1:31],
                        by = colnames(a)[4:31],
                        type = 'inner',
                        match = DS_ALL)[, 1:ncol(a)]
    for (j in 1:dim(y)[1]) {
      y[j, ] <-
        x[c(which(x$INDICATIVO == y[j, 'INDICATIVO'] &
                    x$YEAR == y[j, 'YEAR'] & x$MES == y[j, 'MES'])), ]
    }
    s[[i]] <- y
  }
  return(s)
}
