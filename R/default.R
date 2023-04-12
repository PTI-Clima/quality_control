# Author: Fergus Reig Gracia <http://fergusreig.es/>; Environmental Hydrology, Climate and Human Activity Interactions, Geoenvironmental Processes, IPE, CSIC <http://www.ipe.csic.es/hidrologia-ambiental/>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/> <http://www.gnu.org/licenses/gpl.txt/>.
#####################################################################

# options(repos=structure(c(CRAN="YOUR FAVORITE MIRROR")))

#' Función file.copy con overwrite = TRUE y recursive = TRUE por defecto
#'
#' @param ... parámetros para la función file.copy
#' @return None
#' @export
#'
#' @examples
file.copyf <- function(...){
  file.copy(..., overwrite = TRUE, recursive = TRUE)
}

#' Función write.table con showWarnings = FALSE y recursive = TRUE por defecto y que crea el directorio
#'
#' @param x datos
#' @param file ruta
#' @param ... parámetros para la función write.table
#' @return None
#' @export
#'
#' @examples
write.tablef <- function(x, file = "", ...){
  if (!file.exists(dirname(file))) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  }
  write.table(x, file, sep = ";", ...)
}

#' Función read.table con header = TRUE y check.names = FALSE y quote = "\"" por defecto
#'
#' @param file parámetro para la función read.table
#' @param ... parámetros para la función read.table
#' @return None
#' @export
#'
#' @examples
read.tablef <- function(file, ...){
  read.table(file, sep = ";", header = TRUE, check.names = FALSE, quote = "\"", ...)
}

#' Función plot con type="l" por defecto
#'
#' @param ... parámetros para la función plot
#' @return None
#' @export
#'
#' @examples
plotf <- function(...){
  plot(..., type = "l")
}

#' Precompila el código a cargar, con idea de ganar en tiempo de ejecución
#'
#' @param file fichero a cargar
#'
#' @return None
#' @export
#'
#' @examples
sourcef <- function(file) {
  library(compiler)
  rc = gsub(".R", ".Rc", file)
  if (!file.exists(rc) | file.mtime(file) > file.mtime(rc)) {
    invisible(capture.output(cmpfile(file)))
  }
  loadcmp(rc)
}

#' Carga librerías sin mostrar mensaje error por pantalla
#'
#' @param package librería a cargar
#' @param lib.loc ruta de la librería
#'
#' @return None
#' @export
#'
#' @examples
libraryf <- function(package, lib.loc = NULL) {
  suppressPackageStartupMessages(library(
    deparse(substitute(package)),
    lib.loc = lib.loc,
    character.only = TRUE
  ))
}

#' Carga librerías sin mostrar mensaje error por pantalla
#'
#' @param package librería a cargar
#' @param lib.loc ruta de la librería

#' @return None
#' @export
#'
#' @examples
requiref <- function(package, lib.loc = NULL) {
  suppressPackageStartupMessages(require(
    deparse(substitute(package)),
    lib.loc = lib.loc,
    quietly = TRUE,
    character.only = TRUE
  ))
}

#' Carga al arrancar R el histórico de comandos
#'
#' @return None
#' @export
#'
#' @examples
.First <- function() {
  library(utils)
  if (interactive())
    try(loadhistory("~/.Rhistory"))
}

#' Guarga al cerrar R el histórico de comandos
#'
#' @return None
#' @export
#'
#' @examples
.Last <- function() {
  if (interactive())
    try(savehistory("~/.Rhistory"))
}

#' Salir de R si guardar sesión por defecto
#'
#' @param save guardar sesión actual
#' @param status parámetros para la función q
#' @param runLast parámetros para la función q
#'
#' @return None
#' @export
#'
#' @examples
s <- function(save = 'no',
              status = 0,
              runLast = TRUE) {
  q(save, status, runLast)
}

#' sum con na.rm=TRUE por defecto
#'
#' @param ... lista de valores
#' @param na.rm usar NAs
#'
#' @return suma
#' @export
#'
#' @examples
sumf <- function(..., na.rm = TRUE) {
  sum(..., na.rm = na.rm)
}

#' mean con na.rm=TRUE por defecto
#'
#' @param ... lista de valores
#' @param na.rm usar NAs
#'
#' @return media
#' @export
#'
#' @examples
meanf <- function(..., na.rm = TRUE) {
  mean(..., na.rm = na.rm)
}

#' Máximo, sin contar NAs por defecto
#'
#' @param ... lista de valores
#' @param na.rm usar NAs
#'
#' @return maximo
#' @export
#'
#' @examples
maxf <- function(..., na.rm = TRUE) {
  max(..., na.rm = na.rm)
}

#' Mínimo, sin contar NAs por defecto
#'
#' @param ... lista de valores
#' @param na.rm usar NAs
#'
#' @return mínimo
#' @export
#'
#' @examples
minf <- function(..., na.rm = TRUE) {
  min(..., na.rm = na.rm)
}

#' which con arr.ind=TRUE por defecto
#'
#' @param x parámetro para la función which
#' @param arr.ind parámetro para la función which
#' @param useNames parámetro para la función which
#'
#' @return which
#' @export
#'
#' @examples
whichf <- function(x,
                    arr.ind = TRUE,
                    useNames = TRUE) {
  which(x = x,
        arr.ind = arr.ind,
        useNames = useNames)
}

#' Taken from:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#' Muestra los objetos en memoria y sus pesos
#'
#' @param pos parámetro para pasar a la función ls
#' @param pattern buscar por un texto
#' @param order.by ordenar por...
#' @param decreasing orden decreciente
#' @param head devolver solo el head
#' @param n número de parámetros a mostrar
#'
#' @return lista de objetos
#' @export
#'
#' @examples
.ls.objects <- function(pos = 1,
                         pattern,
                         order.by,
                         decreasing = FALSE,
                         head = FALSE,
                         n = 5) {
  napply <- function(names, fn)
    sapply(names, function(x)
      fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x)
    as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(utils::object.size(x), units = "auto"))
  })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing),]
  if (head)
    out <- head(out, n)
  return(out)
}

#' Lista de objetos más pesados, 10 por defecto
#'
#' @param ... parámetros de .ls.objects
#' @param n número de parámetros a mostrar
#'
#' @return lista de objetos
#' @export
#'
#' @examples
lsos <- function(..., n = 10) {
  .ls.objects(
    ...,
    order.by = "Size",
    decreasing = TRUE,
    head = TRUE,
    n = n
  )
}

#' match (primera aparición del parámetro) sin contar NAs
#'
#' @param x parámetro a buscar
#' @param table donde buscar
#' @param nomatch parámetro para pasar a la función match
#' @param incomparables parámetro para pasar a la función match
#'
#' @return posición del parámetro
#' @export
#'
#' @examples
matchf = function(x,
                  table,
                  nomatch = NA_integer_,
                  incomparables = NULL) {
  aux = match(x, table, nomatch, incomparables)
  return(aux[!is.na(aux)])
}

#local({
#  r <- getOption("repos")
#  r["CRAN"] <- "https://cran.rediris.es"
#  options(repos = r)
#})
