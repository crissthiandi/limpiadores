\name{limpiar_console}
\alias{limpiar_console}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{limpiardor de console
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Funcion que limpia la cosola de Rstudio/R
trabaja con el codigo cat
}
\usage{
limpiar_console(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{}{
%%     ~~Describe \code{x} here~~
  ningun parametro
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Ninguno

Simulacion, metodo de la inversa

despeja el valor de X de la funcion de densidad y dejala en terminos de la imagen luego evalua en y= uniforme y la salida es de esa densidad

          X=F^-1(U)

Metodo de monte carlo

Sea X la variable aleatoria de interes a simular, la cual tiene
funcion de densidad f . Ademas, sea G una variable aleatoria
de la cual conocemos algun metodo para simular, con funcion
de densidad g.

Supongamos que existe un valor c tal que f (x) <= cg(x)

De aqui que f y g deben tener soportes compatibles

Notese tambien que sup{f (x)/g(x)} < c.

Generar una variable aleatoria Y con distribucion como G

Generar U distribuye U(0, 1) independiente de Y .

Si U <= f (Y )/(cg(Y )) entonces X = Y , y si no, regresar al paso 1

Notese que f (Y ) y g(Y ) son variables aleatorias, por lo que f (Y )/cg(Y )
tambien lo es.

la demo se obtiene via densidad condicional e independencia.


Binomial

Generar uniformes 0-1

c = p/(1 − p), i = 0, pr = (1 − p)^n, F = pr

Si U < F, x = i y parar

pr =[c(n−i)/(i+1)]pr, F = F + pr, i = i + 1

ir a paso 3
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
no retorna nada
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Cristhian Diaz Chable

Sabrina Guevara Cervera
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#escribe
limpiar_consola()
#la salida es vacia}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ environment }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
