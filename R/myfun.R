##' my function
##' @param x a numeric vector
##' @param y a numeric vector
##' @export
myfun <- function(x, y) x + y

##' my derivative function
##' @param x a numeric vector
##' @param y a numeric vector
##' @export
mydfun <- function(x, y) NULL

##' Deriv2
##' @param expr expression
##' @param var variable
##' @importFrom Deriv Deriv
##' @importFrom Deriv drule
##' @export
##' @examples
##' myderiv
##' Deriv(myfun(x, y), c("x", "y"))
##' Deriv2(myfun(x, y), c("x", "y"))
##' Deriv(myfun(x, y), c("x", "y"))
Deriv2 <- function(expr, var) {
    drule[["myfun"]] <- alist(x=mydfun(x, x), y=mydfun(y, y))
    Deriv(substitute(expr), var)
}

##' my derivative object
##' @export
myderiv <- Deriv(myfun(x, y), c("x", "y"))

##' my derivative object 2
##' @export
myderiv2 <- Deriv2(myfun(x, y), c("x", "y"))
