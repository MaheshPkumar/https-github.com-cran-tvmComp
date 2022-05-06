#'Calculates the required Rate of Return for a specific given amount of Present Value to become equal to that of a given amount of Future Value in given Number of Years.
#'@details
#'\insertCite{berk;textual}{tvmComp} defines rate of return as the rate at which the value of an amount invested grows to fund the future cash flow(s) of an investment. In some situations, this rate of return has to be computed when the Present Value and Future Value are known and the interest rate (or rate of return) that equates to Present and Future Value needs to be computed. \insertCite{brook;textual}{tvmComp} elaborates on the ten important points about the Time Value of Money equation belonging to a single cash flow. Here, \insertCite{brook;textual}{tvmComp} states that the equation essentially has four variables (r, n, pv and fv), and  three out of these  four variables must be known before you can solve for the missing or unknown variable. The method  \code{computingROR()} computes required rate of return (r) for a specific given amount of Present Value (pv) to become equal to that of a given amount of Future Value (fv) in given number of years (n).
#'So, \code{computingROR()} gives r (interest rate or required rate of return) when values are passed to its three arguments. Here, \code{n} is number of Years, \code{pv} is Present value and \code{fv} is Future Value.
#'The output gives value of \code{r}, the Required Rate of Return that is rounded off to three decimal places. So, an output of 0.186 means a return of 18.6 percent.
#'@param n A number.
#'@param fv A number.
#'@param pv A number.
#'@return Input values to three arguments  \code{n} , \code{pv} and \code{fv}.
#'@references
#'\insertAllCited{}
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@examples
#'computingROR(3,-1200,2000)
#'computingROR(3,1200,-2000)
#'computingROR(3, 1200,2000)
#'@export
computingROR <- function(n,pv,fv){
n <- abs(n)
pv <- abs(pv)
fv <- abs(fv)
div <- fv/pv
r <- (div^(1/n)-1)
(r = round(r, digits = 3))
}


