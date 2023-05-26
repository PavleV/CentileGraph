
# validity method for CentileGraph class
check_centilegraph <- function(object){

  errors <- character()

  if(length(object@cycle_day) != length(object@value)){
    msg <- "Cycle days and values must be of same length."
    errors <- c(errors,msg)

  }


  if(length(errors) == 0) TRUE else errors


}

setClass("CentileGraph", slots=list(cycle_day="numeric",value="numeric",processed="list"), validity= check_centilegraph)

#' Create a CentileGraph object
#'
#' This function initialises a CentileGraph object.
#' It requires days of cycle and value of assay (uNK score, RT-qPCR expression).
#'
#'
#' @param cycle_day Vector with days of cycle (LH+)
#' @param value Vector of raw values to centilise.
#' @return A CentileGraph object
#' @export

createCentileGraph <- function(cycle_day,value){

  new("CentileGraph",cycle_day=cycle_day,value=value,processed=list())

}
