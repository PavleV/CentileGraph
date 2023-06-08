
# validity method for CentileGraph class
check_centilegraph <- function(object){

  errors <- character()

  if(length(object@cycle_day) != length(object@value)){
    msg <- "Cycle days and values must be of same length."
    errors <- c(errors,msg)
  }

  if(length(errors) == 0) TRUE else errors

}

setClass("CentileGraph", slots=list(cycle_day="numeric",value="numeric",quantiles="data.frame",processed="list"), validity= check_centilegraph)

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

  new("CentileGraph",cycle_day=cycle_day,value=value,quantiles=data.frame(),processed=list())

}


#' This function calculates centiles from the days of cycle and values stores in object.
#'
#' @param obj CentileGraph object
#' @param model Distribution to calculate centiles from. Can be "beta" or "normal". Default "beta".
#'
#' @return CentileGraph object with centiles.
#' @export
#'
calculateCentile <- function(obj){

  this.centiles <- obj@value

  obj@processed[["Centiles"]] <- this.centiles

  return(obj)
}

#' Calculation of quantiles
#'
#' @param obj CentileGraph object
#' @param days_of_cycle days of cycle to calculate quantiles for.
#' @param quantiles Set of quantiles to calculate. Default c(0,0.25,0.50,0.75,1)
#' @param model Model distribution. Default "beta".
#'
#' @return Dataframe with quantiles per cycle day.
#' @export
#'
#' @examples calculateQuantiles
calculateQuantiles <- function(obj, days_of_cycle=6:12, quantiles=c(0,0.25,0.5,0.75,1), model="beta"){

  df <- data.frame(cycle_day=numeric(),quantile=numeric(),value=numeric())

  for(i in 1:length(days_of_cycle)){

    values_i <- obj@value[obj@cycle_day == days_of_cycle[i]]

    df_i <- data.frame(cycle_day=rep(days_of_cycle[i],length(quantiles)),quantile=quantiles,value=FUN.quantiles(values_i, quantiles, model=model))

    df <- rbind(df, df_i)

  }

  obj@quantiles <- df
  obj

}

