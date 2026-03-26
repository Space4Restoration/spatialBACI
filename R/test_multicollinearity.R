#' Test multicollinearity of matching covariates
#' 
#' @description
#' The function provides a correlation plot of the (numeric) covariates, and iteratively reports (Generalized) Variance Inflation Factors of the different covariates.
#' After each iteration, the used is prompted whether a matching covariate should be omitted.
#' 
#' @export
#' 
#' @importFrom car vif
#' @importFrom corrplot corrplot
#' @importFrom stats cor as.formula lm
#' 
#' @param matching_input list with names elements "data" and "spat.ref". Output of [collate_matching_layers()].
#' @param colname.id character. Name of the column containing the unique ID of the spatial unit of analysis. Defaults to "cell" if `matching_input$spat.ref` is a SpatRaster.
#' @param colname.treatment character. Name of the column containing the treatment variable. Defaults to "treatment" if `matching_input$spat.ref` is a SpatRaster
#' @param colnames.ignore character vector. Name of columns (other than `colname.id` and `colname.treatment`) in `matching_input` to be ignored as matching covariates.
#' 
#' @returns list. Similar to `matching_input`, with optionally covariates in the data.table under "data" omitted. 

test_multicollinearity <- function(matching_input, colname.id, colname.treatment, colnames.ignore=NULL){
  
  matching_input_dt <-matching_input$data
  colnames_input <- colnames(matching_input_dt)
  
  if(is.SpatVector(matching_input$spat.ref)){
    if(missing(colname.id)) stop('test_multicollinearity - Argument "colname.id" required for SpatVector input')
    if(missing(colname.treatment)) stop('test_multicollinearity - Argument "colname.treatment" required for SpatVector input')
  } else if (is.SpatRaster(matching_input$spat.ref)){
    if(missing(colname.treatment)){
      if("treatment" %in% colnames_input){
        colname.treatment <- "treatment"
      } else {
        stop('test_multicollinearity - argument "colname.treatment" expected')
      }
    }
    if(missing(colname.id)){
      if("cell" %in% colnames_input){
        colname.id <- "cell"
      } else {
        stop('test_multicollinearity - argument "colname.id" expected')
      }
    }
    
  } else {
    stop('test_multicollinearity: "matching_input$spat.ref" must be of class Spatvector or SpatRaster')
  }
  
  matchvar_names <- colnames_input[!colnames_input %in% c(colname.treatment, colname.id, colnames.ignore)]
  
  #correlation matrix
  dt <- matching_input_dt[,..matchvar_names]
  num_dt <- dt[, which(sapply(dt, is.numeric)), with = FALSE]
  corrplot::corrplot(cor(num_dt), method=c("color"), addCoef.col = "black")
  wait <- readline(prompt = "Press <Enter> to continue.")
  
  #VIF
  elim <- 1
  elim_names <- NULL
  while(elim !=0){
    
    frm <- as.formula(paste0(colname.treatment,
                             " ~ ",
                             paste0(matchvar_names, collapse=" + ")))
    model <- lm(frm, data=matching_input_dt)
    VIF <- car::vif(model)
    
    tab_print <- cbind(VIF)
    rnames <- paste(1:length(matchvar_names),matchvar_names, sep=": ")
    rownames(tab_print) <- rnames
    
    cat('\n(Generalized) variance-inflation factors: \n')
    print(tab_print)
    
    
    elim <- readline('Select matching variable to remove, or "0" to keep all: \n')
    #add check on input value
    elim <- as.numeric(elim)
    
    if(elim!=0) {
      elim_names <- c(elim_names, matchvar_names[elim])
      matchvar_names <- matchvar_names[-elim]
    }
  }
  
  dt_out <- matching_input_dt[, setdiff(names(matching_input_dt), elim_names), with = FALSE]
  return(list(data=dt_out,
              spat.ref=matching_input$spat.ref))
  
}

