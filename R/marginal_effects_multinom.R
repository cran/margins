#' @rdname marginal_effects
#' @importFrom prediction find_data
#' @export
marginal_effects.multinom <- 
function(model, 
         data = find_data(model, parent.frame()), 
         variables = NULL,
         type = NULL,
         eps = 1e-7, 
         varslist = NULL,
         as.data.frame = TRUE,
         ...) {
    
    # identify classes of terms in `model`
    if (is.null(varslist)) {
        varslist <- find_terms_in_model(model, variables = variables)
    }
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    # add discrete differences for logical terms
    out1 <- lapply(c(varslist$nnames, varslist$lnames), dydx, data = data, model = model, type = NULL, eps = eps, as.data.frame = as.data.frame, ...)
    
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    out2 <- list()
    for (i in seq_along(varslist$fnames)) {
        out2[[i]] <- dydx.factor(data = data, model = model, varslist$fnames[i], fwrap = FALSE, type = NULL, as.data.frame = as.data.frame, ...)
    }
    
    out <- c(out1, out2)
    if (isTRUE(as.data.frame)) {
        out <- do.call("cbind.data.frame", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    } else {
        out <- do.call("cbind", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    }
    return(out)
}
