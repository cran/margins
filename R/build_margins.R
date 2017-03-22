build_margins <- 
function(model, 
         data,
         type = c("response", "link", "terms"),
         vcov = stats::vcov(model),
         vce = c("delta", "simulation", "bootstrap", "none"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         unit_ses = FALSE,
         eps = 1e-7,
         ...) {
    
    # variables in the model
    allvars <- all.vars(terms(model))[-1]
    
    # march.arg() for arguments
    type <- match.arg(type)
    vce <- match.arg(vce)
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    # obtain gradient with respect to each variable in data
    mes <- marginal_effects(model = model, data = data, type = type, eps = eps, ...)
    
    # variance estimation technique
    if (vce != "none") {
        variances <- get_effect_variances(data = data, model = model, allvars = names(mes), 
                                          type = type, vcov = vcov, vce = vce, 
                                          iterations = iterations, eps = eps, ...)
        variances <- setNames(lapply(variances, rep, nrow(data)), paste0("Var_", names(mes)))
    }
    
    # get unit-specific effect variances (take derivative of `.build_grad_fun()` for every row separately)
    if ((vce == "delta") && (isTRUE(unit_ses))) {
        vmat <- do.call("rbind", lapply(seq_len(nrow(data)), function(datarow) {
            delta_once(data = data[datarow,], model = model, type = type, vcov = vcov, eps = eps, ...)
        }))
        colnames(vmat) <- paste0("SE_", names(mes))
        vmat <- as.data.frame(vmat)
        vmat[] <- lapply(vmat, sqrt)
    }
    
    # obtain predicted values and standard errors
    pred <- prediction(model = model, data = data, type = type)
    
    # setup output structure
    structure(if ((vce == "delta") && (isTRUE(unit_ses))) {
                  cbind(pred, mes, variances, vmat)
              } else if (vce == "none") { 
                  cbind(pred, mes)
              } else { 
                  cbind(pred, mes, variances)
              }, 
              class = "data.frame", 
              row.names = seq_len(nrow(pred)))
}