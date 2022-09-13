#' Compute Quantities of Interest
#' @param fit An output object from \code{run_itr} function.
#' @param type A string indicates the quantity of interests to compute. \code{type} includes PAPE, PAPEp, PAPDp
#' @param m A numeric value indicates which outcome to look at from the vector of \code{outcomes}
#' @export 
#' 
summary.itr <- function(fit, m, type){
    if(type == "PAPE"){
        out <- fit$qoi[[m]]$PAPE %>%
            map(.,~as_tibble(.)) %>%
            bind_rows()
    }else if (type == "PAPEp") {
       out <- fit$qoi[[m]]$PAPEp %>%
        map(.,~as_tibble(.)) %>%
        bind_rows()
    }else if (type == "PAPDp") {
       out <- fit$qoi[[m]]$PAPDp %>%
        map(.,~as_tibble(.)) %>%
        bind_rows()
    }else if (type == "AUPEC") {
       out <- fit_star$qoi[[1]]$AUPEC %>%
        {{temp <<-.}} %>%
        map(., ~.x$aupec_cv) %>%
        bind_rows() %>%
        mutate(algorithm = temp %>% map(., ~.x$outputdf$type %>% unique) %>% unlist)
    }

    class(out) <- c("summary.itr", class(out))

    return(out)
}
