#' Plot the AUPEC curve 
#' @import ggplot2
#' @import ggthemes
#' @importFrom stats sd
#' @importFrom rlang .data
#' @param x An object of \code{evaluate_itr()} class. This is typically an output of \code{evaluate_itr()} function. 
#' @param ... Further arguments passed to the function.
#' @return A plot of ggplot2 object.
#' @export 
plot.itr <- function(x, ...){

# parameters
estimate = x
fit = estimate$qoi
cv = estimate$cv
user_itr = ifelse(is.null(estimate$user_itr), FALSE, estimate$user_itr)

## -----------------------------------------
## format output under cross validation
## -----------------------------------------
if(cv == TRUE){

  # parameters
  outcome = estimate$df$outcome
  data = estimate$df$data
  algorithms = estimate$df$algorithms
  treatment = estimate$df$treatment

  graphLabels <- data.frame(
    type = algorithms,
    Pval = map(
      fit$AUPEC, ~.x$aupec_cv) %>%
      bind_rows() %>%
      mutate(Pval = paste0("AUPEC = ", round(aupec, 2), " (s.e. = ", round(sd, 2), ")")) %>% pull(Pval))

  Tcv = data %>% pull(treatment) %>% as.numeric()
  Ycv = data %>% pull(outcome) %>% as.numeric()

  bind_rows(map(fit$AUPEC, ~.x$aupec_cv)) %>% 
    mutate(type = algorithms) %>%
    inner_join(bind_rows(
      map(fit$AUPEC, ~.x$outputdf)),
      by = "type"
    ) %>%
    mutate(AUPECmin = aupec.y - 1.96*sd,
          AUPECmax = aupec.y + 1.96*sd) %>%
    rename(aupec = aupec.y) -> data
  
}

## -----------------------------------------
## format output under sample splitting
## -----------------------------------------
if(cv == FALSE & user_itr == FALSE){
  
  # parameters
  data = estimate$df$data
  algorithms = estimate$df$algorithms

  graphLabels <- data.frame(
    type = algorithms,
    Pval = map(
      fit$AUPEC, ~.x[c('aupec', 'sd')]) %>% 
      bind_rows() %>% 
      mutate(Pval = paste0("AUPEC = ", round(aupec, 2), " (s.e. = ", round(sd, 2), ")")) %>% pull(Pval))

  Tcv = estimate$estimates[['Tcv']] %>% as.numeric()
  Ycv = estimate$estimates[['Ycv']] %>% as.numeric()
  
  map(fit$AUPEC, ~.x) %>% 
    bind_rows() %>%
    mutate(
          aupec = vec + mean(Ycv),
          fraction = rep(seq(1,length(Ycv))/length(Ycv), length(algorithms)),
          type = lapply(algorithms, function(x)rep(x,length(Ycv))) %>% unlist) %>% 
    mutate(AUPECmin = aupec - 1.96*sd,
        AUPECmax = aupec + 1.96*sd)  -> data          
}

## -----------------------------------------
## format output for user-defined ITR
## -----------------------------------------
if(cv == FALSE & user_itr == TRUE){

  Tcv = estimate$estimates[['Tcv']] %>% as.numeric()
  Ycv = estimate$estimates[['Ycv']] %>% as.numeric()

  graphLabels <- data.frame(
    type = "user-defined ITR",
    Pval = map(
      fit$AUPEC, ~.x[c('aupec', 'sd')]) %>% 
      bind_rows() %>% 
      mutate(Pval = paste0("AUPEC = ", round(aupec, 2), " (s.e. = ", round(sd, 2), ")")) %>% pull(Pval))

  fit$AUPEC %>% 
    bind_rows() %>%
    mutate(
      aupec = vec + mean(Ycv),
      fraction = rep(seq(1,length(Ycv))/length(Ycv),1),
      type = lapply("user-defined ITR", function(x)rep(x,length(Ycv))) %>% unlist) %>% 
    mutate(AUPECmin = aupec - 1.96*sd,
      AUPECmax = aupec + 1.96*sd)  -> data    

}


data %>% 
    ggplot(aes(x=fraction,y=aupec,group=type)) + 
      geom_line(alpha=0.5,colour="red") + 
      scale_colour_few("Dark")+
      xlab("Maximum Proportion Treated")+
      ylab("AUPEC")+
      facet_wrap(~type)+
      scale_x_continuous(labels=scales::percent)+
      scale_y_continuous(
        limits = c(min(data$AUPECmin, na.rm = TRUE)-0.5, max(data$AUPECmax, na.rm = TRUE)+ 0.5))+ 
      theme_few()+ 
      geom_ribbon(
        aes(ymin=AUPECmin, ymax=AUPECmax),fill="tomato1",alpha=0.2) +
      geom_abline(
        intercept = sum(Ycv*(1-Tcv))/sum(1-Tcv), slope = sum(Ycv*Tcv)/sum(Tcv)-sum(Ycv*(1-Tcv))/sum(1-Tcv),linewidth=0.5) +
      geom_text(
        data = graphLabels, aes(x = 0.57, y = max(data$AUPECmax, na.rm = TRUE)+0.35, label = Pval),size=3) +
      theme(text = element_text(size=13.5),
            axis.text = element_text(size=10),
            strip.text = element_text(size = 13.5)) -> out




  return(out)
}
