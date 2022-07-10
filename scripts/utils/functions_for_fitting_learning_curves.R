fit_learning <- function(p,t,y,ret,print_output,which_model){

        # Define the function:
        if (which_model == 'two_param'){
                y_hat = p[1] * exp(-p[2] * (t - 1))
        } else if (which_model == 'three_param'){
                y_hat = p[3] * exp(-p[2] * (t - 1)) + p[1] - p[3]
        }
        
        sse <- sum((y-y_hat)^2)

        if (print_output){
                print(paste0('params: ',p))
                print(paste0('N parameters: ',which_model))
                print(paste0(y,collapse = ' '))
                print(paste0(y_hat,collapse = ' '))
                # print(paste0('sse: ',sse))
        }

        if (ret=='fit') {
                return(y_hat)
        } else {
                return(sse)
        } 
}

get_aic <- function(sse,n_params){
        
        aic_val <- 2*n_params + 2*sse
        
}