#' Organize mcmc.list objects from coda/R2jags
#'
#' Get posterior draws, means, and modes output by MCMC simulations.
#'
#' Takes \code{mcmc.list} objects, which are created by various 
#' functions in packages 'R2jags' and 'coda'. Returns posterior 
#' draws as arrays with dim(observations, output params, draws). Also
#' returns means and X values of modes (corresponding to highest posterior
#' peaks) as matrices with dim(observations, output params).
#' Works across all parallel MCMC chains (if running several in tandem).
#'
#' @author Mike Vlah, \email{vlahm13@gmail.com}
#' @param x an \code{mcmc.list} object.
#' @param ... additional arguments passed to `density`, which 
#'   determines posterior modes.
#' @keywords coda R2jags MCMC
#' @return a list containing arrays of 1) all posterior draws from all
#'   parallel chains, 2) posterior means, 3) posterior modes.
#' @seealso \code{\link{MCMCout_mix}} for organized mixing model output.
#' @export
#' @examples
#' mod1 <- jags.model(model_script, data = list(<data>), 
#'         inits = function()list(<inits>), n.chains = 3, n.adapt = 2000)
#' update(mod1, n.iter=5e3)
#' mod1_out <- coda.samples(mod1, c(<variables out>, n.iter=1e5, thin=100))
#' MCMCout(mod1_out)
MCMCout <- function(mcmc_list){ 
    
    #get dims and names from mcmc.list object
    
    nchains <- length(mcmc_list)
    chain_length <- length(mcmc_list[[1]][,1])
    param_list <- gsub(" *\\[.*?\\] *", '', colnames(mcmc_list[[1]]))
    params <- levels(factor(param_list))
    nparams <- length(params)
    param_lengths <- vector('numeric', length=nparams)
    for(i in 1:nparams){
        param_lengths[i] <- length(which(param_list==paste(params[i])))
    }
    longest_param <- max(param_lengths)
    
    #create arrays of means, modes, and posterior draws across all chains
    
    temp <- matrix(data=NA, nrow=chain_length, ncol=nchains)
    modes <- matrix(data=NA, nrow=longest_param, ncol=nparams)
    means <- modes
    draws <- array(data=NA, 
                   dim=c(longest_param, nparams, chain_length*nchains))
    for(j in 1:nparams){
        indices <- which(param_list==params[j])
        for(i in 1:length(indices)){
            for(k in 1:nchains){
                temp[,k] <- mcmc_list[[k]][,indices[i]][1:chain_length]
            }
            draws[i,j,] <- as.vector(temp)
            modes[i,j] <- density(draws[i,j,], 
                         n=10000)$x[which.max(density(draws[i,j,], n=10000)$y)]
            means[i,j] <- mean(draws[i,j,])
        }
    }
    colnames(modes) <- params
    colnames(means) <- params
    
    return(list(draws, means, modes))
}
