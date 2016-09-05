#' Organize mcmc.list objects from coda/R2jags mixing models
#'
#' Get posterior draws, means, and modes for source proportions output by MCMC mixing model simulations.
#'
#' Takes \code{mcmc.list} objects, which are created by various 
#' functions in packages 'R2jags' and 'coda'. Returns posterior 
#' draws as arrays with dim(sources, observed mixtures, draws). Also
#' returns means and X values of modes (corresponding to highest posterior
#' peaks) as matrices with dim(sources, observed mixtures).
#' Works across all parallel MCMC chains (if running several in tandem).
#'
#' @author Mike Vlah, \email{vlahm13@gmail.com}
#' @param x an \code{mcmc.list} object from a mixing model.
#' @param ... additional arguments passed to `density`, which 
#'   determines posterior modes.
#' @keywords coda R2jags MCMC 'mixing model'
#' @return a list containing arrays of 1) all posterior draws from all
#'   parallel chains, 2) posterior means, 3) posterior modes. Only 
#'   applies to the variable that contains source proportions.  For
#'   any other monitored variables, use \code{\link{MCMCout}}.
#' @seealso \code{\link{MCMCout}} for organized output from non-mixing models.
#' @export
#' @examples
#' mod1 <- jags.model(model_script, data = list(<data>), 
#'         inits = function()list(<inits>), n.chains = 3, n.adapt = 2000)
#' update(mod1, n.iter=5e3)
#' mod1_out <- coda.samples(mod1, c(<variables out>, n.iter=1e5, thin=100))
#' MCMCout_mix(mod1_out, 'prey_proportion', 3)
MCMCout_mix <- function(x, p, nsrc, ...){
    nchains <- length(x)
    chain_length <- length(x[[1]][,1])
    param_list <- gsub(" *\\[.*?\\] *", '', colnames(x[[1]]))
    all_ps <- which(param_list == p)
    num_ps <- length(all_ps)
    first_p <- all_ps[1]
    total_vals <- num_ps * nchains
    sum_chain_lengths <- chain_length * nchains
    indices <- seq(1, nmix*nsrc, nsrc)
    mode_arr <- array(data=NA, dim=c(nsrc,nmix))
    mean_arr <- mode_arr
    draws_arr <- array(data=NA, dim=c(nsrc,nmix,sum_chain_lengths))

    #create vectors of means and modes across chains
    temp <- matrix(data=NA, nrow=chain_length, ncol=nchains)
    mode_vec <- vector('numeric', length=num_ps)

    for(i in 1:num_ps){
        for(j in 1:nchains){
            temp[,j] <- x[[j]][,i+first_p-1][1:chain_length]
        }
        chain <- as.vector(temp)
        mode_vec[i] <- density(chain, n=10000, ...)$x[which.max(
            density(chain, n=10000, ...)$y)]
        mean_vec[i] <- mean(chain)
    }

    #assemble means and modes into array
    for(i in 1:nsrc){
        mean_arr[i, 1:31] <- mean_vec[indices + i-1]
        mode_arr[i, 1:31] <- mode_vec[indices + i-1]
    }

    #make separate array of combined chains
    temp2 <- matrix(data=NA, nrow=chain_length, ncol=nchains)

    for(i in 1:length(indices)){
        for(j in 1:nchains){
            for(s in 1:nsrc){
                temp2[,j] <- x[[j]][,
                             (first_p-1 + (indices + s-1)[i])][1:chain_length]
                draws_arr[s,i,] <- as.vector(temp2)
            }
        }
    }

    return(list(draws_arr, mean_arr, mode_arr))
}

