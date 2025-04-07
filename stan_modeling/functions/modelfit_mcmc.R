
modelfit_mcmc <-function(path, data_path,save_path, mymcmc){
  

  #load model
  load(paste0(path$data,'/modelfit_compile.rdata'))

  # load data
  load(data_path)

  #sample
  fit<- my_compiledmodel$sample(
    data            = data_for_stan,
    iter_sampling   = mymcmc$samples,
    iter_warmup     = mymcmc$warmup,
    chains          = mymcmc$chains,
    parallel_chains = mymcmc$cores)
    # adapt_delta = 0.95,  # Reduce divergences
    # max_treedepth = 15)

  #fit <- my_compiledmodel$optimize(data = data_for_stan, algorithm = "lbfgs")
  #fit=my_compiledmodel$variational(data = data_for_stan,threads=10,draws=1000)
  #fit=my_compiledmodel$pathfinder(data = data_for_stan)
  #save
  if (mymcmc$datatype=='empirical'){fit$save_object(paste0(path$data,'/modelfit_empirical_splines_',save_path,'.rds'))
    cat(paste0('[stan_modeling]:  "modelfit_empirical.rds" was saved at "',path$data,'"'))
  }
  if (mymcmc$datatype=='artificial'){fit$save_object(paste0(path$data,'/modelfit_recovery.rds'))
    cat(paste0('[stan_modeling]:  "modelfit_recovery.rds" was saved at "',path$data,'"'))
    }
}