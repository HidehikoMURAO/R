
#install.packages("dplyr")
#install.packages("FRK")
#install.packages("ggplot2")
#install.packages("IDE")
#install.packages("sp")
#install.packages("spacetime")
library("dplyr")
library("FRK")
library("ggplot2")
library("IDE")
library("sp")
library("spacetime")

##############################################################
############################################################## Case 1
############################################################## Same kernel across space

############################### Data generation
SIM1 <- simIDE(T = 10, nobs = 100, k_spat_invariant = 1)
print(SIM1$g_truth)  ## Data at observation sites
print(SIM1$g_obs)    ## True process

############################### Spatiotemporal modeling
IDEmodel <- IDE(f = z ~ s1 + s2,
                data = SIM1$z_STIDF,
                dt = as.difftime(1, units = "days"),
                grid_size = 41)

fit_results_sim1 <- fit.IDE(IDEmodel,
                            parallelType = 1,itermax=50)

############################### Estimated parameters and kernel 
show_kernel(fit_results_sim1$IDEmodel)
fit_results_sim1$IDEmodel$get("k") %>% unlist()
fit_results_sim1$IDEmodel$get("betahat")## estimated regression coefficients (constant, s1, s2)

############################### Spatiotemporal prediction
ST_grid_df <- predict(fit_results_sim1$IDEmodel)

gpred <- ggplot(ST_grid_df) +
  geom_tile(aes(s1, s2, fill=Ypred)) +
  facet_wrap(~t) +
  scale_fill_distiller(palette="Spectral", limits = c(-0.1,1.4)) +
  coord_fixed(xlim=c(0, 1), ylim = c(0, 1))
gpred        ################ Plot the predicted values

gpredse <- ggplot(ST_grid_df) +
  geom_tile(aes(s1, s2, fill=Ypredse)) +
  facet_wrap(~t) +
  scale_fill_distiller(palette="Spectral") +
  coord_fixed(xlim=c(0, 1), ylim = c(0, 1))
gpredse      ################ Plot standard errors of the predicted values



##############################################################
############################################################## Case 2
############################################################## Varying kernel across space

############################### Data generation
SIM2 <- simIDE(T = 15, nobs = 1000, k_spat_invariant = 0)
print(SIM2$g_truth)  ## Data at observation sites
print(SIM2$g_obs)    ## True process
show_kernel(SIM2$IDEmodel, scale = 0.2)## True kernel

############################### Spatiotemporal modeling
mbasis_1 <- auto_basis(manifold = plane(), # functions on the plane
                       data = SIM2$z_STIDF, # data
                       nres = 1, # 1 resolution
                       type = 'bisquare')

kernel_basis <- list(thetam1 = constant_basis(),
                     thetam2 = constant_basis(),
                     thetam3 = mbasis_1,
                     thetam4 = mbasis_1)

IDEmodel <- IDE(f = z ~ s1 + s2 + 1,
                data = SIM2$z_STIDF,
                dt = as.difftime(1, units = "days"),
                grid_size = 41,
                kernel_basis = kernel_basis)

fit_results_sim2 <- fit.IDE(IDEmodel,
                            parallelType = 1,
                            itermax = 50)## itermax=400 in a manual

############################### Estimated parameters and kernel
show_kernel(fit_results_sim2$IDEmodel, scale = 0.2)
fit_results_sim2$IDEmodel$get("k") %>% unlist()## estimated internal parameters
fit_results_sim2$IDEmodel$get("betahat")## estimated regression coefficients (constant, s1, s2)

############################### Spatiotemporal prediction
ST_grid_df <- predict(fit_results_sim2$IDEmodel)

gpred <- ggplot(ST_grid_df) + 
  geom_tile(aes(s1, s2, fill=Ypred)) +
  facet_wrap(~t) +
  scale_fill_distiller(palette="Spectral", limits = c(-0.1,5)) +
  coord_fixed(xlim=c(0, 1), ylim = c(0, 1))
gpred        ################ Plot the predicted values

gpredse <- ggplot(ST_grid_df) +
  geom_tile(aes(s1, s2, fill=Ypredse)) +
  facet_wrap(~t) +
  scale_fill_distiller(palette="Spectral") +
  coord_fixed(xlim=c(0, 1), ylim = c(0, 1))
gpredse      ################ Plot standard errors of the predicted values


