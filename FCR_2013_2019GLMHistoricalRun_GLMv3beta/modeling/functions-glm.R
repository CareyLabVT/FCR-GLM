#script that contains all packages need for sensitivity and optimization analyses
#written by Robert Ladwig & Tadhg Moore, given to CCC in 2019
#NOTE - search for "EDIT" in this file - you will need to edit multiple lines in this file to make 
# specific for your analysis/calibration & validation period/dynamic library files on your machine
#Last updated by CCC 3 June 2020

read.packages <- function(){
  Packages <- c("dplyr", "ggplot2", "tidyverse", "cluster", "zoo", "gtools", 
                "nloptr", "lubridate", "adagio", "ncdf4", "glmtools", 
                "likelihood", "hydroGOF", "akima", "pracma", "signal")
  lapply(Packages, library, character.only = TRUE)
}

stringwstring <- function(textarg = "** Default text **" ){ 
  text <- (noquote(textarg)) 
  return(text) 
}

get_glm_description <- function(iter, perm, id, description){
  permutations_id_matrix <-read.table(perm,header = TRUE, sep =",")
  id_matrix <- read_csv(id, 
                 col_names = TRUE)
  id_description <- read_csv(description, col_names = TRUE)
  
  val1 <- c()
  for (ii in 2:ncol(permutations_id_matrix)){
    val1 <- append(val1, id_matrix[permutations_id_matrix[iter, ii], ii-1])
  }
  
  val2 <- c();val3 <- c()
  for (ii in 1:length(val1)){
    val2 <- append(val2, id_description$description[id_description$mode == val1[ii]])
    val3 <- append(val3, id_description$value[id_description$mode == val1[ii]])
  }
  val <- cbind(val2, val3)
  return(val)
}

plot_contour <- function(mod_nc, reference = "surface", h, var, unit, tlevels){
  ncin <- nc_open(mod_nc)
  watdep <- ncvar_get(ncin, "z")
  wattemp <- ncvar_get(ncin, var)
  
  time <- ncvar_get(ncin, "time")
  time.units <- ncatt_get(ncin, "time", "units")
  #sub("^.*\\s2","",time.units$value)
  time.start <- as.POSIXct(strptime(sub("hours since ","",time.units$value), 
                                    format = "%Y-%m-%d %H:%M:%S"))
  datetime <- time.start + time*3600
  
  layer <- ncvar_get(ncin, "NS")
  nc_close(ncin)
  watdep[which(watdep == max(watdep))] <- NaN
  wattemp[which(wattemp == max(wattemp))] <- NaN
  
  sim.watdep <- 0.0*watdep - 999
  for (i in 1:length(datetime)){
    max_depth <- watdep[layer[i],i]
    sim.watdep[1,i] <- max_depth - watdep[1,i]/2 
    for (j in 2:layer[i]){
      sim.watdep[j, i] <- max_depth - (watdep[j,i] + watdep[j-1, i ])/2 
    }
  }
  
  int.dep <- rev(seq(0.25,round(max(watdep,na.rm = TRUE)),0.1))
  sim.wattemp <- matrix(0, nrow = length(int.dep), ncol= length(datetime))
  for (i in 1:length(datetime)){
    sim.approx <- approx(na.omit(sim.watdep[,i]), na.omit(wattemp[,i]), int.dep)
    sim.wattemp[1:length(int.dep),i] <- sim.approx$y
  }
  
  if ((median(apply(sim.watdep,2,max,na.rm=TRUE))+median(apply(sim.watdep,2,sd,na.rm=TRUE)))<
      max(sim.watdep[,1],na.rm=TRUE)){
    max.plot.dep <- ceiling(median(apply(sim.watdep,2,max,na.rm=TRUE)))
  } else {
    max.plot.dep <-ceiling(max(sim.watdep[,1],na.rm=TRUE))
  }
  
  if (max.plot.dep<=20){iter.plot.dep = 1} else if (max.plot.dep<=50){iter.plot.dep = 5} else (iter.plot.dep = 10)
  
  spectral.colors <-  colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral') )

  inMeter <- function(x) {paste0(x, " m")}
  inCelsius <- function(x) {paste0(x, paste(" ",unit,sep=''))}
  filled.contour(x=as.numeric(datetime), y=(int.dep)*(-1), z = t(sim.wattemp),
                 levels=tlevels,
                 col=rev(spectral.colors(length(tlevels))), main=h, cex = 1.5, cex.main = 3., 
                 plot.axes = {axis(1, labels=format(pretty(datetime,20), "%Y-%b"), at = as.numeric(pretty(datetime,20)), cex.axis=2,las=1.5);
                   axis(2, labels=inMeter(rev(seq(0,max.plot.dep,iter.plot.dep))*(-1)), at = rev(seq(0,max(max.plot.dep),iter.plot.dep))*(-1), cex.axis=1.5)},
                 key.axes = {axis(4,at=unique(tlevels),labels=(inCelsius(unique(tlevels))))})
}

get_wattemp <- function(mod_nc, reference = "surface", h, var){
  ncin <- nc_open(mod_nc)
  watdep <- ncvar_get(ncin, "z")
  wattemp <- ncvar_get(ncin, var)

  time <- ncvar_get(ncin, "time")
  time.units <- ncatt_get(ncin, "time", "units")
  #sub("^.*\\s2","",time.units$value)
  time.start <- as.POSIXct(strptime(sub("hours since ","",time.units$value), 
                                    format = "%Y-%m-%d %H:%M:%S"))
  datetime <- time.start + time*3600
  
  layer <- ncvar_get(ncin, "NS")
  nc_close(ncin)
  watdep[which(watdep == max(watdep))] <- NaN
  wattemp[which(wattemp == max(wattemp))] <- NaN
  
  sim.watdep <- 0.0*watdep - 999
  for (i in 1:length(datetime)){
    max_depth <- watdep[layer[i],i]
    sim.watdep[1,i] <- max_depth - watdep[1,i]/2 
    for (j in 2:layer[i]){
      sim.watdep[j, i] <- max_depth - (watdep[j,i] + watdep[j-1, i ])/2 
    }
  }
  
  int.dep <- rev(seq(0.25,round(max(watdep,na.rm = TRUE)),0.1))
  sim.wattemp <- matrix(0, nrow = length(int.dep), ncol= length(datetime))
  for (i in 1:length(datetime)){
    sim.approx <- approx(na.omit(sim.watdep[,i]), na.omit(wattemp[,i]), int.dep)
    sim.wattemp[1:length(int.dep),i] <- sim.approx$y
  }
  
  return(list('sim' = sim.wattemp, 'time' = datetime, 'depth' = int.dep))
}

save_ncdf <- function(name, time, z, wtr, oxy){
  long = 50
  lat = 50
  
  dimX = ncdim_def("lon", "degrees", long)
  dimY = ncdim_def("lat", "degrees", lat)
  dimT = ncdim_def("time", "hours since", as.double(time))
  dimZ = ncdim_def("depth", "meters", z)
  
  fillvalue <- 1e32
  dlname <- 'water temperature'
  tmp_def <- ncvar_def("wtr", "deg_C", list(dimZ, dimT), fillvalue, dlname, prec="single")
  dlname <- 'dissolved oxygen'
  oxy_def <- ncvar_def("do", "mgperLitre", list(dimZ, dimT), fillvalue, dlname, prec="single")
  
  ncfname <- paste(name,".nc",sep="")
  ncout <- nc_create(ncfname, list(tmp_def, oxy_def), force_v4 = TRUE)
  
  ncvar_put(ncout, tmp_def, wtr)
  ncvar_put(ncout, oxy_def, oxy)
  
  nc_close(ncout)
}

# tadgh's function
mod2obs <- function(mod_nc, obs, reference = 'surface', var){
  deps = unique(obs[,2])
  #tim = unique(obs[,1])
  mod <- glmtools::get_var(file = mod_nc,var,reference = reference, z_out = deps)
  mod <- match.tstep1(obs, mod) #From gotm_functions.R        #EDIT THIS
  mod <- reshape2::melt(mod, id.vars = 1)
  mod[,2] <- as.character(mod[,2])
  mod[,2] <- as.numeric(gsub(paste(var,"_",sep=''),'',mod[,2]))
  colnames(mod) <- c('DateTime', 'Depth', var)
  mod <- mod[order(mod$DateTime, mod$Depth),]
  if(nrow(mod) != nrow(obs)){
    mod <- merge(obs, mod, by = c(1,2), all.x = T)
    #mod <- merge(obs, mod, by = c(1,2), all = T)
    mod <- mod[order(mod$DateTime, mod$Depth),]
    mod <- mod[,c(1,2,4)]
    colnames(mod) <- c('DateTime', 'Depth', var)
  }
  return(mod)
}

run_glm <- function(os){
  if (os == "Windows"){
    system('run_glm3.bat',ignore.stdout=TRUE)
  } else if (os == "Unix"){
    system("glm",ignore.stdout=TRUE)
  } else if (os == "Original"){
    file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
    file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
    system("glm",ignore.stdout=TRUE)
  } else if (os == "Compiled"){
    sim_folder = "/Users/cayelan/Dropbox/ComputerFiles/SCC/FCR-GLM/FCR_2013_2019GLMHistoricalRun_GLMv3beta" #BE SURE TO EDIT THIS!
    system2(paste0(sim_folder, "/", "glm"), stdout = TRUE, stderr = TRUE, env = paste0("DYLD_LIBRARY_PATH=",sim_folder))
  }
}

# gotm_functions.R
match.tstep <- function(df1, df2){
  if(df1[1,1] == df1[2,1]){
    df = data.frame(DateTime = unique(df1[,1]))
    df = merge(df, df2, by = 1)
    return(df)
  }else{
    tim1 = df1[,1]
    tim2 = df2[,1]
    ind = c()
    pb = txtProgressBar(min = 0, max = length(tim1), style = 3)
    for(i in 1:length(tim1)){
      ind = append(ind, which(tim2 == tim1[i]))
      setTxtProgressBar(pb, i)
    }
    close(pb)
    df2.mat = df2[ind,]
    return(df2.mat)
  }
}

#Cayelan's modified function to deal with time step issue EDIT THIS
match.tstep1 <- function(df1,df2){
  df1$DateTime <- as.POSIXct(strptime(df1$DateTime, "%Y-%m-%d", tz="EST"))
  df2$DateTime <- as.POSIXct(strptime(df2$DateTime, "%Y-%m-%d", tz="EST"))
  return(df2)
}




get_nse <- function(x, y){
  id1 <- !is.na(obs[,3]) 
  obs <- y[id1,3]
  mods <- x[id1,3]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2)
  sum_bottom <- sum((obs-mean(obs))^2)
  nse <- 1- sum_up/sum_bottom
  return(nse)
  
}

get_rmse <- function(mods, obs){
  id1 <- !is.na(obs[,3]) 
  obs <- obs[id1,3]
  mods <- mods[id1,3]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2)
  rmse <- sqrt(sum_up/length(obs))
  return(rmse)
}

create_timeseries <- function(mods, obs, var, var_range, var_unit){
  for (ii in 1:length(unique(obs$DateTime))){
    obs_f <- obs %>%
      dplyr::filter(DateTime == unique(obs$DateTime)[ii]) 
    mods_f <- mods %>%
      dplyr::filter(DateTime == unique(obs$DateTime)[ii]) 
    nse <- get_nse(mods_f,obs_f)
    png(paste0('results/',var,'/',ii,'.png'), width = 300, height = 400)
    plot(obs_f[,3], obs_f$Depth, xlim = var_range, ylim = rev(range(obs_f$Depth)),  xlab = var_unit,
         ylab = 'Depth m',
         main =paste(unique(obs_f$DateTime),"NSE:",round(nse,2)))
    lines(mods_f[,3], mods_f$Depth)
    dev.off()
  }
  return()
}
  
# gotm_functions.R
diag.plots <- function(mod, obs, ggplot = T){
  stats = sum_stat(mod, obs, depth = T)
  if(max(mod[,2]) >= 0){ #Makes depths negative
    mod[,2] <- -mod[,2]
  }
  if(ggplot == F){
    dif = mod[,3] - obs[,3]
    par(mfrow=c(2,3))
    
    xfit <- seq(min(dif, na.rm = T), max(dif, na.rm = T), length=40)
    yfit_density <- dnorm(xfit, mean=mean(0, na.rm = T), sd=sd(dif, na.rm = T))
    
    # frequency
    h_freq <- hist(dif, breaks=50, col="blue", xlab="Model - Obs (C)", main='Histogram of residuals',probability = F, xlim = c(min(na.rm =T,  dif),max(na.rm =T,  dif)))
    yfit_freq <- yfit_density*diff(h_freq$mids[1:2])*length(dif)
    lines(xfit, yfit_freq, col="red",lty =2, lwd=2)
    mn <- round(mean(dif, na.rm =T),2)
    abline(v = mn,lty =2,lwd =2, col = 'green')
    std.dev <- round(sd(dif, na.rm =T),2)
    eqn <- bquote(Mean == .(mn) * "," ~~ S.D. == .(std.dev))
    Corner_text(eqn)
    
    plot(mod[,3], dif, cex = 0.5, pch ='.', main = 'Residuals vs. Modelled',
         ylab = 'Residuals', xlab = 'Modelled values')
    abline( h =0, col =2, lty =2)
    
    plot(mod[,1], dif, ylab = 'Time', xlab = 'Residuals', main = 'Residuals vs. Time', pch = '.')
    abline(h =0, lty =2, col =2)
    
    if(min(mod[,2]) >= 0){
      mod[,2] = -mod[,2]
    }
    plot(dif, mod[,2], ylim = range(mod[,2]), ylab = 'Depth (m)', xlab = 'Residuals', main = 'Residuals vs. Depth', pch = '.')
    abline(v =0, lty =2, col =2)
    
    plot(mod[,3], obs[,3], pch ='.', main = 'Obs vs. Mod', ylab = 'Obs',
         xlab ='Mod', ylim = range(mod[,3], obs[,3], na.rm =T), xlim = range(mod[,3], obs[,3], na.rm =T))
    abline(0,1, col =2, lty =2)
    eqn <- bquote(Pear_R == .(round(stats$Pearson_r,2)) * "," ~~ var.obs == .(round(stats$Variance_obs,2)) *
                    "," ~~ var.mod == .(round(stats$Variance_mod,2)) *  "," ~~ NSE == .(round(stats$NSE,2)))
    eqn2 <- bquote(cov == .(round(stats$Covariance,2)) * "," ~~ bias == .(round(stats$Bias,2)) *
                     "," ~~ MAE == .(round(stats$MAE,2)) * "," ~~ RMSE == .(round(stats$RMSE,2)))
    Corner_text(eqn)
    Corner_text(eqn2,location = 'bottomright')
    
    qqnorm(dif)
    abline(0,1, lty =2, col =2)
  }else{
    #ggplot2 version - put all variables in one dataframe
    mod$res <- mod[,3] - obs[,3]
    deps <- unique(mod[,2])
    deps <- deps[order(deps)]
    if(length(deps) < 10){
      lgd.sz = 4
    }else{
      lgd.sz =2
    }
    mod$fdepth <- factor(mod[,2], levels = as.character(deps))
    mod$obs <- obs[,3]
    
    mean.res = round(mean(mod$res, na.rm =T),2)
    med.res = round(median(mod$res, na.rm = T),2)
    std.dev = round(sd(mod$res, na.rm =T), 2)
    n = nrow(mod[!is.na(mod$res),])
    bw = 0.2
    min.res = min(mod$res, na.rm =T)
    max.res = max(mod$res, na.rm =T)
    
    # Create text to be added to plots
    grob1 <- grid::grobTree(grid::textGrob(paste0("Mean = ", mean.res,'; S.D = ', std.dev), x=0.5,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob2 <- grid::grobTree(grid::textGrob(paste0("Pear_R = ", round(stats$Pearson_r,2),'; v.obs = ', round(stats$Variance_obs,2),'; v.mod = ', round(stats$Variance_mod,2),'; NSE = ',round(stats$NSE,2)), x=0.05,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob3 <- grid::grobTree(grid::textGrob(paste0("cov = ", round(stats$Covariance,2),'; bias = ', round(stats$Bias,2),'; MAE = ', round(stats$MAE,2),'; RMSE = ',round(stats$RMSE,2)), x=0.05,  y=0.05, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    
    
    #Plots
    p1 <-ggplot(mod, aes(x = res)) + 
      geom_histogram(fill = "blue", colour = 'black', breaks = seq(min.res, max.res, bw)) + 
      stat_function( 
        fun = function(x, mean, sd, n, bw){ 
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        }, 
        args = c(mean = 0, sd = std.dev, n = n, bw = bw), colour = 'red', linetype = 'dashed', size = 1.2) + 
      scale_x_continuous("Model - Obs (C)")+
      scale_y_continuous("Frequency")+
      ggtitle('Histogram of residuals')+
      coord_cartesian(xlim = c(min(mod$res, na.rm = T),max(mod$res,na.rm =T)))+
      geom_vline(xintercept = med.res, colour = 'green', linetype = 'dashed', size = 1.2)+
      theme_bw()
    p1 <- p1 + annotation_custom(grob1)
    
    p2 <- ggplot(mod, aes_string(names(mod)[3], 'res', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      xlab('Modelled values')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Modelled')+
      scale_color_discrete(name = 'Depths', guide = F)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      theme_bw()
    
    p3 <- ggplot(mod, aes_string(names(mod)[1], 'res', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      xlab('Time')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Time')+
      #scale_color_gradientn(colors = rev(my.cols), name = 'Depths')+
      scale_color_discrete(name = 'Depths', guide = F)+
      #guides(colour = guide_legend(override.aes = list(size=lgd.sz)))+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      theme_bw()#+
    #theme(legend.text=element_text(size= (lgd.sz*2.5)))
    
    p4 <- ggplot(mod, aes_string('res', names(mod)[2], colour = 'fdepth'))+
      geom_point(size = 0.1)+
      ylab('Depth')+
      xlab('Residuals')+
      ggtitle('Residuals vs. Depth')+
      scale_color_discrete(name = 'Depths', guide = F)+
      geom_vline(xintercept = 0, linetype = 'dashed', size = 1)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      theme_bw()
    
    
    p5 <- ggplot(mod,aes_string(names(mod)[3], 'obs', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      ylab('Obs')+
      xlab('Modelled')+
      ggtitle('Obs vs. Mod')+
      scale_color_discrete(name = 'Depths', guide = F)+
      coord_cartesian(xlim = range(mod[,3], obs[,3], na.rm =T), ylim = range(mod[,3], obs[,3], na.rm =T))+
      geom_abline(slope = 1, intercept = 0, colour = 'black', linetype = 'dashed', size =1)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      theme_bw()
    p5 <- p5 + annotation_custom(grob2) + annotation_custom(grob3)   
    
    p6 <- ggplot(mod, aes(sample = res))+
      stat_qq()+
      geom_abline(slope = 1, intercept = 0, size =1, linetype = 'dashed')+
      xlab('Sample Quantiles')+
      ylab('Theoretical Quantiles')+
      ggtitle('Normal Q-Q Plot')+
      theme_bw()
    
    g <- gridExtra::arrangeGrob(p1,p2,p3,p4,p5,p6, nrow = 2)
    gridExtra::grid.arrange(g)
    
    return(g)
  }
}
  
# gotmtools.R
sum_stat <- function(mod, obs, depth =F,na.rm =T, depth.range =NULL){
  if(depth == T){
    if(!is.null(depth.range)){
      obs = obs[(obs[,2] <= depth.range[1] & obs[,2] >= depth.range[2]),]
      mod = mod[(mod[,2] <= depth.range[1] & mod[,2] >= depth.range[2]),]
    }
    dif = mod[,3]- obs[,3]
    pear_r = cor.test(obs[,3], mod[,3], method = 'pearson')
    var_obs = mean(((obs[,3]-mean(obs[,3], na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod[,3]-mean(mod[,3], na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs[,3], na.rm = na.rm)
    SD_mod = sd(mod[,3], na.rm = na.rm)
    cov = mean((obs[,3]-mean(obs[,3], na.rm = na.rm))*(mod[,3]-mean(mod[,3], na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = NSE(mod[,3], obs[,3])
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }else{
    dif = mod- obs
    pear_r = cor.test(obs, mod, method = 'pearson')
    var_obs = mean(((obs-mean(obs, na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod-mean(mod, na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs, na.rm = na.rm)
    SD_mod = sd(mod, na.rm = na.rm)
    cov = mean((obs-mean(obs, na.rm = na.rm))*(mod-mean(mod, na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = NSE(mod, obs)
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }
  
}

compare_depths <- function(obs, mods, dep, var,var_unit,mult){

surftemp_obs <- obs %>%
  dplyr::filter(Depth == dep) %>%
  select(DateTime, var)

surftemp_mods <- mods %>%
  dplyr::filter(Depth == dep) %>%
  select(DateTime, var)

surftemp <- data.frame("DateTime" = surftemp_obs$DateTime, "obs" = surftemp_obs[,2], 
                       "mods" = surftemp_mods[,2] * 1/mult)
surftemp <- reshape2::melt(surftemp, id.var = "DateTime")
g <- ggplot(surftemp, aes(x = DateTime, y = value, col = variable)) +
   geom_line(aes(linetype = variable)) +
   ggtitle(paste0(var,' [',var_unit,'], ',dep," m", sep=''))+
   theme_bw() +
   theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
g <- ggplot()+
  geom_line(data=data.frame(cbind(surftemp$DateTime[surftemp$variable == 'mods'],surftemp$value[surftemp$variable == 'mods'])),aes(x=surftemp$DateTime[surftemp$variable == 'mods'], y=surftemp$value[surftemp$variable == 'mods'], col='mods')) +
  geom_point(data=data.frame(cbind(surftemp$DateTime[surftemp$variable == 'obs'],surftemp$value[surftemp$variable == 'obs'])),aes(x=surftemp$DateTime[surftemp$variable == 'obs'], y=surftemp$value[surftemp$variable == 'obs'], col='obs'))+
  xlab("Datetime") +
  ylab("diss. oxygen [mg/L]") +
  ggtitle(paste0(var,' [',var_unit,'], ',dep," m", sep=''))+
  theme_bw() +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
return(g)
}


random_sampling <- function(x0,p,del,lb,ub){
  k <- length(x0)
  m <- k+1
  
  (B <- matrix(0, m, k))
  lower.tri(B)
  B[lower.tri(B)] <- 1
  
  D_star <- diag(plus_minus_one(k), k, k)
  J_mk <- matrix( 1, m, k)
  
  (1/2)*((2*B-J_mk)%*%D_star+J_mk)  
  
  x_star <- sample(random_value(p,del),k)
  
  P_star <- matrix(0,k,k)
  sample_P_star <- sample(k)
  for (j in 1:ncol(P_star)){
    P_star[sample_P_star[j],j] <- 1
  }
  
  J_mk_x_star <- matrix(rep(x_star, each = m),m,k)
  
  # J_mk %*% x_star
  
  B_star <- (J_mk_x_star+ (del/2) * ((2 * B - J_mk) %*% D_star + J_mk)) %*% P_star
  
  p = c(B_star[1,])#,lb,ub)
  
  #glmFUNsa(p)
  
  result_matrix <- matrix(0,nrow(B_star),1)
  for (j in 1:nrow(result_matrix)){
    result_matrix[j,]<- glmFUNsa(c(B_star[j,]))
  }
  
  ee <- matrix(0,ncol=k,nrow=1)
  for (i in 1:ncol(ee)){
    ind <- which(diff(B_star[,i])!=0)
    if (diff(B_star[,1])[ind] > 0){
      ee[i] <- (result_matrix[ind+1] - result_matrix[ind])/del}
    else {
      ee[i] <- (result_matrix[ind] - result_matrix[ind+1])/del
    }
  }
  return(ee)
}

glmFUNsa <- function(p){
  #Catch non-numeric arguments
  if(!is.numeric(p)){
    p = values.optim
  }
  
  p <- wrapper_scales(p, lb, ub)
  eg_nml <- read_nml(nml_file = nml_file)
  
  for(i in 1:length(pars[!duplicated(pars)])){
    if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
      eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i], 
                        p[which(pars[!duplicated(pars)][i] == pars)])
    } else {
      eg_nml <- set_nml(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
    }
  }
  
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  
  run_glm("Compiled") #changed from Unix
  
  mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)
  
  fit = sum((mod[,3] - obs[,3])^2,na.rm = T)

  print(paste('SAE', round(fit,1)))
  return(fit)
}

wrapper_scales <- function(x, lb, ub){
  y <-  lb+(ub-lb)/(10)*(x)
  return(y)
}

plus_minus_one <- function(x){
  a <- runif(x,0,1)
  a[a<=1/2] <- -1
  a[a>1/2] <- 1
  return(a)}

random_value <- function(p,del){
  iter <- c()
  for (n in 1:((1-del)*(p-1))){
    iter <- cbind(iter,n/(p-1))  
  }
  return(c(0,iter))
}

glmFUN <- function(p){
  p <- wrapper_scales(p, lb, ub)
  eg_nml <- read_nml(nml_file)
  
  for(i in 1:nrow(calib)){
    eg_nml <- set_nml(eg_nml,pars[i],p[i])
  }
  
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  
  run_glm(os)
  
  # uni.deps = unique(obs)
  # if(length(uni.deps) > 300){
  #   mod <- resample_to_field(out, 'temp.csv')
  # }else{
  mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)
  # }
  
  # if(metric == 'RMSE'){
  #   fit = rmse(mod[,3], obs[,3])
  # }else if(metric == 'lnlikelihood'){
  #   fit <- lnlike(mod[,3], obs[,3],sd = T)
  # }
  
  
  
  
  #print(paste(metric, fit))
  return(mod[,3])
}


glmFUNrmse <- function(p){
  #Catch non-numeric arguments
  if(!is.numeric(p)){
    p = values.optim
  }
  
  p <- wrapper_scales(p, lb, ub)
  eg_nml <- read_nml(nml_file)
  
  for(i in 1:length(pars[!duplicated(pars)])){
    if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
      eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i], 
                        p[which(pars[!duplicated(pars)][i] == pars)])
    } else {
      eg_nml <- set_nml(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
    }
  }
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  
  run_glm(os)
  
  mod <- mod2obs(mod_nc = out, obs = obs, reference = 'surface', var)
  
  fit = rmse(mod[,3], obs[,3])
  
  #Create a data frame to output each calibration attempt
  dat = data.frame(matrix(NA, ncol = (length(pars)+2), nrow = 1, dimnames = list(c(1), c('DateTime', pars, calib.metric))))
  dat[1,] = c(format(Sys.time()),p,fit)
  
  #Opens and writes a csv file with datetime, parameters,and fitness
  if(!file.exists(paste0('results/calib_results_',calib.metric,'_',var,'.csv'))){
    write.csv(dat,paste0('results/calib_results_',calib.metric,'_',var,'.csv'), row.names = F, quote = F)
  }else{
    df = read.csv(paste0('results/calib_results_',calib.metric,'_',var,'.csv'))
    df = rbind.data.frame(dat, df)
    write.csv(df,paste0('results/calib_results_',calib.metric,'_',var,'.csv'), row.names = F, quote = F)
  }
  
  print(paste(calib.metric, fit))
  return(fit)
}

run_sensitivity <- function(var, max_r, x0, lb, ub, pars, obs, nml_file){
  calib <- read.csv(paste0('sensitivity/sample_sensitivity_config_',var,'.csv'), stringsAsFactors = F)
  
  all_ee <- matrix(0, nrow=max_r, ncol=length(x0))
  
  if (nrow(calib) %% 2 > 0){
    p <- nrow(calib) *2
  } else {
    p <- nrow(calib) *2
  }
  #p = nrow(calib) *2 # should be even
  del = p/(2*(p-1))
  
 # obs <- read_field_obs('field_data/field_mendota.csv', var)

  for (r in 1:max_r){
    ee <- random_sampling(x0,p,del,lb,ub)
    ee[is.na(ee)] <- ee[which.max(abs(ee))] #replace NA's with max value
    all_ee[r,] <- ee
  }
  
  ee_norm <- (abs(all_ee) - min(abs(all_ee)))/(max(abs(all_ee)) - min(abs(all_ee)))
  
  morris_res <- data.frame('pars'=c(pars), 'mean' = apply(abs(all_ee),2,mean), 'std' = apply(all_ee,2,sd))
  colnames(all_ee) <- pars
  write.csv(all_ee, paste0('results/SA_ee_results_',var,'.csv'), quote = F, row.names = F)
  morris_norm <- data.frame('pars'=c(pars), 'mean' = apply(abs(ee_norm),2,mean), 'std' = apply(ee_norm,2,sd))
  p6 <- ggplot(morris_norm, aes(pars,mean))+ #EDIT THIS- Originally I had it as (morris_res, aes(pars,mean))
    geom_bar(stat="identity", fill = 'blue')+
    geom_hline(yintercept = 0.1, colour = 'red', linetype = 'dashed')+
    ylab('Normalized Mean')+
    xlab('Parameters')+
    ggtitle('Normalized Sensitivity')+
    theme_bw()
  
  ggsave(file=paste0('results/SA_plot_',var,'.png'), p6, dpi = 300,width = 384,height = 216, units = 'mm') #saves g
  
  
  morris_cluster = morris_res[,c('mean','std')]
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:(nrow(morris_cluster)-1)
  mean_ss <- k.values*0
  for (k in k.values){
    km.res <- kmeans(morris_cluster, centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(morris_cluster))
    mean_ss[k-1] <- mean(ss[, 3])
  }
  
  k_means <- kmeans(morris_cluster,k.values[index(max(mean_ss))])
  morris_res_clust <- data.frame(morris_res, "cluster" = k_means$cluster)
  
  p7 <- ggplot(morris_res_clust, aes(mean, std, col= factor(cluster), shape = pars))+
    geom_point()+
    scale_shape_manual(values = seq(0,20,1))+
    ylab('std EE')+
    xlab('mean EE')+
    ggtitle('Sensitivity')+
    theme_bw()
  ggsave(file=paste0('results/SA_plot_',var,'-clust.png'), p7, dpi = 300,width = 150,height = 150, units = 'mm') #saves g
  
  cal_pars = calib[c(which(morris_res_clust$cluster == row(k_means$centers)[k_means$centers==max(k_means$centers)])),]
  
  #Separate parameters to be calibrated and default parameters
  cal_pars = calib[c(which(morris_norm$mean >= 0.1)),]
  def_pars = calib[-c(which(morris_norm$mean >= 0.1)),]
  
  if (any(cal_pars$par %in% pars[duplicated(pars)])) { #to deal with duplicated parameters
    tt <- match(pars[duplicated(pars)],cal_pars$par,pars[duplicated(pars)])
    tt <- tt[!is.na(tt)]
    for (i in 1:length(tt)){
      id <- cal_pars[tt,]$par
      cal_pars <- cal_pars[-tt[i],]
      tt2 <-  which(id == calib$par)
      cal_pars<-rbind(cal_pars, calib[tt2,])
    }
  }
  
  # eg_nml <- read_nml(nml_file = nml_file)
  # for(i in 1:length(pars[!duplicated(pars)])){
  #     if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
  #       eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i],
  #                         p[which(pars[!duplicated(pars)][i] == pars)])
  #     } else {
  #       eg_nml <- set_nml(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
  #     }
  #   }

    
  write.csv(cal_pars, paste0('calibration_file_',var,'.csv'), row.names = F, quote = F)
  
  return()
}



run_calibvalid <- function(var, var_unit, var_seq, cal_pars, pars, ub, lb, init.val, 
                           obs, method, calib.metric, os, target_fit, target_iter,nml_file, flag){
  
  
  #Reset original values OR calibrated values
  for (p in flag){
    if (length(flag) == 0){
      file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
      file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
    } else {
      if (p == 'temp'){
        calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',p,'.csv'))
        eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
        nml <- read_nml('glm3.nml')
        for(i in 2:(ncol(best_par)-1)){
          nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
        }
        write_nml(nml, file = 'glm3.nml')
      } else {
        calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',p,'.csv'))
        eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
        nml <- read_nml('aed2/aed2.nml')
        for(i in 2:(ncol(best_par)-1)){
          nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
        }
        write_nml(nml, file = 'aed2/aed2.nml')
      }
    }
  }
  # 
  # if (flag == 1){
  #   file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
  #   file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
  # } else if (flag == 2){
  #   file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
  # }

  calibration.list <- list("start" = '2013-05-15 12:00:00',
                           "stop" = '2018-12-31 12:00:00')  #EDIT THIS!
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = calibration.list)
  write_nml(nml, 'glm3.nml')
  
  # glmOPT <- pureCMAES(init.val, glmFUNrmse, lower = rep(0,length(init.val)), 
  #                     upper = rep(10,length(init.val)), 
  #                     sigma = 0.3, 
  #                     stopfitness = target_fit, 
  #                     stopeval = target_iter)
  # glmFUNrmse(glmOPT$xmin)
  # 
  # #read in calibration data
  # calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',var,'.csv'))
  # eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
  # write.csv(best_par, paste0('results/calib_par_',var,'.csv'), row.names = F, quote = F)
  # 
  # #Input best parameter set
  # nml <- read_nml(nml_file = nml_file)
  # for(i in 2:(ncol(best_par)-1)){
  #   nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
  # }
  # write_nml(nml, file = nml_file)
  
  
  #begin Robert's version of the glmOPT function
  glmOPT <- pureCMAES(init.val, glmFUNrmse, lower = rep(0,length(init.val)), 
                      upper = rep(10,length(init.val)), 
                      sigma = 0.5, 
                      stopfitness = target_fit, 
                      stopeval = target_iter)
  glmFUNrmse(glmOPT$xmin)
  
  #read in calibration data
  calib <- read.csv(paste0('results/calib_results_',calib.metric,'_',var,'.csv'))
  eval(parse(text = paste0('best_par <- calib[which.min(calib$',calib.metric,'),]')))
  write.csv(best_par, paste0('results/calib_par_',var,'.csv'), row.names = F, quote = F)
  
  best_par <- read.csv(paste0('results/calib_par_',var,'.csv'))
  
  #Input best parameter set
  nml <- read_nml(nml_file = nml_file)
  check_duplicates <- c()
  for (i in 2:(ncol(best_par)-2)){
    string1 <- colnames(best_par)[i]
    for (j in (i+1):(ncol(best_par)-1)){
      string2 <- colnames(best_par)[j]
      if (substr(string1,1,floor(nchar(string1)*9/10)) == substr(string2,1,floor(nchar(string1)*9/10))){
        check_duplicates <- append(check_duplicates, i)
        check_duplicates <- append(check_duplicates, j)
      }
    }
  }
  checked <- 2:(ncol(best_par)-1)
  for (i in 1:length(check_duplicates)){
    checked <- checked[!checked == check_duplicates[i]]
  }
  
  for(i in checked){
    nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
  }
  
  check_duplicates <- matrix(check_duplicates,ncol=2, byrow = TRUE)
  find_dupl_groups <- list()
  it <- 1
  for (ii in 1:nrow(check_duplicates)){
    if (ii == 1){
      find_dupl_groups[[it]] <- (check_duplicates[ii,])
    } else {
      if (ii > 1){
        if (any(check_duplicates[ii,] %in% find_dupl_groups[[it]])){
          place <- !(check_duplicates[ii,] %in% find_dupl_groups[[it]])
          find_dupl_groups[[it]]<-append(find_dupl_groups[[it]], check_duplicates[ii, which(place == TRUE)])
        } else {
          it <- it+1
          find_dupl_groups[[it]] <- (check_duplicates[ii,])
        } 
      }
    } }
  
  for (i in 1:length(find_dupl_groups)){
    nml <- set_nml(nml,
                   gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]],
                   as.numeric(best_par[find_dupl_groups[[i]]]))
    print(gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]])
    print(as.numeric(best_par[unlist(find_dupl_groups[[i]])]))
  }
  #end Robert's version of the glmOPT function
  
  #Run GLM
  run_glm(os)
  h <- paste(filename,', RMSE',
             round(get_rmse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                            obs),2),var_unit,'NSE',
             round(get_nse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                           obs),2),sep=" ")
  png(paste0('results/',var,'_calibration',filename,'.png'), width = 1600, height = 900)
  plot_contour(mod_nc = out, reference = 'surface', h , var, var_unit,var_seq) 
  dev.off()
  
  
  validation.list <- list("start" = '2019-01-01 00:00:00',
                          "stop" = '2019-12-31 12:00:00') # EDITED THIS
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = validation.list)
  write_nml(nml, 'glm3.nml')
  run_glm(os)
  h <- paste(filename,', RMSE',
             round(get_rmse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                            obs),2),var_unit,'NSE',
             round(get_nse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                           obs),2),sep=" ")
  png(paste0('results/',var,'wtemp_validation',filename,'.png'), width = 1600, height = 900)
  plot_contour(mod_nc = out, reference = 'surface', h , var,var_unit,var_seq) 
  dev.off()
  
  
  total.list <- list("start" = '2013-05-15 12:00:00', "stop" = '2019-12-31 12:00:00') #EDIT THIS!
                #EDITED THIS
                #list("start" = '1980-04-01 00:00:00',
                #     "stop" = '2015-12-31 00:00:00')
  nml <- read_nml('glm3.nml')
  nml <- set_nml(nml, arg_list = total.list)
  write_nml(nml, 'glm3.nml')
  run_glm(os)
  h <- paste(filename,', RMSE',
             round(get_rmse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                            obs),2),var_unit,'NSE',
             round(get_nse(temp_mods <- mod2obs(out, obs, reference = 'surface', var), 
                           obs),2),sep=" ")
  png(paste0('results/',var,'wtemp_total',filename,'.png'), width = 1600, height = 900)
  plot_contour(mod_nc = out, reference = 'surface', h , var, var_unit,var_seq) 
  dev.off()
  
  g1 <- diag.plots(mod2obs(out, obs, reference = 'surface', var), obs)
  ggsave(file=paste0('results/mod_obs_',var,'totalperiod_',filename,'.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')
  
  
  
  
  return()
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

plot_contour_thermodep <- function(mod_nc, reference = "surface", h, var, unit, tlevels,
                                   td){
  ncin <- nc_open(mod_nc)
  watdep <- ncvar_get(ncin, "z")
  wattemp <- ncvar_get(ncin, var)
  
  time <- ncvar_get(ncin, "time")
  time.units <- ncatt_get(ncin, "time", "units")
  #sub("^.*\\s2","",time.units$value)
  time.start <- as.POSIXct(strptime(sub("hours since ","",time.units$value), 
                                    format = "%Y-%m-%d %H:%M:%S"))
  datetime <- time.start + time*3600
  
  layer <- ncvar_get(ncin, "NS")
  nc_close(ncin)
  watdep[which(watdep == max(watdep))] <- NaN
  wattemp[which(wattemp == max(wattemp))] <- NaN
  
  sim.watdep <- 0.0*watdep - 999
  for (i in 1:length(datetime)){
    max_depth <- watdep[layer[i],i]
    sim.watdep[1,i] <- max_depth - watdep[1,i]/2 
    for (j in 2:layer[i]){
      sim.watdep[j, i] <- max_depth - (watdep[j,i] + watdep[j-1, i ])/2 
    }
  }
  
  int.dep <- rev(seq(0.25,round(max(watdep,na.rm = TRUE)),0.1))
  sim.wattemp <- matrix(0, nrow = length(int.dep), ncol= length(datetime))
  for (i in 1:length(datetime)){
    sim.approx <- approx(na.omit(sim.watdep[,i]), na.omit(wattemp[,i]), int.dep)
    sim.wattemp[1:length(int.dep),i] <- sim.approx$y
  }
  
  if ((median(apply(sim.watdep,2,max,na.rm=TRUE))+median(apply(sim.watdep,2,sd,na.rm=TRUE)))<
      max(sim.watdep[,1],na.rm=TRUE)){
    max.plot.dep <- ceiling(median(apply(sim.watdep,2,max,na.rm=TRUE)))
  } else {
    max.plot.dep <-ceiling(max(sim.watdep[,1],na.rm=TRUE))
  }
  
  if (max.plot.dep<=20){iter.plot.dep = 1} else if (max.plot.dep<=50){iter.plot.dep = 5} else (iter.plot.dep = 10)
  
  spectral.colors <-  colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral') )
  
  inMeter <- function(x) {paste0(x, " m")}
  inCelsius <- function(x) {paste0(x, paste(" ",unit,sep=''))}
  plot(td$datetime, (-1)*td$thermo.depth, type = 'l', ylim = (range(int.dep*(-1))),ylab='Depth [m]',
       xlab = 'Datetime',cex.lab=1.5, cex.axis=1.5)
  .filled.contour(x=(td$datetime), 
                  y=(int.dep)*(-1), 
                  z = t(sim.wattemp),
                 levels=tlevels,
                 col=rev(spectral.colors(length(tlevels))))
  axis(1, labels=format(pretty(datetime,20), "%Y-%b"), at = as.numeric(pretty(datetime,20)), cex.axis=0.8,las=2)
 # axis(2, labels=inMeter(rev(seq(0,max.plot.dep,iter.plot.dep))*(-1)), at = rev(seq(0,max(max.plot.dep),iter.plot.dep))*(-1), cex.axis=1)
  lines(td$datetime, td$thermo.depth*(-1), col='black', lwd = 2, lty = 'dashed')
     #            key.axes = {axis(4,at=unique(tlevels),labels=(inCelsius(unique(tlevels))))})
}

resample_depth <- function(elevs, temps, elevs_out){
  #strip out NAs
  rmv_i <- temps>=1e30 | elevs>=1e30
  elevs <- elevs[!rmv_i]
  temps <- temps[!rmv_i]
  num_z <- length(elevs)
  layer_mids <- c(elevs[1]/2, elevs[1:num_z-1] + diff(elevs)/2)
  temps_re <- c(temps[1], temps, tail(temps,1))
  elevs_re <- c(0, layer_mids, tail(elevs, 1))
  
  temps <- approx(x = elevs_re, y = temps_re, xout = elevs_out)$y
  return(temps)
}

mod2obs_phy <- function(mod_nc, obs, reference = 'surface', var){
  deps = seq(0,2,by = 0.5)
  #tim = unique(obs[,1])
  mod <- glmtools::get_var(file = mod_nc,var,reference = reference, z_out = deps)
  mod <- data.frame('DateTime' = mod$DateTime, 'Var' = apply(mod[,2:ncol(mod)],1,mean))
  obs$DateTime <- as.Date(obs$DateTime,format='%Y-%m-%d')
  obs <- as.data.frame(obs)
  mod$DateTime <- as.Date(mod$DateTime,format='%Y-%m-%d')
  mod <- match.tstep1(obs, mod) #From gotm_functions.R #EDIT THIS
  colnames(mod) <- c('DateTime', var)
  mod <- mod[order(mod$DateTime),]
  if(nrow(mod) != nrow(obs)){
    mod <- merge(obs, mod, by = c(1), all.x = T)
    mod <- mod[order(mod$DateTime),]
    mod <- mod[,c(1,3)]
    colnames(mod) <- c('DateTime', var)
  }
  return(mod)
}
