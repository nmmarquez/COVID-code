rm(list=ls())
library(tidyverse)
library(gganimate)

N <- 100
steps <- 100

update_xy <- function(xy, sigma = .04){
    newxy <- xy
    for(i in 1:nrow(xy)){
        valid <- FALSE
        while(!valid){
            update_check <- xy[i,] + rnorm(2, sd = sigma)
            valid <- all(update_check > 0) & all(update_check < 1)
        }
        newxy[i,] <- update_check
    }
    newxy
}

run_simulation <- function(samples, steps, sigma = .04){
    sims <- matrix(runif(samples*2), samples, 2)
    sim_list <- list(sims)
    for(j in 2:steps){
        sims <- update_xy(sims)
        sim_list <- c(sim_list, list(sims))
    }
    as_tibble(do.call(rbind, sim_list)) %>%
        rename(x = V1, y = V2) %>%
        mutate(obs = rep(1:samples, steps), time = rep(1:steps, each=samples))
}

run_simulation(100, 25) %>%
    ggplot(aes(x, y)) +
    geom_point(alpha = .3) +
    theme_classic() +
    transition_time(time)
