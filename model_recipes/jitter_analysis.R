##
# Jitter Test for Global Convergence with Stock Synthesis 
# Stock Synthesis (tested in version 3_30_X for Windows) 
# r4ss (tested in version(s) 1.35.1 - 1.35.3)  
# R (tested in version(s) 3.3.2 - 4.1.1 64 bit Windows)
# code updated 1/20/2022 by Meg Oshima for ss3diags::simple model
##
#devtools::install_github("r4ss/r4ss", ref="development")
library(r4ss)
library(tidyverse)

# Step 1. Define the root directory for the run
dirname.root <- file.path("./model_recipes")
dirname.root

# Step 2. Define the directory where a completed "base" model run is located
dirname.base <- paste0(dirname.root,'/reference_run')
dirname.base

# Step 3. Create a subdirectory for the jitter run
dirname.jitter <- paste0(dirname.root,'/jitter')
dirname.jitter
dir.create(path=dirname.jitter, showWarnings = TRUE, recursive = TRUE)

# Step 4. Create a subdirectory for the output
dirname.plots <- paste0(dirname.root,"/jitter_plots")
dirname.plots
dir.create(dirname.plots)

# Step 5. Copy base model files to jitter directory 
file.copy(paste(dirname.base,       "starter.ss", sep="/"),
          paste(dirname.jitter,     "starter.ss", sep="/"))
file.copy(paste(dirname.base,       "em.CTL", sep="/"),
          paste(dirname.jitter,     "em.CTL", sep="/"))
file.copy(paste(dirname.base,       "ss3.DAT", sep="/"),
          paste(dirname.jitter,     "ss3.DAT", sep="/"))	
file.copy(paste(dirname.base,       "forecast.ss", sep="/"),
          paste(dirname.jitter,     "forecast.ss", sep="/"))
file.copy(paste(dirname.base,       "ss.exe", sep="/"),
          paste(dirname.jitter,     "ss.exe", sep="/"))
file.copy(paste(dirname.base,       "ss.par", sep="/"),
          paste(dirname.jitter,     "ss.par", sep="/"))


#------------ Run Jitter Test for Global Convergence with Stock Synthesis -------------------------------

# Step 6. Set the number of iteration  
Njitter=200

# Step 7. Run jitter using this function (deafult is nohess)
jit.likes <- SS_RunJitter(mydir=dirname.base, 
                          Njitter=Njitter, 
                          jitter_fraction = 0.1, 
                          init_values_src = 1)

# Step 11. Total likelihoods necessary to assess global convergence are saved to "jit.likes"
x <- as.numeric(jit.likes)
global.convergence.check <- table(x,exclude = NULL)
write.table(jit.likes, paste0(dirname.plots, "/jit_like.csv"))
write.table(global.convergence.check, paste0(dirname.plots, "/global_convergence_check.csv"))


# Step 12. Summarize more Jitter results
#jitter=seq(1:Njitter)

jit_mods <- SSgetoutput(keyvec = 0:Njitter, #include reference run
                       getcomp = FALSE, 
                       dirvec = dirname.base, 
                       getcovar = FALSE) 
jit_summary <- SSsummarize(jit_mods)

#Likelihood across runs
likes=jit_summary$likelihoods

#Derived quants across runs
quants=jit_summary$quants

#Estimated parameters across runs
pars=jit_summary$pars

#Write more output tables to jitter directory
write.table(quants, paste0(dirname.plots, "/Quants.csv"))
write.table(pars, paste0(dirname.plots,"/Pars.csv"))
write.table(likes, paste0(dirname.plots,"/Likelihoods.csv"))

#Retabulate total likelihoods necessary to assess global convergence and compare to jit.likes from above 
x <- likes %>% 
  filter(str_detect(Label, "TOTAL")) %>% 
  select(-Label) %>% 
  mutate_all(~as.numeric(.)) %>% 
  unlist(use.names = FALSE)

global.convergence <- table(x,exclude = NULL)
write.table(global.convergence, paste0(dirname.plots, "/global_convergence.csv"))

#Check convergence by seeing if the estimated spawning biomass is really big (+2x base spawning biomass) or really small (<0.5x base spawning biomass). Based on `check_convergence()` from SSMSE. 
converged_ssb <- jit_summary$SpawnBio %>%  
  mutate(across(c(1:201),
                 .fns = ~./replist0)) %>% 
  select(-Label) %>% 
  pivot_longer(col = c(1:201), names_to = "jitter", values_to = "SSB") %>% 
  pivot_wider(names_from = Yr, values_from = SSB) %>% 
  mutate(rownumber = seq(1, nrow(.))) %>% 
  column_to_rownames("jitter") %>% 
  filter_at(vars(1:78), all_vars((.) < 2 & (.) > 0.5)) %>% 
  select(rownumber) %>% 
  pull(rownumber)

### checking to make sure max gradient is small
converged_grad <- which(jit_summary$maxgrad < 0.001)


converged_jitters <- jit_mods[converged_grad]
converged_sum <- SSsummarize(converged_jitters)

#------------ Make plots with r4ss for runs ending at a converged solution -------------------------------

#make some plots
#plot of likelihood for all jitter runs, regardless of convergence
jit_summary$likelihoods %>% 
  filter(str_detect(Label, "TOTAL")) %>% 
  select(-Label) %>% 
  pivot_longer(cols = everything(), names_to = "jitter", values_to = "likelihood") %>% 
  separate(jitter, into = c("replist", "jitter"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate(jitter = as.numeric(jitter)) %>% 
  ggplot(aes(x = jitter, y = likelihood)) +
  geom_point(size = 2) +
  geom_hline(aes(yintercept = likelihood[1]), color = "red") +
  theme_classic() +
  labs(y = "Total Likelihood", 
       x = "Jitter runs")
ggsave(paste0(dirname.plots, "/all_likelihoods.png")) #make sure to run plot above right before this line, it defaults to saving the last plot run

SSplotComparisons(jit_summary, 
                  subplots = 2, 
                  pch = "", 
                  legend=FALSE, 
                  lwd = 1, 
                  new = F, 
                  print = TRUE, 
                  plotdir = dirname.plots, 
                  filenameprefix = "all_jitters_",
                  ylimAdj=1)


# Repeat for all converged runs 
converged_sum$likelihoods %>% 
  filter(str_detect(Label, "TOTAL")) %>% 
  select(-Label) %>% 
  pivot_longer(cols = everything(), names_to = "jitter", values_to = "likelihood") %>% 
  separate(jitter, into = c("replist", "jitter"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate(jitter = as.numeric(jitter)) %>% 
  ggplot(aes(x = jitter, y = likelihood)) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = likelihood[1]), color = "red") +
  theme_classic() +
  labs(y = "Total Likelihood", 
       x = "Jitter runs at a converged solution")
ggsave(paste0(dirname.plots, "/converged_likelihoods.png"))


SSplotComparisons(converged_sum,     
                  subplots = 2, 
                  pch = "",
                  legend=FALSE,
                  lwd = 1,
                  new = F, 
                  print = TRUE,
                  plotdir = dirname.plots, 
                  filenameprefix = "converged_",
                  ylimAdj=1)


#Repeat for converged runs at the minimum solution 
#Converged runs at min converged solution (should be same as base case to pass the test) 
#min(na.omit(jit.likes))
y <- which(jit_summary$likelihoods[jit_summary$likelihoods$Label=="TOTAL",1:Njitter]==min(na.omit(jit.likes)))

jit_min <- jit_mods[y]
min_sum <- SSsummarize(jit_min)

SSplotComparisons(min_sum,     
                  subplots =  2, 
                  pch = "",
                  legend=FALSE,
                  lwd = 1,
                  new = F, 
                  print = TRUE,
                  plotdir = dirname.plots, 
                  filenameprefix = "converged_min_",
                  ylimAdj=1)

