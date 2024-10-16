#libraries--------------------------------------------------
library(dplyr)
#install.packages("DesTools")
library(DesTools) #Konfidenzintervall wird angezeigt. Liegen die meisten Punkte im Koinzidenzintervall, so ist
# von einer NV auszugehen -> Funktion: PlotQQ()
#install.packages("ggplot2")
library(ggplot2) #Funktion ggplot(data, aes(sample= zB Größe))+ stat_qq+stat_qq_line(), 
library(tidyr)
library(reshape2)

#setwd("/Users/emiliathoma/Desktop")
#read data--------------------------------------
#ratte
filenames_rat=dir("/Users/emiliathoma/Desktop/rat", full.names = T)

#results_rat_supra = read.csv(file="/Users/emiliathoma/Desktop/results_supra_rat.csv", header=T)
results_rat_supra_new = read.csv(file="/Users/emiliathoma/Desktop/results_supra_rat_stim_V4.csv", header=T)
results_rat_synapse = do.call(rbind, lapply(filenames_rat, read.csv))




#data preparation--------------------------------
#vector stimuli currents
stimuli = as.character(c(-300,-200,-150, -100, -50, 50, 100, 150,200,250,300,350,400,450,500,550,600,700,800,900))
#all columns that start with freq_Hz and end with 100ms get written into freqs dataframe
freqs = results_rat_supra_new[,grep("freq_Hz.*100ms",colnames(results_rat_supra_new))]
#naming columnnames of freqs stimuli currents
colnames(freqs)=stimuli

###needs datfrme freqs (see below)
#classification of neurons 
for (i in  1:length(freqs$`-300`)){
  if(isTRUE(max(freqs[i,])>= 150)){
    results_rat_supra_new$neuron_class[i] ="fastspiking IN"
  }
  else if(isTRUE(max(freqs[i,]) < 150) & isTRUE(max(freqs[i,]) >= 100)){
    results_rat_supra_new$neuron_class[i] ="non-fastspiking IN"
  }
  else if(isTRUE(max(freqs[i,])< 100)){
    results_rat_supra_new$neuron_class[i] ="PN"
  }
 
}

#frequency as a function of stimuli current by neuron class
IF_data = data.frame(rows = results_rat_supra_new$neuron_class, stack(freqs))

# means and SD of frequency as a function of stimuli current by neuron class
IF_means = IF_data %>% group_by(rows, ind) %>% summarise(mean(values), sd(values)) 
colnames(IF_means)=c("class", "current", "frequency", "sd")

# all tsynapses are read in of all experiments are connected and clust
#directory with all tsynapses (results)
# initialisation of dataframe for clustering of tsynapse results
cluster = data.frame(filename = character(),
                     cluster_size = integer(),
                     connectivity = double(),
                     synapses = integer(),
                     connections = integer()
)

# calculations of: cluster size, connectivity, connection probability for every experiment/tsynapse => write in Cluster df
for (i in 1:length(filenames_rat)){
  current_file = read.csv(filenames_rat[i])
  synapses = length(rownames(current_file))
  cluster_size = n_distinct(current_file$pre_syn)
  #print(nodes)
  connections = sum(current_file$is_connected)
  connectivity = connections/synapses
  print(connectivity)
  cluster=rbind(cluster,c(basename(filenames_rat[i]), cluster_size, connectivity, synapses, connections))
  
}
#renaming of cluster df columnnames
colnames(cluster)=c("filename", "cluster_size", "connectivity", "synapses", "connections")

#cluster analysis df, mean connectivity, standard deviation, coefficient of variance for every cluster
cluster_analysis = cluster %>% 
  group_by(cluster_size) %>% 
  summarise(mean(as.numeric(connectivity)), sd(connectivity),mean(as.numeric(connectivity))/sd(connectivity))
colnames(cluster_analysis)=c("cluster_size", "mean_connectivity", "SD", "CV")

for(i in 1:length(results_rat_supra_new$cell_ID)){
  results_rat_synapse$pre_syn_class[grep(results_rat_supra_new$cell_ID[i], results_rat_synapse$pre_syn )]=results_rat_supra_new$neuron_class[i]
}

for(i in 1:length(results_rat_supra_new$cell_ID)){
  results_rat_synapse$post_syn_class[grep(results_rat_supra_new$cell_ID[i], results_rat_synapse$post_syn )]=results_rat_supra_new$neuron_class[i]
  
}


#table for connectivity between different neuron class combination
con_rat = results_rat_synapse %>% group_by(pre_syn_class,post_syn_class) %>% summarise(connectivity = mean(is_connected), tested_synapses=n(), connections=sum(is_connected))

#data visualisation--------------------


#freq Hz 0ms-100ms
ggplot(results_rat_supra_new, aes(x = neuron_class, y = freq_Hz_350pA_0ms_100ms)) +
  geom_point() +                  # Plot individual data points
  theme_minimal() +                # Minimal theme for cleaner look
  labs(title = "frequency rat at 350pA", x = "neuron Type", y = "freq in Hz")




#input resistance for the diffenerent classes
ggplot(results_rat_supra_new, aes(x = neuron_class, y = abs(input_resistance/1e6))) +
  geom_point() +                  # Plot individual data points
  theme_minimal() +                # Minimal theme for cleaner look
  labs(title = "input resistance rat", x = "neuron type", y = expression("R" ["in "]*"in M"*Omega))


ggplot(results_rat_supra_new, aes(x = neuron_class, y = half_duration_ms)) +
  geom_point() +                  # Plot individual data points
  theme_minimal() +                # Minimal theme for cleaner look
  labs(title = "half duration in ms at 350 pA", x = "neuron type", y = "t in ms")

bp2=subset(results_rat_supra_new, resting_pot < -45) %>% ggplot(aes(x = neuron_class, y = abs(input_resistance/1e6)))+
  geom_boxplot(aes(group = neuron_class))+
  geom_jitter(width = 0.1)+ labs(title = "input resistance per neuron class", x = "firing frequency", y = expression("R" ["in "]*"in M"*Omega))

bp2
bp=subset(results_rat_supra_new,half_duration_ms != 0) %>% ggplot(aes(x = neuron_class, y = half_duration_ms))+
  geom_boxplot(aes(group = neuron_class))+
  geom_jitter(width = 0.1)+ labs(title = "half duration rat at 350 pA", x = "neuron type", y = "t in ms")
bp
vp=results_rat_supra_new %>% ggplot(aes(x = neuron_class, y = half_duration_ms))+ geom_violin(aes(group = neuron_class))
vp
vp=results_rat_supra_new %>% ggplot(aes(x = neuron_class, y = input_resistance/1e6))+ geom_violin(aes(group = neuron_class))
vp

#synapses rat
maximum_freq = c()
for (i in 1:length(freqs$`50`)){
  maximum_freq[i]= max(freqs[i,])
}
maximum_freq=as.data.frame(maximum_freq)

table(cluster$cluster_size)
ggplot(dat = cluster, aes(x = cluster_size)) + 
  geom_bar()+ labs(title = "number of experiments per cluster size", x = "cluster size", y = "count")

results_rat_supra_new=cbind(results_rat_supra_new, maximum_freq)


results_rat_supra_new %>%  filter(resting_pot < -45 & maximum_freq > 0) %>% 
  ggplot(aes(x=maximum_freq, y= input_resistance))+geom_point()+ geom_smooth(method = "lm")+geom_point(aes(color=neuron_class))+ 
  labs(title = "input resistance depending on maximum firing frequency rat", x = "neuron type", y = expression("R" ["in "]*"in M"*Omega))



results_rat_supra_new %>%  filter(resting_pot < -45 & maximum_freq > 0) %>%ggplot( aes(x = neuron_class, y = maximum_freq)) +
  geom_point() +                  # Plot individual data points
  theme_minimal() +                # Minimal theme for cleaner look
  labs(title = "filtered max. frequency rat", x = "neuron type", y = "f in Hz")


bp2=results_rat_supra_new %>%  filter(resting_pot < -45 & maximum_freq > 0) %>% 
  ggplot(aes(x = neuron_class, y = abs(input_resistance/1e6)))+
  geom_boxplot(aes(group = neuron_class))+
  geom_jitter(width = 0.1)+ labs(title = "filtered input resistance", x = "neuron type", y = expression("R" ["in "]*"in M"*Omega))
bp2


cluster_analysis.long <- melt(cluster_analysis,id = "cluster_size", measure = c("mean_connectivity", "SD", "CV"))
#cluster_analysis.long$cluste r_size = 
ggplot(data=cluster_analysis.long, aes(x=as.numeric(cluster_size), y= value,color=variable))+
  geom_point()+ labs(title = "connectivity per cluster size", x = "cluster size", y = "mean connectivity, SD, CV")
  
ggplot(data=cluster, aes(x=cluster_size, y= as.numeric(connectivity),color=connectivity))+
  geom_point()+ geom_jitter(width=0.1)+ labs(title = "connectivity per cluster size", x = "cluster size", y = "mean connectivity, SD, CV")



p<- ggplot(IF_means, aes(x=current, y=frequency, group=class, color=class)) +
  geom_line()
  geom_point()+
  geom_errorbar(aes(ymin=frequency-sd, ymax=frequency+sd), width=.2,
                position=position_dodge(0.05))
p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "f-i-curve", x = expression("I" ["stim "]*"in pA"), y = "firing frequency in Hz")

p<- ggplot(IF_means, aes(x=current, y=frequency, group=, color=class)) +
  geom_line()
geom_point()+
  geom_errorbar(aes(ymin=frequency-sd, ymax=frequency+sd), width=.2,
                position=position_dodge(0.05))
p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "f-i-curve", x = expression("I" ["stim "]*"in pA"), y = "firing frequency in Hz")



ggplot(data=IF_data, aes(x=ind, y= values,color=rows))+geom_point()



####synapse results pre and post tpe classification





