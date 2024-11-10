#Libraries------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(ggpubr)

#Importing of data--------------

#Supra
rat_supra = read.csv(file="/Users/emiliathoma/Desktop/results_supra_rat_V6.csv", header=T)
rat_supra$species = "Ratte"


human_supra = read.csv(file="/Users/emiliathoma/Desktop/results_supra_human_V6.csv_V6.csv", header=T)
human_supra$species = "Mensch"

#Synapse
#Directories of tsynapses
dir_el_rat=dir("/Users/emiliathoma/Desktop/rat/electrical", full.names = T)
dir_chem_rat=dir("/Users/emiliathoma/Desktop/rat/chemical", full.names = T)
dir_el_hum=dir("/Users/emiliathoma/Desktop/human/electric_synapses", full.names = T)

#Synapse data
data_chem_rat = do.call(rbind, lapply(dir_chem_rat, read.csv))
data_chem_rat$species="Ratte"
data_chem_rat$synapse_type="chemisch"

data_el_rat = do.call(rbind, lapply(dir_el_rat, read.csv))
data_el_rat$species="Ratte"
data_el_rat$synapse_type="elektrisch"

data_chem_hum = do.call(rbind, lapply(dir_chem_hum, read.csv))
data_chem_hum$species="Mensch"
data_chem_hum$synapse_type="chemisch"

data_el_hum = do.call(rbind, lapply(dir_el_hum, read.csv))
data_el_hum$species="Mensch"
data_el_hum$synapse_type="elektrisch"

#Combined Synapses
combined_synapses = rbind(data_chem_rat,data_el_rat,data_chem_hum, data_el_hum)

combined_synapses %>% filter(is_connected == 1) %>%  group_by(species, synapse_type) %>% summarise(Amplitude = mean(mean_IPSP), PPR=mean(IPSP_stim_2/IPSP_stim_1),SD_A=sd(mean_IPSP), SD_PPR=sd(IPSP_stim_2/IPSP_stim_1), n = n())

#   species synapse_type     Amplitude  PPR     SD_A    SD_PPR     n
#   <chr>   <chr>            <dbl>      <dbl>   <dbl>   <dbl>   <int>
#1  Mensch  chemisch         0.467      0.802   0.375   0.313      7
#2  Mensch  elektrisch       0.469      0.776   0.163   0.0185     2
#3  Ratte   chemisch         0.453      0.824   0.540   0.267     10
#4  Ratte   elektrisch       1.26       0.828   0.851   0.419      3

#Synapse classification by IPSP/EPSP
PSP = read.table(file="/Users/emiliathoma/Downloads/combined_synapse_PSP.csv", sep = ";", header = T)

combined_synapses_connected = combined_synapses %>% filter(is_connected == 1) %>% mutate(psp_type=PSP %>% filter(is_connected==1)%>% pull(psp_type))
combined_synapses_connected %>%  group_by(species, synapse_type, psp_type) %>% summarise(Amplitude = mean(mean_IPSP), PPR=mean(IPSP_stim_2/IPSP_stim_1),SD_A=sd(mean_IPSP), SD_PPR=sd(IPSP_stim_2/IPSP_stim_1), n = n())

#  species synapse_type psp_type Amplitude   PPR    SD_A  SD_PPR     n
#  <chr>   <chr>        <chr>        <dbl> <dbl>   <dbl>   <dbl> <int>
#1 Mensch  chemisch     "EPSP"       0.465 0.869  0.410   0.284      6
#2 Mensch  chemisch     "IPSP"       0.484 0.404 NA      NA          1
#3 Mensch  elektrisch   ""           0.469 0.776  0.163   0.0185     2
#4 Ratte   chemisch     "EPSP"       0.302 1.08   0.0838  0.397      2
#5 Ratte   chemisch     "IPSP"       0.490 0.760  0.605   0.214      8
#6 Ratte   elektrisch   ""           1.26  0.828  0.851   0.419      3


#QC of combined
QC=read.table(file="/Users/emiliathoma/Downloads/combined_supra_yes_no.csv", sep = ";", header = T)

combined_supra = rbind(human_supra, rat_supra)
all_supra_with_included = merge(combined_supra,QC)
combined_supra_QC = all_supra_with_included %>% filter(included == "1")

rat_supra_QC = combined_supra_QC %>% filter(species=="Ratte")
human_supra_QC = combined_supra_QC %>% filter(species=="Mensch")

failed_QC = all_supra_with_included %>% filter(included == "0")
failed_QC$neuron_class = "nc N"
failed_QC$maximum_freq = "NA"

#Frequency and Neuron type classification
#vector stimuli currents
stimuli = as.character(c(-300,-200,-150, -100, -50,0, 50, 100, 150,200,250,300,350,400,450,500,550,600,700,800,900))

#combined QC
freqs_combined_QC = combined_supra_QC[,grep("freq_Hz.*100ms",colnames(combined_supra_QC))]


#combined no QC
freqs_combined = combined_supra[,grep("freq_Hz.*100ms",colnames(combined_supra))]

colnames(freqs_combined)=stimuli
colnames(freqs_combined_QC)=stimuli

#WORK in PROGRESS
for (i in  1:length(freqs_combined_QC$`-300`)){
  combined_supra_QC$max_freq[i]=max(freqs_combined_QC[i,])
  if(isTRUE(max(freqs_combined_QC[i,])>= 150 & combined_supra_QC$input_resistance[i] <= 2*10^8 & combined_supra_QC$half_duration_ms[i] <= 0.65) ){
    combined_supra_QC$neuron_class[i] ="FSIN"
  }
  else if(isTRUE(max(freqs_combined_QC[i,])>80 & combined_supra_QC$half_duration_ms[i] > 0.65  & combined_supra_QC$half_duration_ms[i] <= 1 & combined_supra_QC$input_resistance[i] <= 2*10^8)){  
    combined_supra_QC$neuron_class[i] ="NFSIN"
  }
  else{
    combined_supra_QC$neuron_class[i] ="UDN"
  }
}

combined_supra_QC %>% group_by(neuron_class) %>% summarise(freq=mean(max_freq), res=mean(input_resistance), duration= mean(half_duration_ms),n=n())

combined_supra_QC %>%filter(species=="Mensch") %>%  group_by(neuron_class) %>% summarise(freq=mean(max_freq), res=mean(input_resistance), duration= mean(half_duration_ms), resting= mean(resting_pot),AP_ampl= mean(AP_Amplitude),uptodown = mean(abs(upstroke_mVperms/downstroke_mVperms)),down=mean(downstroke_mVperms),up=mean(upstroke_mVperms),n=n())

#WORK in PROGRESS
for (i in  1:length(freqs_combined$`-300`)){
  combined_supra$max_freq[i]=max(freqs_combined[i,])
  if(isTRUE(max(freqs_combined[i,])>= 150 & combined_supra$input_resistance[i] <= 2*10^8 & combined_supra$half_duration_ms[i] <= 0.65) ){
    combined_supra$neuron_class[i] ="FSIN"
  }
  else if(isTRUE(max(freqs_combined[i,])>80 & combined_supra$half_duration_ms[i] > 0.65  & combined_supra$half_duration_ms[i] <= 1 & combined_supra$input_resistance[i] <= 2*10^8)){  
    combined_supra$neuron_class[i] ="NFSIN"
  }
  else{
    combined_supra$neuron_class[i] ="UDN"
  }
}
combined_supra_QC %>% group_by(neuron_class, species) %>% summarise(freq=mean(max_freq), res=mean(input_resistance), duration= mean(half_duration_ms, na.rm = T),n=n())



Electrophysiology_res = data.frame(Neuron_class = character(),
                                   Tested Variable = character(),
                                   species = character(),
                                   cluster_size = integer(),
                                   connectivity = double(),
                                   connectivity_theo = double(),
                                   synapses = integer(),
                                   connections = integer())
)




library(rstatix)
supra_half = combined_supra_QC[!is.na(combined_supra_QC$half_duration_ms),]
supra_half %>% group_by(neuron_class)  %>%
  wilcox_test(data =., half_duration_ms ~ species) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance("p.adj")



Results2=combined_supra_QC %>% group_by(neuron_class, species) %>% summarise(mean(max_freq, na.rm = T),sd(max_freq, na.rm = T),mean(freq_Hz_350pA_0ms_100ms, na.rm = T),sd(freq_Hz_350pA_0ms_100ms, na.rm = T),mean(freq_Hz_600pA_0ms_100ms, na.rm = T),sd(freq_Hz_600pA_0ms_100ms, na.rm = T),mean(freq_Hz_900pA_0ms_100ms, na.rm = T),sd(freq_Hz_900pA_0ms_100ms, na.rm = T),  mean(na.omit(input_resistance)),sd(input_resistance, na.rm=T),res=mean(resting_pot, na.rm=T),sd(resting_pot, na.rm=T),mean(AP_Amplitude, na.rm=T),sd(AP_Amplitude, na.rm=T), duration= mean(half_duration_ms, na.rm = T),sd(half_duration_ms, na.rm = T), mean(upstroke_mVperms, na.rm=T), sd(upstroke_mVperms, na.rm = T) ,mean(downstroke_mVperms, na.rm=T), sd(downstroke_mVperms, na.rm = T),mean(upstroke_mVperms/downstroke_mVperms, na.rm=T), sd(upstroke_mVperms/downstroke_mVperms, na.rm = T),n=n())

combined_supra_QC %>% group_by(neuron_class, species) %>% summarise(res=mean(input_resistance),sd(input_resistance, na.rm=T))

#neuron_class .y.         group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#<chr>        <chr>       <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>      
#1 FSIN         resting_pot Mensch Ratte      5    15        35 0.866 0.866 ns          
#2 NFSIN        resting_pot Mensch Ratte      1    11         0 0.167 0.250 ns          
#3 UDN          resting_pot Mensch Ratte      6    16        16 0.017 0.051 ns

#species .y.         group1 group2    n1    n2    statistic p p.adj p.adj.signif
#<chr>   <chr>       <chr>  <chr>  <int> <int>     <dbl>    <dbl> <dbl> <chr>      
#1 Mensch  resting_pot FSIN   NFSIN      5     1         4 0.667 1     ns          
#2 Mensch  resting_pot FSIN   UDN        5     6        20 0.429 0.858 ns          
#3 Mensch  resting_pot NFSIN  UDN        1     6         3 1     1     ns          
#4 Ratte   resting_pot FSIN   NFSIN     15    11        55 0.164 0.492 ns          
#5 Ratte   resting_pot FSIN   UDN       15    16        83 0.151 0.492 ns          
#6 Ratte   resting_pot NFSIN  UDN       11    16        84 0.865 1     ns  

#neuron_class .y.      group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#<chr>        <chr>    <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>      
#1 FSIN         max_freq Mensch Ratte      5    15        13 0.0328 0.0984 ns          
#2 NFSIN        max_freq Mensch Ratte      1    11         5 1      1      ns          
#3 UDN          max_freq Mensch Ratte      6    16        33 0.294  0.441  ns  

#species .y.      group1 group2    n1    n2 statistic         p    p.adj p.adj.signif
#<chr>   <chr>    <chr>  <chr>  <int> <int>     <dbl>     <dbl>    <dbl> <chr>      
#1 Mensch  max_freq FSIN   NFSIN      5     1        5  0.333     0.500    ns          
#2 Mensch  max_freq FSIN   UDN        5     6       25  0.082     0.164    ns          
#3 Mensch  max_freq NFSIN  UDN        1     6        4  0.857     0.857    ns          
#4 Ratte   max_freq FSIN   NFSIN     15    11      127  0.02      0.06     ns          
#5 Ratte   max_freq FSIN   UDN       15    16      217  0.0000371 0.000223 ***        
#6 Ratte   max_freq NFSIN  UDN       11    16      100. 0.554     0.665    ns  

#neuron_class .y.         group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#<chr>        <chr>       <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>      
#1 FSIN         resting_pot Mensch Ratte      5    15        35 0.866 0.866 ns          
#2 NFSIN        resting_pot Mensch Ratte      1    11         0 0.167 0.250 ns          
#3 UDN          resting_pot Mensch Ratte      6    16        16 0.017 0.051 ns

# A tibble: 6 × 10
#species .y.                     group1 group2    n1    n2 statistic          p     p.adj p.adj.signif
#<chr>   <chr>                   <chr>  <chr>  <int> <int>     <dbl>      <dbl>     <dbl> <chr>      
#1 Mensch  freq_Hz_900pA_0ms_100ms FSIN   NFSIN      5     1       5   0.333      0.500     ns          
#2 Mensch  freq_Hz_900pA_0ms_100ms FSIN   UDN        5     6      25   0.082      0.164     ns          
#3 Mensch  freq_Hz_900pA_0ms_100ms NFSIN  UDN        1     6       4   0.857      0.857     ns          
#4 Ratte   freq_Hz_900pA_0ms_100ms FSIN   NFSIN     15    11     152   0.000299   0.000897  ***        
#5 Ratte   freq_Hz_900pA_0ms_100ms FSIN   UDN       15    16     240   0.00000135 0.0000081 ****        
#6 Ratte   freq_Hz_900pA_0ms_100ms NFSIN  UDN       11    16      95.5 0.681      0.817     ns  

# A tibble: 6 × 10
#species .y.                group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#<chr>   <chr>              <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>      
#1 Mensch  downstroke_mVperms FSIN   NFSIN      5     1         0 0.333 0.400 ns          
#2 Mensch  downstroke_mVperms FSIN   UDN        5     6         4 0.052 0.078 ns          
#3 Mensch  downstroke_mVperms NFSIN  UDN        1     6         3 1     1     ns          
#4 Ratte   downstroke_mVperms FSIN   NFSIN     15    11        38 0.022 0.044 *          
#5 Ratte   downstroke_mVperms FSIN   UDN       15    16        37 0.001 0.006 **          
#6 Ratte   downstroke_mVperms NFSIN  UDN       11    16        31 0.005 0.015 *  

# A tibble: 6 × 10
#species .y.              group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#<chr>   <chr>            <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>      
#1 Mensch  upstroke_mVperms FSIN   NFSIN      5     1         3 1     1     ns          
#2 Mensch  upstroke_mVperms FSIN   UDN        5     6        16 0.931 1     ns          
#3 Mensch  upstroke_mVperms NFSIN  UDN        1     6         2 0.857 1     ns          
#4 Ratte   upstroke_mVperms FSIN   NFSIN     15    11        57 0.194 0.554 ns          
#5 Ratte   upstroke_mVperms FSIN   UDN       15    16       148 0.277 0.554 ns          
#6 Ratte   upstroke_mVperms NFSIN  UDN       11    16       136 0.019 0.114 ns


IF_data= data.frame(neuron_class =combined_supra_QC$neuron_class, species=combined_supra_QC$species, stimuli=stack(freqs_combined_QC))
IF_means = IF_data %>% group_by(neuron_class, stimuli.ind,species) %>% summarise(mean(stimuli.values), sd(stimuli.values),sd(stimuli.values)/sqrt(n()) , n=n())

colnames(IF_means)=c("neuron_class", "stimuli", "species", "frequency", "sd","sem","n")


p = IF_means %>% filter(species=="Ratte") %>% ggplot(aes(x=stimuli, y=frequency, group=neuron_class, color=neuron_class))+
  geom_line() +
  geom_point()+
  
  #ylim(0,250)+
  geom_errorbar(aes(ymin=frequency-sem, ymax=frequency+sem), width=.2,
                position=position_dodge(0.2))
p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Ratte", x = expression("I" ["stim "]*"in pA"), y = "f in Hz")+theme_classic()+guides(color = guide_legend(title = "Neuron"))  














combined_all =  merge(combined_synapses, combined_supra_QC[,c("neuron_class","cell_ID","included")], by.x = "pre_syn", by.y = "cell_ID", all.x = TRUE)
names(combined_all)[names(combined_all) == "included"] <- "pre_syn_included"

combined_all =  merge(combined_all, combined_supra_QC[,c("neuron_class", "cell_ID","included")], by.x = "post_syn", by.y = "cell_ID", all.x = TRUE)
names(combined_all)[names(combined_all) == "included"] <- "post_syn_included"

combined_all =  merge(combined_all, combined_synapses_connected[,c("pre_syn","psp_type")], by.x = "pre_syn", by.y = "pre_syn", all.x = TRUE)
names(combined_all)[names(combined_all) == "included"] <- "post_syn_included"



combined_all %>% filter(is_connected == 1 & pre_syn_included+post_syn_included==2) %>% group_by(species, synapse_type) %>% summarize(n=n())

combined_all %>% filter(is_connected == 1 & pre_syn_included==1) %>% group_by(species, synapse_type) %>% summarize(n=n())


#Connectivity Read data
cluster = data.frame(filename = character(),
                     synapse_type = character(),
                     species = character(),
                     cluster_size = integer(),
                     connectivity = double(),
                     connectivity_theo = double(),
                     synapses = integer(),
                     connections = integer())


synapse_filenames = c(dir_chem_rat, dir_el_rat,dir_chem_hum,dir_el_hum)
# calculations of: cluster size, connectivity, connection probability for every experiment/tsynapse => write in Cluster df
for (i in 1:length(synapse_filenames)){
  if (i <= length(dir_chem_rat)){
    current_file = read.csv(dir_chem_rat[i])
    synapse_type = "chemisch"
    species = "Ratte"}
  else if (i <= length(dir_chem_rat)+length(dir_el_rat)){
    x=-(length(dir_chem_rat)) + i
    
    current_file = read.csv(dir_el_rat[x])
    synapse_type = "elektrisch"
    species = "Ratte"}
  else if (i <= length(dir_chem_rat)+length(dir_el_rat)+length(dir_chem_hum)){
    x=-(length(dir_chem_rat)+length(dir_el_rat)) + i
    
    current_file = read.csv(dir_chem_hum[x])
    synapse_type = "chemisch"
    species = "Mensch"}
  else if (i <= length(dir_chem_rat)+length(dir_el_rat)+length(dir_chem_hum)+length(dir_el_hum)){
    x=-(length(dir_chem_rat)+length(dir_el_rat)+length(dir_chem_hum)) + i
    current_file = read.csv(dir_el_hum[x])
    synapse_type = "elektrisch"
    species = "Mensch"}
  
  synapses = length(rownames(current_file))
  cluster_size = n_distinct(current_file$pre_syn)
  #print(nodes)
  connections = sum(current_file$is_connected)
  connectivity = connections/synapses
  connectivity_theo = connections/(cluster_size*(cluster_size-1))
  print(connectivity)
  cluster=rbind(cluster,c(basename(synapse_filenames[i]),synapse_type,species, cluster_size, connectivity,connectivity_theo, synapses, connections))
  
}
#renaming of cluster df columnnames
colnames(cluster)=c("filename","synapse_type","species", "cluster_size", "connectivity","connectivity_theo", "synapses", "connections")

connec_prob = cluster %>% group_by(species, synapse_type) %>% summarise(connections = sum(as.numeric(connections)), n=sum(as.numeric(synapses))) %>% ungroup(synapse_type) %>% reframe(connections, sum = sum(n), connectivity = connections/sum)
connec_prob$synapse_type=c("chemisch","elektrisch", "chemisch","elektrisch")


plot =connec_prob %>% filter(synapse_type=="elektrisch") %>% ggbarplot( x = "synapse_type", y = "connectivity",
                                                                        color = "species",
                                                                        group = "species",
                                                                        palette =c("#6666DD", "#DD6666"),
                                                                        fill = "species",
                                                                        position = position_dodge(1),
                                                                        xlab="Elektrische Synapsen",
                                                                        ylab="Verbindungswahrscheinlichkeit",
                                                                        
                                                                        
)
ggpar(plot, legend.title = "")+theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+ ylim(0,1)+
  geom_text(aes(x = synapse_type, y = connectivity,label=stri_c(connections,"/",sum), group = species),vjust=-0.25, position = position_dodge(1))



p=combined_all %>% filter(synapse_type=="chemisch")  %>% replace_na(list(neuron_class.x='NA', neuron_class.y='NA'))%>%  mutate(neuron_pair=stri_c(neuron_class.x,"->", neuron_class.y)) %>% group_by(species,neuron_pair) %>% summarise(prob=sum(is_connected)/n(), n=n(), con = sum(is_connected)) %>%
  ggbarplot( x = "neuron_pair", y = "prob",
             color = "species",
             group = "species",
             palette =c("#6666DD", "#DD6666"),
             fill = "species",
             position = position_dodge(0.73),
             #size = 0.7,
             xlab="Synapsen Paare",
             ylab="Verbindungswahrscheinlichkeit"
             
             
             
  )+geom_text(aes(x = neuron_pair, y = prob,label=stri_c(con,"/",n) ,group=species),vjust=-0.25, position = position_dodge(0.9))

ggpar(p, legend.title = "") + theme(axis.text.x = element_text(angle = 60, vjust = 1
                                                               , hjust=1
))+ylim(-0.05, 1)

p=combined_all %>% filter(is_connected=="1", synapse_type=="chemisch") %>% group_by(species, neuron_pair) %>% summarize(mean_psp = mean(mean_IPSP), n=n()) %>%
  
  ggdotchart( x = "neuron_pair", y = "mean_psp",
              color = "species",                                # Color by groups
              #palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
              sorting = "descending",                       # Sort value in descending order
              #add = "segments",                             # Add segments from y = 0 to dots
              rotate = F,                                # Rotate vertically
              group = "species",
              palette =c("#6666DD", "#DD6666"),# Order by groups
              dot.size =3,
              position = position_dodge(0),
              xlab="Synapsen Paare",
              ylab="Mitllere Amplitude",
              shape = "species", # Add mpg values as dot labels
              font.label = list(color = "white", size = 9,
                                vjust = 0.5),               # Adjust label parameters
              ggtheme = theme_pubr()                        # ggplot2 theme
  )+geom_text(aes(x = neuron_pair, y = mean_psp,label=round(mean_psp,2) ,group=species, size=2),vjust=-0.5, position = position_dodge(0.7))


ggpar(p, legend.title = "") + theme(axis.text.x = element_text(angle = 60, vjust = 1
                                                               , hjust=1
))

p=combined_all %>% filter(is_connected=="1", synapse_type=="chemisch") %>% group_by(species, neuron_pair) %>% summarize(mean_psp = mean(mean_IPSP), n=n()) %>%
  
  ggdotchart( x = "neuron_pair", y = "mean_psp",
              color = "species",                                # Color by groups
              #palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
              #sorting = "descending",
              sort.x = "descending",# Sort value in descending order
              #add = "segments",                             # Add segments from y = 0 to dots
              rotate = F,                                # Rotate vertically
              group = "species",
              palette =c("#6666DD", "#DD6666"),# Order by groups
              dot.size =3,
              position = position_dodge(0),
              xlab="Synapsen Paare",
              ylab="Mittlere Amplitude",
              shape = "species", # Add mpg values as dot labels
              font.label = list(color = "white", size = 9,
                                vjust = 0.5),               # Adjust label parameters
              ggtheme = theme_pubr()                        # ggplot2 theme
  )+geom_text(aes(x = neuron_pair, y = mean_psp,label=round(mean_psp,2) ,group=species),vjust=-0.5, position = position_dodge(0.7))


ggpar(p, legend.title = "") + theme(axis.text.x = element_text(angle = 60, vjust = 1
                                                               , hjust=1
))+scale_y_discrete(breaks = NULL, expand = c(0, 0.6))


p=combined_all %>% filter(is_connected=="1", synapse_type=="chemisch") %>% group_by(species, neuron_pair) %>% summarize(mean_ppr = mean(IPSP_stim_2/IPSP_stim_1), n=n()) %>%
  
  ggdotchart( x = "neuron_pair", y = "mean_ppr",
              color = "species",                                # Color by groups
              #palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
              sorting = "descending",                       # Sort value in descending order
              #add = "segments",                             # Add segments from y = 0 to dots
              rotate = F,                                # Rotate vertically
              group = "species",
              palette =c("#6666DD", "#DD6666"),# Order by groups
              dot.size =3,
              position = position_dodge(0),
              xlab="Synapsen Paare",
              ylab="Mittlerer PPR",
              
              shape = "species", # Add mpg values as dot labels
              font.label = list(color = "white", size = 9,
                                vjust = 0.5),               # Adjust label parameters
              ggtheme = theme_pubr()                        # ggplot2 theme
  )+geom_text(aes(x = neuron_pair, y = mean_ppr,label=round(mean_ppr,2) ,group=species, size = 3),vjust=-0.5, position = position_dodge(0.7))


ggpar(p, legend.title = "") + theme(axis.text.x = element_text(angle = 60, vjust = 1
                                                               , hjust=1
))+ylim(0,1.4)


PSP=combined_all %>% filter(is_connected=="1", synapse_type=="chemisch")
just_PSP= PSP[,5:8]



combined_all %>% filter(is_connected=="1", synapse_type=="chemisch") %>% group_by(neuron_class.x, psp_type) %>% summarise(mean(mean_IPSP))

PSP_data= data.frame(post_syn=PSP$post_syn,neuron_pair=PSP$neuron_pair,Präsynapse = PSP$neuron_class.x, species=PSP$species, values=stack(just_PSP))
PSP_means = PSP_data %>% group_by(species, neuron_pair, values.ind) %>% summarise(PSPmean=mean(values.values))

colnames(IF_means)=c("neuron_class", "stimuli", "species", "frequency", "sd","sem","n")


p = PSP_data %>% filter(neuron_pair %in% PSP_data$neuron_pair[grep("SIN->", PSP_data$neuron_pair)]) %>%  ggplot(aes(x=values.ind, y=values.values, group=post_syn,color=species))+
  geom_line() +
  geom_point(aes(size = 2,shape=Präsynapse))

#ylim(0,250)+
# geom_errorbar(aes(ymin=frequency-sem, ymax=frequency+sem), width=.2,
#              position=position_dodge(0.2))
p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "IPSP", x = expression("IPSP"),y="Postsynaptisches Potential in mV")+theme_classic()+guides(color = guide_legend(title = "Neuron"))+ scale_color_manual(values =c("#6666DD", "#DD6666"))+guides(size = guide_legend(show = FALSE) )



p = PSP_means %>% filter(neuron_pair %in% PSP_data$neuron_pair[grep("SIN->", PSP_data$neuron_pair)]) %>%  ggplot(aes(x=values.ind, y=PSPmean, group=interaction(neuron_pair,species),color=species, label=neuron_pair))+
  geom_line() +
  geom_point(aes(shape = species, size = 0.7))+theme_classic()+guides(color http://127.0.0.1:20833/graphics/4aef2908-9957-4d38-b6eb-3ddf719cb22e.png= guide_legend(title = "Neuron pair"))+scale_color_manual(values =c("#6666DD", "#DD6666"))
# geom_errorbar(aes(ymin=frequency-sem, ymax=frequency+sem), width=.2,
#              position=position_dodge(0.2))


p+ theme(text = element_text(size = 14))+labs(title = "IPSP", x = expression("IPSP"),y="Postsynaptisches Potential in mV")







combined_all = combined_all  %>% replace_na(list(neuron_class.x='NA', neuron_class.y='NA'))%>% mutate(neuron_pair = stri_c(neuron_class.x, "->", neuron_class.y))
combined_all = combined_all %>% mutate(name_pair = stri_c(pre_syn, "->", post_syn))

combined_all = combined_all %>% distinct(name_pair  , .keep_all = TRUE)

pairwise_connec_prob=combined_all %>% group_by(species,neuron_pair) %>% summarise(prob=sum(is_connected)/n(), n=n())
filtereed_pairwise_connec_prob=combined_all  %>% filter(!neuron_class.x=='NA' & !neuron_class.y=='NA') %>% group_by(species,neuron_pair) %>% summarise(prob=sum(is_connected)/n(), n=n())

#FILTERED FOR CONNECTIONS PER PRE SYN


combined_all %>% filter(is_connected=="1") %>% group_by(species) %>% mutate(n_spec=n()) %>% filter(is_connected=="1",pre_syn_included=="1")%>%   group_by(species,neuron_class.x) %>% summarise(n_spec, n_conn=n(), prob= n_conn/n_spec)

combined_all %>% filter(is_connected=="1") %>% group_by(species) %>% mutate(n_spec=n()) %>% filter(is_connected=="1")%>%   group_by(species,neuron_class.x, neuron_class.y) %>% summarise(n_spec, n_conn=n(), prob= n_conn/n_spec)


combined_all_lowQC =  merge(combined_synapses, combined_supra[,c("neuron_class","cell_ID")], by.x = "pre_syn", by.y = "cell_ID", all.x = TRUE)
names(combined_all_lowQC)[names(combined_all_lowQC) == "neuron_class"] <- "pre_syn_class"

combined_all_lowQC =  merge(combined_all_lowQC, combined_supra[,c("neuron_class", "cell_ID")], by.x = "post_syn", by.y = "cell_ID", all.x = TRUE)
names(combined_all_lowQC)[names(combined_all_lowQC) == "neuron_class"] <- "post_syn_class"


combined_all_lowQC =  merge(combined_all_lowQC, combined_synapses_connected[,c("pre_syn","psp_type")], by.x = "pre_syn", by.y = "pre_syn", all.x = TRUE)
names(combined_all_lowQC)[names(combined_all_lowQC) == "included"] <- "psp_type"

combined_all_lowQC = combined_all_lowQC %>% mutate(pairs = stri_c(pre_syn,post_syn)) %>% distinct(pairs, .keep_all = TRUE)

combined_all_lowQC %>% filter(is_connected=="1") %>% group_by(species) %>%
  mutate(n_spec=n()) %>% filter(is_connected=="1")%>%
  group_by(species,pre_syn_class, post_syn_class) %>%
  summarise(n_spec, n_conn=n(), prob= n_conn/n_spec)  %>%
  distinct(pre_syn_class ,post_syn_class, .keep_all = T )  


combined_all_lowQC %>% filter(is_connected=="1") %>%
  group_by(species) %>% mutate(n_spec=n()) %>% filter(is_connected=="1")%>%
  group_by(species,pre_syn_class, psp_type) %>%
  summarise(n_spec, n_conn=n(), prob= n_conn/n_spec) %>%
  distinct(pre_syn_class ,psp_type, .keep_all = TRUE)                                                                                                                                                                                                                                                      
, .keep_all = TRUE)
#species pre_syn_class psp_type n_spec n_conn   prob
#<chr>   <chr>         <chr>     <int>  <int>  <dbl>
#1 Mensch  FSIN          ""            9      1 0.111
#2 Mensch  NFSIN         ""            9      1 0.111
#3 Mensch  NFSIN         "IPSP"        9      1 0.111
#4 Mensch  UDN           "EPSP"        9      6 0.667
#5 Ratte   FSIN          ""           13      2 0.154
#6 Ratte   FSIN          "IPSP"       13      5 0.385
#7 Ratte   NFSIN         "IPSP"       13      2 0.154
#8 Ratte   UDN           ""           13      1 0.0769
#9 Ratte   UDN           "EPSP"       13      2 0.154
#10 Ratte   UDN           "IPSP"       13      1 0.0769

###FISCHER TEST CONNECTIVITY
dat <- data.frame(
  "connect_yes" = c(7, 10),
  "connect_no" = c(23,104),
  row.names = c("Human", "Ratte"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("Connect_yes", "Connect_no")

chisq.test(dat)

test <- fisher.test(dat)
test


combined_supra_QC_noZdro = combined_supra_QC[combined_supra_QC==0] = NA



combined_all %>%  filter(psp_type=="EPSP") %>% group_by(species) %>% summarise(n=n(), mean(max_freq), median(max_freq), min(max_freq), max_freq)
combined_supra_QC_noZdro

combined_all =  merge(combined_all, combined_supra[,c("max_freq","cell_ID")], by.x = "pre_syn", by.y = "cell_ID", all.x = TRUE)

)
