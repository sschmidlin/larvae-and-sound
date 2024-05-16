#Comparison of the effects of reef and anthropogenic soundscapes on oyster larvae settlement

This repository is a collection of the data and the scripts necessary to reproduce the results of the paper: 

Schmidlin, S., & Parcerisas, C.., Hubert, J.,  Watson, M.S., Mees, J., Botteldooren, D., Devos,  P., 
Debusschere, E., & Hablützel, P.I., Navigating soundscapes: Attractant effect of reef sound on oyster 
settlement may be attenuated by vessel noise. Submitted to Scientific Reports in Februrary 2024.

The folder data/ contains several files: 
* sound_data.xlsx: data from settlement counts
* sound_data.csv: csv version from sound_data.xlsx ready to produce plots 
* model_output.csv: output obtained from the model. This csv is obtained by running the "larvae and sounds.R script"
* results_spl_original_files.csv: acoustic features computed directly from the field recordings. This csv is obtained 
by running the sound_exposition_analysis.py script
* results_spl.csv: acoustic features computed on the 1-h recordings in the lab. 
This csv is obtained by running the sound_exposition_analysis.py script

The folder sound_processing_python/ contains several scripts: 
* compare_test_with_recorded.py: this script was used to decide the volume of each playback
* process_sound_files.py: script to downsample/upsample all the selected files 
* sound_exposition_analysis.py: script to analyze the differences in spectrum and acoustic features between field and 
lab recordings per treatment (produces Figures 4 and 5)


The scripts in the main folder are: 
* larvae and sounds.R: script to compute the main statistics of the paper (produces Table 1)
* plot_model_results.py: script to produce a scatter plot from the model output (Figure 3)


The folder figures/ contains the obtained figures for the results of the paper.

The wav files used for the exposure experiment will be available on the Marine Data Archive (MDA). 

Please cite correspondingly when using it for your own work. 
