# UAV_kelp_monitoring
This repository provides datasets and R code for the analysis in "Rapid loss of temperate kelp forests revealed by unmanned aerial vehicle (UAV) photography and underwater observations” by Masaaki Sato, Junji Kinoshita, Kyoji Ishita, Shiori Arima, Masayuki Fudo, Hisami Kuwahara to examine temporal changes in kelp forests at Manazuru in pacific coasts of Central Japan.  

The raw data described below are housed in the folder /data.

- Make figure 2 for water temperature at Enoura: Enoura_wtemp5m_2008-2020.csv, Enoura_maxTemp_hist.csv, and Enoura_16cTemp_hist.csv
- Make figure 3 for catch of herbivore fish: Herbivore_setnet.csv
- Make figure 4 for Significant wave height at Shimoda: Shimoda_wave_2008-2020.csv
- Make figure 4 for large typhoon tracks 2019: typhoon_table2019.csv
- Make figure 5 for cover of algal species: Iwa_algae_data_2019-2020.csv
- Perform multivariate analysis with figure 5: Iwa_algal_community_multivariate.csv
- Sea-truth data in Iwa and Manazuru for mapping kelp forests using UAV photos: Data_sea-truth.xlsx


To make figures or run the analyses, use the scripts below. 

- Make figure 2 for water temperature at Enoura: Enoura_physical_variable.R
- Make figure 3 for catch of herbivore fish: Herbivore_catch_seisho.R
- Make figure 4 for Significant wave height at Shimoda: Wave_current_velocity.R
- Make figure 4 for large typhoon tracks 2019: typhoon_track2019.R
- Make figure 5 for cover of algal species, and perform multivariate analysis: Iwa_algal_community.R

# Citation
Sato M, Kinoshita J, Ishita K, Arima S, Fudo M, Kuwahara H (2025) Rapid loss of temperate kelp forests revealed by unmanned aerial vehicle (UAV) photography and underwater observations. Aquatic Botany 200:103900  https://authors.elsevier.com/a/1l1dm_6wxKSVx3
