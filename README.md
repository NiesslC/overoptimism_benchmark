Source code for manuscript 'Over-optimism in benchmark studies and the multiplicity of design and analysis options when interpreting their results'
by Christina Nießl, Moritz Herrmann, Chiara Wiedemann, Giuseppe Casalicchio and Anne-Laure Boulesteix

The code has been written and executed using R version 4.0.2 (2020-06-22) (Platform: x86_64-w64-mingw32/x64 (64-bit))
with package versions latex2exp_0.4.0, stringi_1.5.3, RColorBrewer_1.1-2 forcats_0.5.0, scales_1.1.1,      
tidyr_1.1.2, ggrepel_0.8.2, ggplot2_3.3.2, gridExtra_2.3, smacof_2.1-1, e1071_1.7-4, colorspace_1.4-1,
plotrix_3.7-8, dplyr_1.0.2, reshape2_1.4.4 


The folder "R" consists of:
**01_generate_rankdata.R**
-  generates the rank data for 288 combinations of design and analysis options (rankdata.RData) and
for 774 combinations of design and analysis options (rankdata_datasample.RData)

**02_unfolding_models.R**
- generates three unfolding models: 
  - model 1 representing 288 combinations (unfolding_model.RData),
  - model 2 representing 774 combinations with ibrier as performance measure (unfolding_model_datasample_ibrier.RData),
  - model 3 representing 774 combinations with cindex as performance measure (unfolding_model_datasample_cindex.RData)
- generates goodness-of-fit measures and figures for all unfolding models

** 03_results.R **
- generates the figures shown in the results section of this paper

**helper_fcts_generate_rankdata.R**
- helper functions for 01_generate_rankdata.R

**helper_fcts_results.R**
- helper functions for 03_results.R

