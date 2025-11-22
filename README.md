## Replication Materials: _Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting by Legislators and the Mass Public Using Social Media Data_

Replication materials for Barberá, Casas, Nagler, Egan, Bonneau, Jost & Tucker, "Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting by Legislators and the Mass Public Using Social Media Data", American Political Science Review, 2019.

> __Abstract:__
> Are legislators responsive to the priorities of the public? Research demonstrates a strong correspondence between the issues about which the public cares and the issues addressed by politicians, but conclusive evidence about who leads whom in setting the political agenda has yet to be uncovered. We answer this question with fine-grained temporal analyses of Twitter messages by legislators and the public during the 113th US Congress. After employing an unsupervised method that classifies tweets sent by legislators and citizens into topics, we use vector autoregression models to explore whose priorities more strongly predict the relationship between citizens and politicians. We find that legislators are more likely to follow, than to lead, discussion of public issues, results that hold even after controlling for the agenda-setting effects of the media. We also find, however, that legislators are more likely to be responsive to their supporters than to the general public.

The published version is [here](https://www.cambridge.org/core/journals/american-political-science-review/article/who-leads-who-follows-measuring-issue-attention-and-agenda-setting-by-legislators-and-the-mass-public-using-social-media-data/D855849CE288A241529E9EC2E4FBD3A8)

## Tutorial 

This README file provides an overview of the replications materials for the article. The R codes used in the article can be found under the folder **Codes**. The time series data provided by the author which are used to replicate the figures are under the folder **data**. All the images for the main paper are available at the folder **output**.

If other researchers are interested in the processed datasets, please contact the authors directly. 

The codes below do not fully replicate the results since we cannot share the full hydrated datasets. However, they should work as a guide for the methods used in the paper, and could be applied to a similar collection of tweets from the Twitter API. 

## Codes
- `01-lda_k_selection_replication.r`: runs multiple LDA models to choose the right number of topics based on cross-validated model fit

- `Replication04.r`: reconstruct Congressional group-level topic attention time serie using the authors’ LDA model (lda_results-twokenizer.Rdata) together with our preprocessed and sampled tweet corpus.

- `Replication04.r`: construct the final topic–attention time series used in the  paper

- `Replication04.r`: computes intercoder reliability for the 100 LDA topics—producing the pairwise agreement matrix, APIR, Cronbach’s alpha, and consensus labels using the authors’ topic-classification file.

- `Replication_01Table3.r`: replicates Table 3 of the paper, where the authors show the correlation between the issue attention distribution of the different groups under analysis. 

- `Replication_02Figure1.r`: replicates Figure 1 of the paper, where the authors show the average attention paid to each topic for the whole period of analysis. 

- `Replication_03Figure2.r`: replicates Figure 2 of the paper, where the authors show the results of our main VAR model, by showing 15-day Impulse Response Functions for one-time as well as permanent 10-percentage point increases in attention.

- `Replication_04Figure3.r`: replicates Figure 3 of the paper, where the authors explore in more detail the ability of politicians versus groups of the public to lead the agenda of the other; and viceversa.

- `Replication_05Figure4.r`: replicates Figure 4 of the paper, where the authors show the issue-level IRFs.

- `Replication_06Figure5.r`: replicates Figure 5 of the paper, where the authors show the correlation between issue-level responsiveness and issue salience.

- `Replication_08Figure6.r`: replicates Figure 6 of the paper, where the authors show the ability of the the different groups under study to lead the agenda of the media, and viceversa.
 

## Data

- `tweets_congress.csv`: used to replicate the LDA model. Please notice that this is not the data set the authors used. Because the data set is too big, we can not commit it onto github.Please see it [here](https://www.dropbox.com/scl/fi/mj8ldtyqwpkelnpyv0aja/tweets_congress.csv?rlkey=oltuf3odk3trrwjfl0a6ka25t&e=1&dl=0)

- `main-time-series.csv, onetime-structural-shock-irfs-results.csv, pa2our_topics_crosswalk_merged_subissues`: used to replicate the VAR models and create the figures and tables.

- `main-time-series-PRE.csv`: pre-processed daily issue-attention time series for all groups, used as input for the VAR and IRF replications.

- `k_topics_results_cv.Rdata`: saved cross-validation results from LDA models with different numbers of topics, used to assess and compare K-selection.

- `lda_results-twokenizer.Rdata`: fitted LDA model output (including topic distributions) estimated on tweets tokenized with the twokenizer pipeline.

## Report

The final replication report is located in the `report/` folder:

- `Replication.tex` — LaTeX source of the written report  
- `Replication.bib` — bibliography file  
- `Replication_of_Who_Leads_Who_Follows.pdf` — compiled PDF version of the report  

This document summarizes the Methods, Results, Differences, Autopsy, and proposed Extensions of the replication.
