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

- `Replication_01Table3.r`: replicates Table 3 of the paper, where the authors show the correlation between the issue attention distribution of the different groups under analysis. 

- `Replication_02Figure1.r`: replicates Figure 1 of the paper, where the authors show the average attention paid to each topic for the whole period of analysis. 

- `Replication_03Figure2.r`: replicates Figure 2 of the paper, where the authors show the results of our main VAR model, by showing 15-day Impulse Response Functions for one-time as well as permanent 10-percentage point increases in attention.

- `Replication_04Figure3.r`: replicates Figure 3 of the paper, where the authors explore in more detail the ability of politicians versus groups of the public to lead the agenda of the other; and viceversa.

- `Replication_05Figure4.r`: replicates Figure 4 of the paper, where the authors show the issue-level IRFs.

- `Replication_06Figure5.r`: replicates Figure 5 of the paper, where the authors show the correlation between issue-level responsiveness and issue salience.

- `Replication_08Figure6.r`: replicates Figure 6 of the paper, where the authors show the ability of the the different groups under study to lead the agenda of the media, and viceversa.
 

## Data

- `tweets_congress.csv`: used to replicate the LDA model. Please notice that this is not the data set the authors used. Because the data set is too big, we can not commit it onto github.Please see it [here](https://www.dropbox.com/scl/fi/jz14130dlzj5xvca9txj4/midterm_candidates_labeled_all_May05.csv?rlkey=b0m1h6kk1af3uz9tjqyelthh3&dl=0)

- `main-time-series.csv, onetime-structural-shock-irfs-results.csv, pa2our_topics_crosswalk_merged_subissues`: used to replicate the VAR models and create the figures and tables.
