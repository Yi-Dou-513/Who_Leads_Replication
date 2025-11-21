## Replication Materials: _Winning! Election Returns and Engagement in Social Media_

Replication materials for Barberá, Casas, Nagler, Egan, Bonneau, Jost & Tucker, "Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting by Legislators and the Mass Public Using Social Media Data", American Political Science Review, 2019.

> __Abstract:__
> Are legislators responsive to the priorities of the public? Research demonstrates a strong correspondence between the issues about which the public cares and the issues addressed by politicians, but conclusive evidence about who leads whom in setting the political agenda has yet to be uncovered. We answer this question with fine-grained temporal analyses of Twitter messages by legislators and the public during the 113th US Congress. After employing an unsupervised method that classifies tweets sent by legislators and citizens into topics, we use vector autoregression models to explore whose priorities more strongly predict the relationship between citizens and politicians. We find that legislators are more likely to follow, than to lead, discussion of public issues, results that hold even after controlling for the agenda-setting effects of the media. We also find, however, that legislators are more likely to be responsive to their supporters than to the general public.

The published version is [here](https://www.cambridge.org/core/journals/american-political-science-review/article/who-leads-who-follows-measuring-issue-attention-and-agenda-setting-by-legislators-and-the-mass-public-using-social-media-data/D855849CE288A241529E9EC2E4FBD3A8)

## Tutorial 

This README file provides an overview of the replications materials for the article. The R codes used in the article can be found under the folder **Codes**. The congress data we used to replicate the topic models and the time series data provided by the author are under the folder **data**. All the images for the main paper are available at the folder **output**.

If other researchers are interested in the processed datasets, please contact the authors directly. 

The codes below do not fully replicate the results since we cannot share the full hydrated datasets. However, they should work as a guide for the methods used in the paper, and could be applied to a similar collection of tweets from the Twitter API. 

## Codes

- `Replication_01Table3.r`: replicate Table 3 of the paper, where the authors show the correlation between the issue attention distribution of the different groups under analysis. 

- `Replication_02Figure1.r`: replicate Figure 1 of the paper, where the authors show the average attention paid to each topic for the whole period of analysis. 

- `Replication_03Figure2.r`: replicate Figure 2 of the paper, where the authors show the results of our main VAR model, by showing 15-day Impulse Response Functions for one-time as well as permanent 10-percentage point increases in attention.

- `Replication_04Figure3.r`: replicate Figure 3 of the paper, where the authors explore in more detail the ability of politicians versus groups of the public to lead the agenda of the other; and viceversa.

- `Replication_05Figure4.r`: replicate Figure 4 of the paper, where the authors show the issue-level IRFs.

- `Replication_06Figure5.r`: replicate Figure 5 of the paper, where the authors show the correlation between issue-level responsiveness and issue salience.

- `Replication_08Figure6.r`: replicate Figure 6 of the paper, where the authors show the ability of the the different groups under study to lead the agenda of the media, and viceversa.
 

## Data

Tweet ids for all the four cases are available under the folder data. 
