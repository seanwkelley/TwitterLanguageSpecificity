# TwitterLanguageSpecificity

## Graphs

### Figures1_2.R

Code for Figure 1 and Figure 2 in main text 

Figure 1
Associations between 9 self-reported psychiatric questionnaires and mean values over the past year of the top 10 LIWC text features associated with depression severity, controlling for age and gender. Dashed lines indicate p-values below 0.05.  

Figure 2
Associations between 9 self-reported psychiatric questionnaires with Twitter meta-data features.

### Figure3.R

Elastic Net predictive performance of a depression model on itself, and  9 other aspects of mental health 

### Figure4.R

Comparison of text feature only random forest models trained on either i) a depression self-report questionnaire or ii) depression relevant keywords. 

### Figure5.R

Depression  model text feature selection frequencies and dendrogram of language similarities between aspects of mental health

### Figure6.R

Transdiagnostic dimension text feature selection frequencies 

### FiguresS1_S2_S6.R

Code for Supplmentary Figures S1, S2, S3 in supplementary text 

Figure S1
Associations between 9 psychiatric questionnaires and age/gender 

Figure S2 
Bivariate correlations among the 9 psychiatric questionnaires and age. 

Figure S6
Histograms of 9 psychiatric scales with means and standard deviations. 


### FigureS3.R

Predictive performance (R2) of an anxious depression trained model tested on three transdiagnostic dimensions: anxious depression, compulsivity and intrusive thoughts, and social withdrawal. 

### FigureS4.R

Depression residuals from LIWC text feature only model

### FigureS5.R

Predictive performance of a depression model trained using only Tweets (n = 756), Retweets (n = 637), or Likes (n = 902). 

### FigureS7.R

Power to detect an effect size double the observed depression predictive performance (i.e., r = 0.32) in simulated data

## Machine Learning 

### Elastic_net_features.py

Text features plus age and gender to train elastic net models 

### Twitter_CV_TF.py

Elastic net models trained on text features only with depression as the target outcome and tested on 8 other psychiatric scales

### Twitter_CV_TD.py

Elastic net models trained on text features only with Anxious Depression as the target outcome and tested on two other transdiagnostic dimension (Compulsivity and Instrusive Thoughts/Social Withdrawal)

### Twitter_CV_AgeGender.py

Elastic net models trained on text features plus age/gender, trained on depression and tested on all other psychiatric dimensions

### Selection_Frequencies.py

Text feature selection frequencies for models trained/tested separately on each psychiatric dimension 

### Best_models.py

Elastic net model performance for models trained and tested on each psychiatric scale 







 










