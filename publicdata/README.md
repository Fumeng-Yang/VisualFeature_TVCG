
# Identifying Visual Features
### judgments_data.csv 

Each row represents a single judgment (choose between left or right). Each participant completed 50 x 4 = 200 judgments.
Please refer to our supplementary materials to find out the definition of a visual feature (notion).

The columns in this file are (aka information for one judgment): 

- **judgmentId**: index of each jugdment in a trial
- **participantId**: participant id string
- **approach**: whether rbase was approached from above or below
- **rbase**: correlation (r value) condition
- **rv**: the correlation (r value) for the other scatterplot
- **rL**: the correlation (r value) of the left scatterplot 
- **rR**: the correlation (r value) of the right scatterplot 
- **correctans**: correct answer to the judgment
- **answer**: the participant's answer to the judgment
- **ellipse_major ... conf_box_ratio_reverse_base**: the computed difference in a visual feature between the two side-by-side scatterplots 
- **visandsign**: always scatterplots-positive (to aligh with Harrison et al. 2014)


### judgments_r.csv

A subset of judgments_data.csv. It only consists of r (correlation) related columns.


# Comparing Perceptual Models
### individual_jnds.csv

This file contains individual jnds for r and four "top-performing" visual features.
Each record represents a single trial. Each participant completed 4 trials. 

Note that all four visual features are listed as different rows. Therefore, other columns may contain redundant rows. 

The columns in this file are (aka information for one trial): 

- **vf**: the notion of a visual feature (conf_bounding_box_perp, dist_line_sd, ellipse_minor, or ellipse_area)
- **participantId**: participant id string
- **approach**: whether rbase was approached from above or below
- **rbase**: correlation (r value) condition
- **jnd_50_r**: the jnd of r in a trial, inferred using a weighted logistic regression
- **jnd_50**:  the jnd of a visual feature in a trial, inferred using a weighted logistic regression
- **jnd_r**: the jnd of r in a trial, inferred using the average of the first converged 24 judgments (see Rensink and Baldridge 2010, Harrison et al. 2014) 

### individual_jnds_r.csv

A subset of individual_jnds.csv. It only consists of r (correlation) related columns.