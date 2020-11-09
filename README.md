# Visualising changes in the construction of meaning with Word Vector Space: A Digital Humanities Approach
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4264497.svg)](https://doi.org/10.5281/zenodo.4264497)

Natural language processing (NLP) techniques concerned with language and meaning (e.g., neural word embeddings) are mostly based on distributional semantics theory ([Harris 1954](https://www.tandfonline.com/doi/pdf/10.1080/00437956.1954.11659520); [Firth 1957](https://scholar.google.com/scholar_lookuppublication_year=1957&title=Studies+in+Linguistic+Analysis)) and typically focus on mapping different senses expressed by single words. In recent years, although there has been an upsurge in such NLP studies, the investigation of language and meaning continues to stay for the majority on word level while its relation with discourse remains largely neglected. In this project, we propose an experimental method that aims to overcome such limitation. Specifically, we explore the value of merging neural word embeddings with the discourse-historical approach (DHA) ([Reisigl and Wodak 2001](https://books.google.lu/bookshl=en&lr=&id=xJV1f4zvn1MC&oi=fnd&pg=PA63&ots=YpxRJHrBkY&sig=0Sqeju54--CibI5OgZQtUiT1GIc&redir_esc=y#v=onepage&q&f=false)) to investigate the historical changes in the semantic space of public discourse of **migration** in the United Kingdom. As data-set, we use the Times Digital Archive (TDA) from 1900 to 2000. For the computational part, we use publicly available TDA word2vec models ([Kenter et al. 2015; Martinez-Ortiz et al. 2016](http://doi.org/10.5281/zenodo.1494141)); these models have been trained according to sliding time windows with the specific intention to map conceptual change. We then use DHA to triangulate the results generated by the word vector models with social and historical data to identify plausible explanations for the changes in the public debate. By bringing the focus of the analysis to the level of discourse, with this method, we aim to go beyond mapping different senses expressed by single words and to add the currently missing sociohistorical and sociolinguistic depth to the computational results. This repository shows the visualisation of the word2vec results. Please refer to [Viola and Verheul](https://www.frontiersin.org/articles/10.3389/frai.2020.00064/full#B80) (2020) for the discussion of the socio-historical triangulation.

![Immigration](https://github.com/lorellav/MigrationInTheTimes/blob/master/examples/Figure%209_plot_time_w2v_1985-2000_immigration.jpg)

## Table of contents

1. [Installation](#1-installation)
2. [Computing distance between terms from word2vec models](#2-computing-distance-between-terms-from-word2vec-models)
3. [Visualising similarity scores](#3-visualising-similarity-scores)
4. [License](#4-license)
5. [References](#5-references)
6. [Contact and how to cite](#6-contact-and-how-to-cite)

## 1. Installation

This project requires Python 3.5 or higher and R. Install the dependencies with the
code below.

```sh
pip install -r requirements.txt
Rscript install.R
```

## 2. Computing distance between terms from word2vec models

Pretrained Word2Vec models are available for the Times Digital Archive since 1900. Models were trained on a 10 year slice with a 2 year sliding window (e.g., one model from 1900 to 1910 and sliding windows of 1900–1902, 1901–1903, 1902–1904, and so on). The models ara available [here](https://zenodo.org/record/1494141#.XepjkpNKiRt).

Each Word2Vec model contain thousands of words. To reduce computational complexity, relevant terms are selected in the file `word_selection.txt`. Each of these terms is used to compute the cosine distance with the words *immigration*, *migration* and *emigration*. The result is a formatted file in the output folder name `.csv`.

```
python scripts/get_sim_scores.py
```

The output has the following format:

```
start_year,end_year,topic,word,similarity
1900,1902,immigration,emigration,0.7420879006385803
1900,1902,immigration,migration,0.560883104801178
1900,1902,immigration,immigrants,0.7614476680755615
1900,1902,immigration,immigration,1.0
1900,1902,immigration,emigrants,0.4798796772956848
1900,1902,immigration,colonization,0.3869989514350891
1900,1902,immigration,settlers,0.5858354568481445
1900,1902,immigration,population,0.553298830986023
1900,1902,immigration,colonies,0.5236546397209167
...
2007,2009,emigration,laws,0.27103185653686523
2007,2009,emigration,enforcing,0.22432316839694977
2007,2009,emigration,asylum,0.2906147539615631
2007,2009,emigration,visas,0.20456579327583313
2007,2009,emigration,status,0.14212839305400848
2007,2009,emigration,humanitarian,0.3443276882171631
2007,2009,emigration,extradition,0.1944943070411682
2007,2009,emigration,legislation,0.1761290729045868
2007,2009,emigration,violation,0.17413166165351868
2007,2009,emigration,criminal,0.10719547420740128
```

**Explanation**

Between 1900 and 1902, the cosine similarity between the terms *immigration* and *emigration* is 0.742087. Score of 1 implies that the meaning of both terms in the Word2Vec model is identical, while -1 is the direct opposite.

## 3. Visualising similarity scores

To enhance readibility and make the comparisons more informative, we computed the following plots which allow to compare multiple terms over time.

![Emigration](https://github.com/lorellav/MigrationInTheTimes/blob/master/examples/Figure%202_plot_time_w2v_1900-1910_emigration.jpg)


![Immigration](https://github.com/lorellav/MigrationInTheTimes/blob/master/examples/Figure%203_plot_time_w2v_1900-1910_immigration.jpg)

Open and run the R-script `plot_similarity.R` to check all plots or to create custom plots.

``` sh
Rscript scripts/plot_similarity.R
```

## 4. License

Copyright © 2020 Viola and De Bruin. This is an open-access repository
distributed under the terms of the Creative Commons Attribution License (CC
BY). The scripts in this repository are available under the [GNU GPL-3 license](LICENSE).
The use, distribution or reproduction in other forums is
permitted, provided the original author(s) and the copyright owner(s) are
credited and that the original publication is cited, in accordance with
accepted academic practice. No use, distribution or reproduction is permitted
which does not comply with these terms.

## 5. References

Firth, J. R., (ed.). (1957). *Studies in Linguistic Analysis*. Oxford: Blackwell.

Harris, Z. S. (1954). Distributional structure. *Word* 10, 146–162.

Martinez-Ortiz, Carlos. (2018). Time shifting word2vec models from Times [Data set]. Zenodo. http://doi.org/10.5281/zenodo.1494141

Viola, Lorella and Verheul, Jaap. (2020). One Hundred Years of Migration Discourse in The Times: A Discourse-Historical Word Vector Space Approach to the Construction of Meaning. *Frontiers in Artifical Intelligence. Special Issue on Computaional Sociolinguistics*. https://doi.org/10.3389/frai.2020.00064

Wodak, R., and Meyer, M., (eds.). (2001). *Methods of Critical Discourse Analysis*. London: SAGE.

## 6. Contact and how to cite

*Migration in The Times* is a project by [Lorella Viola](https://www.lorellaviola.me.uk) and
[Jaap Verheul](https://www.uu.nl/medewerkers/JVerheul) and received
support from the Research Engineering team of Utrecht University. The technical
implementation was provided by [Jonathan de Bruin](http://github.com/J535D165)
(j.debruin1@uu.nl).
***Correspondence***: Lorella Viola (lorella.viola@uni.lu)

To cite this repository, please use the following format according to the APA style guide:

Viola, Lorella and de Bruin, Jonathan. 2020. *Visualising changes in the construction of meaning with Word Vector Space: A Digital Humanities Approach*. Luxembourg: Luxembourg Centre for Contemporary and Digital History (C2DH)
