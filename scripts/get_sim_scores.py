from pathlib import Path

from gensim.models import KeyedVectors

import pandas

WORD2VEC_TIMES_BASE_URL = "https://zenodo.org/record/1494141/files/"
MIN_START_YEAR = 1900
MAX_START_YEAR = 2007

TOPICS = [
    "immigration",
    "migration",
    "emigration"
]

# Make a manual list.
# COMPARE_WORDS = ["emigration",
#                  "migration",
#                  "immigrants",
#                  "immigration",
#                  "emigrants",
#                  "colonization",
#                  "settlers",
#                  "population",
#                  "colonies",
#                  "promoting",
#                  "canada",
#                  "recruiting",
#                  "unemployed"
#                  ]

# Use a text file to collect the terms.
COMPARE_WORDS = pandas. \
    read_csv("word_selection.txt", header=None)[0].\
    drop_duplicates(). \
    tolist()

result = []

for start_year in range(MIN_START_YEAR, MAX_START_YEAR + 1):

    print("Start with year:", start_year)

    # load the word embedding
    wv = KeyedVectors.load_word2vec_format(
        WORD2VEC_TIMES_BASE_URL + "{}_{}.w2v".format(
            start_year, start_year + 2
        ),
        binary=True
    )

    # get the similarity for all topics
    for topic in TOPICS:

        for word in COMPARE_WORDS:

            try:
                sim_score = wv.similarity(topic, word)
            except Exception:
                sim_score = None

            result.append(
                {
                    'start_year': start_year,
                    'end_year': start_year + 2,
                    'topic': topic,
                    'word': word,
                    'similarity': sim_score
                }
            )

Path("output").mkdir(parents=True, exist_ok=True)

df_result = pandas.DataFrame(result)
df_result.to_csv("output/similarity_file.csv", index=False)
