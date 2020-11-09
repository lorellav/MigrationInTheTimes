# Developer tricks

Deduplicate the terms for word selection. 

``` python
import pandas as pd

df = pd. \
    read_csv("word_selection.txt", header=None). \
    drop_duplicates().reset_index(drop=True). \
    to_csv("word_selection.txt", index=False, header=False)
```