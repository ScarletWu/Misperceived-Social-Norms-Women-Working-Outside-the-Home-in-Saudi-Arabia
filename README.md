## Overview
This repo replicated data from "Misperceived Social Norms:
Women Working Outside the Home in Saudi Arabia" (https://www.aeaweb.org/articles?id=10.1257/aer.20180975), conducted an analysis and discussion about the females working outside the home in Saudi Arabia. The data was downloaded, cleaned, simulated, analyzed, and then concluded. The paper and analysis is made reproducible so that it can be used again by others.
In this repo, WWOH stands for women working outside the home.

## File Structure
The repo is structured as:
-   `input/data` contains the data used in analysis including original raw data, original cleaned data, and the data we cleaned using original raw data.
-   `outputs/paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `outputs/figures` and `outputs/table` contains the table and figures replicated, while `r_code` contains the code that generate the replicated visualizations.
-   `scripts` contains the R scripts used to simulate, download and clean the data.
-   `LLM` contains the link and text of LLM usage.

## LLM Usage
ChatGPT was used for code modification, explanation, and grammar mistake correction. The chat can be found in LLM/llm.txt. No other LLM was used.