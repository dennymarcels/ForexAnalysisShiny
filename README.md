# ForexAnalysisShiny

This shiny app is hosted at https://dennymc.shinyapps.io/ForexAnalysis/

## Instructions

1. Go to the MyFxBook page you want to analyse, then click on `Export` (top right of the first graph), then `CSV`. Save the file with the `nameOfEA`.
1. Repeat the process above for as many EAs as you wish to analyse. If you are saving more than one file per EA (for example, same EA but more than one MyFxBook account), the file name should be in the format `nameOfEA_index`, where `index` is any text string. **Do not forget the underscore symbol!**
1. At the EA Analysis page (this one you are looking at!), click on `Browse...` on the left panel, choose the CSV files you just downloaded.
1. After the files are loaded, you will get a short summary with file names and the initial and final trade dates on each file, for your reference.
1. Below the file/date summary, you can choose the period you wish to analyse. By default, the period is set to the **last 6 months** available in the datasets.
1. Below the date selection menu, you can filter strategies by **MagicNumber** (strategy).
1. After you picked the desired MagicNumbers, below their selection menu, you can filter strategies by their **minimum Note**. The *Note* is defined as the sum of pips that the strategy reached (*NetPips*) + 1 (to avoid dividing by 0) divided by the *Drawdown (DD)*, in the period. Negative notes correspond to strategies with negative performance. Zero corresponds to strategies with no net pips. One, for example, corresponds to strategies whose performance was equal to the drawdown. This field will allow the whole interval of notes for the period chosen, and is defaulted to zero.
1. The table in the ***Results*** tab will show performance indicators for each EA/MagicNumber chosen from the provided files, according to the dates and the minimum note chosen.
1. Select rows from this table to see a graph showing the monthly performance (Net Pips) of the desired EA/MagicNumber(s).
