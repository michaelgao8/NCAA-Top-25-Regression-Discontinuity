all: FinalProject.html clean

.PHONY: all

FinalProject.html: FinalProject.Rmd AllSeasons.Rdata SeasonRanks.Rdata
	Rscript -e "library(rmarkdown);render('FinalProject.Rmd')"


AllSeasons.Rdata: get_tvrankings.R
	Rscript get_tvrankings.R

SeasonRanks.Rdata: get_top25rankings.R 
	Rscript get_top25rankings.R

.PHONY: clean

clean:
	-rm -f FinalProject.html
	-rm -f SeasonRanks.Rdata
	-rm -f AllSeasons.Rdata