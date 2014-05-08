# makefile for figures for our paper "Fiji Earthquake and Moomin paintball"

all : output/figure01.pdf output/figure02.pdf

output/figure01.pdf : figure01.R source.R config.R
	Rscript --vanilla figure01.R

output/figure02.pdf : figure02.R source.R config.R
	Rscript --vanilla figure02.R

clean :
	rm output/*
