#!/bin/bash

for prog in programmes/*
do
	printf "$prog : "
	res=$(./eval < $prog)
	printf "flux de sortie : [$res] \n"
	if [[ $res != "42" ]]; then
		printf "[\e[40m $prog \e[0m is \e[45m not properly evaluated \e[0m ]\n\n"
	else
		printf "\n[\e[40m $prog \e[0m is \e[44m correctly evaluated \e[0m ]\n\n"
	fi
done
