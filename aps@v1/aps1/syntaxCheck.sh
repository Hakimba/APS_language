#!/bin/bash

for prog in programmes/*
do
	res=$(./aps < $prog)
	if [[ $vres != *""* ]]; then
		printf "[\e[40m $prog \e[0m syntax is  \e[45m  incorrect \e[0m ]\n\n"
	else
		printf "\n[\e[40m $prog \e[0m syntax is \e[44m correct \e[0m ]\n\n"
	fi
done