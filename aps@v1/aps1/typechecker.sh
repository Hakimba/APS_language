#!/bin/bash

for prog in programmes/*
do
	res=$(./type < $prog)
	printf "terme prolog : $res"
	var=$(./type < $prog | swipl -s typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$var\n"
		printf "[\e[40m $prog \e[0m is \e[45m not properly typed \e[0m ]\n\n"
	else
		printf "\n[\e[40m $prog \e[0m is \e[44m correctly typed \e[0m ]\n\n"
	fi
done