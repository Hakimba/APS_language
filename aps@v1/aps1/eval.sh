#!/bin/bash

for prog in programmes/*
do
	printf "$prog : "
	res=$(./eval < $prog)
	printf "flux de sortie -> [$res] \n"
done
