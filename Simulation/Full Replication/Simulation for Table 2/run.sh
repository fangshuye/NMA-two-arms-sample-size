#!/bin/bash

for r in 1 2 3 4
do
	sbatch even_allocation.sh $r
	sbatch uneven_allocation.sh $r
done

