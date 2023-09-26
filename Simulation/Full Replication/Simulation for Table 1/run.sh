#!/bin/bash

for r in {1..20}
do
	sbatch sim_each_row.sh $r
done

