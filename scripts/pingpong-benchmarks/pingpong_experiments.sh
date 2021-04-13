#!/bin/bash
experiments=$1
limited=$2
increments=20

requests=100
rampup=2

wd=`pwd`

echo "Removing contents of results directory"
rm -r $wd/scripts/pingpong-benchmarks/results/control/* > /dev/null 2>&1
rm -r $wd/scripts/pingpong-benchmarks/results/monitored/* > /dev/null 2>&1
rm -r $wd/scripts/pingpong-benchmarks/results/detached_monitored/* > /dev/null 2>&1

if [ "$limited" ]; then
  echo "Running a limited number of experiments for the Ping Pong benchmark"

  sh $wd/scripts/pingpong-benchmarks/control_experiment.sh 100 2 $experiments
  sh $wd/scripts/pingpong-benchmarks/monitored_experiment.sh 100 2 $experiments
  sh $wd/scripts/pingpong-benchmarks/detached_monitored_experiment.sh 100 2 $experiments

  sh $wd/scripts/pingpong-benchmarks/control_experiment.sh 500 10 $experiments
  sh $wd/scripts/pingpong-benchmarks/monitored_experiment.sh 500 10 $experiments
  sh $wd/scripts/pingpong-benchmarks/detached_monitored_experiment.sh 500 10 $experiments

  python3 $wd/scripts/pingpong-benchmarks/pingpong-plots.py $wd $experiments True

  echo "Limited number of experiments for the Ping Pong benchmark done"

  exit
fi

while [ "$increments" -ne 0 ] ; do

  sh $wd/scripts/pingpong-benchmarks/control_experiment.sh $requests $rampup $experiments
  sh $wd/scripts/pingpong-benchmarks/monitored_experiment.sh $requests $rampup $experiments
  sh $wd/scripts/pingpong-benchmarks/detached_monitored_experiment.sh $requests $rampup $experiments

  requests=$((requests+100))
  rampup=$((rampup+2))
  increments=$((increments-1))
done

python3 $wd/scripts/pingpong-benchmarks/pingpong-plots.py $wd $experiments