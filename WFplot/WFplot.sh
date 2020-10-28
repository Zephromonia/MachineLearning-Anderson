#!/bin/bash

evec=${1:-""}
color=${2:-1}
frame=${3:-0}

echo $evec $color $frame

currdir=`pwd`
echo $currdir

#WFPLOT=$HOME/Projects/MachineLearning/WFplot/WFplot.GF
WFPLOT=/storage/disqs/MachineLearning-Anderson/WFplot/WFplot.GF
#WFPLOT=$MLdir"/../WFplot/WFplot.GF"
#WFPLOT=/media/phsht/DataDrive/MachineLearning/Anderson/WFplot/WFplot.GF

echo -ne "$evec\n$color\n$frame" | $WFPLOT
