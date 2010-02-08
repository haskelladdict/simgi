#!/bin/bash
#
# short wrapper for runing a number of reversible
# simulations and then checking if we can reproduce
# the correct average product concentration
#

# expected values of molecule counts
a_expect=0.000000000000000
b_expect=0.000000000000000
c_expect=1000.000000000000000

# 
simgi_exe="${1}"

# our global status
status=0

for ((counter=0; counter <= 100; counter++)); do

  # provide a little "progressbar"
  printf "."

  # run and process
  ${simgi_exe} irreversible.1.sgl >& /dev/null 

  a=$(tail -n 1 irreversible.1.dat | gawk ' { print $3 }')
  b=$(tail -n 1 irreversible.1.dat | gawk ' { print $4 }')
  c=$(tail -n 1 irreversible.1.dat | gawk ' { print $5 }')

  if [[ ${a_expect} != ${a} || ${b_expect} != ${b} \
        || ${c_expect} != ${c} ]];
  then
    status=1
  fi

  # unlink
  rm -f irreversible.1.dat || return 1
  
done

# check if all the tests passed

# brief output
if [[ ${status} == 0 ]]; then
  echo
  echo
  echo "Congratulations - the irreversible reaction test 1 passed!"
  echo
  echo
else
  echo
  echo
  echo "Error - the irreversible reaction test failed. Please check!"
  echo
  echo
fi

# return status of deviation check
exit ${status}