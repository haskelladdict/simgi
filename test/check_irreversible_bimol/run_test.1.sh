#!/bin/bash
#
# short wrapper for runing a number of reversible
# simulations and then checking if we can reproduce
# the correct average product concentration
#

# expected values of molecule counts
a_expect=0.0
b_expect=0.0
c_expect=1000.0

# 
simgi_exe="${1}"

# our global status
status=0

for ((counter=0; counter <= 100; counter++)); do

  # provide a little "progressbar"
  printf "."

  # run and process
  ${simgi_exe} irreversible_bimol.1.sgl >& /dev/null 

  a=$(tail -n 1 irreversible_bimol.1.dat | gawk ' { print $3 }')
  b=$(tail -n 1 irreversible_bimol.1.dat | gawk ' { print $4 }')
  c=$(tail -n 1 irreversible_bimol.1.dat | gawk ' { print $5 }')

  result_a=$(echo "${a} == ${a_expect}" | bc)
  result_b=$(echo "${b} == ${b_expect}" | bc)
  result_c=$(echo "${c} == ${c_expect}" | bc)

  if [[ ${result_a} == 0 || ${result_b} == 0 || ${result_c} == 0 ]];
  then
    status=1
  fi

  # unlink
  rm -f irreversible_bimol.1.dat || return 1
  
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
