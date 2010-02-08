#!/bin/bash
#
# short wrapper for runing a number of reversible
# simulations and then checking if we can reproduce
# the correct average product concentration
#

# 
simgi_exe="${1}"

# create a global tempfile to collect the data
globalFile=$(mktemp simgi.XXXXXXXXXXXXX)

for ((counter=0; counter <= 100; counter++)); do

  # provide a little "progressbar"
  printf "."

  # run and process
  ${simgi_exe} -s ${counter} reversible.sgl >& /dev/null 
  tail -n 400 reversible.dat | gawk ' { print $5}' | ./average >> ${globalFile} || return 1

  # unlink
  rm -f reversible.dat || return 1
  
done

# check if we're within the specs
# the expected number of products is 430.643462709951
# and we allow 0.5% tolerance
./check_deviation ${globalFile} 430.643462709951 5e-3
status=$?

# brief output
if [[ ${status} == 0 ]]; then
  echo
  echo
  echo "Congratulations - the reversible reaction test passed!"
  echo
  echo
else
  echo
  echo
  echo "Error - the reversible reaction test failed. Please check!"
  echo
  echo
fi

# remove files
rm -f ${globalFile}

# return status of deviation check
exit ${status}
