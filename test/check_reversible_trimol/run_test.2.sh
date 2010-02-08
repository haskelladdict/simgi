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

for ((counter=0; counter <= 200; counter++)); do

  # provide a little "progressbar"
  printf "."

  # run and process
  ${simgi_exe} -s ${counter} reversible_trimol.2.sgl >& /dev/null 
  tail -n 2000 reversible_trimol.2.dat | ../test_helpers/average >> ${globalFile} || return 1

  # unlink
  rm -f reversible_trimol.2.dat || return 1
  
done

# check if we're within the specs
# the expected concentration is 2.1986e-05 mol
# and we allow 0.05% relative deviation
../test_helpers/check_deviation ${globalFile} 4.2482e-5 5e-4
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
