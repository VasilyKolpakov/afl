#!/bin/bash

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

./run_lisp_file.sh lisp_test.scm > lisp_test_actual_output

if cmp -n $(wc -c < lisp_test_expected_output) lisp_test_expected_output lisp_test_actual_output ; then
    echo "${green}OK${reset}"
    cp lisp_test_actual_output lisp_test_expected_output
else
    echo "${red}FAIL${reset}"
    diff -y lisp_test_expected_output lisp_test_actual_output
fi
