#!/bin/bash

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

./run_file_non_interactive.sh lisp_test.lisp > lisp_test_actual_output
if cmp lisp_test_expected_output lisp_test_actual_output ; then
    echo "${green}OK${reset}"
else
    echo "${red}FAIL${reset}"
    diff -y lisp_test_expected_output lisp_test_actual_output
fi
