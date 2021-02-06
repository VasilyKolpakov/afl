function cat_imports {
    ./list_imports.sh "$1" |                # list all imports with duplicates in dependency-first sort order
    awk '{printf("%04d\t%s\n", NR, $0)}' |  # prepend each line with a padded line number, e.g. 0042
    sort -k 2 |                             # sort by file name
    uniq -f 1 |                             # remove duplicate names
    sort |                                  # sort by original line number
    cut -f 2 |                              # remove line numbers
    xargs cat /dev/null                     # cat all files
}
(cat_imports $1 ; cat $1; cat) | ./run_boot.sh 
