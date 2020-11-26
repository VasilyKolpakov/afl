echo bootstrap.2s # implicit import
grep -oP '(?<=^\# import )(\w|\.)+$' "$1" | # collect all imports from file
xargs -I {} sh -c "./$0 {}; echo {}"        # recursively call this script for each import and print import
