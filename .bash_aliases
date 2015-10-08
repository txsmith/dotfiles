alias tree-git="tree -I \$(sed -e 's/\///g; /^$\|#/d' .gitignore | tr '\n' '|')"

