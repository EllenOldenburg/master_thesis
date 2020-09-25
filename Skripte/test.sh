# in allen Dateien *.csv -> ï zu i aendern
for filehandle in `ls *.csv`:
do
ex $filehandle <<'EOF'
:%s/ï/i/g
wq
EOF
done

