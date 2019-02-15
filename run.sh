
ocamlfind ocamlopt -o out -linkpkg -package sat,dpll,cdcl,str src/main.ml

for file in `\find dimacs/n100_m403 -maxdepth 1 -type f`; do
    ./out $file >> dimacs/result/n100_m403.txt
done
for file in `\find dimacs/n100_m449 -maxdepth 1 -type f`; do
    ./out $file >> dimacs/result/n100_m449.txt
done
for file in `\find dimacs/n125_m538 -maxdepth 1 -type f`; do
    ./out $file >> dimacs/result/n125_m538.txt
done


rm src/main.cmi
rm src/main.cmx
rm src/main.o
