
echo "Options: $options"
echo

for name in /home/delour/Stage-2017/closure-congruence-HO/implem/exemple/tests/*.smt2; do
   ./closure.native -smt2 $name;
   echo $name;
    echo "=====================================";
done
