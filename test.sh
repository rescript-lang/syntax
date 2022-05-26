#!/bin/zsh

setopt extendedglob

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

taskCount=0
function maybeWait {
  let taskCount+=1
  # spawn in batch of 20 processes
  [[ $((taskCount % 20)) = 0 ]] && wait
}

# parsing
for file in tests/parsing/{errors,infiniteLoops,recovery}/**/*.(res|resi); do
  lib/rescript.exe -recover -print ml $file &> $(exp $file) & maybeWait
done
for file in tests/parsing/{grammar,other}/**/*.(res|resi); do
  lib/rescript.exe -print ml $file &> $(exp $file) & maybeWait
done

# printing
for file in tests/{printer,conversion}/**/*.(res|resi|ml|mli); do
  lib/rescript.exe $file &> $(exp $file) & maybeWait
done

# printing with ppx
for file in tests/ppx/react/*.(res|resi); do
  lib/rescript.exe -ppx jsx $file &> $(exp $file) & maybeWait
done

wait

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified tests/**/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  exit 1
fi

# roundtrip tests
if [[ $ROUNDTRIP_TEST = 1 ]]; then
  echo "Running roundtrip tests…"
  mkdir -p temp
  roundtripTestsResult="temp/result.txt"
  touch $roundtripTestsResult

  for file in tests/{idempotency,printer}/**/*.(res|resi|re|rei|ml|mli); do {
    mkdir -p temp/$(dirname $file)
    sexpAst1=temp/$file.sexp
    sexpAst2=temp/$file.2.sexp
    rescript1=temp/$file.res
    rescript2=temp/$file.2.res

    case $file in
      *.ml   ) class="ml" ; resIntf=""         ;;
      *.mli  ) class="ml" ; resIntf=-interface ;;
      *.res  ) class="res"; resIntf=""         ;;
      *.resi ) class="res"; resIntf=-interface ;;
    esac

    lib/rescript.exe $resIntf -parse $class -print sexp $file > $sexpAst1
    lib/rescript.exe $resIntf -parse $class -print res $file > $rescript1

    lib/rescript.exe $resIntf -print sexp $rescript1 > $sexpAst2
    lib/rescript.exe $resIntf -print res $rescript1 > $rescript2

    diff --unified $sexpAst1 $sexpAst2
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
    diff --unified $rescript1 $rescript2
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
  } & maybeWait
  done

  wait

  result=$(cat $roundtripTestsResult)
  rm -r temp/

  if [[ $result = "1" ]]; then
    printf "${warningYellow}⚠️ Roundtrip tests failed.${reset}\n"
    exit 1
  else
    printf "${successGreen}✅ Roundtrip tests succeeded.${reset}\n"
  fi

fi
