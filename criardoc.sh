#!/usr/bin/env bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd $DIR/src
haddock -h -o ../doc/html Tarefa5_2019li1g181.hs