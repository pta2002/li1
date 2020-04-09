SHELL := /bin/bash
# Corre a tarefa 5
run5: Tarefa5
	cd src && ../Tarefa5

Tarefa5:
	cd src && ghc -o ../Tarefa5 ./Tarefa5_2019li1g181.hs

clean:
	rm **/*.hi
	rm **/*.o
	rm ./Tarefa5

doc:
	./criardoc.sh

.PHONY: Tarefa5 clean doc