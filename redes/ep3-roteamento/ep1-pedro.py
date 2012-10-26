#! /usr/bin/python
#-*- coding: utf-8 -*-
'''
Aluno: Pedro Paulo Vezzá Campos - 7538743
MAC0448-2012 - Programação para Redes de Computadores - Tarefa 3: Prot. de Roteamento
'''

from sys import argv

class Simulador(object):
	'''
	Documentacao
	'''

	def __init__(self, adj):
		self._roteadores = []
		for i in range(len(adj)):
			self._roteadores.append(Roteador(i))
		
		
		for i in range(len(adj)):
			for j in range(len(adj[i])):
				if adj[i][j] == -1:
					continue
				
				origem = self._roteadores[i]
				destino = self._roteadores[j]
				custo = adj[i][j]
				origem.adicionar_adjacente(destino, custo)

		print str(self._roteadores)

class Roteador(object):
	'''
	Documentacao
	'''

	def __init__(self, ident):
		self._adj = []
		self._ident = ident
	
	def adicionar_adjacente(self, novo, custo):
		self._adj.append((novo, custo))
	
	def __str__
if __name__ == '__main__':
	if len(argv) == 1:
		print "Forneca o nome do arquivo a ser utilizado como matriz de adjacencias."
		print "Ex: {0} roteadores.txt".format(argv[0])
		exit()
	
	matriz = []
	with open(argv[1]) as arq:
		for linha in arq:
			if len(linha.strip()) != 0:
				matriz.append([float(n) for n in linha.split()])

	Simulador(matriz)

