#! /usr/bin/python
#-*- coding: utf-8 -*-
'''
Aluno: Pedro Paulo Vezzá Campos - 7538743
MAC0448-2012 - Programação para Redes de Computadores
Tarefa 3: Protocolos de Roteamento
'''

from sys import argv, exit
from Dijkstra import *

class Simulador(object):
	'''
	Documentacao
	'''

	def __init__(self, adj):
		self._roteadores = []
		for i in range(len(adj)):
			self._roteadores.append(Roteador(i))
		
		print "Interconectando roteadores ...",
		
		for i in range(len(adj)):
			for j in range(len(adj[i])):
				if adj[i][j] == -1:
					continue
				
				origem = self._roteadores[i]
				destino = self._roteadores[j]
				custo = adj[i][j]
				origem.adicionar_adjacente(destino, custo)

		print "ok. Mapa da rede:"
		print "\n".join(map(lambda r: r.__repr__(), self._roteadores))
		
		print "== Iniciando roteamento link-state: =="
		print "Iniciando anuncio do estado atual:"
		for r in self._roteadores:
			r.anunciar_estado()
		print "Anuncio do estado atual completo."
		print "Calculando tabelas de roteamento:"
		for r in self._roteadores:
			r.calcular_tabela_roteamento_linkstate()
		print "Tabelas de roteamento calculadas."
		print "== Roteamento link-state completo. =="
		print "== Iniciando roteamento distance-vector: =="
		print "Iniciando anuncio dos vetores distancia para atraso"
		for r in self._roteadores:
			r.anunciar_vetor_distancia('a')
		print "Encerrado anuncio dos vetores distancia para atraso"
		print "Iniciando anuncio dos vetores distancia para numero de hops"
		for r in self._roteadores:
			r.anunciar_vetor_distancia('h')
		print "Encerrado anuncio dos vetores distancia para numero de hops"
		print "== Roteamento distance-vector completo. =="
		
		while True:
			s = raw_input('> ')
			if s.strip() == "exit":
				exit()
			comando = s.split()
			
			if len(comando) < 4:
				print "Forneca comando da forma <algor> <origem> <destino> <metrica>"
				continue
			if comando[0] != "ee" and comando[0] != "vd":
				print "Algoritmo desconhecido. Utilize 'ee' ou 'vd'"
				continue

			origem = int(comando[1])
			destino = int(comando[2])
			
			if origem < 0 or origem >= len(self._roteadores):
				print "Origem invalida. Utilize valor entre 0 e {0}".format(len(self._roteadores) - 1)
				continue
			if destino < 0 or destino >= len(self._roteadores):
				print "Destino invalido. Utilize valor entre 0 e {0}".format(len(self._roteadores) - 1)
				continue
			if comando[3] != "a" and comando[3] != "h":
				print "Metrica invalida. Utilize 'a' ou 'h'"
				continue
			
			if comando[0] == "ee" and comando[3] == "a":
				(rota, dist) = self._roteadores[origem].obter_rota_linkstate(destino, False)
			if comando[0] == "ee" and comando[3] == "h":
				(rota, dist) = self._roteadores[origem].obter_rota_linkstate(destino, True)
			if comando[0] == "vd":
				(rota, dist) = self._roteadores[origem].obter_rota_vd(destino, comando[3])

			nome_metrica = "milisegundos"
			if comando[3] == "h":
				nome_metrica = "hops"
				
			print "{0} ({1} {2})".format(" ".join(map(lambda r: str(r), rota)), \
				str(dist), nome_metrica)
			
class Roteador(object):
	'''
	Documentacao
	'''

	def __init__(self, ident):
		self._adj = []
		self._ident = ident
		
		#Atributos Link-state
		self._mapa_rede = {ident: {}}
		self._dist = []
		self._pred = []
		
		#Atributos Distance-vector
		self._vetor_dist_a = {}
		self._vetor_dist_h = {}
		
	def get_id(self):
		return self._ident
	
	def adicionar_adjacente(self, novo, custo):
		self._adj.append((novo, custo))
		self._mapa_rede[self._ident][novo.get_id()] = custo;
		self._vetor_dist_a[novo.get_id()] = (novo.get_id(), custo)
		self._vetor_dist_h[novo.get_id()] = (novo.get_id(), 1)
	
	def anunciar_estado(self):
		for (r, _) in self._adj:
			r.receber_anuncio(self._ident, self.obter_ids_custos_adj())
	
	def receber_anuncio(self, ident, adj):
		if ident in self._mapa_rede:
			return
		
		self._mapa_rede[ident] = {}
		for (r, c) in adj:
			self._mapa_rede[ident][r] = c;

		print str(self._ident) + ": Recebi anuncio de " + str(ident) + \
			" contendo " + str(adj) + " meu novo mapa da rede: "
		print self._mapa_rede
		
		for (r, _) in self._adj:
			r.receber_anuncio(ident, adj)
	
	def calcular_tabela_roteamento_linkstate(self):
		(self._dist_a, self._pred_a) = Dijkstra(self._mapa_rede, self._ident, use_hops=False)
		(self._dist_h, self._pred_h) = Dijkstra(self._mapa_rede, self._ident, use_hops=True)
		
		print str(self._ident) + ": Terminei de calcular as rotas:"
		print "(Distancias ate os destinos, Predecessor dos destinos na rota)"
		print "Considerando atraso:"
		print (self._dist_a, self._pred_a)
		print "Considerando hops:"
		print (self._dist_h, self._pred_h)
	
	def obter_rota_linkstate(self, destino, usando_hops=False):
		pred = self._pred_a
		dist = self._dist_a[destino]
		if usando_hops:
			pred = self._pred_h
			dist = self._dist_h[destino]
		
		rota = []
		while True:
			rota.append(destino)
			if destino == self._ident: break
			destino = pred[destino]
		rota.reverse()
		return (rota, dist)
	
	def anunciar_vetor_distancia(self, tipo):
		vetor_dist = self._vetor_dist_a
		if tipo == 'h':
			vetor_dist = self._vetor_dist_h

		anuncio = {}
		for (dest, (_, custo)) in vetor_dist.items():
			anuncio[dest] = custo
		
		print "\n{0}: Anunciando meu vd: {1}".format(\
			self._ident, anuncio)
		
		for (r, _) in self._adj:
			r.receber_anuncio_vetor_distancia(self._ident, anuncio, tipo)
	
	def receber_anuncio_vetor_distancia(self, origem, anuncio, tipo):
		vetor_dist = self._vetor_dist_a
		if tipo == 'h':
			vetor_dist = self._vetor_dist_h
		
		atualizou = False
		(_, custo_origem) = vetor_dist[origem]

		for (dest, custo) in anuncio.items():
			if dest == self._ident:
				continue
			
			if dest not in vetor_dist:
				vetor_dist[dest] = (origem, custo + custo_origem)
				atualizou = True
				continue

			(_, custo_destino) = vetor_dist[dest]
			
			if custo + custo_origem < custo_destino:
				vetor_dist[dest] = (origem, custo + custo_origem)
				atualizou = True
		
		print "{0}: atualizei meu vd para {1}".format(\
			self._ident, vetor_dist)
		
		if atualizou:
			self.anunciar_vetor_distancia(tipo)
	
	def obter_rota_vd(self, destino, metrica):
		if destino == self._ident:	
			return ([self._ident], 0)
		
		vetor_dist = self._vetor_dist_a
		if metrica == 'h':
			vetor_dist = self._vetor_dist_h
		
		#TODO: Isso vai dar problema se o grafo for desconexo
		(prox, custo) = vetor_dist[destino]
		
		for (r, _) in self._adj:
			if r.get_id() == prox:
				rprox = r
				break
		
		(rota, _) = rprox.obter_rota_vd(destino, metrica)
		rota.insert(0, self._ident)
		
		return (rota, custo)
		
	def __repr__(self):
		return "<" + str(self._ident) + ", " + str(self.obter_ids_custos_adj()) + ">"
	
	def obter_ids_custos_adj(self):
		return map(lambda (r, c): (r.get_id(), c), self._adj)
		
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

