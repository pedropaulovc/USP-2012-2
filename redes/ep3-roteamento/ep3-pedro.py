#! /usr/bin/python
#-*- coding: utf-8 -*-
"""
Aluno: Pedro Paulo Vezzá Campos - 7538743
MAC0448-2012 - Programação para Redes de Computadores
Tarefa 3: Protocolos de Roteamento
"""

from sys import argv, exit
from Dijkstra import *

class Simulador(object):
	"""
	Classe responsável por atuar como interface em modo texto para a realizacão
	de consultas as tabelas de roteamento de cada um dos roteadores definidos.
	Ainda, e responsavel por gerar os eventos de atualizacao das tabelas dos
	roteadores, passando a se comunicarem para atingir para convergirem as rotas
	otimas.
	"""

	def __init__(self, adj):
		"""
		Construtor e unico metodo da classe Simulador. Realiza as tarefas de 
		interface e geracao de eventos descritos acima.
		"""
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
		
		print "== Iniciando roteamento estado enlace: =="
		print "Iniciando anuncio do estado atual:"
		for r in self._roteadores:
			r.anunciar_ee()
		print "Anuncio do estado atual completo."
		print "Calculando tabelas de roteamento:"
		for r in self._roteadores:
			r.calcular_tabela_roteamento_ee()
		print "Tabelas de roteamento calculadas."
		print "== Roteamento estado enlace completo. =="
		print "== Iniciando roteamento vetor distancia: =="
		print "Iniciando anuncio dos vetores distancia para atraso"
		for r in self._roteadores:
			r.anunciar_vd('a')
		print "Encerrado anuncio dos vetores distancia para atraso"
		print "Iniciando anuncio dos vetores distancia para numero de hops"
		for r in self._roteadores:
			r.anunciar_vd('h')
		print "Encerrado anuncio dos vetores distancia para numero de hops"
		print "== Roteamento vetor distancia completo. =="
		print "== Roteadores prontos para consultas =="
		
		while True:
			s = raw_input('> ')
			if s.strip() == "exit" or s.strip() == "quit":
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
			
			if comando[0] == "ee":
				(rota, dist) = self._roteadores[origem].obter_rota_ee(destino, comando[3])
			if comando[0] == "vd":
				(rota, dist) = self._roteadores[origem].obter_rota_vd(destino, comando[3])

			nome_metrica = "milisegundo"
			if comando[3] == "h":
				nome_metrica = "hop"
			if dist > 1:
				nome_metrica += "s"
			
			if rota == []:
				print "Nao existe rota entre {0} e {1}".format(origem, destino)
				continue
				
			print "{0} ({1} {2})".format(" ".join(map(lambda r: str(r), rota)), \
				str(dist), nome_metrica)
			
class Roteador(object):
	"""
	Classe representante de um roteador. Aqui sao implementados os algoritmos de
	roteamento baseados no estado do enlace (metodos _ee) e baseados em vetor
	distancia (metodos _vd)
	"""

	def __init__(self, ident):
		"""
		Construtor da classe Roteador, apenas é responsavel por inicializar
		as estruturas de dados utilizadas 
		"""
		
		self._adj = [] #Roteadores adjacentes na forma [(rot1, custo1, (rot2, custo2), ...]
		self._ident = ident #ID do roteador
		
		#Atributos estado enlace
		self._mapa_rede = {ident: {}} #Grafo na forma {v1: {w1: c1, w2: c2, ...}, ...}
		self._dist_a = [] #Vetor que mapeia ID de roteador em atraso total
		self._dist_h = [] #Vetor que mapeia ID de roteador em num hops total
		self._pred_a = [] #Vetor que indica o predecessor do roteador r na rota otima p/ atraso
		self._pred_h = [] #Vetor que indica o predecessor do roteador r na rota otima p/ hops
		
		#Atributos vetor distancia
		#Mapas da forma {r1: (id_prox, custo_tot1), r2: (id_prox, custo_tot2), ...}
		self._vetor_dist_a = {} 
		self._vetor_dist_h = {}
		
	def obter_id(self):
		"""
		Metodo getter da ID do roteador
		"""
		return self._ident
	
	def adicionar_adjacente(self, novo, custo):
		"""
		Adiciona uma ligacao da forma self -> novo com atraso = custo. Atualiza
		as estruturas de dados de maneira adequada.
		"""
		self._adj.append((novo, custo))
		self._mapa_rede[self._ident][novo.obter_id()] = custo;
		self._vetor_dist_a[novo.obter_id()] = (novo.obter_id(), custo)
		self._vetor_dist_h[novo.obter_id()] = (novo.obter_id(), 1)
	
	def anunciar_ee(self):
		"""
		Informa a todos os roteadores adjacentes o estado das suas conexoes no
		momento
		"""
		for (r, _) in self._adj:
			r.receber_anuncio_ee(self._ident, self.obter_ids_custos_adj())
	
	def receber_anuncio_ee(self, ident, adj):
		"""
		Recebe um anuncio de um roteador adjacente sobre alguma mudanca detectada
		no estados dos enlaces conhecidos ate entao. Verifica se o grafo dos
		roteadores esta atualizado. Caso esteja nao faz nada, senao atualiza e
		informa todos os adjacentes da mudanca.
		"""
		if ident in self._mapa_rede:
			return
		
		self._mapa_rede[ident] = {}
		for (r, c) in adj:
			self._mapa_rede[ident][r] = c;

		print str(self._ident) + ": Recebi anuncio de " + str(ident) + \
			" contendo " + str(adj) + " meu novo mapa da rede: "
		print self._mapa_rede
		
		for (r, _) in self._adj:
			r.receber_anuncio_ee(ident, adj)
	
	def calcular_tabela_roteamento_ee(self):
		"""
		Uma vez que o grafo da rede esta completo, executa o algoritmo de Dijkstra
		para determinar e armazenar as rotas otimas com relacao a atraso e
		numero de hops.
		"""
		(self._dist_a, self._pred_a) = Dijkstra(self._mapa_rede, self._ident, use_hops=False)
		(self._dist_h, self._pred_h) = Dijkstra(self._mapa_rede, self._ident, use_hops=True)
		
		print str(self._ident) + ": Terminei de calcular as rotas:"
		print "(Distancias ate os destinos, Predecessor dos destinos na rota)"
		print "Considerando atraso:"
		print (self._dist_a, self._pred_a)
		print "Considerando hops:"
		print (self._dist_h, self._pred_h)
	
	def obter_rota_ee(self, destino, metrica):
		"""
		Assume que calcular_tabela_roteamento_ee() ja foi executado, determina
		e retorna a rota otima e o custo total de acordo com a metrica = 'a' ou 
		'h' para o destino na forma ([ident, r1, r2, ..., destino], custo).
		Caso nao haja rota retorna ([], -1)
		"""
		if destino not in self._dist_a:
			return ([], -1)
		pred = self._pred_a
		dist = self._dist_a[destino]
		if metrica == 'h':
			pred = self._pred_h
			dist = self._dist_h[destino]
		
		rota = []
		while True:
			rota.append(destino)
			if destino == self._ident: break
			destino = pred[destino]
		rota.reverse()
		return (rota, dist)
	
	def anunciar_vd(self, metrica):
		"""
		Envia a todos os roteadores adjacentes o vetor distancia mais atual que
		o roteador possui, na forma {dest1: custo1, dest2: custo2, ...} considerando
		a metrica = 'a' ou 'h'
		"""
		vetor_dist = self._vetor_dist_a
		if metrica == 'h':
			vetor_dist = self._vetor_dist_h

		anuncio = {}
		for (dest, (_, custo)) in vetor_dist.items():
			anuncio[dest] = custo
		
		print "\n{0}: Anunciando meu vd: {1}".format(\
			self._ident, anuncio)
		
		for (r, _) in self._adj:
			r.receber_anuncio_vd(self._ident, anuncio, metrica)
	
	def receber_anuncio_vd(self, origem, anuncio, metrica):
		"""
		Processa um anuncio recebido de origem utilizando a metrica = 'a' ou 'h'
		Verifica se o vetor distancia interno deve ser atualizado e somente em
		caso afirmativo faz a atualizacao e anuncia o novo vetor aos adjacentes.
		"""
		vetor_dist = self._vetor_dist_a
		if metrica == 'h':
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
			self.anunciar_vd(metrica)
	
	def obter_rota_vd(self, destino, metrica):
		"""
		Consulta proximo roteador na rota otima para a metrica passada ('a' ou
		'h') pedindo a rota deste roteador ate o destino. Retorna a concatencacao
		da rota recebida com a id atual junto com o custo total. Caso o destino
		seja o proprio roteador retorna ([destino], 0) e caso o destino nao conste
		no vetor distancia retorna ([], -1)
		"""
		if destino == self._ident:	
			return ([self._ident], 0)
		
		vetor_dist = self._vetor_dist_a
		if metrica == 'h':
			vetor_dist = self._vetor_dist_h
		
		if destino not in vetor_dist:
			return ([], -1)
		
		(prox, custo) = vetor_dist[destino]
		
		for (r, _) in self._adj:
			if r.obter_id() == prox:
				rprox = r
				break
		
		(rota, _) = rprox.obter_rota_vd(destino, metrica)
		rota.insert(0, self._ident)
		
		return (rota, custo)
		
	def __repr__(self):
		"""
		Retorna uma representacao textual do roteador, composta por:
		<ident, [(adj1, custo1), (adj2, custo2), ...]>
		"""
		return "<" + str(self._ident) + ", " + str(self.obter_ids_custos_adj()) + ">"
	
	def obter_ids_custos_adj(self):
		"""
		Retorna uma lista contendo pares de id de roteadores adjacentes e seus
		respectivos custos.
		"""
		return map(lambda (r, c): (r.obter_id(), c), self._adj)
		
if __name__ == '__main__':
	"""
	Trecho main do programa. Le a matriz do arquivo passado e inicia o Simulador.
	"""
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

