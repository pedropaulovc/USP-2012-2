#! /usr/bin/python
#-*- coding: utf-8 -*-
"""
Aluno: Pedro Paulo Vezzá Campos - 7538743
MAC0448-2012 - Programação para Redes de Computadores
Tarefa 4: Protocolos de Roteamento Multicast
"""

from sys import argv, exit
from Dijkstra import *

class Simulador(object):
	"""
	Classe responsável por atuar como interface em modo texto para a realizacão
	de consultas as tabelas de roteamento de cada um dos roteadores definidos.
	Ainda, realiza as interconexões entre os roteadores segundo a topologia
	da rede fornecida. Uma vez conectados os roteadores passam a funcionar de
	maneira autonoma na geracao das tabelas de roteamento.
	"""

	def __init__(self, adj):
		"""
		Construtor e unico metodo da classe Simulador. Realiza as tarefas de 
		interface e interconexao de roteadores como descrito acima.
		"""
		self._roteadores = []
		for i in range(len(adj)):
			self._roteadores.append(Roteador(i))
		
		print "Simulador: Interconectando roteadores."
		
		for i in range(len(adj)):
			for j in range(len(adj[i])):
				if adj[i][j] == -1:
					continue
				
				origem = self._roteadores[i]
				destino = self._roteadores[j]
				custo = adj[i][j]
				origem.adicionar_adjacente(destino, custo)

		print "Simulador: Roteadores interconectados. Mapa da rede: "
		print "\n".join(map(lambda r: r.__repr__(), self._roteadores))
		print "Aguardando consultas as tabelas de roteamento."
		
		while True:
			s = raw_input()
			if s.strip() == "exit" or s.strip() == "quit":
				exit()
			comando = s.split()
			
			if len(comando) < 2:
				print "Comandos disponiveis:"
				print " - send <netid>"
				print " - join <netid> <group>"
				print " - leave <netid> <group>"
				print " - exit"
				continue
				
			if comando[0] not in ['send', 'join', 'leave']:
				print "Comando desconhecido. Utilize 'send' ou 'join' ou 'leave'"
				continue
			
			netid = comando[1]
			
			if comando[0] == "send":
				pass
				
			if comando[0] == "join":
				if len(comando) < 3:
					print "Comando join e da forma 'join <netid> <group>'"
					continue
			if comando[0] == "leave":
				if len(comando) < 3:
					print "Comando leave e da forma 'leave <netid> <group>'"
					continue

			
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
		#Grafo na forma {v1: {w1: c1, w2: c2, ...}, ...}
		self._mapa_rede = {ident: {}} 
		#Vetor que mapeia ID de roteador em atraso total
		self._dist_a = [] 
		#Vetor que mapeia ID de roteador em num hops total
		self._dist_h = [] 
		#Vetor que indica o predecessor do roteador r na rota otima p/ atraso
		self._pred_a = [] 
		#Vetor que indica o predecessor do roteador r na rota otima p/ hops
		self._pred_h = [] 
		#Mapa que indica qual o ultimo numero de sequencia de anuncio recebido
		self._ultima_seq_ee = {ident: 0} 
		
		#Atributos vetor distancia
		#Mapas da forma {r1: (id_prox, custo_tot1), r2: (id_prox, custo_tot2), ...}
		self._vetor_dist_a = {} 
		self._vetor_dist_h = {}
		self._vetor_dist_tmp = {} #Conexoes ainda nao completas
		
		
	def obter_id(self):
		"""
		Metodo getter da ID do roteador
		"""
		return self._ident
	
	def adicionar_adjacente(self, novo, custo):
		"""
		Adiciona uma ligacao da forma self -> novo com atraso = custo. Atualiza
		as estruturas de dados de maneira adequada. Caso detecte que novo -> self
		gera anuncios de atualizacao das tabelas de roteamento.
		"""
		id_novo = novo.obter_id()
		
		#print "\n{0}: Me conectei a {1} com custo {2}".format(self._ident, \
		#id_novo, custo) 
		
		self._adj.append((novo, custo))
		self._mapa_rede[self._ident][id_novo] = custo;
		if id_novo not in self._mapa_rede:
			self._mapa_rede[id_novo] = {}
		
		confirmou = novo.confirmar_conexao(self._ident)	
		if confirmou:
			#print "{0}: {1} confirmou a conexao.".format(self._ident, id_novo)
			#print "{0}: Anunciando mapa.".format(self._ident)
			self.anunciar_ee()
			#print "{0}: Fim anuncio de novo mapa.".format(self._ident)
			#print "{0}: Atualizando tabela de roteamento ee.".format(self._ident)
			self.calcular_tabela_roteamento_ee()
			#print "{0}: Fim atualizacao tabela de roteamento ee.".format(self._ident)

			#print "{0}: Anunciando vetor distancia 'a'.".format(self._ident)
			if id_novo in self._vetor_dist_a:
				(prox_atual, custo_atual) = self._vetor_dist_a[id_novo]
				if custo < custo_atual :
					self._vetor_dist_a[id_novo] = (id_novo, custo)
			else:
				self._vetor_dist_a[id_novo] = (id_novo, custo)
			self.anunciar_vd('a')
			#print "{0}: Fim anuncio vetor distancia 'a'.".format(self._ident)
			#print "{0}: Anunciando vetor distancia 'h'.".format(self._ident)
			self._vetor_dist_h[id_novo] = (id_novo, 1)
			self.anunciar_vd('h')
			#print "{0}: Fim anuncio vetor distancia 'h'".format(self._ident)
		else:
			self._vetor_dist_tmp[id_novo] = (id_novo, custo)
			#print "{0}: {1} ainda nao confirmou a conexao.".format(self._ident, id_novo)
			
	def confirmar_conexao(self, ident):
		"""
		Retorna um booleano indicando se self identifica ident no seu mapa da
		rede (Foi feita a conexao self -> ident).
		"""
		return ident in self._mapa_rede and ident in self._mapa_rede[self._ident]
	
	def anunciar_ee(self):
		"""
		Informa a todos os roteadores adjacentes o estado das suas conexoes no
		momento
		"""
		
		seq = self._ultima_seq_ee[self._ident]
		self._ultima_seq_ee[self._ident] += 1
		for (r, _) in self._adj:
			r.receber_anuncio_ee(self._ident, self.obter_ids_custos_adj(), seq)
	
	def receber_anuncio_ee(self, origem, adj, seq):
		"""
		Recebe um anuncio de um roteador adjacente sobre alguma mudanca detectada
		no estados dos enlaces conhecidos ate entao. Verifica se o numero de
		sequencia seq da mensagem ja foi recebido. Se sim, nao faz nada. Senao,
		repassa a mensagem aos vizinhos e verifica se houve alguma mudanca 
		no seu proprio estado dos enlaces. Em caso afirmativo envia um 
		novo anuncio informando da mudanca e atualiza a tabela de roteamento.
		"""
		if  origem in self._ultima_seq_ee and \
			seq <= self._ultima_seq_ee[origem]:
			return
		
		if origem not in self._mapa_rede:
			self._mapa_rede[origem] = {}
		
		novo = {}
		for (r, c) in adj:
			novo[r] = c;
			if r not in self._mapa_rede: 
				self._mapa_rede[r] = {}
		
		self._ultima_seq_ee[origem] = seq
		
		#print "{0}: Recebi anuncio num {1} de '{2}' contendo {3}.".format(\
		#	str(self._ident), str(seq), str(origem), str(adj))
			
		anunciar = (novo != self._mapa_rede[origem])
		if anunciar:
			self._mapa_rede[origem] = novo
			#print "   Novo mapa: {0}".format(str(self._mapa_rede))
		else:
			#print "   Mapa ja atualizado. Apenas repassando anuncio."
			pass
		
		for (r, _) in self._adj:
			r.receber_anuncio_ee(origem, adj, seq)
		
		if anunciar:
			self.calcular_tabela_roteamento_ee()
			self.anunciar_ee()
	
	def calcular_tabela_roteamento_ee(self):
		"""
		Uma vez que o grafo da rede esta completo, executa o algoritmo de Dijkstra
		para determinar e armazenar as rotas otimas com relacao a atraso e
		numero de hops.
		"""
		
		(self._dist_a, self._pred_a) = Dijkstra(self._mapa_rede, self._ident, use_hops=False)
		(self._dist_h, self._pred_h) = Dijkstra(self._mapa_rede, self._ident, use_hops=True)
		
		#print "   (Distancias ate os destinos, Predecessor dos destinos na rota)"
		#print "   Considerando atraso:"
		#print "   " + str((self._dist_a, self._pred_a))
		#print "   Considerando hops:"
		#print "   " + str((self._dist_h, self._pred_h))
	
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
		
		#print "\n{0}: Anunciando meu vd: {1}".format(\
		#	self._ident, anuncio)
		
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
		
		if metrica == 'h':
			custo_atual = -1
			if origem in vetor_dist:
				(_, custo_atual) = vetor_dist[origem]
			if custo_atual != 1:
				vetor_dist[origem] = (origem, 1)
				atualizou = True
		
		if metrica == 'a' and origem in self._vetor_dist_tmp:
			if origem in vetor_dist:
				(_, custo_atual) = vetor_dist[origem]
				(_, custo_tmp) = self._vetor_dist_tmp[origem]
				if custo_tmp < custo_atual:
					atualizou = True
					vetor_dist[origem] = self._vetor_dist_tmp[origem]
			else:
				atualizou = True
				vetor_dist[origem] = self._vetor_dist_tmp[origem]
			del self._vetor_dist_tmp[origem]
		
		if origem not in vetor_dist:
			return
		
		custo_origem = 1
		if metrica == 'a':
			for (r, c) in self._adj:
				if r.obter_id() == origem:
					custo_origem = c
					break
		
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
		
		if atualizou:
			#print "{0}: atualizei meu vd para {1}".format(self._ident, vetor_dist)
			self.anunciar_vd(metrica)
		else:
			#print "{0}: Nao atualizei meu vd.".format(self._ident)
			pass
	
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
		print "Forneca o nome do arquivo a ser utilizado como matriz de \
		adjacencias e o metodo utilizado para a montagem das arvores multicast."
		print "Ex: {0} roteadores.txt shared|source" % (argv[0])
		exit()
	
	matriz = []
	with open(argv[1]) as arq:
		for linha in arq:
			if len(linha.strip()) != 0:
				matriz.append([float(n) for n in linha.split()])

	Simulador(matriz)

