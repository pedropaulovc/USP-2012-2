#! /usr/bin/python
#-*- coding: utf-8 -*-
"""
Aluno: Pedro Paulo Vezzá Campos - 7538743
MAC0448-2012 - Programação para Redes de Computadores
Tarefa 4: Protocolos de Roteamento Multicast
"""

from sys import argv, exit
from Dijkstra import *
from collections import deque

class Simulador(object):
	"""
	Classe responsável por atuar como interface em modo texto para a realizacão
	de consultas as tabelas de roteamento de cada um dos roteadores definidos.
	Ainda, realiza as interconexões entre os roteadores segundo a topologia
	da rede fornecida. Uma vez conectados os roteadores passam a funcionar de
	maneira autonoma na geracao das tabelas de roteamento.
	"""

	def __init__(self, adj, metodo):
		"""
		Construtor da classe Simulador. Realiza as tarefas de 
		interface e interconexao de roteadores como descrito acima.
		"""
		self._mc_metodo = metodo
		self._mc_rendezvous = 0
		self._roteadores = []
		for i in range(len(adj)):
			self._roteadores.append(Roteador(i, metodo, self._mc_rendezvous))
		
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
		if metodo == 'shared':
			print "Simulador: Árvore compartilhada:"
			self.exibir_arvore_source(self._mc_rendezvous, -1)
		print "Simulador: Aguardando consultas multicast."
			
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
			
			netid = int(comando[1])
			
			if netid < 0 or netid >= len(self._roteadores):
				print "netid invalido"
				continue
			
			if comando[0] == "send":
				id_grupo = self._roteadores[netid].criar_grupo()
				print "Grupo multicast criado com a id %d" % (id_grupo)
				continue
				
			if len(comando) < 3:
				print "Forneca o grupo que deseja (des)conectar"
				continue
					
			group = int(comando[2])
			
			if comando[0] == "join":
				self._roteadores[netid].conectar_grupo(group)
				self.exibir_grupos(netid)
				continue
				
			if comando[0] == "leave":
				self._roteadores[netid].desconectar_grupo(group)
				self.exibir_grupos(netid)
				continue
	
	def exibir_grupos(self, netid):
		"""
		Função responsável por exibir os grupos multicast atualmente presentes
		na rede invocando os métodos de impressão do número de receptores e 
		a árvore multicast.
		"""
		for grupo, fonte in self._roteadores[netid]._mc_conhecidos.iteritems():
			self.exibir_qtd_receptores(grupo, fonte)
			print "         ---"
			if self._mc_metodo == 'shared':
				self.exibir_arvore_shared(fonte, grupo)
			else:
				self.exibir_arvore_source(fonte, grupo)
	
	def imprimir_receptores(self, atual, grupo):
		"""
		Função de impressão de uma linha das quantidades de receptores que um roteador
		tem.
		"""
		qtd = self._roteadores[atual]._mc_receptores[grupo]
		if qtd == 0:
			return
			
		receptores = "receptor"
		if qtd > 1:
			receptores += "es"
		print "         netid %d tem %d %s dos dados" % (atual, qtd, receptores)
	
	def exibir_qtd_receptores(self, grupo, fonte):
		"""
		Função de impressão das quantidades de receptores que cada roteador
		possui.
		"""
		print "group %d: netid %d eh a fonte dos dados" % (grupo, fonte)
		
		fila = deque([fonte])
		
		while len(fila) > 0:
			atual = fila.pop()
			self.imprimir_receptores(atual, grupo)
			fila.extend(self._roteadores[atual]._mc_encaminhar[grupo])
			
	def exibir_arvore_source(self, fonte, grupo):
		"""
		Função de impressão de uma árvore multicast segundo o método source.
		"""
		bfs = {1: []}
		fila = deque([(fonte, 1)])
		
		while len(fila) > 0:
			(atual, nivel) = fila.popleft()
			bfs[nivel].append(atual)
			
			interessados = set([])
			if grupo in self._roteadores[atual]._mc_encaminhar:
				interessados = self._roteadores[atual]._mc_encaminhar[grupo]
			
			if interessados != set([]) and nivel + 1 not in bfs:
				bfs[nivel + 1] = []
			
			for interessado in interessados:
				fila.append((interessado, nivel + 1))
		
		del bfs[1]
		print "         arvore raiz: netid %d" % (fonte)
		for nivel in sorted(bfs.keys()):
			print "         arvore nivel %d: netid %s" % (nivel, \
			", netid ".join(str(v) for v in bfs[nivel]))
		
		
	def exibir_arvore_shared(self, fonte, grupo):
		"""
		Função de impressão de uma árvore multicast segundo o método shared.
		"""

		fonte_ate_centro = []
		atual = fonte
		
		while atual != self._mc_rendezvous:
			fonte_ate_centro.append(atual)
			atual = self._roteadores[atual].descobir_proximo_roteador(self._mc_rendezvous).obter_id()
		
		fonte_ate_centro.append(self._mc_rendezvous)
		
		bfs = {}
		for i, r in enumerate(reversed(fonte_ate_centro)):
			bfs[i + 1] = [r]

		bfs[1] = []
		fila = deque([(self._mc_rendezvous, 1)])
		visitados = set([])
		
		while len(fila) > 0:
			(atual, nivel) = fila.popleft()
			if atual in visitados:
				continue
				
			bfs[nivel].append(atual)
			visitados.add(atual)
			
			interessados = set([])
			if grupo in self._roteadores[atual]._mc_encaminhar:
				interessados = self._roteadores[atual]._mc_encaminhar[grupo]
			
			if interessados != set([]) and nivel + 1 not in bfs:
				bfs[nivel + 1] = []
			
			for interessado in interessados:
				fila.append((interessado, nivel + 1))
		
		
		fila = deque([(fonte, len(fonte_ate_centro))])
		while len(fila) > 0:
			(atual, nivel) = fila.popleft()
			if atual in visitados:
				continue
			
			if atual not in fonte_ate_centro:
				if nivel + 2 not in bfs:
					bfs[nivel + 2] = []
				bfs[nivel + 2].append(atual)
			visitados.add(atual)
			
			interessados = set([])
			if grupo in self._roteadores[atual]._mc_encaminhar:
				interessados = self._roteadores[atual]._mc_encaminhar[grupo]
			
			if interessados != set([]) and nivel - 1 not in bfs:
				bfs[nivel - 1] = []
			
			for interessado in interessados:
				fila.append((interessado, nivel - 1))
		
		del bfs[1]
		print "         arvore raiz: netid %d" % (self._mc_rendezvous)
		for nivel in sorted(bfs.keys()):
			print "         arvore nivel %d: netid %s" % (nivel, \
			", netid ".join(str(v) for v in bfs[nivel]))
		
	
class Roteador(object):
	"""
	Classe representante de um roteador. Aqui sao implementados os algoritmos de
	roteamento baseados no estado do enlace (metodos _ee) e baseados em vetor
	distancia (metodos _vd) e os métodos de criação da árvore multicast baseada
	na fonte (métodos _source) e compartilhada (métodos _shared).
	"""

	def __init__(self, ident, metodo, rendezvous):
		"""
		Construtor da classe Roteador, apenas é responsavel por inicializar
		as estruturas de dados utilizadas. O parâmetro ident é o ID do roteador,
		metodo pode ser 'shared' ou 'source', rendezvous é o centro da árvore
		para o método de árvore compartilhada.
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
		
		self._mc_metodo     = metodo
		
		self._mc_conhecidos = {} #id_grupo -> origem dos dados
		self._mc_encaminhar = {} #id_grupo -> destinos
		self._mc_receptores = {} #id_grupo -> qtd receptores
		self._mc_conectados = set([]) #set([id_grupo])
		self._mc_ultimo_grupo = -1 #Ultimo grupo multicast reconhecido.
		
		self._mc_rendezvous = rendezvous #Centro da arvore compartilhada
		self._mc_pai_shared = None #Roteador pai na arvore compartilhada
		
	def criar_grupo(self):
		"""
		Função responsável por inicializar um novo grupo multicast no roteador.
		Inicia um broadcast aos outros roteadores para que tomem conhecimento
		do novo grupo criado. É independente de método.
		"""
		self._mc_ultimo_grupo += 1
		novo_id = self._mc_ultimo_grupo
		self._mc_conhecidos[novo_id] = self._ident
		self._mc_encaminhar[novo_id] = set([])
		self._mc_receptores[novo_id] = 0
		self._mc_conectados.add(novo_id)
		
		for (r, _) in self._adj:
			r.receber_novo_grupo_multicast(novo_id, self._ident)
		
		return novo_id
	
	def _remover_grupo(self, grupo):
		"""
		Função responsável por desalocar as estruturas que representam um grupo
		multicast no roteador e ainda iniciar um broadcast avisando da remoção
		do grupo.
		"""
		if grupo not in self._mc_conhecidos:
			return

		del self._mc_conhecidos[grupo]
		del self._mc_encaminhar[grupo]
		del self._mc_receptores[grupo]
		if grupo in self._mc_conectados:
			self._mc_conectados.remove(grupo)
		
		print "%d: Removendo grupo %d. Grupos ainda existentes: %s"\
		% (self._ident, grupo, self._mc_conhecidos)
		
		for (r, _) in self._adj:
			r._remover_grupo(grupo)
		
	def receber_novo_grupo_multicast(self, novo_id, origem):
		"""
		Cadastra no roteador a presença de um novo grupo multicast na rede de 
		id novo_id e de fonte dos dados origem.
		"""
		if novo_id in self._mc_conhecidos:
			return
		
		self._mc_ultimo_grupo = max(novo_id, self._mc_ultimo_grupo)
		self._mc_conhecidos[novo_id] = origem
		self._mc_encaminhar[novo_id] = set([])
		self._mc_receptores[novo_id] = 0
		
		for (r, _) in self._adj:
				r.receber_novo_grupo_multicast(novo_id, origem)

		print "%d: Recebi novo grupo multicast. Grupos conhecidos: %s" \
		% (self._ident, self._mc_conhecidos)
		
		if self._mc_metodo == 'shared' and self._ident == self._mc_rendezvous:
			print "%d: Sou o centro da árvore. Vou me conectar à fonte." % (self._ident)
			self._mc_conectados.add(novo_id)
			proximo = self.descobir_proximo_roteador(origem)
			proximo.conectar_fonte_shared(novo_id, self._ident)
		
	
	def conectar_grupo(self, grupo):
		"""
		Função fachada para o método de conexão a um grupo multicast existente.
		"""
		self._conectar_grupo(grupo, self._ident)
	
	def _conectar_grupo(self, grupo, origem):
		"""
		Contém a lógica para conectar o roteador ao grupo multicast grupo a 
		pedido do roteador origem. origem pode ser o próprio roteador, neste caso
		incrementa a quantidade de receptores para o grupo dado. Verifica se o
		roteador já está na árvore multicast e caso não esteja propaga o pedido
		até que se atinja a fonte dos dados.
		"""
		if grupo not in self._mc_conhecidos:
			print "%d: Grupo %d desconhecido" % (self._ident, grupo)
			return
		
		if origem == self._ident:
			self._mc_receptores[grupo] += 1
			print "%d: Incrementando quantidade de receptores para %d" \
			% (self._ident, self._mc_receptores[grupo])
		
		centro = self._mc_rendezvous
		if self._mc_metodo == 'source':
			centro = self._mc_conhecidos[grupo]
		
		#Ja faz parte da arvore multicast
		if grupo in self._mc_conectados:
			if origem in self._mc_encaminhar[grupo]:
				return
			
			if origem != self._ident:
				self._mc_encaminhar[grupo].add(origem)
				print "%d: Atualizando interessados no grupo %d: %s" \
				% (self._ident, grupo, self._mc_encaminhar[grupo])

			return
		
		self._mc_conectados.add(grupo)
		
		#O roteador nao faz parte da arvore. Propagamos a conexao ate o centro
		if origem != self._ident:
			self._mc_encaminhar[grupo].add(origem)
	
		print "%d: Entrei na arvore %d. Interessado: %d" \
		% (self._ident, grupo, origem)
		
		proximo = self.descobir_proximo_roteador(centro)
		
		proximo._conectar_grupo(grupo, self._ident)

	
	def conectar_fonte_shared(self, grupo, origem):
		"""
		Função executada pelos roteadores no caminho do centro da árvore compartilhada
		até a fonte dos dados propriamente dita. Todos estes roteadores passam a
		integrar a árvore multicast mesmo que não haja receptores ainda.
		"""
		if grupo not in self._mc_conhecidos:
			print "%d: Grupo %d desconhecido" % (self._ident, grupo)
			return
		
		self._mc_conectados.add(grupo)
		self._mc_encaminhar[grupo].add(origem)
		fonte = self._mc_conhecidos[grupo]
		
		print "%d: Entrei na arvore %d. Interessado: %d" \
		% (self._ident, grupo, origem)

		if self._ident != fonte:
			proximo = self.descobir_proximo_roteador(fonte)
			proximo.conectar_fonte_shared(grupo, self._ident)
		
	def desconectar_grupo(self, grupo):
		"""
		Função de fachada para a desconexão de um receptor de uma transmissão
		multicast no roteador.
		"""
		self._desconectar_grupo(grupo, self._ident)
		
	def _desconectar_grupo(self, grupo, origem):
		"""
		Caso a função receba origem == self._ident, representa que um receptor
		da transmissão multicast deseja desconectar-se, caso contrário indica
		que um roteador deseja não receber mais a transmissão multicast do grupo
		grupo. Verifica se ainda há receptores conectados ou roteadores que 
		dependem deste para receber a transmissão. Caso não haja mais nenhum destes
		propaga a desconexão para o próximo nó na árvore.
		"""
		if grupo not in self._mc_conhecidos:
			print "%d: Grupo %d desconhecido" % (self._ident, grupo)
			return

		if grupo not in self._mc_conectados:
			print "%d: Não estou conectado ao grupo %d" % (self._ident, grupo)
			return

		if self._ident == self._mc_rendezvous and len(self._mc_encaminhar[grupo]) == 0\
		and self._mc_receptores[grupo] == 0:
			print "%d: Estou aguardando a primeira conexão antes de permitir que o grupo seja removido."\
			% (self._ident)
			return
		
		if origem == self._ident and self._mc_receptores[grupo] > 0:
			self._mc_receptores[grupo] -= 1
			print "%d: Decrementando quantidade de receptores para %d" \
			% (self._ident, self._mc_receptores[grupo])
		
		if origem in self._mc_encaminhar[grupo]:
			self._mc_encaminhar[grupo].remove(origem)
			print "%d: %d não está mais interessado em %d. Ainda interessados: %s"\
			% (self._ident, origem, grupo, self._mc_encaminhar[grupo])
		
		if len(self._mc_encaminhar[grupo]) > 0 or self._mc_receptores[grupo] > 0:
			print "%d: Não desconectei. Ainda há interessados." % (self._ident)
			return

		print "%d: Sem receptores ou roteadores a repassar pacotes do grupo %d. Desconectando."\
		% (self._ident, grupo)
		
		self._mc_conectados.remove(grupo)
		
		centro = self._mc_conhecidos[grupo]
		if self._mc_metodo == 'shared':
			centro = self._mc_rendezvous
		
		if self._ident == centro:
			print "%d: Iniciando broadcast informando da remoção do grupo %d"\
			% (self._ident, grupo)
			self._remover_grupo(grupo)
			return
		
		proximo = self.descobir_proximo_roteador(centro)
		proximo._desconectar_grupo(grupo, self._ident)

	
	def _atualizar_arvore_shared(self):
		"""
		Função responsável por determinar o caminho a ser seguido deste roteador
		para o centro da árvore multicast compartilhada. Caso a melhor rota tenha
		sido alterada, notifica caso necessário o antigo roteador pai na árvore,
		que deseja desafiliar-se e requisita ao novo roteador na rota ótima 
		conexão na árvore.
		"""
		if self._mc_rendezvous not in self._vetor_dist_a:
			return
		
		proximo = self.descobir_proximo_roteador(self._mc_rendezvous)
		
		if self._mc_rendezvous == self._ident:
			return
		
		if self._mc_pai_shared != proximo.obter_id():
			if self._mc_pai_shared != None:
				[pai] = [r for (r, _) in self._adj if r.obter_id() == self._mc_pai_shared]
				print "%d: Descadastrando %d como meu pai na árvore compartilhada"\
				% (self._ident, self._mc_pai_shared)
				pai.descadastrar_ramo_shared(self._ident)
			
			self._mc_pai_shared = proximo.obter_id()

			print "%d: Atualizando meu pai na árvore compartilhada para %d"\
			% (self._ident, self._mc_pai_shared)
			proximo.cadastrar_ramo_shared(self._ident)
		
	def cadastrar_ramo_shared(self, origem):
		"""
		Função executada pelo novo roteador a se conectar na árvore multicast.
		Cadatra o roteador origem na árvore compartilhada.
		"""
		if -1 not in self._mc_encaminhar:
			self._mc_encaminhar[-1] = set([])
			
		print "%d: Encaminhando pacotes multicast para %d" % (self._ident, origem)
		self._mc_encaminhar[-1].add(origem)
	
	def descadastrar_ramo_shared(self, origem):
		"""
		Função executada pelo roteador na antiga árvore multicast.
		Descadastra o roteador origem na árvore compartilhada.
		"""
		if origem not in self._mc_encaminhar[-1]:
			print "%d: %d não está na minha lista de roteadores a encaminhar pacotes multicast"\
			% (self._ident, origem)
			return
		
		print "%d: Não encaminho mais pacotes multicast para %d" % (self._ident, origem)
		self._mc_encaminhar[-1].remove(origem)
	
	def descobir_proximo_roteador(self, destino):
		"""
		Função auxiliar para determinar o próximo salto do roteador atual para o
		roteador destino. Retorna uma referência a esse próximo salto.
		"""
		(prox, _) = self._vetor_dist_a[destino]
			
		proximo = None
		for (r, _) in self._adj:
			if r.obter_id() == prox:		
				proximo = r
	
		return proximo
	
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
			if self._mc_metodo == 'shared':
				self._atualizar_arvore_shared()
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
		
		if self._mc_metodo == 'shared':
			self._atualizar_arvore_shared()
		
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
	if len(argv) < 3:
		print "Forneca o nome do arquivo a ser utilizado como matriz de \
		adjacencias e o metodo utilizado para a montagem das arvores multicast."
		print "Ex: {0} roteadores.txt shared|source" % (argv[0])
		exit()
	
	matriz = []
	with open(argv[1]) as arq:
		for linha in arq:
			if len(linha.strip()) != 0:
				matriz.append([float(n) for n in linha.split()])

	Simulador(matriz, argv[2])

