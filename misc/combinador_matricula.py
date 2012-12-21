import itertools
from datetime import datetime
from math import ceil


class Turma:
	def __init__(self, codigo, turma, horarios, professor):
		self.codigo = codigo
		self.turma = turma
		self.horarios = map(lambda (d,i,f): (d, datetime.strptime(i, "%H:%M"),
			datetime.strptime(f, "%H:%M")), horarios)
		self.professor = professor
	
	def __repr__(self):
		return "(%s-%d, %s)" % (self.codigo, self.turma, self.professor)

	def horario_conflita(self, (d1, i1, f1), (d2, i2, f2)):
		return d1 == d2 and ((i2 >= i1 and i2 < f1) or (i1 >= i2 and i1 < f2))
	
	def conflita(self, turma):
		for t1 in self.horarios:
			for t2 in turma.horarios:
				if self.horario_conflita(t1,t2):
					return True
		return False
		
		
class Combinador:
	def __init__(self):
		self.materias = {}
	
	def adicionar_turma(self, turma):
		if turma.codigo not in self.materias:
			self.materias[turma.codigo] = set([])
		self.materias[turma.codigo].add(turma)
	
	def gerar_combinacoes(self):
		cartesiano = itertools.product(*self.materias.itervalues())
		nao_tem_conflito = lambda x: not self.tem_conflito(x)		
		sem_conflito = itertools.ifilter(nao_tem_conflito, cartesiano)
		return list(sem_conflito)
		
	def tem_conflito(self, plano):
		for m1, m2 in itertools.combinations(plano, 2):
			if m1.conflita(m2):
				return True
		return False
			
	
turmas = [
	Turma("LCB0311", 11, [(3,"8:00","9:50"), (6,"10:00","11:50")], "Paulo Roberto"),
	Turma("LCB0311", 12, [(3,"8:00","9:50"), (6,"08:00","09:50")], "Paulo Roberto"),
	Turma("LCB0311", 14, [(3,"8:00","9:50"), (6,"14:00","15:50")], "Paulo Roberto"),
	Turma("LCB0311", 17, [(2,"8:00","9:50"), (5,"16:00","17:50")], "Paulo Roberto e Ricardo Alfredo"),
	Turma("LCB0311", 18, [(2,"8:00","9:50"), (5,"08:00","09:50")], "Paulo Roberto e Ricardo Ferraz"),
	
	Turma("LEB0306", 11, [(3,"14:00","15:50"), (5,"16:00","17:50")], "??"),
	Turma("LEB0306", 12, [(3,"16:00","17:50"), (5,"10:00","11:50")], "??"),
	
	Turma("LEB0340", 13, [(2,"07:00","09:50"), (4,"13:00","15:50")], "Rubens Angulo"),
	Turma("LEB0340", 14, [(2,"07:00","09:50"), (6,"07:00","09:50")], "Rubens Angulo"),
	Turma("LEB0340", 17, [(2,"13:00","15:50"), (3,"10:00","12:50")], "Peterson Ricardo"),
	Turma("LEB0340", 18, [(2,"13:00","15:50"), (4,"10:00","12:50")], "Peterson Ricardo"),
	
	Turma("LFN0321", 13, [(4,"10:00","11:50"), (6,"14:00","15:50")], "Luis Eduardo e Francisco Andre"),
	Turma("LFN0321", 16, [(3,"08:00","09:50"), (5,"16:00","17:50")], "Luis Eduardo e Armando Bergamin"),
	Turma("LFN0321", 19, [(3,"08:00","09:50"), (4,"14:00","15:50")], "Luis Eduardo e Nelson Sidnei"),
	Turma("LFN0321", 20, [(3,"08:00","09:50"), (4,"16:00","17:50")], "Luis Eduardo e Nelson Sidnei"),
	
	Turma("LGN0215", 17, [(5,"10:00","11:50"), (6,"08:00","09:50")], "Elizabeth Ann e Giancarlo Conde"),
	Turma("LGN0215", 18, [(5,"10:00","11:50"), (6,"08:00","09:50")], "Elizabeth Ann"),
	
	Turma("LSO0300", 12, [(4,"08:00","09:50"), (3,"16:00","17:50")], "Carlos Eduardo e Jorge de Castro"),
	Turma("LSO0300", 13, [(2,"10:00","11:50"), (2,"16:00","17:50")], "Carlos Eduardo e Jorge de Castro"),
	Turma("LSO0300", 17, [(3,"14:00","15:50"), (4,"08:00","09:50")], "Carlos Eduardo"),
	
	Turma("LSO0310", 03, [(6,"08:00","09:50")], "Jairo Antonio"),
	Turma("LSO0310", 05, [(6,"14:00","15:50")], "Jairo Antonio"),
	Turma("LSO0310", 06, [(6,"16:00","17:50")], "Jairo Antonio"),
	Turma("LSO0310", 07, [(5,"08:00","09:50")], "Jairo Antonio"),

	Turma("LZT0430", 11, [(4,"14:00","15:50"), (4,"16:00","17:50")], "??"),
	
]

def gerar_tabela(combinacao):
	cal = list(list("            " for _ in range(5)) for _ in range(11)) #Acesso: cal[hora - 7][dia_semana - 2]

	for turma in combinacao:
		for dia, ini, fim in turma.horarios:
			horas_duracao = int(ceil((fim - ini).seconds/3600))
			for h in range(ini.hour, ini.hour + horas_duracao + 1):
				cal[h - 7][dia - 2] = " %s-%02d " % (turma.codigo, turma.turma)
	
	
	linha =       "+---------+------------+------------+------------+------------+------------+\n"
	
	calendario =  linha
	calendario += "|   Hora  |    SEG     |    TER     |    QUA     |    QUI     |    SEX     |\n"
	calendario += linha
	for h, c in enumerate(cal):
		calendario += "|  %2d:00  |" % (h + 7)
		calendario += "|".join(c) 
		calendario += "|\n"
		calendario += linha
	
	return calendario
		

comb = Combinador()

for t in turmas:
	comb.adicionar_turma(t)

for c in comb.gerar_combinacoes():
	print gerar_tabela(c)
	print "============================================================================\n"

