package pfc

private case class Term(coef: Double, exp: Int) {
	require(coef != 0 && exp >= 0)
		
	
	override def toString = {
		
		var c: String = "" // coeficiente
		var x: String = "" // parte do x
		var e: String = "" // expoente
		
		if (coef == coef.toInt) {
			if (coef != 1 && coef != -1)
				c = (abs(coef).toInt).toString
			else //coef == 1 || coef == -1
				if (exp == 0)
					c = (abs(coef).toInt).toString
		}	
		else c = (abs(coef)).toString
		
		if (exp > 1) {
			x = "x^"
			e = exp.toString
		}
		else if (exp == 1)
			x = "x"
		
		c + x + e
	}
	
	def sinal: String =
		if (coef < 0) " - "
		else " + "
		
	def abs(x: Double): Double =
		if (x > 0) x
		else -x

}

class Pol private (private val terms: List[Term]) {

  // construtor auxiliar (n.b.: tanto o construtor primario como o auxiliar sao privados)
	private def this(coef: Double, exp: Int) = this (List(Term(coef, exp)))

  // aritmetica de polinomios
	def + (that: Pol): Pol = Pol (Pol.add (this.terms, that.terms))
	def - (that: Pol): Pol = this + -that  
	def * (that: Pol): Pol = (that.terms map (x => this * x)).foldLeft (Pol(List()))((a,b) => b + a)
	def / (that: Pol): Tuple2[Pol, Pol] = {
		if (this.terms.isEmpty)
			Pol(0)
		else {
			if (this.degree < that.degree) new Tuple2(Pol(0), this)
			val q = Pol (this.terms.head.coef / that.terms.head.coef, this.degree - that.degree)
			val r = this - (that * q)
            val t = r / y
            new Tuple2 (q + t._1, t._2)			
		}
	}

  // operadores unarios
	def unary_+ : Pol = Pol (this.terms)
	def unary_- : Pol = *(-1)

  // aritmetica mista (o operando 1 e' um polinomio, o operando 2 e' um numero)
	def + (d: Double): Pol = this + Pol (d,0)
	def - (d: Double): Pol = this - Pol (d,0)
	def * (d: Double): Pol = Pol (terms map (x => Term(x.coef * d, x.exp)))
	def / (d: Double): Pol = this * (1/d)

  // grau, potenciacao e derivacao (E INTEGRACAO! HOHOHOHO)
	def degree: Int = terms.head.exp
	def ^(n: Int): Pol = {
		var Acc: Pol = Pol (1,0) // unidade multiplicativa
		for (i <- 0 until n)
			Acc = Acc * this
		Acc
	}
	def deriv: Pol = this!
	def ! : Pol = Pol ((terms filter (t => t.exp > 0)) map (x => Term(x.coef * x.exp, x.exp -1)))
		
	def integ: Pol = ~this	
	def unary_~ : Pol = Pol (terms map (x => Term(x.coef / (x.exp+1), x.exp + 1)))
	def integ (a: Double, b: Double): Double = (~this).apply(b) - (~this).apply(a)
	


  // calcula o valor do polinomio alvo para um dado valor de x
	def apply(x: Double): Double = (terms map (i => i.coef * Math.pow (x,i.exp))).foldLeft(0.0)((a,b) => a+b)
	
  // composicao do polinomio alvo com outro polinomio
    def apply(that: Pol): Pol = (terms map (x => x.coef * (that ^ x.exp))) foldLeft (Pol(0))((a,b) => a + b)

  // sobrescrita de metodos da classe Any
  // override def equals(other: Any): Boolean
  // override def hashCode: Int
	override def toString = {
		if (terms.isEmpty)
			"0"
		else {
			var s = new String()
			if (terms.head.sinal == " - ")
				s += "-"
			s += terms.head.toString
			for (i <- terms.tail) {
				s += i.sinal
				s += i.toString
			}
			s
		}
	}

  // metodo auxiliar que multiplica o polinomio alvo por um termo simples
	private def * (term: Term): Pol = Pol (terms map (x => Term(x.coef * term.coef, x.exp + term.exp)))

}

object Pol {

  // conversao implicita de Double em Pol
	implicit def doubleToPol(d: Double): Pol = Pol (d,0)

  // metodos de fabrica acessiveis para os clientes
	def apply(coef: Double, exp: Int): Pol = if (coef == 0) new Pol (List()) else new Pol (coef, exp)
	def apply(coef: Double): Pol = new Pol (coef, 0)

  // metodo de fabrica interno (serve apenas para evitar o uso de new)
	private def apply(terms: List[Term]): Pol = new Pol (terms)

  // metodo auxiliar para as operacoes de adicao e subtracao de polinomios
	private def add(L1: List[Term], L2: List[Term]): List[Term] = soma (L1, L2, Nil)

	private def soma (L1: List[Term], L2: List[Term], S: List[Term]): List[Term] = {
		if (L1.isEmpty && L2.isEmpty)
			S	
		else
			if (L1.isEmpty) 
				S++L2
			else
				if (L2.isEmpty)
					S++L1
				else { //caso normal, 2 listas cheias
					val H1 = L1.head
					val H2 = L2.head
					if (H1.exp > H2.exp)
						soma (L1.tail, L2, S++List(H1))
					else //H1.exp <= H2.exp
						if (H1.exp < H2.exp)
							soma (L1, L2.tail, S++List(H2))
						else  //H1.exp == H2.exp
							if (H1.coef + H2.coef == 0) // soma da zero
								soma (L1.tail, L2.tail, S)
							else { //soma nao da zero
								val TermoNovo = new Term (H1.coef + H2.coef, H1.exp)
								soma (L1.tail, L2.tail, S++List(TermoNovo))
							}
				}	
	}

}
