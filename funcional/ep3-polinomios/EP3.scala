package pfc

import java.text.DecimalFormat
import java.text.NumberFormat

private case class Term(coef: Double, exp: Int) {
  require(coef != 0 && exp >= 0)

  override def toString = {
    val nf: NumberFormat = new DecimalFormat("0.################");
    val c: String = 
    	if(math.abs(coef) == 1 && exp != 0)
    		""
	    else
		    nf.format(math.abs(coef)) // coeficiente

    if (exp > 1)
      c + "x^" + exp.toString
    else if (exp == 1)
      c + "x"
    else
      c
  }

  def sinal: String =
    if (coef < 0)
      " - "
    else
      " + "

  override def equals(other: Any) = {
    other match {
      case that: pfc.Term => coef == that.coef && exp == that.exp
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + coef.hashCode) + exp.hashCode
  }
}

class Pol private (private val terms: List[Term]) {
  // construtor auxiliar (n.b.: tanto o construtor primario como o auxiliar sao privados)
  private def this(coef: Double, exp: Int) = this(List(Term(coef, exp)))

  // aritmetica de polinomios
  def +(that: Pol): Pol = Pol(Pol.add(this.terms, that.terms))
  def -(that: Pol): Pol = this + -that
  def *(that: Pol): Pol = terms.foldLeft(Pol(0))((acc, t) => (that * t) + acc)
  def /(that: Pol): Tuple2[Pol, Pol] = {
    require(!that.terms.isEmpty)

    if (this.degree < that.degree)
      (Pol(0), this)
    else {
      val q = Pol(this.terms.head.coef / that.terms.head.coef, this.degree - that.degree)
      val r = this - (that * q)
      val t = r / that
      (q + t._1, t._2)
    }
  }

  // operadores unarios
  def unary_+ : Pol = Pol(this.terms)
  def unary_- : Pol = *(-1)

  // aritmetica mista (o operando 1 e' um polinomio, o operando 2 e' um numero)
  def +(d: Double): Pol = this + Pol(d, 0)
  def -(d: Double): Pol = this - Pol(d, 0)
  def *(d: Double): Pol = Pol(terms map (x => Term(x.coef * d, x.exp)))
  def /(d: Double): Pol = {
    require(d != 0)
    this * (1 / d)
  }

  // grau, potenciacao e derivacao e integração
  def degree: Int = if (terms.isEmpty) 0 else terms.head.exp
  def ^(n: Int): Pol = {
    require(n >= 0)
    pot(n, Pol(1))
  }
  
  private def pot(n: Int, acc: Pol): Pol = if (n == 0) acc else pot(n - 1, acc * this)
  
  def deriv: Pol = Pol((terms filter (t => t.exp > 0)) map (x => Term(x.coef * x.exp, x.exp - 1)))
  def ! : Pol = deriv

  def integ: Pol = Pol(terms map (x => Term(x.coef / (x.exp + 1), x.exp + 1)))
  def unary_~ : Pol = integ
  def integ(a: Double, b: Double): Double = (~this).apply(b) - (~this).apply(a)

  // calcula o valor do polinomio alvo para um dado valor de x
  def apply(x: Double): Double = terms.foldLeft(0.0)((acc, t) => t.coef * math.pow(x, t.exp) + acc)
  // composicao do polinomio alvo com outro polinomio
  def apply(that: Pol): Pol = terms.foldLeft(Pol(0))((acc, t) => (that ^ t.exp) * t.coef + acc)

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
  private def *(term: Term): Pol = Pol(terms map (x => Term(x.coef * term.coef, x.exp + term.exp)))

  override def equals(other: Any) = {
    other match {
      case that: pfc.Pol => terms == that.terms
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + terms.hashCode
  }

}

object Pol {

  // conversao implicita de Double em Pol
  implicit def doubleToPol(d: Double): Pol = Pol(d, 0)

  // metodos de fabrica acessiveis para os clientes
  def apply(coef: Double, exp: Int): Pol = if (coef == 0) Pol(List()) else new Pol(coef, exp)
  def apply(coef: Double): Pol = Pol(coef, 0)

  // metodo de fabrica interno (serve apenas para evitar o uso de new)
  private def apply(terms: List[Term]): Pol = new Pol(terms)

  // metodo auxiliar para as operacoes de adicao e subtracao de polinomios
  private def add(l1: List[Term], l2: List[Term]): List[Term] = addR(l1, l2, Nil).reverse

  private def addR(l1: List[Term], l2: List[Term], s: List[Term]): List[Term] =
    (l1, l2) match { 
      case (Nil, _) => l2.reverse ::: s
      case (_, Nil) => l1.reverse ::: s
      case (h1 :: l1a, h2 :: l2a) => 
        if (h1.exp > h2.exp)
          addR(l1a, l2, h1 :: s)
        else if (h1.exp < h2.exp)
          addR(l1, l2a, h2 :: s)
        else if (h1.coef + h2.coef == 0)
          addR(l1a, l2a, s)
        else
          addR(l1a, l2a, Term(h1.coef + h2.coef, h1.exp) :: s)
    }


}
