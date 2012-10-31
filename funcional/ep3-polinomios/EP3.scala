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

  def canEqual(other: Any) = {
    other.isInstanceOf[pfc.Term]
  }

  override def equals(other: Any) = {
    other match {
      case that: pfc.Term => that.canEqual(Term.this) && coef == that.coef && exp == that.exp
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
  def *(that: Pol): Pol = (that.terms map (x => this * x)).foldLeft(Pol(List()))((a, b) => b + a)
  def /(that: Pol): Tuple2[Pol, Pol] = {
    require(!that.terms.isEmpty)

    if (this.degree < that.degree)
      new Tuple2(Pol(List()), this)
    else {
      val q = Pol(this.terms.head.coef / that.terms.head.coef, this.degree - that.degree)
      val r = this - (that * q)
      val t = r / that
      new Tuple2(q + t._1, t._2)
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
    var Acc: Pol = Pol(1, 0) // unidade multiplicativa
    for (i <- 0 until n)
      Acc = Acc * this
    Acc
  }
  def deriv: Pol = this!
  def ! : Pol = Pol((terms filter (t => t.exp > 0)) map (x => Term(x.coef * x.exp, x.exp - 1)))

  def integ: Pol = ~this
  def unary_~ : Pol = Pol(terms map (x => Term(x.coef / (x.exp + 1), x.exp + 1)))
  def integ(a: Double, b: Double): Double = (~this).apply(b) - (~this).apply(a)

  // calcula o valor do polinomio alvo para um dado valor de x
  def apply(x: Double): Double = (terms map (i => i.coef * math.pow(x, i.exp))).foldLeft(0.0)((a, b) => a + b)

  // composicao do polinomio alvo com outro polinomio
  def apply(that: Pol): Pol = (terms map (x => (that ^ x.exp) * x.coef)).foldLeft(Pol(0))((a, b) => a + b)

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

  def canEqual(other: Any) = {
    other.isInstanceOf[pfc.Pol]
  }

  override def equals(other: Any) = {
    other match {
      case that: pfc.Pol => that.canEqual(Pol.this) && terms == that.terms
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
  private def add(L1: List[Term], L2: List[Term]): List[Term] = soma(L1, L2, Nil)

  private def soma(L1: List[Term], L2: List[Term], S: List[Term]): List[Term] = {
    if (L2.size < L1.size)
      soma(L2, L1, S)
    else if (L1.isEmpty)
      S ++ L2
    else {
      val H1 = L1.head
      val H2 = L2.head
      
      if (H1.exp > H2.exp)
        soma(L1.tail, L2, S ++ List(H1))
      else if (H1.exp < H2.exp)
        soma(L1, L2.tail, S ++ List(H2))
      else if (H1.coef + H2.coef == 0)
        soma(L1.tail, L2.tail, S)
      else 
        soma(L1.tail, L2.tail, S ++ List(Term(H1.coef + H2.coef, H1.exp)))
    }
  }

}
