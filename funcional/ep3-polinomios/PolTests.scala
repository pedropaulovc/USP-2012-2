package pfc

object PolTests {
  def main(args: Array[String]) {
    require(Pol(3, 7).toString == "3x^7")
    require((Pol(1, 4) - Pol(2, 2) + Pol(1, 0)).toString == "x^4 - 2x^2 + 1")
    require((Pol(0.5, 2) + Pol(-math.Pi, 1) - Pol(math.sqrt(2), 0)).toString == "0.5x^2 - 3.141592653589793x - 1.4142135623730951")
    val pol1 = Pol(3, 2) - Pol(4, 3) + Pol(1, 0) - Pol(2, 1)
    val pol2 = Pol(1, 0) - Pol(2, 1) + Pol(3, 2) - Pol(4, 3)
    require(pol1.toString == "-4x^3 + 3x^2 - 2x + 1")
    require(pol2.toString == "-4x^3 + 3x^2 - 2x + 1")
    require(pol1 == pol2)
    var p = Pol(1, 2) - Pol(2, 1) + Pol(1, 0) 
    var q = Pol(1, 1) - Pol(1)
    require(p.toString == "x^2 - 2x + 1")
    require(q.toString == "x - 1")
    require((p + q).toString == "x^2 - x")
    require((q * q).toString == "x^2 - 2x + 1")
    require((p / q).toString == "(x - 1,0)")
    require((p / q)._2 == Pol(0,0))
    require(Pol(1,1) / (Pol(1,2) + Pol(2,1) + Pol(3,0)) == (Pol(0), Pol(1,1)))
    require(p == q*q)
    require(p == p + Pol(0))
    require(p != p + q)
    require(+p == p)
    require(-q == Pol(0) - q)
    require((-q).toString == "-x + 1")
    q = -q
    require(q.toString == "-x + 1")
    require((p + 100).toString == "x^2 - 2x + 101")
    require((q - 41.7).toString == "-x - 40.7")
    require((p * 5.3).toString == "5.3x^2 - 10.6x + 5.3")
    require(Pol(7, 100) + Pol(42) == Pol(7, 100) + 42)
    require((p / 2).toString == "0.5x^2 - x + 0.5")
    require((7 + p).toString == "x^2 - 2x + 8")
    require((100 - p).toString == "-x^2 + 2x + 99")
    require((5 * p).toString == "5x^2 - 10x + 5")
    require((10 / p).toString == "(0,10)")
    require(p.degree == 2)
    require((p^2).toString == "x^4 - 4x^3 + 6x^2 - 4x + 1")
    require((p^3).toString == "x^6 - 6x^5 + 15x^4 - 20x^3 + 15x^2 - 6x + 1")
    require(((p^4) + (q^2)).toString == "x^8 - 8x^7 + 28x^6 - 56x^5 + 70x^4 - 56x^3 + 29x^2 - 10x + 2")
    require(((p^4) + (q^2)).degree == 8)
    require(p.deriv.toString == "2x - 2")
    require((p!).toString == "2x - 2")
    require(((p^3)!).toString == "6x^5 - 30x^4 + 60x^3 - 60x^2 + 30x - 6")
    require((p^3).deriv.deriv.toString == "30x^4 - 120x^3 + 180x^2 - 120x + 30")
    require((((p^3)!)!).toString == "30x^4 - 120x^3 + 180x^2 - 120x + 30")
    require(p.toString == "x^2 - 2x + 1")
    require(p(5) == 16.0)
    require((p^3)(4) == 729.0)
    require(Pol(1, 10)(2) == 1024.0)
    require(p(q).toString == "x^2")
    require(q(p).toString == "-x^2 + 2x")
    require(p(Pol(1, 2)).toString == "x^4 - 2x^2 + 1")
    require(p(Pol(1, 4)).toString == "x^8 - 2x^4 + 1")
    require(p(p).toString == "x^4 - 4x^3 + 4x^2")
    require((p(p))(p).toString == "x^8 - 8x^7 + 24x^6 - 32x^5 + 14x^4 + 8x^3 - 8x^2 + 1")
    require((p(p))(5)  == 225.0)
    require(p(p(p)).toString == "x^8 - 8x^7 + 24x^6 - 32x^5 + 14x^4 + 8x^3 - 8x^2 + 1")
    require(p(p(5)) == 225.0)
    require(p(p(p))(5) == 50176.0)

    var a = Pol(0)
  
    p = Pol(0)
    p = Pol(4, 5) + Pol(-1, 3) + Pol(1, 2)
    require (p.toString == "4x^5 - x^3 + x^2")
  
    q = Pol(2, 2) + Pol(3, 4) + Pol(1, 3) + Pol(-1, 0)
    require (q.toString == "3x^4 + x^3 + 2x^2 - 1")
  
    var r = p + q
    require(r.toString == "4x^5 + 3x^4 + 3x^2 - 1")
  
    var s = p * q
    require(s.toString == "12x^9 + 4x^8 + 5x^7 + 2x^6 - 5x^5 + 2x^4 + x^3 - x^2")
  
  
    var w = s / p 
    require(w._1.toString == "3x^4 + x^3 + 2x^2 - 1")
  
    var y = -s
    require(y.toString == "-12x^9 - 4x^8 - 5x^7 - 2x^6 + 5x^5 - 2x^4 - x^3 + x^2")
  
    var t = s - r 
    require(t.toString == "12x^9 + 4x^8 + 5x^7 + 2x^6 - 9x^5 - x^4 + x^3 - 4x^2 + 1")
  
    p = p * p 
    require(p .toString == "16x^10 - 8x^8 + 8x^7 + x^6 - 2x^5 + x^4")
  
    p = p * p 
    require(p.toString == "256x^20 - 256x^18 + 256x^17 + 96x^16 - 192x^15 + 80x^14 + 48x^13 - 47x^12 + 12x^11 + 6x^10 - 4x^9 + x^8")
  
    a = Pol(4, 5) + Pol(-1, 3) + Pol(1, 2)
    require(a.toString == "4x^5 - x^3 + x^2")
  
    var b = Pol(1, 1) + Pol(1)
    require(b.toString == "x + 1") 
  
    var u = ( a + q ) * b 
    require(u.toString == "4x^6 + 7x^5 + 3x^4 + 3x^3 + 3x^2 - x - 1") 
  
    var v = ( a + q ) / b 
    require(v._1.toString == "4x^4 - x^3 + x^2 + 2x - 2") 
  
    w = ( a + q ) / b 
    require(w._2.toString == "1") 
  
    b = Pol(-1, 2) + Pol(9, 0)
    require(b.toString == "-x^2 + 9") 
  
    w = (a + q) / b 
    require(w._2.toString == "324x + 269") 
  
    u = u - w._2
    require(u.toString == "4x^6 + 7x^5 + 3x^4 + 3x^3 + 3x^2 - 325x - 270")
    println("OK")
  }
}

