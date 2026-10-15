package object Derivacion {
  private val h = 0.1

  // (f') = (f(x - 2h) - 8*f(x - h) + 8*f(x + h) - f(x + 2h)) / (12 * h)
  def derivada(f: Double => Double): Double => Double = { x =>
    (f(x - 2 * h) - 8 * f(x - h) + 8 * f(x + h) - f(x + 2 * h)) / (12 * h)
  }

  // (f + g)' = f' + g'
  def derivadaSuma(f: Double => Double, g: Double => Double): Double => Double = { x =>
    derivada(f)(x) + derivada(g)(x)
  }

  // (f - g)' = f' - g'
  def derivadaResta(f: Double => Double, g: Double => Double): Double => Double = { x =>
    derivada(f)(x) - derivada(g)(x)
  }

  // (f * g)' = f' * g + f * g'
  def derivadaMult(f: Double => Double, g: Double => Double): Double => Double = { x =>
    derivada(f)(x) * g(x) + f(x) * derivada(g)(x)
  }

  // (f / g)' = (f' * g - f * g') / (g^2)
  def derivadaDiv(f: Double => Double, g: Double => Double): Double => Double = {
    val df = derivada(f)
    val dg = derivada(g)
    (x: Double) => {
      val denominador = g(x)
      if (denominador == 0) throw new ArithmeticException("No se puede derivar un cociente con denominador cero")
      (df(x) * denominador - f(x) * dg(x)) / (denominador * denominador)
    }
  }
}