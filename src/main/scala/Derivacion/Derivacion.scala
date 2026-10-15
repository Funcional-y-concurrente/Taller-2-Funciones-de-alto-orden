package object Derivacion {
  private val h = 0.1

  def derivada(f: Double => Double): Double => Double = { x =>
    (f(x - 2 * h) - 8 * f(x - h) + 8 * f(x + h) - f(x + 2 * h)) / (12 * h)
  }

  def derivadaSuma(f: Double => Double, g: Double => Double): Double => Double = { x =>
    derivada(f)(x) + derivada(g)(x)
  }

  def derivadaResta(f: Double => Double, g: Double => Double): Double => Double = { x =>
    derivada(f)(x) - derivada(g)(x)
  }

  def derivadaMult(f: Double => Double, g: Double => Double): Double => Double = { x =>
    derivada(f)(x) * g(x) + f(x) * derivada(g)(x)
  }

  def derivadaDiv(f: Double => Double, g: Double => Double): Double => Double = { x =>
    val denominador = g(x)
    if denominador == 0 then
      throw new ArithmeticException("No se puede derivar un cociente con denominador cero")
    (derivada(f)(x) * denominador - f(x) * derivada(g)(x)) / (denominador * denominador)
  }
}