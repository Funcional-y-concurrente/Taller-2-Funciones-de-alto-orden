import Derivacion.*

def verificar(nombre: String, obtenido: Double, esperado: Double): Unit =
  assert(math.abs(obtenido - esperado) < 1e-4, s"$nombre: $obtenido != $esperado")

// ========== Funciones auxiliares ==========
val identidad: Double => Double = x => x
val constante: Double => Double = _ => 3
val cuadrado: Double => Double = x => x * x
val cubo: Double => Double = x => x * x * x
val seno: Double => Double = x => math.sin(x)
val coseno: Double => Double = x => math.cos(x)
val exponencial: Double => Double = x => math.exp(x)
val raiz: Double => Double = x => math.sqrt(x)
val logaritmo: Double => Double = x => math.log(x)

// ========== PRUEBAS PARA derivada (5 casos) ==========
verificar("derivada_cubo_x2", derivada(cubo)(2), 12)              // f'(x)=3x² → 3*4=12
verificar("derivada_seno_0", derivada(seno)(0), 1)                 // f'(x)=cos(x) → cos(0)=1
verificar("derivada_exp_0", derivada(exponencial)(0), 1)           // f'(x)=e^x → e^0=1
verificar("derivada_raiz_4", derivada(raiz)(4), 0.25)              // f'(x)=1/(2√x) → 1/(2*2)=0.25
verificar("derivada_cuadrado_neg", derivada(cuadrado)(-3), -6)     // f'(x)=2x → 2*(-3)=-6

// ========== PRUEBAS PARA derivadaSuma (3 casos) ==========
val suma1 = derivadaSuma(cubo, seno)
verificar("suma_cubo_seno_0", suma1(0), 1)                         // 3*0² + cos(0) = 0+1=1

val suma2 = derivadaSuma(exponencial, identidad)
verificar("suma_exp_id_0", suma2(0), 2)                            // e^0 + 1 = 1+1=2

val suma3 = derivadaSuma(cuadrado, constante)
verificar("suma_cuad_cte_5", suma3(5), 10)                         // 2*5 + 0 = 10

// ========== PRUEBAS PARA derivadaResta (3 casos) ==========
val resta1 = derivadaResta(cubo, cuadrado)
verificar("resta_cubo_cuad_1", resta1(1), 1)                       // 3*1² - 2*1 = 3-2=1

val resta2 = derivadaResta(seno, coseno)
verificar("resta_sen_cos_0", resta2(0), 1)                         // cos(0) - (-sin(0)) = 1-0=1

val resta3 = derivadaResta(exponencial, constante)
verificar("resta_exp_cte_0", resta3(0), 1)                         // e^0 - 0 = 1

// ========== PRUEBAS PARA derivadaMult (2 casos) ==========
val mult1 = derivadaMult(identidad, cuadrado)  // x * x² = x³, derivada = 3x²
verificar("mult_id_cuad_2", mult1(2), 12)                          // 3*(2²) = 12

val mult2 = derivadaMult(constante, exponencial)  // 3*e^x, derivada = 3*e^x
verificar("mult_cte_exp_0", mult2(0), 3)                           // 3*e^0 = 3

// ========== PRUEBAS PARA derivadaDiv (2 casos) ==========
val div1 = derivadaDiv(cubo, cuadrado)  // x³/x² = x, derivada = 1
verificar("div_cubo_cuad_5", div1(5), 1)                           // Derivada de x es 1

val div2 = derivadaDiv(cuadrado, identidad)  // x²/x = x, derivada = 1
verificar("div_cuad_id_10", div2(10), 1)                           // Derivada de x es 1

// ========== CASO ESPECIAL: División por cero ==========
try {
  val divCero = derivadaDiv(constante, identidad)  // 3/x
  divCero(0)
  println("ERROR: Debería lanzar excepción en división por cero")
} catch {
  case _: ArithmeticException => println("✓ Correcto: lanzó excepción en división por cero")
}

println("\n✅ Todas las pruebas pasaron correctamente")