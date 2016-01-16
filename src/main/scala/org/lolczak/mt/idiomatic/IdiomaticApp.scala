package org.lolczak.mt.idiomatic

import org.lolczak.lambda.Lit

object IdiomaticApp extends App {

    val exp = Lit(12)

    val result = runEval(Map.empty, 0, eval(exp))

    println(result.unsafePerformIO())

}
