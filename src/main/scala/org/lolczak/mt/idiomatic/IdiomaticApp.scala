package org.lolczak.mt.idiomatic

import org.lolczak.lambda.{App => Apply,_}

object IdiomaticApp extends App {

    val exp = Add(Lit(12), Apply(Abs("x", Var("x")), Add(Lit(4), Lit(2))))

    val result = runEval(Map.empty, 0, eval(exp))

    println(result.unsafePerformIO())

}
