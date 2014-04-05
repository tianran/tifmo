#!/bin/sh
SCALA=scala
exec $SCALA "$0" "$@"
!#

if (args.length != 2) {
	println("USAGE: AccuracyRTE.sh file threshold")
	sys.exit()
}

import java.io.BufferedReader
import java.io.FileReader

val file = args(0)
val threshold = args(1).toDouble

var total = 0
var correct = 0
var y_gold = 0
var y_sys = 0
var y_correct = 0
val br = new BufferedReader(new FileReader(file))
def loop() {
	val s = br.readLine()
	if (s != null) {
		val sp = s.split(",")
		val gold = sp(2)
		val sys = if (sp(3).toDouble > threshold) "Y" else "N"
		total += 1
		if (gold == sys) correct += 1
		if (gold == "Y") y_gold += 1
		if (sys == "Y") y_sys += 1
		if (gold == "Y" && sys == "Y") y_correct += 1
		loop()
	}
}
loop()
br.close()

println("total: " + total)
println("correct: " + correct)
println("accuracy: " + (correct.toDouble / total))
println("precision: " + (y_correct.toDouble / y_sys))
println("recall: " + (y_correct.toDouble / y_gold))
