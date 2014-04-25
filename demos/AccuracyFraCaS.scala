if (args.length != 4) {
	println("USAGE: AccuracyFraCaS.sh file single/multi fromID toID")
	sys.exit()
}

import java.io.BufferedReader
import java.io.FileReader

val file = args(0)
val sm = args(1)
val fromID = args(2).toInt
val toID = args(3).toInt

var total = 0
var correct = 0
val br = new BufferedReader(new FileReader(file))
def loop() {
	val s = br.readLine()
	if (s != null) {
		val sp = s.split(",")
		val id = sp(0).toInt
		val xsm = sp(1)
		val gold = sp(2)
		if (fromID <= id && id <= toID && xsm == sm && gold != "undef") {
			val sys = sp(3)
			total += 1
			if (gold == sys) correct += 1
		}
		loop()
	}
}
loop()
br.close()

println("total: " + total)
println("correct: " + correct)
println("accuracy: " + (correct.toDouble / total))
