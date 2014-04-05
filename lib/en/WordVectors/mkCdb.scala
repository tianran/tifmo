
import java.io.BufferedReader
import java.io.FileReader
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

import com.strangegizmo.cdb.CdbMake

val input_file = args(0)
val output_file = args(1)
val dim = args(2).toInt

val br = new BufferedReader(new FileReader(input_file))

val cdbmk = new CdbMake
cdbmk.start(output_file)
def loop() {
	val s = br.readLine()
	if (s != null) {
		val sp = s.split(" ")
		val k = sp(0)
		val v = sp.tail.map(_.toFloat)
		assert(v.length == dim)
		
		val baos = new ByteArrayOutputStream
		val oos = new ObjectOutputStream(baos)
		oos.writeObject(v)
		oos.close()
		cdbmk.add(k.getBytes("UTF-8"), baos.toByteArray())
		
		loop()
	}
}
loop()

br.close()
cdbmk.finish()

