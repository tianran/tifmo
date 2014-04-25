package mylib

import com.strangegizmo.cdb.Cdb

import java.io.ByteArrayInputStream
import java.io.ObjectInputStream

package res.en {
	
	object EnTurian10 {
		
		private[this] val dir = EnTurian10.getClass.getClassLoader.getResource("en/WordVectors/Turian10.cdb").getFile
		
		private[this] val cdb = new Cdb(dir)
		
		def lookup(x: String) = {
			val tmp = cdb.find(x.getBytes("UTF-8"))
			if (tmp == null) null else (new ObjectInputStream(new ByteArrayInputStream(tmp))).readObject().asInstanceOf[Array[Float]]
		}
		
	}
	
}
