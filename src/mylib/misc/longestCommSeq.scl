package mylib


package misc {
	
	object longestCommSeq {
		
		def length[T](a: Seq[T], b: Seq[T]) = {
			val tab = Array.ofDim[Int](a.length + 1, b.length + 1)
			for (i <- 0 until a.length; j <- 0 until b.length) {
				if (a(i) == b(j)) {
					tab(i + 1)(j + 1) = tab(i)(j) + 1
				} else {
					tab(i + 1)(j + 1) = tab(i)(j + 1) max tab(i + 1)(j)
				}
			}
			tab(a.length)(b.length)
		}
		
		def rateAve[T](a: Seq[T], b: Seq[T]) = {
			2.0 * length[T](a, b) / (a.length + b.length)
		}
		
		def rateMin[T](a: Seq[T], b: Seq[T]) = {
			length[T](a, b) / (a.length min b.length)
		}
		
		def rateMax[T](a: Seq[T], b: Seq[T]) = {
			length[T](a, b) / (a.length max b.length)
		}
		
	}
	
}

