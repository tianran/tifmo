package tifmo


package main.en {
	/**
	 * Cosine similarities of word vectors.
	 */
	abstract class EnSimilarity {
		
		val dim: Int
		
		protected[this] def lookup(x: String): Array[Float] 
		
		val res: EnResources
		
		val wordSimMax: Float
		
		///////////////////////
		
		protected[this] def arrayNorm(x: Array[Float]) = {
			val tmp = x.map(z => z * z).sum
			if (tmp == 0.0) 1.0f else math.sqrt(tmp).toFloat
		}
		protected[this] def arrayDot(x: Array[Float], y: Array[Float]) = {
			(for (i <- 0 until dim) yield (x(i) * y(i))).sum
		}
		protected[this] def arraySum(x: Array[Float], y: Array[Float]) = {
			val ret = new Array[Float](dim)
			for (i <- 0 until dim) ret(i) = x(i) + y(i)
			ret
		}
		
		def similarity(as: Iterable[EnWord], bs: Iterable[EnWord]) = {
			
			val bvecs = bs.toList.map(x => (x, lookup(x.lemma)))
			
			val (bnull, bnormal) = bvecs.partition(_._2 == null)
			if (bnull.forall(x => as.exists(y => res.semrel(y, x._1) || res.hyponym(y, x._1)))) {
				val afil = for (x <- as.toList; vec = lookup(x.lemma); if vec != null) yield ((x, vec))
				
				var dotsum = 0.0f
				for ((a, av) <- afil; (b, bv) <- bnormal) {
					dotsum += arrayDot(av, bv).min(wordSimMax)
				}
				
				val atot = ((new Array[Float](dim)) /: afil.map(_._2))(arraySum(_, _))
				val btot = ((new Array[Float](dim)) /: bnormal.map(_._2))(arraySum(_, _))
				
				val pre = (dotsum / (arrayNorm(atot) * arrayNorm(btot))).max(0.0f)
				
				(pre * bnormal.length + wordSimMax * bnull.length) / bvecs.length
				
			} else {
				0.0f
			}
		}
		
	}
	
}
