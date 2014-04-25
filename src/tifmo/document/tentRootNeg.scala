package tifmo


package document {
	
	object tentRootNeg extends (Document => Unit) {
		
		def apply(doc: Document) {
			
			for (rn <- doc.allRootNodes) {
				if (!rn.sign) {
					rn.sign = true
					rn.rootNeg = true
				}
			}
		}
		
	}
}
