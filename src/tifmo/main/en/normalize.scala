package tifmo

import mylib.res.en.EnWordNet

import ac.biu.nlp.normalization.BiuNormalizer

package main.en {
	
	object normalize extends (String => String) {
		
		private[this] val numbers = Set("twenty-one", "twenty-two", "twenty-three", "twenty-four", "twenty-five", "twenty-six", "twenty-seven", "twenty-eight", "twenty-nine", "thirty-one", "thirty-two", "thirty-three", "thirty-four", "thirty-five", "thirty-six", "thirty-seven", "thirty-eight", "thirty-nine", "forty-one", "forty-two", "forty-three", "forty-four", "forty-five", "forty-six", "forty-seven", "forty-eight", "forty-nine", "fifty-one", "fifty-two", "fifty-three", "fifty-four", "fifty-five", "fifty-six", "fifty-seven", "fifty-eight", "fifty-nine", "sixty-one", "sixty-two", "sixty-three", "sixty-four", "sixty-five", "sixty-six", "sixty-seven", "sixty-eight", "sixty-nine", "seventy-one", "seventy-two", "seventy-three", "seventy-four", "seventy-five", "seventy-six", "seventy-seven", "seventy-eight", "seventy-nine", "eighty-one", "eighty-two", "eighty-three", "eighty-four", "eighty-five", "eighty-six", "eighty-seven", "eighty-eight", "eighty-nine", "ninety-one", "ninety-two", "ninety-three", "ninety-four", "ninety-five", "ninety-six", "ninety-seven", "ninety-eight", "ninety-nine")
		
		private[this] val biun = new BiuNormalizer(new java.io.File(classOf[BiuNormalizer].getClassLoader.getResource("en/BiuNormalizer_rules.txt").toURI))
		
		def apply(s: String) = {
			
			var tmp = s.trim
			
			// zenkaku -> hankaku
			tmp = for (c <- tmp) yield {
				if (c >= 0xff10 && c <= 0xff5a) {
					(c - 0xff00 + 0x0020).toChar
				} else {
					c
				}
			}
			
			// US$ rule
			tmp = tmp.replaceAll("US\\$", "\\$")
			
			// hyphen hyphen rule
			tmp = tmp.replaceAll("--+", ". ")
			
			var tmpsp = tmp.split(" +")
			
			// hyphen rule
			tmpsp = for (pw <- tmpsp) yield {
				if (pw.indexOf("-") != -1) {
					if (pw == "-") {
						","
					} else if (pw.matches("-[A-Za-z].+")) {
						pw.replaceAll("-", "(")
					} else if (pw.substring(pw.length - 1, pw.length) == "-") {
						pw.replaceAll("-", ")")
					} else {
						val regex = "(.*[A-Za-z0-9])([^A-Za-z0-9]*)".r
						val regex(normal, punc) = pw
						if (numbers.contains(normal.toLowerCase)) {
							pw.replaceAll("-", " ")
						} else if (!EnWordNet.hasWord(normal) && normal.split("-").forall(x => x.matches("[0-9\\.]+") || EnWordNet.hasWord(x))) {
							pw.replaceAll("-", " ")
						} else {
							pw
						}
					}
				} else {
					pw
				}
			}
			
			// slash rule
			tmpsp = for (pw <- tmpsp) yield {
				if (pw.matches("[a-zA-Z]+/[a-zA-Z]+")) {
					pw.replaceAll("/", " ")
				} else {
					pw
				}
			}
			
			// number normalize
			biun.normalize(tmpsp.mkString("", " ", ""))
		}
		
	}
}
