trait Ordre {
	def < (that: Any /*objet ou type de base*/): Boolean
	def <= (that: Any): Boolean = (this < that) || (this == that)
	def > (that: Any): Boolean = !(this <= that)
	def >= (that: Any): Boolean = !(this < that)
}

case class Date(year:Int, month:Int, day:Int) extends Ordre {
	override def toString(): String = year + "-" + month + "-" + day

	def < (that: Any): Boolean = that match {
		case Date(y, m, d) => (year <  y) || (y == year && (month < m || (m == month && day < d)))
		case _ => sys.error("cannot compare " + that + "and a date")
	}
}

/*class Date(y: Int, m: Int, d: Int) extends Ordre {
	def year = y
	def month = m
	def day = d

	override def toString(): String = y + "-" + m + "-" + d

	override def equals(that: Any): Boolean = {
		that.isInstanceOf[Date] && {
			val d = that.asInstanceOf[Date]
			d.day == day && d.month == month && d.year == year
		}
	}

	def < (that:Any): Boolean = {
		if(!that.isInstanceOf[Date])
			sys.error("cannot compare " + that + "and a date")
		val d = that.asInstanceOf[Date]
		(year < d.year) || (year == d.year && (month < d.month || (month == d.month && day < d.day)))
	}
}*/