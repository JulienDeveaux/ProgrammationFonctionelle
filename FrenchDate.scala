import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._ // importer tous les champs satiques de la classe et le * est remplacé par _

object FrenchDate {
	def main(args: Array[String]) = {
		val now = new Date // val est une constante // on peut virer les () vides de la classe
		val df = getDateInstance(LONG, Locale.FRANCE)
		// pas val df = DateFormat.getDateInstance(LONG, Locale.FRANCE) a cause de l'import statique
		println(df.format(now))
	}
}