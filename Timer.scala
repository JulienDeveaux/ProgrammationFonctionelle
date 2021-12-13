object Timer {
	def oncePerSecond(callback: ()/* rien en param */ => Unit/* pareil que void */) = {
		while(true) {
			callback()
			Thread.sleep(1000)
		}
	}

	/*def timerFlies() = {
		println("tick")
	}*/

	def main(args: Array [String]) = {
		//oncePerSecond(timerFlies)
		//oncePerSecond(() => println("tick"))
		oncePerSecond(() => {
			println("tick")
			println("tack")
		})
	}
}