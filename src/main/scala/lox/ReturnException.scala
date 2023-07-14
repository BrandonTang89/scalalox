package lox

// Exception used to return
class ReturnException(val value: Any) extends RuntimeException(null, null, false, false)
