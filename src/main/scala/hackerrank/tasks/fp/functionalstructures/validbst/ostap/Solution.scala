package hackerrank.tasks.fp.functionalstructures.validbst.ostap

object PolishCalculator {

  final val SumSign = '+'
  final val SubtractSign = '-'
  final val MultiplySign = '*'
  final val DevideSign = '/'
  final val LParenthesesSign = '('
  final val RParenthesesSign = ')'
  final val MinPriority = 1

  def formatString(s: String) = {
    s.filterNot((x: Char) => x.isWhitespace)
  }

  sealed trait Term

  final case class OperationTerm(leftOperand: Term, rightOperand: Term, operation: BinaryOperation) extends Term
  final case class SequencedTerm(terms: Seq[Term], operations: Seq[BinaryOperation]) extends Term
  final case class NumberTerm(number: Int) extends Term

  sealed abstract class BinaryOperation(val sign: Char, val priority: Int) {
    def operate(lOperand: Int, rOperand: Int): Int
  }

  final case class SumOperation() extends BinaryOperation(sign = SumSign, priority = 1) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand + rOperand
    }
  }

  final case class SubtractOperation() extends BinaryOperation(sign = SubtractSign, priority = 1) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand - rOperand
    }
  }

  private final case class MultiplyOperation() extends BinaryOperation(sign = MultiplySign, priority = 0) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand * rOperand
    }
  }

  private final case class DevideOperation() extends BinaryOperation(sign = DevideSign,  priority = 0) {
    override def operate(lOperand: Int, rOperand: Int): Int = {
      lOperand / rOperand
    }
  }

  def evaluateTerm(term: Term): Int = {

    term match {
      case NumberTerm(n) => n
      case SequencedTerm(str, sop) =>
        evaluateTerm(getTermFromSeq(str, sop, 0).terms.head)
      case OperationTerm(l, r, op) =>
        l match {
          case NumberTerm(n) => op.operate(n, evaluateTerm(r))
          case SequencedTerm(str, sop) => op.operate(evaluateTerm(getTermFromSeq(str, sop, 0).terms.head), evaluateTerm(r))
          case OperationTerm(ll,lr,lop) => op.operate(lop.operate(evaluateTerm(ll), evaluateTerm(lr)), evaluateTerm(r))
        }
    }

  }

  @scala.annotation.tailrec
  def getTermFromSeq(terms: Seq[Term], ops: Seq[BinaryOperation], priority: Int = MinPriority, acc: Seq[Term] = Seq.empty[Term]): SequencedTerm = {

    println("getTermFromSeq() terms="+terms)

    @scala.annotation.tailrec
    def getTermsSeqForPriority(
                                termsSeq: Seq[Term],
                                opsSeq: Seq[BinaryOperation],
                                previousTerm: Term,
                                termsAcc: Seq[Term] = Seq.empty[Term],
                                opsAcc: Seq[BinaryOperation] = Seq.empty[BinaryOperation]): SequencedTerm = {

      println("getTermsSeqForPriority() terms="+terms+" priority="+priority)


      opsSeq match {
        case op :: Nil =>
          if(op.priority == priority) {
            SequencedTerm(Seq(OperationTerm(previousTerm, terms.head, op)), Seq.empty[BinaryOperation])
          } else {
            SequencedTerm(termsAcc ++ termsSeq, opsAcc.appended(op))
          }
        case op :: tail => if(op.priority == priority){
          val prevTerm = OperationTerm(previousTerm, termsSeq.head, op)
          getTermsSeqForPriority(termsSeq.tail, opsSeq.tail, prevTerm, termsAcc, opsAcc)
        } else {
          getTermsSeqForPriority(termsSeq.tail, opsSeq.tail, termsSeq.head, termsAcc.appended(previousTerm), opsAcc.appended(op))
        }
      }
    }

    val termOnPriotity = getTermsSeqForPriority(terms.tail, ops, terms.head)
    if(priority < MinPriority){
      getTermFromSeq(termOnPriotity.terms, termOnPriotity.operations, priority+1)
    } else {
      termOnPriotity
    }
  }

  @scala.annotation.tailrec
  def getTermStringInParentheses(toParse: String, acc: String, openParenthesesNum: Int = 0): String = {
    val symbol = toParse.head
    symbol match {
      case LParenthesesSign =>
        getTermStringInParentheses(toParse.tail, acc.appended(symbol), openParenthesesNum+1)
      case RParenthesesSign =>
        if(openParenthesesNum - 1 == 0)acc.appended(RParenthesesSign) else getTermStringInParentheses(toParse.tail, acc.appended(symbol), openParenthesesNum - 1)
      case _ => getTermStringInParentheses(toParse.tail, acc.appended(symbol), openParenthesesNum)
    }
  }

  def parseStringToTermSeq(
                            toParse: String,
                            termsAcc: Seq[Term] = Seq.empty[Term],
                            operationsAcc: Seq[BinaryOperation] = Seq.empty[BinaryOperation],
                            numberAcc: String = "",
                            acc: Term = NumberTerm(0)): SequencedTerm = {//dont forget 0 is here!!!!

    println("toParse.head="+toParse.head)

    toParse.head match {
      case c if(c.isDigit) =>
        if(toParse.length > 1){
          parseStringToTermSeq(toParse.tail, termsAcc, operationsAcc, numberAcc.appended(c), acc)
        } else {
          SequencedTerm(termsAcc.appended(NumberTerm((numberAcc.appended(c)).toInt)), operationsAcc)
        }
      case SumSign =>
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseStringToTermSeq(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(SumOperation()), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case SubtractSign => //DRY!!!
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseStringToTermSeq(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(SubtractOperation()), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case MultiplySign =>
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseStringToTermSeq(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(MultiplyOperation()), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case DevideSign =>
        if(numberAcc.length > 0){//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DRY!!!
          val numToCache = numberAcc.toInt
          parseStringToTermSeq(toParse.tail, termsAcc.appended(NumberTerm(numToCache)), operationsAcc.appended(DevideOperation()), "", acc)//add digit tio cache and operation to remember
        } else {
          throw new Exception("Operation symbol is in incorrect position")
        }
      case LParenthesesSign =>
        val str = getTermStringInParentheses(toParse, "")
        println("getTermStringInParentheses="+str + " toParse="+toParse)
        if(toParse.length > str.length){
          val removedParentheses = (str.tail).reverse.tail.reverse
          val leftOperand = parseStringToTermSeq(removedParentheses)
          val binaryOperationC = toParse.charAt(str.length)
          val rightOperand = toParse.substring(str.length+1)
          val op = binaryOperationC match {
            case SubtractSign => SubtractOperation()
            case SumSign => SumOperation()
            case DevideSign => DevideOperation()
            case MultiplySign => MultiplyOperation()
          }
          val term = OperationTerm(leftOperand, parseStringToTermSeq(rightOperand), op)
          SequencedTerm(termsAcc.appended(term), operationsAcc)
        } else {
          val removedParentheses = (str.tail).reverse.tail.reverse
          val SequencedTerm(ts, os) = parseStringToTermSeq(removedParentheses)
          SequencedTerm(termsAcc ++ ts, operationsAcc ++ os)
        }
    }
  }

  def main(args: Array[String]) {
    val str = formatString("2 + 2 + 6 + 4")
    val term = parseStringToTermSeq(str)
//    println("term="+term)
    val res = evaluateTerm(term)
//    println(res)
  }
}
