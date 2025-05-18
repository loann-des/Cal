package pretty_printer

import scala.util.Try

/** définition d'une exception pour le cas des listes vides de commandes
  */
case object ExceptionListeVide extends Exception

/** UN PRETTY-PRINTER POUR LE LANGAGE WHILE
  */
object Prettyprinter {

  /** définition d'un type pour les spécifications
    */
  sealed trait Spec
  case object WHILE extends Spec
  case object FOR extends Spec
  case object IF extends Spec
  case object PROGR extends Spec

  /** définition d'un type pour les listes de spécifications d'indentation
    */
  type IndentSpec = Map[Spec, Int]

  /** définition d'une valeur d'indentation par défaut
    */
  val indentDefault: Int = 1

  /** TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
    */

  /** @param expression
    *   : un AST décrivant une expression du langage WHILE
    * @return
    *   une chaîne représentant la syntaxe concrète de l'expression
    */
  def prettyPrintExpr(expression: Expression): String = expression match {
    case Nl => "nil"
    case Cst(name) => name
    case VarExp(name) => name
    case Cons(arg1, arg2) =>"(cons " + 
      prettyPrintExpr(arg1) + " " +
      prettyPrintExpr(arg2) + ")"
    case Hd(arg) => "(hd " + prettyPrintExpr(arg)+ ")"
    case Tl(arg) => "(tl " + prettyPrintExpr(arg)+")"
    case Eq(arg1, arg2) => prettyPrintExpr(arg1) + " =? " +
      prettyPrintExpr(arg2)
  }

  /** FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES
    * COMMANDES OU LA PRESENTATION DU PROGRAMME
    */

  /** recherche d'une valeur d'indentation dans une liste de spécifications
    * d'indentation
    *
    * @param context
    *   une chaîne de caractères décrivant un contexte d'indentation
    * @param is
    *   une liste de spécifications d'indentation, chaque spécification étant un
    *   couple (un contexte,une indentation) les contextes possibles seront, en
    *   majuscules, "WHILE", "FOR", "IF", ou "PROGR".
    * @return
    *   l'indentation correspondant à context
    */

  def indentSearch(context: Spec, is: IndentSpec): Int =
    is.getOrElse(context,indentDefault) 

  /** création d'une indentation
    *
    * @param n
    *   un nombre d'espaces
    * @return
    *   une chaîne de n espaces
    */

  def makeIndent(n: Int): String =
    if(n==0) then "" else " "*n

  /** ajout d'une chaîne devant chaque élément d'une liste non vide de chaînes
    *
    * @param pref
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de pref devant chaque
    *   élément de strings
    */

  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] =
    strings match
      case Nil => Nil
      case x::Nil => pref + x::Nil
      case x :: xs => pref + x::appendStringBeforeAll(pref, xs) 
    

  /** ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après chaque
    *   élément de strings
    */

  def appendStringAfterAll(suff: String, strings: List[String]): List[String] =
    strings match
      case Nil => Nil
      case x::Nil => x+suff::Nil
      case x :: xs => x+suff::appendStringAfterAll(suff, xs) 
    

  /** ajout d'une chaîne après le dernier élément d'une liste non vide de
    * chaînes
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après le
    *   dernier élément de strings
    */

  def appendStringAfterLast(suff: String, strings: List[String]): List[String] =
    strings match
      case Nil => Nil
      case x::Nil => x+ suff::Nil
      case x :: xs => x::appendStringAfterLast(suff, xs) 
    

  /** ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
    * sauf le dernier
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après chaque
    *   élément de strings sauf le dernier
    */

  def appendStringAfterAllButLast(suff: String, strings: List[String]): List[String] = 
    strings match
      case Nil => Nil
      case x::Nil => x::Nil
      case x :: xs => x+suff::appendStringAfterAllButLast(suff, xs) 
    

  /** TRAITEMENT DES COMMANDES DU LANGAGE WHILE
    */

  //TODOfonction anaxe

  /** @param context
    *   une chaîne de caractères décrivant un contexte d'indentation
    * 
    * @param body
    *   : une liste non vide d'AST décrivant une liste non vide de commandes du
    *   langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    */
  def prettyPrintBody(context: Spec, body: List[Command], is: IndentSpec): List[String] =
    appendStringBeforeAll(
      makeIndent(indentSearch(context,is)),
      prettyPrintCommands(body,is)
    )

  /** @param command
    *   : un AST décrivant une commande du langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète de la commande
    */
  // TODO TP2
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] =
    command match
      case Nop => "nop"::Nil
      case Set(Var(name), expression) =>
        name + " := " + prettyPrintExpr(expression)::Nil
      case While(condition, body) =>List.concat(
        "while " + prettyPrintExpr(condition) + " do"::Nil,
          prettyPrintBody(WHILE,body,is),
        "od"::Nil
        )
      case For(count, body) =>List.concat(
        "for "  + prettyPrintExpr(count)+ " do"::Nil,
          prettyPrintBody(FOR,body,is),
        "od"::Nil
        )
      case If(condition, then_commands, else_commands) =>List.concat(
        "if " + prettyPrintExpr(condition) + " then"::Nil,
          prettyPrintBody(IF,then_commands,is),
        "else"::Nil,
          prettyPrintBody(IF,else_commands,is),
        "fi"::Nil
        )
    

  /** @param commands
    *   : une liste non vide d'AST décrivant une liste non vide de commandes du
    *   langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète de la liste de
    *   commandes
    */
  // TODO TP2
  def prettyPrintCommands(
      commands: List[Command],
      is: IndentSpec
  ): List[String] = 
  commands match
    case Nil => Nil
    case x::Nil => prettyPrintCommand(x,is)
    case x :: xs => List.concat(
      appendStringAfterLast(" ;", prettyPrintCommand(x,is)),
      prettyPrintCommands(xs,is)
      )
  

  /** TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
    */

  /** @param vars
    *   : une liste non vide décrivant les paramètres d'entrée d'un programme du
    *   langage WHILE
    * @return
    *   une liste de chaînes représentant la syntaxe concrète des paramètres
    *   d'entrée du programme
    */
  // TODO TP2
  def prettyPrintIn(vars: List[Variable]): String =
    vars match
      case Nil => ""
      case Var(name)::Nil => name
      case Var(name) :: xs => name + ", " +prettyPrintIn(xs)
      

  /** @param vars
    *   : une liste non vide décrivant les paramètres de sortie d'un programme
    *   du langage WHILE
    * @return
    *   une liste de chaînes représentant la syntaxe concrète des paramètres de
    *   sortie du programme
    */
  // TODO TP2
  def prettyPrintOut(vars: List[Variable]): String = 
    vars match
      case Nil => ""
      case Var(name)::Nil => name
      case Var(name) :: xs => name + ", " +prettyPrintOut(xs)

  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète du programme
    */
  // TODO TP2
  def prettyPrintProgram(program: Program, is: IndentSpec): List[String] = 
    program match
      case Progr(in, body, out)=>List.concat(
        ("read " + prettyPrintIn(in))::Nil,
        "%"::Nil,
        prettyPrintBody(PROGR,body,is),
        "%"::Nil,
        ("write " + prettyPrintOut(out))::Nil
      )
    

  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une chaîne représentant la syntaxe concrète du programme
    */
  // TODO TP2
  def prettyPrint(program: Program, is: IndentSpec): String = 
    appendStringAfterAllButLast("\n",prettyPrintProgram(program,is)).mkString
    

  val program: Program =
    Progr(
      List(Var("X")),
      List(
        Set(Var("Y"), Nl),
        While(
          VarExp("X"),
          List(
            Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))
          )
        )
      ),
      List(Var("Y"))
    );

  val is: IndentSpec = Map(PROGR -> 2, WHILE -> 5);
  
  def main(args: Array[String]): Unit = {
    println("Bienvenue au projet pretty_printer.")
    // Décommentez la ligne ci-dessous lorsque prettyPrint sera définie.
    println(prettyPrint(program, is));
  }

}
