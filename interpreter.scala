package interpreter

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 *
 * ETUDIANT 1 : Ethan Billard
 *
 * ETUDIANT 2 : Desbles Loann
 *
 */

/* Les exceptions suivantes sont définies pour le cas où vous voudriez garantir
 *  - que des listes de variables ne sont pas vides
 *  - que des listes de commandes ne sont pas vides
 *  - que des listes de variables et de valeurs sont de même longueur
 *
 * Déclencher ces exceptions se fait par  :
 *   - throw ExceptionListeVide
 *   - throw ExceptionListesDeLongueursDifferentes
 */

/** définition d'une exception pour le cas des listes vides
  */
case object ExceptionListeVide extends Exception

/** définition d'une exception pour le cas des listes de tailles différentes
  */
case object ExceptionListesDeLongueursDifferentes extends Exception


object Interpreter {

  /** UN INTERPRETER POUR LE LANGAGE WHILE
    */

  /** GESTION DE LA MEMOIRE DE L'INTERPRETEUR
    */

  /** définition d'un type Memory pour représenter une mémoire
    */
  type Memory = Map[Variable, Value]

  /** @param v
    *   : une variable
    * @param mem
    *   : une mémoire
    * @return
    *   m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem, la
    *   valeur par défaut si la variable v n'est pas présente dans la mémoire
    *   mem
    */
  def lookUp(v: Variable, mem: Memory): Value =
    if mem.contains(v) then mem(v) else NlValue


  /** @param v
    *   : une variable
    * @param d
    *   : une valeur
    * @param mem
    *   : une mémoire
    * @return
    *   la mémoire modifiée par l'affectation [v->d]
    */
  def assign(v: Variable, d: Value, mem: Memory): Memory = 
    mem + (v->d)


  /** TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
    */

  /** @param expression
    *   : un AST décrivant une expression du langage WHILE
    * @return
    *   la valeur de l'expression
    */
  def interpreterExpr(expression: Expression, mem: Memory): Value = 
    expression match {
      case Nl => NlValue
      case Cst(name) => CstValue(name)
      case VarExp(name) => lookUp(Var(name),mem)
      case Cons(arg1,arg2) => ConsValue(interpreterExpr(arg1 , mem), interpreterExpr(arg2 ,mem) )
      case Hd(arg) => interpreterExpr(arg,mem) match {
        case ConsValue(arg1, arg2) => arg1
        case _ => NlValue
      }
      case Tl(arg) => interpreterExpr(arg,mem) match {
        case ConsValue(arg1, arg2) => arg2
        case _ => NlValue
      }
      case Eq(arg1, arg2) => if interpreterExpr(arg1,mem) == interpreterExpr(arg2,mem)
                              then CstValue("1") else NlValue

    }


  /** La fonction interpreterExpr ci-dessus calcule la valeur associée à une
    * expression ; il peut être utile de produire à l'inverse une expression
    * associée à une valeur. La fonction valueToExpression ci-dessous construira
    * l'expression la plus simple associée à une valeur
    *
    * @param value
    *   : une valeur du langage WHILE
    * @return
    *   l'AST décrivant l'expression de cette valeur
    */
  def valueToExpression(value: Value): Expression = 
    value match
      case NlValue => Nl
      case CstValue(name) => Cst(name)
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1),valueToExpression(arg2))
    


  /** TRAITEMENT DES COMMANDES DU LANGAGE WHILE
    */

  /** @param command
    *   : un AST décrivant une commande du langage WHILE
    * @param memory
    *   : une mémoire
    * @return
    *   la mémoire après l'interprétation de la commande
    */
  def interpreterCommand(command: Command, memory: Memory): Memory =
    command match
      case Nop => memory
      case Set(variable, expression) => assign(variable,interpreterExpr(expression,memory),memory)

      case While(condition, body) =>interpreterExpr(condition,memory) match
        case NlValue => memory
        case _ =>interpreterCommand(While(condition, body),interpreterCommands(body,memory))

      case For(count, body) => interpreterExpr(count,memory) match
        case NlValue => memory
        case ConsValue(arg1, arg2) =>interpreterCommand(For(valueToExpression(arg2), body),interpreterCommands(body,memory))
        case _ =>interpreterCommand(Nop,interpreterCommands(body,memory))

      case If(condition, then_commands, else_commands) => interpreterExpr(condition,memory) match
        case NlValue => interpreterCommands(else_commands,memory)
        case _ => interpreterCommands(then_commands,memory)
    


  /** @param commands
    *   : une liste non vide d'AST décrivant une liste non vide de commandes du
    *   langage WHILE
    * @param memory
    *   : une mémoire
    * @return
    *   la mémoire après l'interprétation de la liste de commandes
    */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory =
    commands match
      case head :: Nil => interpreterCommand(head , memory)
      case head :: next => interpreterCommands(next,interpreterCommand(head , memory))
      case Nil => throw ExceptionListeVide
      
  /** TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
    */

  /** @param vars
    *   : une liste non vide décrivant les variables d'entrée d'un programme du
    *   langage WHILE
    * @param vals
    *   : une liste non vide de valeurs
    * @return
    *   une mémoire associant chaque valeur à la variable d'entrée correspondant
    */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = 
    (vars ,vals) match 
      case (x::Nil,y::Nil) =>interpreterCommand(Set(x,valueToExpression(y)),Map())
      case (x::xs,y::ys) =>interpreterCommand(Set(x,valueToExpression(y)),interpreterMemorySet(xs,ys)) 
      case (_,_) => throw ExceptionListesDeLongueursDifferentes
    


  /** @param vars
    *   : une liste non vide décrivant les variables de sortie d'un programme du
    *   langage WHILE
    * @param memory
    *   : une mémoire
    * @return
    *   la liste des valeurs des variables de sortie
    */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = 
    vars match {
      case x::Nil => lookUp(x,memory)::Nil
      case x::xs => lookUp(x,memory)::interpreterMemoryGet(xs,memory)
      case Nil => throw ExceptionListeVide
    }

  
  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param vals
    *   : une liste de valeurs
    * @return
    *   la liste des valeurs des variables de sortie
    */
  def interpreter(program: Program, vals: List[Value]): List[Value] = 
        program match 
          case Progr(in,body,out)=> interpreterMemoryGet(
            out ,interpreterCommands(
              body,interpreterMemorySet(
                in, vals
              )
            )
          )


}
