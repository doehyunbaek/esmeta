package esmeta.ty

import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import esmeta.util.SystemUtils.*
import esmeta.ty.util.JsonProtocol.given

/** type modeling */
// TODO consider refactoring
case class TyModel(infos: Map[String, TyInfo] = Map()) {

  /** get method map */
  // TODO optimize
  def getMethod(tname: String): Map[String, String] = infos.get(tname) match {
    case Some(info) =>
      val parentMethods = info.parent.map(getMethod).getOrElse(Map())
      parentMethods ++ info.methods
    case None => Map()
  }

  /** direct subtypes */
  private lazy val directSubTys: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      (name, info) <- infos
      parent <- info.parent
      set = children.getOrElse(parent, Set())
    } children += parent -> (set + name)
    children
  }

  /** subtypes */
  lazy val subTys: Map[String, Set[String]] = {
    var descs = Map[String, Set[String]]()
    def aux(name: String): Set[String] = descs.get(name) match {
      case Some(set) => set
      case None =>
        val set = (for {
          sub <- directSubTys.getOrElse(name, Set())
          elem <- aux(sub)
        } yield elem) + name
        descs += name -> set
        set
    }
    infos.collect { case (name, TyInfo(None, _, _)) => aux(name) }
    descs
  }
  def isSubTy(l: String, r: String): Boolean =
    (l == r) || subTys.get(r).fold(false)(_ contains l)
  def isSubTy(l: String, rset: Set[String]): Boolean =
    rset.exists(r => isSubTy(l, r))
  def isSubTy(l: String, rset: BSet[String]): Boolean = rset match
    case Inf       => true
    case Fin(rset) => isSubTy(l, rset)
  def isSubTy(lset: Set[String], rset: Set[String]): Boolean =
    lset.forall(l => isSubTy(l, rset))

  /** property map alias */
  type PropMap = Map[String, ValueTy]

  /** get types of property */
  def getProp(tname: String, p: String, check: Boolean = false): ValueTy =
    if (tname == "IntrinsicsRecord" && p.startsWith("%") && p.endsWith("%"))
      NameT("Object")
    else
      propMap
        .getOrElse(tname, Map())
        .getOrElse(
          p, {
            if (check) warn(s"unknown property access: $tname.$p")
            AbsentT
          },
        )

  /** property type */
  private lazy val propMap: Map[String, PropMap] = (for {
    name <- infos.keySet
  } yield name -> getPropMap(name)).toMap

  /** get property map */
  private def getPropMap(name: String): PropMap =
    val upper = getUpperPropMap(name)
    val lower = getLowerPropMap(name)
    lower.foldLeft(upper) {
      case (map, (k, t)) =>
        val newT = t || map.getOrElse(k, BotT)
        map + (k -> newT)
    }

  /** get property map from ancestors */
  private def getUpperPropMap(name: String): PropMap = infos.get(name) match
    case Some(info) =>
      val parentProps = info.parent.map(getUpperPropMap).getOrElse(Map())
      val props = info.props
      weakMerge(parentProps, props)
    case None => Map()

  /** get property map of name */
  private def getSamePropMap(name: String): PropMap =
    infos.get(name).map(_.props).getOrElse(Map())

  /** get property map from ancestors */
  private def getLowerPropMap(name: String): PropMap =
    directSubTys.get(name) match
      case Some(children) =>
        children
          .map(child => {
            val lower = getLowerPropMap(child)
            val props = getSamePropMap(child)
            weakMerge(lower, props)
          })
          .reduce(parallelWeakMerge)
      case None => getSamePropMap(name)

  /** weak merge */
  private def weakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, BotT)
        val rt = rmap.getOrElse(k, BotT)
        k -> (lt || rt)
      })
      .toMap
  }

  /** parallel weak merge */
  private def parallelWeakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, AbsentT)
        val rt = rmap.getOrElse(k, AbsentT)
        k -> (lt || rt)
      })
      .toMap
  }

}
object TyModel {
  val es = readJson[TyModel](
    "src/main/resources/manuals/default/tymodel.json",
  )
}

/** type information */
case class TyInfo(
  parent: Option[String] = None,
  methods: Map[String, String] = Map(),
  fields: Map[String, ValueTy] = Map(),
) {
  lazy val props: Map[String, ValueTy] =
    val keys = methods.keySet ++ fields.keySet
    (for {
      k <- keys
      fs = fields.getOrElse(k, BotT)
      tys = methods.get(k) match
        case None         => fs
        case Some(method) => fs || CloT(method)
    } yield k -> tys).toMap
}
