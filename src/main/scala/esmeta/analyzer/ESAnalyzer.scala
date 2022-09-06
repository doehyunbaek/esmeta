package esmeta.analyzer

import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*

/** meta-level static analyzer for ECMAScript */
class ESAnalyzer(cfg: CFG) {
  // set CFG for analysis
  setCFG(cfg)

  /** initial control point */
  lazy val initCp =
    val runJobs = cfg.fnameMap("RunJobs")
    val entry = runJobs.entry
    NodePoint(runJobs, entry, View())

  /** perform analysis for a given ECMAScript code */
  def apply(sourceText: String): AbsSemantics =
    val sem = AbsSemantics(
      npMap = Map(
        initCp -> AbsState.Empty.defineGlobal(
          Global(builtin.SOURCE_TEXT) -> AbsValue(sourceText),
        ),
      ),
    )
    sem.fixpoint
}
