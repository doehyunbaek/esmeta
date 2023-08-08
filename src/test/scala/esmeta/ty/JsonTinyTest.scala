package esmeta.ty

import esmeta.ty.util.JsonProtocol.given
import esmeta.state.{Nt, Number}
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*

/** JSON test */
class JsonTinyTest extends TyTest {
  val name: String = "tyJsonTest"

  // registration
  def init: Unit = {
    checkJsonWithString("Ty")(
      AnyT -> "Any",
      PureValueT -> "PureValue",
      AbruptT -> "Abrupt",
      NormalT(NumberT) -> "Normal[Number]",
      SubMapT(
        StrT,
        NameT("Binding"),
      ) -> "SubMap[String |-> Binding]",
      CloT -> "Clo",
      CloT("ToString:clo0") -> "Clo[\"ToString:clo0\"]",
      ContT -> "Cont",
      ContT(42, 3) -> "Cont[3, 42]",
      ESValueT -> "ESValue",
      UnknownTy() -> "Unknown",
      UnknownTy(Some("T")) -> "Unknown[\"T\"]",
      NameT -> "AnyName",
      NameT("Cat") -> "Cat",
      NameT("Cat", "Dog") -> "Cat | Dog",
      RecordT -> "AnyRecord",
      RecordT("A" -> NumberT, "B" -> BoolT) ->
      "{ [[A]]: Number, [[B]]: Boolean }",
      RecordT(Set("Key", "Value")) ->
      "{ [[Key]], [[Value]] }",
      RecordT("Key" -> ValueTy.Top, "Value" -> ValueTy.Top, "Dummy" -> BotT) ->
      "{ [[Key]], [[Value]] }",
      (ObjectT || RecordT(
        "P" -> ValueTy.Top,
        "S" -> ValueTy.Top,
        "Q" -> NumberT,
        "R" -> BoolT,
      )) -> "Object | { [[P]], [[Q]]: Number, [[R]]: Boolean, [[S]] }",
      NilT -> "Nil",
      ListT(NumberT) -> "List[Number]",
      SymbolT -> "Symbol",
      AstT -> "Ast",
      AstT("Literal") -> "Ast[Literal]",
      AstSingleT("Member", 1, 3) -> "Ast:Member[1,3]",
      NtT(
        Nt("Literal", List(true)),
        Nt("Identifier", List(false, true, false)),
      ) -> "Nt[|Identifier|[FTF], |Literal|[T]]",
      CodeUnitT -> "CodeUnit",
      ConstT("key") -> "Const[~key~]",
      ConstT("key", "value") -> "Const[~key~, ~value~]",
      MathT -> "Math",
      MathT(0, 1) -> "Math[0, 1]",
      NumberT -> "Number",
      NumberT(Number(Double.PositiveInfinity)) -> "Number[+INF]",
      NumberT(Number(Double.NegativeInfinity)) -> "Number[-INF]",
      NumberT(Number(Double.NaN)) -> "Number[NaN]",
      NumberT(
        Number(Double.PositiveInfinity),
        Number(Double.NegativeInfinity),
        Number(Double.NaN),
        Number(-0.0),
        Number(0.0),
      ) -> "Number[-INF, -0.0, 0.0, +INF, NaN]",
      BigIntT -> "BigInt",
      StrT -> "String",
      StrT("a") -> "String[\"a\"]",
      BoolT -> "Boolean",
      UndefT -> "Undefined",
      NullT -> "Null",
      AbsentT -> "Absent",
    )
  }
  object HardcodedTyModel {

    /** alias */
    val EMPTY = ConstT("empty")
    val UNRESOLVABLE = ConstT("unresolvable")
    val LEXICAL = ConstT("lexical")
    val INITIALIZED = ConstT("initialized")
    val UNINITIALIZED = ConstT("uninitialized")
    val FIELD = ConstT("field")
    val METHOD = ConstT("method")
    val ACCESSOR = ConstT("accessor")
    val BASE = ConstT("base")
    val DERIVED = ConstT("derived")
    val STRICT = ConstT("strict")
    val GLOBAL = ConstT("global")
    val UNLINKED = ConstT("unlinked")
    val LINKING = ConstT("linking")
    val LINKED = ConstT("linked")
    val EVALUATING = ConstT("evaluating")
    val EVALUATING_ASYNC = ConstT("evaluating-async")
    val EVALUATED = ConstT("evaluated")
    val NUMBER = ConstT("Number")
    val BIGINT = ConstT("BigInt")
    val ALL = ConstT("all")
    val ALL_BUT_DEFAULT = ConstT("all-but-default")
    val NORMAL = ConstT("normal")
    val BREAK = ConstT("break")
    val CONTINUE = ConstT("continue")
    val RETURN = ConstT("return")
    val THROW = ConstT("throw")
    val SUSPENDED_START = ConstT("suspendedStart")
    val SUSPENDED_YIELD = ConstT("suspendedYield")
    val EXECUTING = ConstT("executing")
    val AWAITING_RETURN = ConstT("awaitingDASHreturn")
    val COMPLETED = ConstT("completed")
    val PENDING = ConstT("pending")
    val FULFILLED = ConstT("fulfilled")
    val REJECTED = ConstT("rejected")
    val FULFILL = ConstT("Fulfill")
    val REJECT = ConstT("Reject")
    val NAMESPACE_OBJ = ConstT("namespace-object")
    val NAMESPACE = ConstT("NAMESPACE")

    // TODO extract type model from spec
    lazy val es: TyModel = TyModel(
      Map(
        // property descriptor
        "PropertyDescriptor" -> TyInfo(
          fields = Map(
            "Value" -> (ESValueT || AbsentT),
            "Writable" -> (BoolT || AbsentT),
            "Get" -> (NameT("FunctionObject") || UndefT || AbsentT),
            "Set" -> (NameT("FunctionObject") || UndefT || AbsentT),
            "Enumerable" -> (BoolT || AbsentT),
            "Configurable" -> (BoolT || AbsentT),
          ),
        ),

        // realm record
        "RealmRecord" -> TyInfo(
          fields = Map(
            "Intrinsics" -> NameT("IntrinsicsRecord"),
            "GlobalObject" -> (UndefT || NameT("Object")),
            "GlobalEnv" -> NameT("GlobalEnvironmentRecord"),
            "TemplateMap" -> ListT(NameT("TemplatePair")),
            "HostDefined" -> UndefT,
          ),
        ),
        "TemplatePair" -> TyInfo(
          fields = Map(
            "Site" -> AstT("TemplateLiteral"),
            "Array" -> NameT("Object"),
          ),
        ),

        // execution contexts
        "ExecutionContext" -> TyInfo(
          fields = Map(
            "Function" -> (NameT("FunctionObject") || NullT),
            "Realm" -> NameT("RealmRecord"),
            "ScriptOrModule" -> (NameT(
              "ScriptRecord",
              "ModuleRecord",
            ) || NullT),
            "LexicalEnvironment" -> NameT("EnvironmentRecord"),
            "VariableEnvironment" -> NameT("EnvironmentRecord"),
            "PrivateEnvironment" -> (NameT(
              "PrivateEnvironmentRecord",
            ) || NullT),
            "Generator" -> NameT("Object"),
          ),
        ),

        // reference record
        "ReferenceRecord" -> TyInfo(
          fields = Map(
            "Base" -> (
              ESPrimT ||
              NameT("Object", "EnvironmentRecord") ||
              UNRESOLVABLE,
            ),
            "ReferencedName" -> (StrT || SymbolT || NameT("PrivateName")),
            "Strict" -> BoolT,
            "ThisValue" -> (ESValueT || EMPTY),
          ),
        ),

        // private name
        "PrivateName" -> TyInfo(
          fields = Map("Description" -> StrT),
        ),

        // private element
        "PrivateElement" -> TyInfo(
          fields = Map(
            "Key" -> NameT("PrivateName"),
            "Kind" -> (FIELD || METHOD || ACCESSOR),
            "Value" -> (AbsentT || ESValueT),
            "Get" -> (NameT("FunctionObject") || UndefT || AbsentT),
            "Set" -> (NameT("FunctionObject") || UndefT || AbsentT),
          ),
        ),

        // class field definition record
        "ClassFieldDefinitionRecord" -> TyInfo(
          fields = Map(
            "Name" -> (NameT("PrivateName") || StrT || SymbolT),
            "Initializer" -> (NameT("FunctionObject") || EMPTY),
          ),
        ),

        // class static block definition record
        "ClassStaticBlockDefinitionRecord" -> TyInfo(
          fields = Map("BodyFunction" -> NameT("FunctionObject")),
        ),

        // iterator record
        "IteratorRecord" -> TyInfo(
          fields = Map(
            "Iterator" -> NameT("OrdinaryObject"),
            "NextMethod" -> NameT("FunctionObject"),
            "Done" -> BoolT,
          ),
        ),

        // objects
        "Object" -> TyInfo(
          parent = None,
          methods = Map(
            "GetPrototypeOf" -> "OrdinaryObject.GetPrototypeOf",
            "SetPrototypeOf" -> "OrdinaryObject.SetPrototypeOf",
            "IsExtensible" -> "OrdinaryObject.IsExtensible",
            "PreventExtensions" -> "OrdinaryObject.PreventExtensions",
            "GetOwnProperty" -> "OrdinaryObject.GetOwnProperty",
            "DefineOwnProperty" -> "OrdinaryObject.DefineOwnProperty",
            "HasProperty" -> "OrdinaryObject.HasProperty",
            "Get" -> "OrdinaryObject.Get",
            "Set" -> "OrdinaryObject.Set",
            "Delete" -> "OrdinaryObject.Delete",
            "OwnPropertyKeys" -> "OrdinaryObject.OwnPropertyKeys",
          ),
          fields = Map(
            "Extensible" -> BoolT,
            "Prototype" -> (NameT("Object") || NullT),
            "SubMap" -> SubMapT(StrT || SymbolT, NameT("PropertyDescriptor")),
            "PrivateElements" -> ListT(NameT("PrivateElement")),
          ),
        ),
        "OrdinaryObject" -> TyInfo(
          parent = Some("Object"),
        ),
        "FunctionObject" -> TyInfo(
          parent = Some("OrdinaryObject"),
        ),
        "ECMAScriptFunctionObject" -> TyInfo(
          parent = Some("FunctionObject"),
          methods = Map(
            "Call" -> "ECMAScriptFunctionObject.Call",
            "Construct" -> "ECMAScriptFunctionObject.Construct",
          ),
          fields = Map(
            "Environment" -> NameT("EnvironmentRecord"),
            "PrivateEnvironment" -> (
              NameT("PrivateEnvironmentRecord") ||
              NullT
            ),
            "FormalParameters" -> AstT,
            "ECMAScriptCode" -> AstT,
            "ConstructorKind" -> (BASE || DERIVED),
            "Realm" -> NameT("RealmRecord"),
            "ScriptOrModule" -> (
              NameT("ScriptRecord", "ModuleRecord") ||
              NullT
            ),
            "ThisMode" -> (LEXICAL || STRICT || GLOBAL),
            "Strict" -> BoolT,
            "HomeObject" -> (NameT("Object") || UndefT),
            "SourceText" -> StrT,
            "Fields" -> ListT(NameT("ClassFieldDefinitionRecord")),
            "PrivateMethods" -> ListT(NameT("PrivateElement")),
            "ClassFieldInitializerName" ->
            (StrT || SymbolT || NameT("PrivateName") || EMPTY),
            "IsClassConstructor" -> BoolT,
          ),
        ),
        "BuiltinFunctionObject" -> TyInfo(
          parent = Some("FunctionObject"),
          methods = Map(
            "Call" -> "BuiltinFunctionObject.Call",
            // XXX "Construct" -> "BuiltinFunctionObject.Construct",
          ),
          fields = Map(
            "Code" -> CloT,
            "Realm" -> NameT("RealmRecord"),
            "InitialName" -> (NullT || StrT),
          ),
        ),
        "BoundFunctionExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "Call" -> "BoundFunctionExoticObject.Call",
            "Construct" -> "BoundFunctionExoticObject.Construct",
          ),
          fields = Map(
            "BoundTargetFunction" -> NameT("FunctionObject"),
            "BoundThis" -> ESValueT,
            "BoundArguments" -> ListT(ESValueT),
          ),
        ),
        "ArrayExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "DefineOwnProperty" -> "ArrayExoticObject.DefineOwnProperty",
          ),
        ),
        "StringExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "GetOwnProperty" -> "StringExoticObject.GetOwnProperty",
            "DefineOwnProperty" ->
            "StringExoticObject.DefineOwnProperty",
            "OwnPropertyKeys" -> "StringExoticObject.OwnPropertyKeys",
          ),
          fields = Map("StringData" -> StrT),
        ),
        "ArgumentsExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "GetOwnProperty" -> "ArgumentsExoticObject.GetOwnProperty",
            "DefineOwnProperty" ->
            "ArgumentsExoticObject.DefineOwnProperty",
            "Get" -> "ArgumentsExoticObject.Get",
            "Set" -> "ArgumentsExoticObject.Set",
            "Delete" -> "ArgumentsExoticObject.Delete",
          ),
          fields = Map(
            "ParameterMap" -> NameT("OrdinaryObject"),
          ),
        ),
        "IntegerIndexedExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "GetOwnProperty" ->
            "IntegerIndexedExoticObject.GetOwnProperty",
            "HasProperty" -> "IntegerIndexedExoticObject.HasProperty",
            "DefineOwnProperty" ->
            "IntegerIndexedExoticObject.DefineOwnProperty",
            "Get" -> "IntegerIndexedExoticObject.Get",
            "Set" -> "IntegerIndexedExoticObject.Set",
            "Delete" -> "IntegerIndexedExoticObject.Delete",
            "OwnPropertyKeys" ->
            "IntegerIndexedExoticObject.OwnPropertyKeys",
          ),
          fields = Map(
            "ViewedArrayBuffer" -> NameT("ArrayBufferObject"),
            "ArrayLength" -> MathT,
            "ByteOffset" -> MathT,
            "ContentType" -> (NUMBER || BIGINT),
            "TypedArrayName" -> StrT,
          ),
        ),
        "ModuleNamespaceExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "GetPrototypeOf" -> "ModuleNamespaceExoticObject.GetPrototypeOf",
            "SetPrototypeOf" -> "ModuleNamespaceExoticObject.SetPrototypeOf",
            "IsExtensible" -> "ModuleNamespaceExoticObject.IsExtensible",
            "PreventExtensions" -> "ModuleNamespaceExoticObject.PreventExtensions",
            "GetOwnProperty" -> "ModuleNamespaceExoticObject.GetOwnProperty",
            "DefineOwnProperty" -> "ModuleNamespaceExoticObject.DefineOwnProperty",
            "HasProperty" -> "ModuleNamespaceExoticObject.HasProperty",
            "Get" -> "ModuleNamespaceExoticObject.Get",
            "Set" -> "ModuleNamespaceExoticObject.Set",
            "Delete" -> "ModuleNamespaceExoticObject.Delete",
            "OwnPropertyKeys" -> "ModuleNamespaceExoticObject.OwnPropertyKeys",
          ),
          fields = Map(
            "Module" -> NameT("ModuleRecord"),
            "Exports" -> ListT(StrT),
          ),
        ),
        "ImmutablePrototypeExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "SetPrototypeOf" ->
            "ImmutablePrototypeExoticObject.SetPrototypeOf",
          ),
        ),
        "ProxyExoticObject" -> TyInfo(
          parent = Some("Object"),
          methods = Map(
            "GetPrototypeOf" -> "ProxyExoticObject.GetPrototypeOf",
            "SetPrototypeOf" -> "ProxyExoticObject.SetPrototypeOf",
            "IsExtensible" -> "ProxyExoticObject.IsExtensible",
            "PreventExtensions" -> "ProxyExoticObject.PreventExtensions",
            "GetOwnProperty" -> "ProxyExoticObject.GetOwnProperty",
            "DefineOwnProperty" -> "ProxyExoticObject.DefineOwnProperty",
            "HasProperty" -> "ProxyExoticObject.HasProperty",
            "Get" -> "ProxyExoticObject.Get",
            "Set" -> "ProxyExoticObject.Set",
            "Delete" -> "ProxyExoticObject.Delete",
            "OwnPropertyKeys" -> "ProxyExoticObject.OwnPropertyKeys",
            "Call" -> "ProxyExoticObject.Call",
            "Construct" -> "ProxyExoticObject.Construct",
          ),
          fields = Map(
            "ProxyHandler" -> (NameT("Object") || NullT),
            "ProxyTarget" -> (NameT("Object") || NullT),
          ),
        ),
        "ArrayBufferObject" -> TyInfo(parent = Some("Object")),
        "BooleanObject" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "BooleanData" -> BoolT,
          ),
        ),
        "BigIntObject" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "BigIntData" -> BigIntT,
          ),
        ),
        "NumberObject" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "NumberData" -> NumberT,
          ),
        ),
        "SymbolObject" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "SymbolData" -> SymbolT,
          ),
        ),
        "SetInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "SetData" -> ListT(ESValueT),
          ),
        ),
        "MapInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "MapData" -> ListT(
              RecordT("Key" -> (ESValueT || EMPTY), "Value" -> ESValueT),
            ),
          ),
        ),
        "WeakSetInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "WeakSetData" -> ListT(ESValueT),
          ),
        ),
        "WeakMapInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "WeakMapData" -> ListT(
              RecordT("Key" -> (ESValueT || EMPTY), "Value" -> ESValueT),
            ),
          ),
        ),
        "ErrorInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "ErrorData" -> UndefT,
          ),
        ),
        // special instances
        "ForInIteratorInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "Object" -> NameT("Object"),
            "ObjectWasVisited" -> BoolT,
            "VisitedKeys" -> ListT(StrT),
            "RemainingKeys" -> ListT(StrT),
          ),
        ),
        "AsyncFromSyncIteratorInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "SyncIteratorRecord" -> NameT("IteratorRecord"),
          ),
        ),
        "PromiseCapabilityRecord" -> TyInfo(
          fields = Map(
            "Promise" -> NameT("Object"),
            "Resolve" -> NameT("FunctionObject"),
            "Reject" -> NameT("FunctionObject"),
          ),
        ),
        "PromiseReaction" -> TyInfo(
          fields = Map(
            "Capability" -> (NameT("PromiseCapabilityRecord") || UndefT),
            "Type" -> (FULFILL || REJECT),
            "Handler" -> (NameT("JobCallbackRecord") || EMPTY),
          ),
        ),
        "PromiseInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "PromiseState" -> (PENDING || FULFILLED || REJECTED),
            "PromiseResult" -> ESValueT,
            "PromiseFulfillReactions" -> ListT(NameT("PromiseReaction")),
            "PromiseRejectReactions" -> ListT(NameT("PromiseReaction")),
            "PromiseIsHandled" -> BoolT,
          ),
        ),
        "GeneratorInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "GeneratorState" -> (
              UndefT ||
              SUSPENDED_START ||
              SUSPENDED_YIELD ||
              EXECUTING ||
              COMPLETED
            ),
            "GeneratorContext" -> NameT("ExecutionContext"),
            "GeneratorBrand" -> (StrT || EMPTY),
          ),
        ),
        "AsyncGeneratorInstance" -> TyInfo(
          parent = Some("OrdinaryObject"),
          fields = Map(
            "AsyncGeneratorState" -> (
              UndefT ||
              SUSPENDED_START ||
              SUSPENDED_YIELD ||
              EXECUTING ||
              AWAITING_RETURN ||
              COMPLETED
            ),
            "AsyncGeneratorContext" -> NameT("ExecutionContext"),
            "AsyncGeneratorQueue" -> ListT(
              NameT("AsyncGeneratorRequestRecord"),
            ),
            "GeneratorBrand" -> (StrT || EMPTY),
          ),
        ),
        "AsyncGeneratorRequestRecord" -> TyInfo(
          fields = Map(
            // TODO "Completion" -> (NormalT(_) || AbruptT),
            "Capability" -> NameT("PromiseCapabilityRecord"),
          ),
        ),

        // environment records
        "EnvironmentRecord" -> TyInfo(
          fields = Map(
            "OuterEnv" -> (NullT || NameT("EnvironmentRecord")),
            "SubMap" -> SubMapT(StrT, NameT("Binding")),
          ),
        ),
        "Binding" -> TyInfo(
          fields = Map(
            "BoundValue" -> ESValueT,
            "initialized" -> BoolT,
          ),
        ),
        "MutableBinding" -> TyInfo(parent = Some("Binding")),
        "ImmutableBinding" -> TyInfo(
          parent = Some("Binding"),
          fields = Map("strict" -> BoolT),
        ),
        "DeclarativeEnvironmentRecord" -> TyInfo(
          parent = Some("EnvironmentRecord"),
          methods = Map(
            "HasBinding" -> "DeclarativeEnvironmentRecord.HasBinding",
            "CreateMutableBinding" ->
            "DeclarativeEnvironmentRecord.CreateMutableBinding",
            "CreateImmutableBinding" ->
            "DeclarativeEnvironmentRecord.CreateImmutableBinding",
            "InitializeBinding" ->
            "DeclarativeEnvironmentRecord.InitializeBinding",
            "SetMutableBinding" ->
            "DeclarativeEnvironmentRecord.SetMutableBinding",
            "GetBindingValue" ->
            "DeclarativeEnvironmentRecord.GetBindingValue",
            "DeleteBinding" ->
            "DeclarativeEnvironmentRecord.DeleteBinding",
            "HasThisBinding" ->
            "DeclarativeEnvironmentRecord.HasThisBinding",
            "HasSuperBinding" ->
            "DeclarativeEnvironmentRecord.HasSuperBinding",
            "WithBaseObject" ->
            "DeclarativeEnvironmentRecord.WithBaseObject",
          ),
        ),
        "FunctionEnvironmentRecord" -> TyInfo(
          parent = Some("DeclarativeEnvironmentRecord"),
          methods = Map(
            "BindThisValue" -> "FunctionEnvironmentRecord.BindThisValue",
            "HasThisBinding" ->
            "FunctionEnvironmentRecord.HasThisBinding",
            "HasSuperBinding" ->
            "FunctionEnvironmentRecord.HasSuperBinding",
            "GetThisBinding" ->
            "FunctionEnvironmentRecord.GetThisBinding",
            "GetSuperBase" -> "FunctionEnvironmentRecord.GetSuperBase",
          ),
          fields = Map(
            "ThisValue" -> ESValueT,
            "ThisBindingStatus" -> (LEXICAL || INITIALIZED || UNINITIALIZED),
            "FunctionObject" -> NameT("FunctionObject"),
            "NewTarget" -> (NameT("Object") || UndefT),
          ),
        ),
        "ModuleEnvironmentRecord" -> TyInfo(
          parent = Some("DeclarativeEnvironmentRecord"),
          methods = Map(
            "GetBindingValue" -> "ModuleEnvironmentRecord.GetBindingValue",
            "HasThisBinding" ->
            "ModuleEnvironmentRecord.HasThisBinding",
            "GetThisBinding" ->
            "ModuleEnvironmentRecord.GetThisBinding",
            "CreateImportBinding" -> "ModuleEnvironmentRecord.CreateImportBinding",
          ),
        ),
        "ObjectEnvironmentRecord" -> TyInfo(
          parent = Some("EnvironmentRecord"),
          methods = Map(
            "HasBinding" -> "ObjectEnvironmentRecord.HasBinding",
            "CreateMutableBinding" ->
            "ObjectEnvironmentRecord.CreateMutableBinding",
            "InitializeBinding" ->
            "ObjectEnvironmentRecord.InitializeBinding",
            "SetMutableBinding" ->
            "ObjectEnvironmentRecord.SetMutableBinding",
            "GetBindingValue" ->
            "ObjectEnvironmentRecord.GetBindingValue",
            "DeleteBinding" -> "ObjectEnvironmentRecord.DeleteBinding",
            "HasThisBinding" -> "ObjectEnvironmentRecord.HasThisBinding",
            "HasSuperBinding" ->
            "ObjectEnvironmentRecord.HasSuperBinding",
            "WithBaseObject" -> "ObjectEnvironmentRecord.WithBaseObject",
          ),
          fields = Map(
            "IsWithEnvironment" -> BoolT,
            "BindingObject" -> NameT("Object"),
          ),
        ),
        "GlobalEnvironmentRecord" -> TyInfo(
          parent = Some("EnvironmentRecord"),
          methods = Map(
            "HasBinding" -> "GlobalEnvironmentRecord.HasBinding",
            "CreateMutableBinding" ->
            "GlobalEnvironmentRecord.CreateMutableBinding",
            "CreateImmutableBinding" ->
            "GlobalEnvironmentRecord.CreateImmutableBinding",
            "InitializeBinding" ->
            "GlobalEnvironmentRecord.InitializeBinding",
            "SetMutableBinding" ->
            "GlobalEnvironmentRecord.SetMutableBinding",
            "GetBindingValue" ->
            "GlobalEnvironmentRecord.GetBindingValue",
            "DeleteBinding" -> "GlobalEnvironmentRecord.DeleteBinding",
            "HasThisBinding" -> "GlobalEnvironmentRecord.HasThisBinding",
            "HasSuperBinding" ->
            "GlobalEnvironmentRecord.HasSuperBinding",
            "WithBaseObject" -> "GlobalEnvironmentRecord.WithBaseObject",
            "GetThisBinding" -> "GlobalEnvironmentRecord.GetThisBinding",
            "HasVarDeclaration" ->
            "GlobalEnvironmentRecord.HasVarDeclaration",
            "HasLexicalDeclaration" ->
            "GlobalEnvironmentRecord.HasLexicalDeclaration",
            "HasRestrictedGlobalProperty" ->
            "GlobalEnvironmentRecord.HasRestrictedGlobalProperty",
            "CanDeclareGlobalVar" ->
            "GlobalEnvironmentRecord.CanDeclareGlobalVar",
            "CanDeclareGlobalFunction" ->
            "GlobalEnvironmentRecord.CanDeclareGlobalFunction",
            "CreateGlobalVarBinding" ->
            "GlobalEnvironmentRecord.CreateGlobalVarBinding",
            "CreateGlobalFunctionBinding" ->
            "GlobalEnvironmentRecord.CreateGlobalFunctionBinding",
          ),
          fields = Map(
            // XXX "OuterEnv" -> NullT,
            "ObjectRecord" -> NameT("ObjectEnvironmentRecord"),
            "GlobalThisValue" -> NameT("Object"),
            "DeclarativeRecord" -> NameT("DeclarativeEnvironmentRecord"),
            "VarNames" -> ListT(StrT),
          ),
        ),

        // private environment record
        "PrivateEnvironmentRecord" -> TyInfo(
          fields = Map(
            "OuterPrivateEnvironment" -> (
              NameT("PrivateEnvironmentRecord") ||
              NullT
            ),
            "Names" -> ListT(NameT("PrivateName")),
          ),
        ),

        // job callback record
        "JobCallbackRecord" -> TyInfo(
          fields = Map(
            "Callback" -> NameT("FunctionObject"),
            "HostDefined" -> EMPTY,
          ),
        ),

        // agent record
        "AgentRecord" -> TyInfo(
          fields = Map(
            "LittleEndian" -> BoolT,
            "CanBlock" -> BoolT,
            "Signifier" -> NameT("AgentSignifier"),
            "IsLockFree1" -> BoolT,
            "IsLockFree2" -> BoolT,
            "IsLockFree8" -> BoolT,
            "CandidateExecution" -> NameT("CandidateExecutionRecord"),
            "KeptAlive" -> ListT(NameT("Object")),
          ),
        ),

        // script record
        "ScriptRecord" -> TyInfo(
          fields = Map(
            "Realm" -> (NameT("RealmRecord") || UndefT),
            "ECMAScriptCode" -> AstT("Script"),
            "HostDefined" -> EMPTY,
          ),
        ),

        // module record
        "ModuleRecord" -> TyInfo(
          fields = Map(
            "Realm" -> NameT("RealmRecord"),
            "Environment" -> (NameT("ModuleEnvironmentRecord") || EMPTY),
            "Namespace" -> (NameT("ModuleNamespaceExoticObject") || EMPTY),
            "HostDefined" -> UndefT,
          ),
        ),
        "CyclicModuleRecord" -> TyInfo(
          parent = Some("ModuleRecord"),
          methods = Map(
            "Link" -> "CyclicModuleRecord.Link",
            "Evaluate" -> "CyclicModuleRecord.Evaluate",
          ),
          fields = Map(
            "Status" ->
            (
              UNLINKED ||
              LINKING ||
              LINKED ||
              EVALUATING ||
              EVALUATING_ASYNC ||
              EVALUATED
            ),
            "EvaluationError" -> (AbruptT || EMPTY),
            "DFSIndex" -> (MathT || EMPTY),
            "DFSAncestorIndex" -> (MathT || EMPTY),
            "RequestedModules" -> ListT(StrT),
            "CycleRoot" -> (NameT("CyclicModuleRecord") || EMPTY),
            "HasTLA" -> BoolT,
            "AsyncEvaluation" -> BoolT,
            "TopLevelCapability" -> (
              NameT("PromiseCapabilityRecord") ||
              EMPTY,
            ),
            "AsyncParentModules" -> ListT(NameT("CyclicModuleRecord")),
            "PendingAsyncDependencies" -> (MathT || EMPTY),
          ),
        ),
        "SourceTextModuleRecord" -> TyInfo(
          parent = Some("CyclicModuleRecord"),
          methods = Map(
            "GetExportedNames" -> "SourceTextModuleRecord.GetExportedNames",
            "ResolveExport" -> "SourceTextModuleRecord.ResolveExport",
            "InitializeEnvironment" -> "SourceTextModuleRecord.InitializeEnvironment",
            "ExecuteModule" -> "SourceTextModuleRecord.ExecuteModule",
          ),
          fields = Map(
            "ECMAScriptCode" -> AstT("Module"),
            "Context" -> NameT("ExecutionContext"),
            "ImportMeta" -> (NameT("Object") || EMPTY),
            "ImportEntries" -> ListT(NameT("ImportEntryRecord")),
            "LocalExportEntries" -> ListT(NameT("ExportEntryRecord")),
            "IndirectExportEntries" -> ListT(NameT("ExportEntryRecord")),
            "StarExportEntries" -> ListT(NameT("ExportEntryRecord")),
          ),
        ),
        "ImportEntryRecord" -> TyInfo(
          fields = Map(
            "ModuleRequest" -> StrT,
            "ImportName" -> (StrT || NAMESPACE_OBJ),
            "LocalName" -> StrT,
          ),
        ),
        "ExportEntryRecord" -> TyInfo(
          fields = Map(
            "ExportName" -> (StrT || NullT),
            "ModuleRequest" -> (StrT || NullT),
            "ImportName" -> (StrT || NullT || ALL || ALL_BUT_DEFAULT),
            "LocalName" -> (StrT || NullT),
          ),
        ),
        "ResolvedBindingRecord" -> TyInfo(
          fields = Map(
            "Module" -> NameT("ModuleRecord"),
            "BindingName" -> (StrT || NAMESPACE),
          ),
        ),

        // symbol registry
        "GlobalSymbolRegistryRecord" -> TyInfo(
          fields = Map(
            "Key" -> StrT,
            "Symbol" -> SymbolT,
          ),
        ),

        // match record
        "MatchRecord" -> TyInfo(
          fields = Map(
            "StartIndex" -> MathT,
            "EndIndex" -> MathT,
          ),
        ),

        // pending job
        "PendingJob" -> TyInfo(
          fields = Map(
            "Job" -> CloT,
            "Realm" -> NameT("RealmRecord"),
            "ScriptOrModule" ->
            (NameT("ScriptRecord", "ModuleRecord") || NullT),
          ),
        ),
      ),
    )
  }
  // Uncomment to dump json
  // esmeta.util.SystemUtils.dumpJson(
  //   HardcodedTyModel.es,
  //   filename = "src/main/resources/manuals/default/tymodel.json",
  // )

  // this causes error
  assert(HardcodedTyModel.es == esmeta.ty.TyModel.es)
  init
}
