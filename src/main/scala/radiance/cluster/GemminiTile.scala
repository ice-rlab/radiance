// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package radiance.cluster

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, TransferSizes}
import freechips.rocketchip.prci.{ClockCrossingType, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.{CanAttachTile, HierarchicalElementCrossingParamsLike, RocketCrossingParams}
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf
import gemmini.Arithmetic._
import gemmini._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.DisableMonitors
import org.chipsalliance.diplomacy.lazymodule._
import radiance.memory._
import radiance.subsystem.{GPUMemParams, GPUMemory, GemminiTileLike, PhysicalCoreParams}
import midas.targetutils.PerfCounter

object GemminiCoreParams extends PhysicalCoreParams {
  override val xLen: Int = 64
}

final case class MxGemminiTileParams(
  tileId: Int = 0,
  mxGemminiConfig: GemminiArrayConfig[MxFloat, Float, Float],
  tileSize: Either[(Int, Int, Int), Int] = Right(4),
  mmioAddress: BigInt,
  override val scalingFactorMem: Option[GemminiScalingFactorMemConfig],
  override val requantizer: Option[GemminiRequantizerConfig],
  override val lookupTable: Option[GemminiLUTConfig],
) extends GemminiTileParams {
  type T = MxFloat
  type U = Float
  type V = Float
  override val gemminiConfig = mxGemminiConfig.copy(
    scale_mem = scalingFactorMem,
    requantizer = requantizer,
    lut = lookupTable
  )
  override val arithmetic = MxFloatArithmetic
}

final case class FloatGemminiTileParams(
  tileId: Int = 0,
  gemminiConfig: GemminiArrayConfig[Float, Float, Float],
  tileSize: Either[(Int, Int, Int), Int] = Right(4),
  mmioAddress: BigInt,
) extends GemminiTileParams {
  type T = Float
  type U = Float
  type V = Float
  override val arithmetic = FloatArithmetic
}

abstract class GemminiTileParams extends InstantiableTileParams[GemminiTile] {
  type T <: Data
  type U <: Data
  type V <: Data
  implicit val arithmetic: Arithmetic[T]
  val gemminiConfig: GemminiArrayConfig[T, U, V]
  val tileSize: Either[(Int, Int, Int), Int]
  val mmioAddress: BigInt
  val scalingFactorMem: Option[GemminiScalingFactorMemConfig] = None
  val requantizer: Option[GemminiRequantizerConfig] = None
  val lookupTable: Option[GemminiLUTConfig] = None
  val hasAccSlave: Boolean = false

  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(
      implicit p: Parameters
  ): GemminiTile = {
    new GemminiTile(this, crossing, lookup)
  }
  val core = GemminiCoreParams
  val name = Some(f"radiance_gemmini_tile_$tileId")
  val clockSinkParams = ClockSinkParameters()
  val blockerCtrlAddr = None
  val icache = None
  val dcache = Some(DCacheParams(
    nSets = 1, nWays = 1, nMSHRs = 0,
    nTLBSets = 0, nTLBWays = 1
  ))
  val btb = None
  val baseName = name.get
  val uniqueName = s"${baseName}_$tileId"
}

case class GemminiTileAttachParams(
  tileParams: GemminiTileParams,
  crossingParams: RocketCrossingParams,
) extends CanAttachTile { type TileType = GemminiTile }

class GemminiTile private (
    val gemminiParams: GemminiTileParams,
    crossing: ClockCrossingType,
    lookup: LookupByHartIdImpl,
    q: Parameters
) extends BaseTile(gemminiParams, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    with GemminiTileLike {

  def this(params: GemminiTileParams, crossing: HierarchicalElementCrossingParamsLike,
      lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val cpuDevice: SimpleDevice = new SimpleDevice(s"gemmini${tileId}", Nil) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(
        name,
        mapping ++ cpuProperties ++ nextLevelCacheProperty ++ tileProperties
      )
    }
  }


  val intOutwardNode = None
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode
  // val statusNode = BundleBridgeSource(() => new GroundTestStatus)

  val accSlaveNode = Option.when(gemminiParams.hasAccSlave)(AccNode.Slave())

  tlOtherMastersNode := tlMasterXbar.node
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  // TODO: evaluate if gemmini write node is required at all

  implicit val arithmetic = gemminiParams.arithmetic
  val gemmini = LazyModule(new Gemmini(gemminiParams.gemminiConfig))
  val base = p(GPUMemory) match {
    case Some(GPUMemParams(baseAddr, _)) => baseAddr
    case _ => BigInt(0)
  }
  // tlMasterXbar.node :=* AddressOrNode(base) :=* gemmini.atlNode
  // tlOtherMastersNode :=* AddressOrNode(base) :=* gemmini.tlNode
  tlMasterXbar.node :=* gemmini.atlNode
  tlOtherMastersNode :=* gemmini.tlNode
  // gemmini.stlNode := tlSlaveXbar.node

  override def connectTLSlave(node: TLNode, bytes: Int): Unit = {}
  override def connectTLSlave(xbarNode: TLOutwardNode, node: TLNode, bytes: Int): Unit = {}

  require(!gemmini.config.sp_singleported, "external scratchpad must be dual ported")

  val regDevice = new SimpleDevice(f"gemmini-cmd-reg-$tileId", Seq(s"gemmini-cmd-reg-$tileId"))
  val regNode = TLRegisterNode(
    address = Seq(AddressSet(gemminiParams.mmioAddress, 0xfff)),
    device = regDevice,
    beatBytes = 8,
    concurrency = 1)

  // regNode := TLFragmenter(4, 4) := TLWidthWidget(8) := TLFragmenter(8, 8) := slaveNode
  regNode := tlSlaveXbar.node

  val scalingFacManager = gemminiParams.scalingFactorMem.map { sfm =>
    // since gemmini slave address starts at 0x3000, +0x5000 means
    // the scaling factor memory starts 0x8000 + shared mem size,
    // which is usually 0x28000 after cluster base address. this address is
    // 32K aligned.

    require(isPow2(sfm.sizeInBytes), "scaling fac memory size must be power of 2")

    TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v2(
        address = Seq(AddressSet(sfm.baseAddr, sfm.sizeInBytes - 1)),
        fifoId = Some(0),
        supports = TLMasterToSlaveTransferSizes(
          // there's no real get support because the scaling factor memory is
          // write-only from the control bus
          get = TransferSizes(1, 8),
          putFull = TransferSizes(1, 8),
          putPartial = TransferSizes(1, 8),
        )
      )),
      beatBytes = 8,
    )))
  }
  scalingFacManager.foreach(_
    := FlitMergeNode(from = 4, to = 8)
    := TLWidthWidget(8)
    := tlSlaveXbar.node)

  val scalingFacClient = gemminiParams.scalingFactorMem.map { sfm =>
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v2(
        name = "gemmini-scaling-fac-client",
        sourceId = IdRange(0, 1 << 4), // TODO: magic number
        emits = TLMasterToSlaveTransferSizes(
          putFull = TransferSizes(sfm.bankWidthBytes, sfm.bankWidthBytes)
        )
      ))
    )))
  }

  val requantizerMuonManager = gemminiParams.requantizer.map { q =>
    val gemminiSpadSizeBytes = gemminiParams.gemminiConfig.sp_capacity
      .asInstanceOf[CapacityInKilobytes].kilobytes * 1024
    require(isPow2(gemminiSpadSizeBytes))
    val reqSize = q.numGPUInputLanes * q.inputBits / 8

    println("quant manager address set")
    println(AddressSet(q.baseAddr, gemminiSpadSizeBytes * q.gpuMaxFactor - 1))
    TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v2(
        address = Seq(AddressSet(q.baseAddr, gemminiSpadSizeBytes * q.gpuMaxFactor - 1)),
        fifoId = Some(0),
        // also real no get support
        supports = TLMasterToSlaveTransferSizes(
          get = TransferSizes(reqSize, reqSize),
          putFull = TransferSizes(reqSize, reqSize),
          putPartial = TransferSizes(reqSize, reqSize),
        )
      )),
      beatBytes = reqSize,
    )))
  }

  // width is max output width & num output lanes
  val requantizerSmemClient = gemminiParams.requantizer.map { q =>
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v2(
        name = "requantizer_out",
        sourceId = IdRange(0, 1 << q.outputIdBits),
        emits = TLMasterToSlaveTransferSizes(
          putFull = TransferSizes(q.numOutputLanes * q.minOutputBits / 8,
            q.numOutputLanes * q.maxOutputBits / 8),
          putPartial = TransferSizes(q.numOutputLanes * q.minOutputBits / 8,
            q.numOutputLanes * q.maxOutputBits / 8)
        )
      ))
    )))
  }

  override lazy val module = new GemminiTileModuleImp(this)
}

class GemminiTileModuleImp(outer: GemminiTile) extends BaseTileModuleImp(outer) {

  { // tie off rocc
    val gemmini_io = outer.gemmini.module.io
    gemmini_io.ptw <> DontCare
    gemmini_io.mem <> DontCare
    gemmini_io.resp <> DontCare
    gemmini_io.fpu_req.ready := false.B
    gemmini_io.fpu_resp.valid := false.B
    gemmini_io.fpu_resp.bits := DontCare
    gemmini_io.exception := DontCare
  }

  // scaling factor
  outer.scalingFacManager.foreach { scalingFacNode =>
    val conf = outer.gemminiParams.scalingFactorMem.get
    val (node, edge) = scalingFacNode.in.head

    val wen = WireInit(node.a.fire)
    val writeData = WireInit(node.a.bits.data)

    val typeSelect = node.a.bits.address(conf.addrBits - 1)
    val writeFullAddr = node.a.bits.address(conf.addrBits - 2, 0)

    // weight and activation respectively; weight addresses have highest bit = 0
    val scalingFacWriteReqs = Seq.fill(2)(Wire(Decoupled(new ScalingFactorWriteReq(
      conf.addrBits - 1, 8 * 8))))
    scalingFacWriteReqs.head.valid := wen && !typeSelect
    scalingFacWriteReqs.last.valid := wen && typeSelect
    scalingFacWriteReqs.foreach(_.bits.addr := writeFullAddr)
    scalingFacWriteReqs.foreach(_.bits.data := writeData)

    val typeReady = Mux(typeSelect, scalingFacWriteReqs.last.ready, scalingFacWriteReqs.head.ready)

    node.a.ready := node.d.ready && typeReady
    node.d.valid := node.a.valid && typeReady
    node.d.bits := edge.AccessAck(node.a.bits)

    // require(node.params.dataBits == conf.bankWidthBytes * 8)
    assert(!node.a.valid || node.a.bits.opcode.isOneOf(TLMessages.PutFullData, TLMessages.PutPartialData))
    assert(!node.a.valid || (node.a.bits.size === 3.U))

    outer.gemmini.module.mx_io.get.scale_mem_write_w <> scalingFacWriteReqs.head
    outer.gemmini.module.mx_io.get.scale_mem_write_act <> scalingFacWriteReqs.last
  }

  outer.scalingFacClient.foreach { scalingFacNode =>
    val (node, edge) = scalingFacNode.out.head
    val out = outer.gemmini.module.mx_io.get.scale_factor_out

    node.a.bits := edge.Put(
      fromSource = 0.U, // overridden
      toAddress = out.bits.addr,
      lgSize = log2Ceil(node.params.dataBits / 8).U,
    data = out.bits.data,
    )._2

    val (sourceReady, _) = SourceGenerator(node)
    out.ready := node.a.ready && sourceReady
    node.a.valid := out.valid && sourceReady
    node.d.ready := true.B
  }

  // requantizer
  outer.gemminiParams.requantizer.foreach { q =>
    val in = Wire(Decoupled(new RequantizerInBundle(q.numGPUInputLanes, q.inputBits)))
    val out = Wire(Decoupled(new RequantizerOutBundle(q.numOutputLanes, q.maxOutputBits)))

    { // input
      val (node, edge) = outer.requantizerMuonManager.get.in.head

      node.d.valid := node.a.valid && in.ready // ensures a.fire <=> d.fire
      node.d.bits := edge.AccessAck(node.a.bits)
      node.a.ready := node.d.ready && in.ready

      assert(node.a.fire === node.d.fire)

      in.valid := node.a.valid
      in.bits.dataType := RequantizerDataType.FP8
      // GPU input word bytes -> output byte address: inputBits/8 bytes per element (was a hardcoded >> 1, 16->8)
      in.bits.address := ((node.a.bits.address - q.baseAddr.U) >> log2Ceil(q.inputBits / 8)).asTypeOf(in.bits.address)
      in.bits.data := node.a.bits.data.asTypeOf(in.bits.data)

      require(in.bits.data.asUInt.getWidth == node.a.bits.data.getWidth)
      assert(!node.a.valid || ((node.a.bits.address & q.baseAddr.U) === q.baseAddr.U))
    }

    { // output
      val (node, edge) = outer.requantizerSmemClient.get.out.head

      // data
      val isFP4 = out.bits.dataType === RequantizerDataType.FP4
      val fullWidth = q.numOutputLanes
      val halfWidth = q.numOutputLanes / 2
      node.a.bits := edge.Put(
        fromSource = 0.U, // gets overridden
        toAddress = out.bits.address,
        lgSize = Mux(isFP4,
          log2Ceil(halfWidth).U, // half byte per lane
          log2Ceil(q.numOutputLanes).U // fp6, fp8: 1 byte per lane
        ),
        data = Mux(isFP4,
          Mux(
            out.bits.address(log2Ceil(halfWidth)), // not aligned to full line
            (out.bits.data(halfWidth - 1, 0) << halfWidth).asTypeOf(UInt(fullWidth.W)),
            out.bits.data
          ),
          out.bits.data
        )
      )._2

      // source
      val (sourceReady, _) = SourceGenerator(node)
      out.ready := node.a.ready && sourceReady
      node.a.valid := out.valid && sourceReady
      assert(out.fire === node.a.fire)
      node.d.ready := true.B
    }

    outer.gemmini.module.mx_io.get.requant_in_gpu <> in
    outer.gemmini.module.mx_io.get.requant_out <> out
  }

  // lut
  val lutIO = outer.gemminiParams.lookupTable.map { c =>
    val lut = Seq.tabulate(c.numTables) { i =>
      Wire(Decoupled(new QuantLutWriteBundle(c(i))))
    }
    val mxIO = outer.gemmini.module.mx_io.get
    (lut zip Seq(mxIO.lut0, mxIO.lut1, mxIO.lut2)).foreach { case (a, b) => a <> b }
    lut
  }

  // cisc
  val cisc = new GemminiCISC(
    accSlave = outer.accSlaveNode,
    busy = outer.gemmini.module.io.busy,
    spadRows = outer.gemminiParams.gemminiConfig.sp_bank_entries *
      outer.gemminiParams.gemminiConfig.sp_banks,
    tileParams = outer.gemminiParams)

  // mmio
  val gemminiIO = outer.gemmini.module.io.cmd

  val regValid = Wire(Bool())
  val regCommand = Wire(gemminiIO.bits.inst.cloneType)
  val gemminiRs1RegLSB = RegInit(0.U(32.W))
  val gemminiRs1RegMSB = RegInit(0.U(32.W))
  val gemminiRs2RegLSB = RegInit(0.U(32.W))
  val gemminiRs2RegMSB = RegInit(0.U(32.W))

  // --- Configuration-Wall / G2Stack instrumentation strobes ---
  // One-cycle pulses marking each configuration-register write, so the
  // GCStack-style classifier below can see the "Configure" phase traffic that
  // the plain RegFields would otherwise absorb silently.
  val rs1WrStrobe  = WireDefault(false.B)
  val rs2WrStrobe  = WireDefault(false.B)
  val ciscWrStrobe = WireDefault(false.B)
  val lutWrStrobe  = WireDefault(false.B)

  def gemminiCommandReg(valid: Bool, bits: UInt): Bool = {
    regValid := valid
    regCommand := bits.asTypeOf(regCommand)
    gemminiIO.ready && !cisc.ciscValid
  }

  // Latch a configuration register and pulse `strobe` on each write, so the
  // Configuration-Wall instrumentation can observe Configure-phase traffic.
  // Datapath behaviour matches the plain RegField.w(reg) it replaces
  // (latch on write, always ready).
  def stagingReg(reg: UInt, strobe: Bool)(valid: Bool, bits: UInt): Bool = {
    when (valid) {
      reg := bits
      strobe := true.B
    }
    true.B
  }

  def gemminiBusyReg(_dReady: Bool): (Bool, UInt) = {
    // (aReady, bits)
    // (!outer.gemmini.module.io.busy, outer.gemmini.module.io.busy.asUInt)
    (true.B, outer.gemmini.module.io.busy.asUInt)
  }

  val loopStarted = Mux(cisc.startsLoop, 1.U, 0.U)
  val mmioLoopStarted = Mux(regValid && (regCommand.funct === GemminiISA.LOOP_WS), 1.U, 0.U)
  val runningLoops = RegInit(0.U(4.W))
  val completionCount = PopCount(outer.gemmini.module.completion_io.completed)
  runningLoops := runningLoops + loopStarted + mmioLoopStarted - completionCount
  assert(runningLoops + loopStarted + mmioLoopStarted >= completionCount)

  def gemminiRunningLoopsReg(_dReady: Bool): (Bool, UInt) = {
    (true.B, runningLoops)
  }

  val gemminiBaseMMIO = Seq(
    0x00 -> Seq(RegField.w(32, gemminiCommandReg(_, _))),
    0x08 -> Seq(RegField.r(32, gemminiIO.ready)),
    0x10 -> Seq(
      RegField.w(32, stagingReg(gemminiRs1RegLSB, rs1WrStrobe)(_, _)),
      RegField.w(32, stagingReg(gemminiRs1RegMSB, rs1WrStrobe)(_, _))),
    0x18 -> Seq(
      RegField.w(32, stagingReg(gemminiRs2RegLSB, rs2WrStrobe)(_, _)),
      RegField.w(32, stagingReg(gemminiRs2RegMSB, rs2WrStrobe)(_, _))),
    0x20 -> Seq(RegField.r(32, gemminiBusyReg(_))),
    0x28 -> Seq(RegField.r(32, gemminiRunningLoopsReg(_))),
  )

  val gemminiCiscMMIO = Option.when(!outer.gemminiParams.hasAccSlave) {
    def gemminiCisc(valid: Bool, bits: UInt): Bool = {
      cisc.accCommandQueue.io.enq.bits := bits
      cisc.accCommandQueue.io.enq.valid := valid
      when (valid) { ciscWrStrobe := true.B }
      true.B
    }
    0x30 -> Seq(RegField.w(32, gemminiCisc(_, _)))
  }.toSeq

  val gemminiLutMMIO = lutIO.map { luts =>
    val config = outer.gemminiParams.lookupTable.get
    require(luts.length == config.numTables)

    val tableBase = 0x80
    val tableSizes = (config.numEntries zip config.numBits).map(x => x._1 * x._2 / 8)
    val tableOffsets = tableSizes.scanLeft(tableBase)(_ + _)

    val flops = RegInit(MixedVecInit(
      (config.numEntries zip config.numBits).map { case (numEntries, numBits) =>
        assert(numBits % 32 == 0)
        VecInit.fill(numEntries)(VecInit.fill(numBits / 32)(0.U(32.W)))
      }
    ))

    val maps = luts.zipWithIndex.flatMap { case (io, i) =>
      io.valid := false.B
      io.bits.data := VecInit(flops(i).map(_.asUInt))

      def lutRegFunc(reg: UInt, trigger: Boolean = false): (Bool, UInt) => Bool = {
        def lut(valid: Bool, bits: UInt): Bool = {
          when (io.ready && valid) {
            reg := bits
            lutWrStrobe := true.B
          }
          if (trigger) {
            // this assumes ready never lowers without a fire first
            io.valid := RegNext(io.ready && valid)
          }
          io.ready
        }
        lut
      }

      Seq.tabulate(config.numEntries(i)) { j =>
        val wordsPerEntry = config.numBits(i) / 32
        Seq.tabulate(wordsPerEntry) { k =>
          val addr = tableOffsets(i) + (wordsPerEntry * j + k) * 4
          val trigger = (j + 1 == config.numEntries(i)) && (k + 1 == wordsPerEntry) // trigger per table

          addr -> Seq(RegField.w(32, lutRegFunc(flops(i)(j)(k), trigger)))
        }
      }.flatten
    }

    maps
  }.toSeq.flatten

  outer.regNode.regmap(
    (gemminiBaseMMIO ++ gemminiCiscMMIO ++ gemminiLutMMIO):_*
  )

  // assert(!regValid || gemminiIO.ready)
  // assert(!cisc.ciscValid || gemminiIO.ready)

  gemminiIO.bits.status := 0.U.asTypeOf(gemminiIO.bits.status)
  gemminiIO.bits.inst := Mux(cisc.ciscValid,
    cisc.ciscInst.inst.asTypeOf(gemminiIO.bits.inst), regCommand)
  gemminiIO.bits.rs1 := Mux(cisc.ciscValid,
    cisc.ciscInst.rs1, Cat(gemminiRs1RegMSB, gemminiRs1RegLSB))
  gemminiIO.bits.rs2 := Mux(cisc.ciscValid,
    cisc.ciscInst.rs2, Cat(gemminiRs2RegMSB, gemminiRs2RegLSB))
  gemminiIO.valid := (cisc.ciscValid && (cisc.ciscInst.inst =/= 0.U)) || regValid
  // assert(gemminiIO.ready || !gemminiIO.valid)

  // ===========================================================================
  // Configuration-Wall cycle-stall accounting ("G2Stack")
  //
  // GCStack-style per-cycle classifier (cf. Muon Backend.scala): every cycle
  // the Gemmini command engine lands in exactly ONE bucket, so -- normalized by
  // local_cycle in the post-run hook -- the five fractions sum to 1.00. Buckets
  // follow the Configuration Wall model's Configure / Launch / Await phases
  // (Van Delm et al., ASPLOS'26). `memData` (data-movement stall) is split out
  // of `compute` using GemminiModule.perf_io.mem_stall, threaded up from the
  // submodule -- the paper (Sec 2.3) treats data movement as orthogonal to
  // configuration, so it must be its own bucket.
  //
  // Priority (top wins). memData is checked FIRST so that any exposed memory
  // time -- including a load's in-flight response latency, when the mesh is
  // idle waiting for data -- is attributed to memData and never leaks into
  // `idle` or `compute`:
  //   mem_stall                -> memData    (DMA in progress while mesh not computing)
  //   busy                     -> compute    (mesh running; overlapped mem/config hidden)
  //   cmd offered but rejected -> awaitStall (sequential-config wall: 0x00 store blocked)
  //   config-register traffic  -> configWrite (Configure phase, exposed)
  //   work command committed    -> launch     (Launch phase, exposed)
  //   otherwise                -> idle       (engine unfed by Muon)
  // ===========================================================================
  {
    val busy        = outer.gemmini.module.io.busy
    // memData = all exposed memory-communication time: a load/store transfer is
    // in progress (incl. in-flight response latency) while the mesh isn't
    // computing. Subsumes TLB-miss, XactTracker-full, TileLink backpressure,
    // and DMA response latency; excludes memory hidden behind compute.
    val memStall      = outer.gemmini.module.perf_io.mem_stall
    val loadMemStall  = outer.gemmini.module.perf_io.load_mem_stall
    val storeMemStall = outer.gemmini.module.perf_io.store_mem_stall
    val cmdOffered  = gemminiIO.valid
    val cmdReady    = gemminiIO.ready
    val isConfigCmd = regValid && (regCommand.funct === GemminiISA.CONFIG_CMD)
    val isWorkCmd   = regValid && (regCommand.funct =/= GemminiISA.CONFIG_CMD)
    val configTraffic = rs1WrStrobe || rs2WrStrobe || ciscWrStrobe || lutWrStrobe || isConfigCmd

    val ggBucket = WireDefault(0.U(3.W)) // 0 = idle
    when      (memStall)                { ggBucket := 5.U } // memData
    .elsewhen (busy)                    { ggBucket := 1.U } // compute
    .elsewhen (cmdOffered && !cmdReady) { ggBucket := 4.U } // awaitStall
    .elsewhen (configTraffic)           { ggBucket := 2.U } // configWrite
    .elsewhen (isWorkCmd)               { ggBucket := 3.U } // launch

    // GCStack-style stall stack (mutually exclusive; raw counts sum to local_cycle).
    midas.targetutils.PerfCounter(ggBucket === 0.U, "ggstack_idle",        "gemmini: engine idle, unfed by Muon")
    midas.targetutils.PerfCounter(ggBucket === 1.U, "ggstack_compute",     "gemmini: busy doing useful compute")
    midas.targetutils.PerfCounter(ggBucket === 2.U, "ggstack_configwrite", "gemmini: exposed Configure-phase register writes")
    midas.targetutils.PerfCounter(ggBucket === 3.U, "ggstack_launch",      "gemmini: exposed Launch (work command committed)")
    midas.targetutils.PerfCounter(ggBucket === 4.U, "ggstack_awaitstall",  "gemmini: sequential-config wall (0x00 store blocked)")
    midas.targetutils.PerfCounter(ggBucket === 5.U, "ggstack_memdata",     "gemmini: busy but stalled on mvin/mvout memory comms")

    // Ground-truth total in the SAME clock domain as the six buckets above:
    // by construction Sum(ggstack_{idle,compute,configwrite,launch,awaitstall,
    // memdata}) === ggstack_total_cycles, every cycle, so the post-run hook can
    // divide each bucket by this and get fractions that sum to exactly 1.00
    // -- independent of local_cycle's reset/clock domain. This counts every
    // tile cycle (the tile is not clock-gated; only Gemmini's internals are).
    // PerfCounter's annotation needs a named hardware node -- a bare `true.B`
    // literal has no FIRRTL target ("Illegal component name ... literals are
    // illegal"), so give it one via a WireDefault.
    val alwaysTrue = WireDefault(true.B)
    midas.targetutils.PerfCounter(alwaysTrue, "ggstack_total_cycles", "gemmini tile: total classified cycles (== sum of ggstack_* buckets)")

    // Free-running configuration-volume counters (NOT part of the stack; they
    // fire even when overlapped behind compute) for the roofline metrics:
    //   N_config_bytes = 8*(rs1_writes + rs2_writes) + CISC/LUT volume
    //   I_OC           = accelerator_ops / N_config_bytes
    // NOTE: rs1/rs2 are each 8 bytes wide but split across two 32-bit
    // RegFields (LSB @ +0x00, MSB @ +0x04), both wired to the SAME strobe --
    // so each *counts a 32-bit word write*, not a full 64-bit rs1/rs2 write.
    // (Confirmed empirically: rs1_word_writes == 2x cmd_fires on real runs,
    // i.e. LSB and MSB land as separate TileLink beats.) Byte accounting
    // below uses 4 bytes/pulse accordingly.
    midas.targetutils.PerfCounter(rs1WrStrobe,    "ggcfg_rs1_word_writes", "gemmini: rs1 32b word writes (x4 bytes each; 2 per rs1 store)")
    midas.targetutils.PerfCounter(rs2WrStrobe,    "ggcfg_rs2_word_writes", "gemmini: rs2 32b word writes (x4 bytes each; 2 per rs2 store)")
    midas.targetutils.PerfCounter(ciscWrStrobe,   "ggcfg_cisc_writes",   "gemmini: CISC compressed-config writes (0x30)")
    midas.targetutils.PerfCounter(lutWrStrobe,    "ggcfg_lut_writes",    "gemmini: LUT config-table word writes (0x80+)")
    midas.targetutils.PerfCounter(isConfigCmd,    "ggcfg_config_cmds",   "gemmini: CONFIG_CMD launches (funct==0)")
    midas.targetutils.PerfCounter(isWorkCmd,      "ggcfg_work_launches", "gemmini: work-command launches (funct!=0)")
    midas.targetutils.PerfCounter(gemminiIO.fire, "ggcfg_cmd_fires",     "gemmini: commands accepted into the reservation station")

    // Per-path memData breakdown (diagnostic; overlaps the memData bucket).
    midas.targetutils.PerfCounter(loadMemStall,   "ggmem_load_stall",    "gemmini: mvin/load DMA stalled (TLB/xact/TL backpressure)")
    midas.targetutils.PerfCounter(storeMemStall,  "ggmem_store_stall",   "gemmini: mvout/store DMA stalled (TLB/xact/TL backpressure)")

    val activeMacs = outer.gemmini.module.perf_io.active_macs
    val nPEs = outer.gemminiParams.gemminiConfig.DIM * outer.gemminiParams.gemminiConfig.DIM

    midas.targetutils.PerfCounter(activeMacs, "ggmac_active_macs",
      "gemmini: sum of active PE MACs per cycle (integrated MAC-cycles; /(DIM^2*cycles) = utilization)")
   
  }

  outer.traceSourceNode.bundle := DontCare
  outer.traceSourceNode.bundle.insns foreach (_.valid := false.B)

  // hacky, but cluster will AND the cease signals from all tiles, and we want
  // the core tiles to determine cluster cease not Gemmini
  outer.reportCease(Some(true.B))
}
