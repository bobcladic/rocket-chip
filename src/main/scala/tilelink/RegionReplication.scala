// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

case object MultiChipMaskKey extends Field[BigInt](0)
case object MultiChipIdKey extends Field(BundleBridgeEphemeralNode[UInt]()(ValName("multi_chip_id")))

trait HasRegionReplicatorParams {
  val replicatorMask: BigInt
}

// Replicate all devices below this adapter that are inside replicationRegion to multiple addreses based on mask.
// If a device was at 0x4000-0x4fff and mask=0x10000, it will now be at 0x04000-0x04fff and 0x14000-0x14fff.
class RegionReplicator(mask: BigInt = 0, region: Option[AddressSet] = Some(AddressSet.everything))(implicit p: Parameters) extends LazyModule {
  val ids = AddressSet.enumerateMask(mask)
  val bits = AddressSet.enumerateBits(mask)

  val node = TLAdapterNode(
    clientFn  = { cp => cp },
    managerFn = { mp => mp.copy(managers = mp.managers.map { m =>
      m.copy(address = m.address.flatMap { a =>
        if (region.map(_.contains(a)).getOrElse(false)) { ids.map { id => AddressSet(a.base | id, a.mask) } }
        else { Seq(a) }
      })
    })}
  )

  val chip_id = BundleBridgeSink[UInt]()
  chip_id := p(MultiChipIdKey)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Which address within the mask routes to local devices?
      val local_address = (bits zip chip_id.bundle.asBools).foldLeft(0.U) {
        case (acc, (bit, sel)) => acc | Mux(sel, bit.U, 0.U)
      }

      val a_addr = in.a.bits.address
      val a_contained = region.foldLeft(false.B)(_ || _.contains(a_addr))
      out.a.bits.address := Mux(a_contained, ~(~a_addr | mask.U), a_addr)

      val b_addr = out.b.bits.address
      val b_contained = region.foldLeft(false.B)(_ || _.contains(b_addr))
      in.b.bits.address := Mux(b_contained, b_addr | local_address, b_addr)

      val c_addr = in.c.bits.address
      val c_contained = region.foldLeft(false.B)(_ || _.contains(c_addr))
      out.c.bits.address := Mux(c_contained, ~(~c_addr | mask.U), c_addr)
    }
  }
}

object RegionReplicator {
  def apply(mask: BigInt = 0, region: Option[AddressSet] = Some(AddressSet.everything))(implicit p: Parameters): TLNode = {
    val replicator = LazyModule(new RegionReplicator(mask, region))
    replicator.node
  }
}
