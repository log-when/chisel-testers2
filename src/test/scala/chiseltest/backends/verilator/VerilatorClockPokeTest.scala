// SPDX-License-Identifier: Apache-2.0
package chiseltest.backends.verilator

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.flatspec.AnyFlatSpec
import treadle2.executable.ClockInfo
import treadle2.{ClockInfoAnnotation, WriteVcdAnnotation}

class VerilatorClockPokeTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Testers2 with a clock input"

  it should "clock-as-bool-verilator-clock-poke" taggedAs RequiresVerilator ignore { // TODO: re-add multi clock support
    test(new Module {
      val inClock = IO(Input(Bool()))
      val out = IO(Output(UInt(8.W)))

      withClock(inClock.asClock) {
        out := Counter(true.B, 8)._1
      }
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      //      c.inClock.low()
      c.inClock.poke(false.B)
      c.out.expect(0.U)

      // Main clock should do nothing
      c.clock.step()
      c.out.expect(0.U)
      c.clock.step()
      c.out.expect(0.U)

      // Output should advance on rising edge, even without main clock edge
      c.inClock.poke(true.B)
      c.out.expect(1.U)
    }
  }
}
