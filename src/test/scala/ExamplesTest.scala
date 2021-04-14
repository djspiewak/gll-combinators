/*
 * Copyright (c) 2021, Daniel Spiewak
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import org.specs2.ScalaCheck
import org.specs2.mutable._

class ExamplesSpecs extends Specification with ScalaCheck {

  "Examples" should {
    "pass arithmetic test" in {
      arithmetic.ArithmeticParser.test("/arithmetic/error.txt") mustEqual false
      arithmetic.ArithmeticParser.test("/arithmetic/junk.txt") mustEqual true
      arithmetic.ArithmeticParser.test("/arithmetic/taylor-sin.txt") mustEqual true
    }

    "pass config test" in {
      config.ConfigParser.test("/config/input1.conf") mustEqual true
    }

    "pass lambdacalc test" in {
      lambdacalc.LambdaCalcParser.test("/lambdacalc/church.f") mustEqual true
      lambdacalc.LambdaCalcParser.test("/lambdacalc/fac.f") mustEqual true
    }

    "pass miniml test" in {
      miniml.MiniMLParser.test("/miniml/arity-clash.sml") mustEqual true
      miniml.MiniMLParser.test("/miniml/example1.sml") mustEqual true
      miniml.MiniMLParser.test("/miniml/example2.sml") mustEqual true
      miniml.MiniMLParser.test("/miniml/example4.sml") mustEqual true
    }

    "pass paren test" in {
      paren.ParenParser.test("/paren/error.txt") mustEqual false
      paren.ParenParser.test("/paren/input1.txt") mustEqual true
      paren.ParenParser.test("/paren/input2.txt") mustEqual true
    }
  }
}
