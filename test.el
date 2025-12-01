;; -*- lexical-binding: t; -*-

;; SDPX-License-Identifier:  GPL-3.0-only
;; SPDX-FileCopyrightText: (c) 2025 Hufoe <foss@hufoe.com>
;; SPDX-FileCopyrightText: (c) 2025 Antero Mejr <mail@antr.me>

(require 'cntlpanel)
(require 'ert)

(ert-deftest test-edid-get-name ()
  (let ((edid-str "00ffffffffffff0006b3682786d103002b1e0104b53c22783b1c95a75549a2260f5054230800d1c0814081809500b30081c0010101014dd000a0f0703e803020350055502100001aa36600a0f0701f803020350055502100001a000000fd00283ca0a03c010a202020202020000000fc00415355532050413237390a20200109020336714e0102031213040e0f1d1e1f9060612309170783010000e2006a681a00000101283c00e305e301e30f0030e606070161561c4dd000a0f0703e803020350055502100001a565e00a0a0a029503020350055502100001a023a801871382d40582c450055502100001e4d6c80a070703e8030203a0055502100001a002e"))
    (should (equal (cntlpanel--parse-edid edid-str)
                   "ASUS PA279"))))

(ert-deftest test-edid-parse-xrandr ()
  (let ((xrandr-str "
DP-2 connected primary 3840x2160+0+0 (normal left inverted right x axis y axis) 600mm x 340mm
        _KDE_SCREEN_INDEX: 1
        EDID:
                00ffffffffffff001e6dc25b00c90300
                0a220104b53c2278fa40b5ae5142ad26
                0f5054210800d1c06140010101010101
                01010101010100d200a0f07050803020
                350058542100001a000000fd00283c1e
                873c000a202020202020000000fc004c
                4720554c54524146494e450a000000ff
                003431304e54414237413036340a0166
                02031f72230907078301000044010304
                10e2006ae305c000e6060501606050a3
                6600a0f0701f80302035005854210000
                1a565e00a0a0a0295030203500585421
                00001a023a801871382d40582c450058
                542100001a0000000000000000000000
                00000000000000000000000000000000
                00000000000000000000000000000033
DP-3 connected (normal left inverted right x axis y axis)"))
    (let  ((monitors (cntlpanel--parse-xrandr xrandr-str)))
      (should (equal (cntlpanel--monitor-primary? (cl-first monitors))
                     t))
      (should (equal (cntlpanel--monitor-disabled? (cl-second monitors))
                     t))
      (should (equal (cntlpanel--monitor-offsetx (cl-first monitors))
                     0))
      (should (equal (cntlpanel--monitor-name (cl-first monitors))
                     "LG ULTRAFINE")))))
