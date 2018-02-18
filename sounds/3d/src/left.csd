<CsoundSynthesizer>
<CsOptions>
-o left.wav
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1

instr 1 

kcps    expon p5, p3, p4
asig	oscil3 0.3, kcps, 1
krvt =  3.5
ilpt =  0.1
aleft	combinv asig, krvt, ilpt
	outs   aleft, 0*asig

endin

</CsInstruments>
<CsScore>
f1 0 4096 10 1
i 1 0 .25 20 2000
e

</CsScore>
</CsoundSynthesizer>
